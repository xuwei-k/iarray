package iarray

import annotation.tailrec
import scalaz._
import java.util.Arrays
import Arrays.{copyOf, copyOfRange}
import collection.generic.CanBuildFrom
import collection.mutable.ArrayBuilder
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import InlineUtil.inlineAndReset

object IArray extends IArrayFunctions{

  def map[A: c.WeakTypeTag, B](c: Context)(f: c.Expr[A => B]): c.Expr[IArray[B]] = {
    import c.universe._
    val A = c.weakTypeOf[A]
    val f0 = inlineAndReset(c)(q"$f(self(i).asInstanceOf[$A])")
    val tree = q"""
      val self = ${c.prefix}.self
      var i = 0
      val array = new Array[AnyRef](self.length)
      while(i < self.length){
        array(i) = $f0.asInstanceOf[AnyRef]
        i += 1
      }
      new _root_.iarray.IArray(array)
    """
    c.Expr[IArray[B]](tree)
  }

}

final class IArray[A](val self: Array[AnyRef]) extends AnyVal{
  import IArray._

  @inline def apply(i: Int): A =
    self(i).asInstanceOf[A]

  @inline def length: Int =
    self.length

  @inline def size: Int =
    self.length

  private[iarray] def unsafeMaxBy[B](f: A => B)(implicit O: scalaz.Order[B]): A = {
    var maxF = f(self(0).asInstanceOf[A])
    var maxElem = self(0).asInstanceOf[A]
    var i = 1
    while(i < self.length){
      val fx = f(self(i).asInstanceOf[A])
      if (O.greaterThan(fx, maxF)) {
        maxElem = self(i).asInstanceOf[A]
        maxF = fx
      }
      i += 1
    }
    maxElem
  }

  def maxBy[B](f: A => B)(implicit O: scalaz.Order[B]): Option[A] =
    if(self.length == 0){
      None
    }else{
      Some(unsafeMaxBy(f))
    }

  private[iarray] def unsafeMinBy[B](f: A => B)(implicit O: scalaz.Order[B]): A = {
    var minF = f(self(0).asInstanceOf[A])
    var minElem = self(0).asInstanceOf[A]
    var i = 1
    while(i < self.length){
      val fx = f(self(i).asInstanceOf[A])
      if (O.lessThan(fx, minF)) {
        minElem = self(i).asInstanceOf[A]
        minF = fx
      }
      i += 1
    }
    minElem
  }

  def minBy[B](f: A => B)(implicit O: scalaz.Order[B]): Option[A] =
    if(self.length == 0){
      None
    }else{
      Some(unsafeMinBy(f))
    }

  private[iarray] def unsafeMax(implicit O: Order[A]): A = {
    var i = 1
    var a = self(0).asInstanceOf[A]
    while(i < length){
      a = O.max(a, self(i).asInstanceOf[A])
      i += 1
    }
    a
  }

  def max(implicit O: Order[A]): Option[A] =
    if(isEmpty) None
    else Some(unsafeMax)

  private[iarray] def unsafeMin(implicit O: Order[A]): A = {
    var i = 1
    var a = self(0).asInstanceOf[A]
    while(i < length){
      a = O.min(a, self(i).asInstanceOf[A])
      i += 1
    }
    a
  }

  def min(implicit O: Order[A]): Option[A] =
    if(isEmpty) None
    else Some(unsafeMin)

  private[iarray] def unsafeMaxOf[B](f: A => B)(implicit O: Order[B]): B = {
    var a = f(self(0).asInstanceOf[A])
    var i = 1
    while(i < self.length){
      a = O.max(a, f(self(i).asInstanceOf[A]))
      i += 1
    }
    a
  }

  def maxOf[B](f: A => B)(implicit O: Order[B]): Option[B] =
    if(self.length == 0) None
    else Some(unsafeMaxOf(f))

  private[iarray] def unsafeMinOf[B](f: A => B)(implicit O: Order[B]): B = {
    var a = f(self(0).asInstanceOf[A])
    var i = 1
    while(i < self.length){
      a = O.min(a, f(self(i).asInstanceOf[A]))
      i += 1
    }
    a
  }

  def minOf[B](f: A => B)(implicit O: Order[B]): Option[B] =
    if(self.length == 0) None
    else Some(unsafeMinOf(f))

  def find(f: A => Boolean): Option[A] = {
    var i = 0
    while(i < self.length){
      if(f(self(i).asInstanceOf[A])){
        return Some(self(i).asInstanceOf[A])
      }
      i += 1
    }
    None
  }

  def findRight(f: A => Boolean): Option[A] = {
    var i = self.length - 1
    while(0 <= i){
      if(f(self(i).asInstanceOf[A])){
        return Some(self(i).asInstanceOf[A])
      }
      i -= 1
    }
    None
  }

  def exists(f: A => Boolean): Boolean = {
    var i = 0
    while(i < self.length){
      if(f(self(i).asInstanceOf[A])){
        return true
      }
      i += 1
    }
    false
  }

  def forall(f: A => Boolean): Boolean =
    indexNot(f) < 0

  def toIterator: Iterator[A] =
    new IArrayIterator[A](self)

  def toIList: IList[A] = {
    var i = self.length - 1
    var acc = IList.empty[A]
    while(0 <= i){
      acc = ICons(self(i).asInstanceOf[A], acc)
      i -= 1
    }
    acc
  }

  def reverseList: List[A] = {
    var i = 0
    var acc: List[A] = Nil
    while(i < self.length){
      acc = new ::(self(i).asInstanceOf[A], acc)
      i += 1
    }
    acc
  }

  def reverseIList: IList[A] = {
    var i = 0
    var acc = IList.empty[A]
    while(i < self.length){
      acc = ICons(self(i).asInstanceOf[A], acc)
      i += 1
    }
    acc
  }

  def reverseArray(implicit A: reflect.ClassTag[A]): Array[A] = {
    val array = new Array[A](self.length)
    var i = 0
    while(i < self.length){
      array(self.length - i - 1) = self(i).asInstanceOf[A]
      i += 1
    }
    array
  }

  def toList: List[A] = {
    var i = self.length - 1
    var acc: List[A] = Nil
    while(0 <= i){
      acc = new ::(self(i).asInstanceOf[A], acc)
      i -= 1
    }
    acc
  }

  def toNel: Option[NonEmptyList[A]] =
    if(isEmpty)
      None
    else
      Some(NonEmptyList.nel(self(0).asInstanceOf[A], toList.tail))

  def toIArray1: Option[IArray1[A]] =
    if(self.length == 0)
      None
    else
      Some(IArray1(self(0).asInstanceOf[A], new IArray[A](copyOfRange(self, 1, self.length))))

  def zipperEnd: Option[Zipper[A]] =
    if(isEmpty){
      None
    }else{
      var i = 0
      var acc: Stream[A] = Stream.Empty
      val len = self.length - 1
      while(i < len){
        val x = acc
        acc = new Stream.Cons(self(i).asInstanceOf[A], x)
        i += 1
      }
      Some(Zipper(acc, self(len).asInstanceOf[A], Stream.Empty))
    }

  def oneAnd: Option[OneAnd[IArray, A]] =
    if(isEmpty)
      None
    else
      Some(OneAnd(self(0).asInstanceOf[A], dropL(1)))

  def toArray(implicit A: reflect.ClassTag[A]): Array[A] =
    if(A.runtimeClass.isPrimitive){
      val array = new Array[A](self.length)
      var i = 0
      while(i < self.length){
        array(i) = self(i).asInstanceOf[A]
        i += 1
      }
      array
    }else{
      copyOf(self, self.length).asInstanceOf[Array[A]]
    }

  @inline def isEmpty: Boolean =
    self.length == 0

  @inline def nonEmpty: Boolean =
    self.length != 0

  // unsafe
  @inline private[iarray] def head: A =
    self(0).asInstanceOf[A]

  // unsafe
  @inline private[iarray] def tail: IArray[A] =
    dropL(1)

  def headOption: Option[A] =
    if(isEmpty) None
    else Some(this(0))

  @inline private[iarray] def unsafeLast: A =
    self(self.length - 1).asInstanceOf[A]

  def lastOption: Option[A] =
    if(isEmpty) None
    else Some(this(length - 1))

  def tailOption: Option[IArray[A]] =
    if(isEmpty) None
    else Some(dropL(1))

  def initOption: Option[IArray[A]] =
    if(isEmpty) None
    else Some(dropR(1))

  def slice(from: Int, until: Int): IArray[A] = {
    if(until <= from || until <= 0 || from >= self.length){
      empty[A]
    }else if(from <= 0 && self.length <= until){
      this
    }else{
      new IArray(copyOfRange(self, Math.max(from, 0), Math.min(until, self.length)))
    }
  }

  def reverse: IArray[A] = {
    var i = 0
    val len = self.length
    val array = new Array[AnyRef](len)
    while(i < len){
      array(len - i - 1) = self(i)
      i += 1
    }
    new IArray(array)
  }

  def reverse_:::(prefix: IArray[A]): IArray[A] =
    if(prefix.length == 0){
      this
    }else{
      val array = new Array[AnyRef](self.length + prefix.length)
      var i = 0
      val len = prefix.length
      while(i < len){
        array(i) = prefix.self(len - i - 1)
        i += 1
      }
      System.arraycopy(self, 0, array, len, self.length)
      new IArray[A](array)
    }

  def count(f: A => Boolean): Int = {
    var i = 0
    var n = 0
    while(i < self.length){
      if(f(self(i).asInstanceOf[A])){
        n += 1
      }
      i += 1
    }
    n
  }

  def splitAt(n: Int): (IArray[A], IArray[A]) =
    if(n <= 0){
      (empty[A], this)
    }else if(n >= self.length){
      (this, empty[A])
    }else{
      (new IArray[A](copyOf(self, n)), new IArray(copyOfRange(self, n, self.length)))
    }

  def takeWhileL(f: A => Boolean): IArray[A] = {
    val len = indexNot(f)
    if(len < 0){
      this
    }else if(len == 0){
      empty[A]
    }else{
      new IArray(copyOf(self, len))
    }
  }

  def takeWhileR(f: A => Boolean): IArray[A] = {
    val len = lastIndex(f) + 1
    if(len <= 0){
      this
    }else if(len == self.length){
      empty[A]
    }else{
      new IArray(copyOfRange(self, len, self.length))
    }
  }

  def dropWhileL(f: A => Boolean): IArray[A] = {
    val len = indexNot(f)
    if(len < 0){
      empty[A]
    }else if(len == 0){
      this
    }else{
      new IArray(copyOfRange(self, len, self.length))
    }
  }

  def dropWhileR(f: A => Boolean): IArray[A] = {
    val len = lastIndex(f) + 1
    if(len <= 0){
      empty[A]
    }else if(len == self.length){
      this
    }else{
      new IArray(copyOf(self, len))
    }
  }

  def takeR(n: Int): IArray[A] =
    if(n <= 0) empty[A]
    else if(n >= length) this
    else new IArray(copyOfRange(self, length - n, length))

  def takeL(n: Int): IArray[A] =
    if(n <= 0) empty[A]
    else if(n >= length) this
    else new IArray(copyOf(self, n))

  def align[B](b: IArray[B]): IArray[A \&/ B] =
    alignWith(b)(conform)

  def alignWith[B, C](that: IArray[B])(f: A \&/ B => C): IArray[C] = {
    import \&/._
    val max = Math.max(length, that.length)
    val min = Math.min(length, that.length)
    var i = 0
    val array = new Array[AnyRef](max)
    while(i < min){
      array(i) = f(Both(self(i).asInstanceOf[A], that(i))).asInstanceOf[AnyRef]
      i += 1
    }
    if(this.length > that.length){
      while(i < max){
        array(i) = f(This(self(i).asInstanceOf[A])).asInstanceOf[AnyRef]
        i += 1
      }
    }else if(this.length < that.length){
      while(i < max){
        array(i) = f(That(that(i))).asInstanceOf[AnyRef]
        i += 1
      }
    }
    new IArray[C](array)
  }

  def withIndex: WithIndex[A] =
    new WithIndex(self)

  def zipWithIndex: IArray[(A, Int)] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = (self(i), i)
      i += 1
    }
    new IArray[(A, Int)](array)
  }

  def zipWith[B, C](that: IArray[B])(f: (A, B) => C): IArray[C] = {
    val len = Math.min(length, that.length)
    var i = 0
    val array = new Array[AnyRef](len)
    val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
    while(i < len){
      array(i) = f0(self(i), that.self(i))
      i += 1
    }
    new IArray(array)
  }

  def zipAll[B](that: IArray[B], a: A, b: B): IArray[(A, B)] = {
    val len = Math.max(self.length, that.length)
    val min = Math.min(self.length, that.length)
    val array = new Array[AnyRef](len)
    var i = 0
    while(i < min){
      array(i) = (self(i).asInstanceOf[A], that.self(i).asInstanceOf[B])
      i += 1
    }
    if(min == self.length){
      while(i < len){
        array(i) = (a, that.self(i).asInstanceOf[B])
        i += 1
      }
    }else{
      while(i < len){
        array(i) = (self(i).asInstanceOf[A], b)
        i += 1
      }
    }
    new IArray(array)
  }

  def zip[B](that: IArray[B]): IArray[(A, B)] = {
    val len = Math.min(length, that.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = (self(i).asInstanceOf[A], that(i))
      i += 1
    }
    new IArray[(A, B)](array)
  }

  def unzip[B, C](implicit e: A <:< Product2[B, C]): (IArray[B], IArray[C]) = {
    var i = 0
    val _1, _2 = new Array[AnyRef](self.length)
    while(i < self.length){
      val x = self(i).asInstanceOf[Product2[AnyRef, AnyRef]]
      _1(i) = x._1
      _2(i) = x._2
      i += 1
    }
    (new IArray(_1), new IArray(_2))
  }

  def unzip2[B, C](implicit e: A <:< Product2[B, C]): (IArray[B], IArray[C]) = unzip[B, C]

  def unzip3[B, C, D](implicit e: A <:< Product3[B, C, D]): (IArray[B], IArray[C], IArray[D]) = {
    var i = 0
    val _1, _2, _3 = new Array[AnyRef](self.length)
    while(i < self.length){
      val x = self(i).asInstanceOf[Product3[AnyRef, AnyRef, AnyRef]]
      _1(i) = x._1
      _2(i) = x._2
      _3(i) = x._3
      i += 1
    }
    (new IArray(_1), new IArray(_2), new IArray(_3))
  }

  def unzip4[B, C, D, E](implicit e: A <:< Product4[B, C, D, E]): (IArray[B], IArray[C], IArray[D], IArray[E]) = {
    var i = 0
    val _1, _2, _3, _4 = new Array[AnyRef](self.length)
    while(i < self.length){
      val x = self(i).asInstanceOf[Product4[AnyRef, AnyRef, AnyRef, AnyRef]]
      _1(i) = x._1
      _2(i) = x._2
      _3(i) = x._3
      _4(i) = x._4
      i += 1
    }
    (new IArray(_1), new IArray(_2), new IArray(_3), new IArray(_4))
  }

  def unzip5[B, C, D, E, F](implicit e: A <:< Product5[B, C, D, E, F]): (IArray[B], IArray[C], IArray[D], IArray[E], IArray[F]) = {
    var i = 0
    val _1, _2, _3, _4, _5 = new Array[AnyRef](self.length)
    while(i < self.length){
      val x = self(i).asInstanceOf[Product5[AnyRef, AnyRef, AnyRef, AnyRef, AnyRef]]
      _1(i) = x._1
      _2(i) = x._2
      _3(i) = x._3
      _4(i) = x._4
      _5(i) = x._5
      i += 1
    }
    (new IArray(_1), new IArray(_2), new IArray(_3), new IArray(_4), new IArray(_5))
  }

  def firsts[B, C](implicit e: A <:< Product2[B, C]): IArray[B] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = self(i).asInstanceOf[Product2[AnyRef, AnyRef]]._1
      i += 1
    }
    new IArray(array)
  }

  def seconds[B, C](implicit e: A <:< Product2[B, C]): IArray[C] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = self(i).asInstanceOf[Product2[AnyRef, AnyRef]]._2
      i += 1
    }
    new IArray(array)
  }

  def reversed[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    var i = self.length - 1
    while(i >= 0){
      buf += self(i).asInstanceOf[A]
      i -= 1
    }
    buf.result
  }

  def to[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    var i = 0
    while(i < self.length){
      buf += self(i).asInstanceOf[A]
      i += 1
    }
    buf.result
  }

  def toOneAnd[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): Option[OneAnd[F, A]] =
    if(isEmpty){
      None
    }else{
      val buf = C()
      var i = 1
      while(i < self.length){
        buf += self(i).asInstanceOf[A]
        i += 1
      }
      Some(OneAnd(self(0).asInstanceOf[A], buf.result))
    }

  def filter(f: A => Boolean): IArray[A] = {
    val buf = new ArrayBuilder.ofRef[AnyRef]
    var i = 0
    while(i < self.length){
      if(f(this(i))){
        buf += self(i)
      }
      i += 1
    }
    new IArray[A](buf.result)
  }

  def withFilter(f: A => Boolean): WithFilter[A] =
    new WithFilter[A](self, f)

  def mapTo[C, B](f: A => B)(implicit C: CanBuildFrom[Nothing, B, C]): C = {
    val buf = C()
    var i = 0
    while(i < self.length){
      buf += f(self(i).asInstanceOf[A])
      i += 1
    }
    buf.result
  }

  def map_[B](f: A => B): IArray[B] =
    macro IArray.map[A, B]

  def map[B](f: A => B): IArray[B] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = f(self(i).asInstanceOf[A]).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  def foreach[U](f: A => U): Unit = {
    var i = 0
    while(i < self.length){
      f(self(i).asInstanceOf[A])
      i += 1
    }
  }

  def contains(a: A)(implicit A: Equal[A]): Boolean = {
    var i = 0
    while(i < self.length){
      if(A.equal(this(i), a)){
        return true
      }
      i += 1
    }
    false
  }

  def dropL(n: Int): IArray[A] = {
    if(n <= 0){
      this
    }else if(n >= self.length){
      empty[A]
    }else{
      new IArray(copyOfRange(self, n, self.length))
    }
  }

  def dropR(n: Int): IArray[A] = {
    if(n <= 0){
      this
    }else if(n >= self.length){
      empty[A]
    }else{
      new IArray(copyOf(self, self.length - n))
    }
  }

  def +:(a: A): IArray[A] = {
    val array = new Array[AnyRef](self.length + 1)
    System.arraycopy(self, 0, array, 1, self.length)
    array(0) = a.asInstanceOf[AnyRef]
    new IArray(array)
  }

  def :+(a: A): IArray[A] = {
    val array = new Array[AnyRef](self.length + 1)
    System.arraycopy(self, 0, array, 0, self.length)
    array(self.length) = a.asInstanceOf[AnyRef]
    new IArray(array)
  }

  def ++(that: IArray[A]): IArray[A] = {
    if(self.length == 0){
      that
    }else if(that.length == 0){
      this
    }else{
      val size1 = self.length
      val size2 = that.length
      val array = new Array[AnyRef](size1 + size2)
      System.arraycopy(self, 0, array, 0, size1)
      System.arraycopy(that.self, 0, array, size1, size2)
      new IArray(array)
    }
  }

  def cobind[B](f: IArray[A] => B): IArray[B] = {
    if(isEmpty) empty
    else{
      val array = new Array[AnyRef](self.length)
      array(0) = f(this).asInstanceOf[AnyRef]
      var i = 1
      while(i < self.length){
        array(i) = f(new IArray(copyOfRange(self, i, self.length))).asInstanceOf[AnyRef]
        i += 1
      }
      new IArray(array)
    }
  }

  def cojoin: IArray[IArray[A]] = {
    if(isEmpty) empty
    else{
      var i = 1
      val array = new Array[AnyRef](self.length)
      array(0) = this.asInstanceOf[AnyRef]
      while(i < self.length){
        array(i) = (new IArray[A](copyOfRange(self, i, self.length))).asInstanceOf[AnyRef]
        i += 1
      }
      new IArray(array)
    }
  }

  def mapAccumL[S, B](z: S)(f: (S, A) => (S, B)): (S, IArray[B]) = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    var acc = z
    while(i < self.length){
      val x = f(acc, self(i).asInstanceOf[A])
      acc = x._1
      array(i) = x._2.asInstanceOf[AnyRef]
      i += 1
    }
    (acc, new IArray(array))
  }

  def mapAccumR[S, B](z: S)(f: (S, A) => (S, B)): (S, IArray[B]) = {
    var i = self.length - 1
    val array = new Array[AnyRef](self.length)
    var acc = z
    while(0 <= i){
      val x = f(acc, self(i).asInstanceOf[A])
      acc = x._1
      array(i) = x._2.asInstanceOf[AnyRef]
      i -= 1
    }
    (acc, new IArray(array))
  }

  def fold(implicit A: Monoid[A]): A = {
    var i = 0
    var acc = A.zero
    while(i < self.length){
      acc = A.append(acc, self(i).asInstanceOf[A])
      i += 1
    }
    acc
  }

  def fold1Opt(implicit A: Semigroup[A]): Option[A] =
    if(isEmpty){
      None
    }else{
      var acc: A = self(0).asInstanceOf[A]
      var i = 1
      while(i < self.length){
        acc = A.append(acc, self(i).asInstanceOf[A])
        i += 1
      }
      Some(acc)
    }

  // unsafe
  private[iarray] def foldMap1[B](f: A => B)(implicit B: Semigroup[B]): B = {
    var acc = f(self(0).asInstanceOf[A])
    var i = 1
    while(i < self.length){
      acc = B.append(acc, f(self(i).asInstanceOf[A]))
      i += 1
    }
    acc
  }

  def foldMap1Opt[B](f: A => B)(implicit B: Semigroup[B]): Option[B] =
    if(isEmpty){
      None
    }else{
      Some(foldMap1(f))
    }

  def flatMap[B](f: A => IArray[B]): IArray[B] = {
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    var i = 0
    while(i < self.length){
      val x = f(this(i))
      var j = 0
      while(j < x.length){
        builder += x(j).asInstanceOf[AnyRef]
        j += 1
      }
      i += 1
    }
    new IArray(builder.result)
  }

  // unsafe
  private[iarray] def foldMapR1[B](z: A => B)(f: (A, B) => B): B = {
    var acc = z(self(self.length - 1).asInstanceOf[A])
    var i = self.length - 2
    while(i >= 0){
      acc = f(self(i).asInstanceOf[A], acc)
      i -= 1
    }
    acc
  }

  def foldMapR1Opt[B](z: A => B)(f: (A, B) => B): Option[B] =
    if(self.length == 0){
      None
    }else{
      Some(foldMapR1(z)(f))
    }

  def foldMapL1[B](z: A => B)(f: (B, A) => B): Option[B] =
    if(self.length == 0){
      None
    } else {
      var acc = z(self(0).asInstanceOf[A])
      var i = 1
      while(i < self.length){
        acc = f(acc, self(i).asInstanceOf[A])
        i += 1
      }
      Some(acc)
    }

  def foldl1(f: (A, A) => A): Option[A] =
    if(isEmpty) None
    else{
      var i = 1
      var acc = self(0).asInstanceOf[A]
      while(i < self.length){
        acc = f(acc, self(i).asInstanceOf[A])
        i += 1
      }
      Some(acc)
    }

  def foldl[B](z: B)(f: (B, A) => B): B = {
    var i = 0
    var acc = z
    while(i < self.length){
      acc = f(acc, this(i))
      i += 1
    }
    acc
  }

  def foldMap[B](f: A => B)(implicit B: Monoid[B]): B = {
    var i = 0
    var acc = B.zero
    val f0 = f.asInstanceOf[AnyRef => B]
    while(i < self.length){
      acc = B.append(acc, f0(self(i)))
      i += 1
    }
    acc
  }

  def foldr[B](z: B)(f: (A, B) => B): B = {
    var i = self.length - 1
    var acc = z
    while(i >= 0){
      acc = f(this(i), acc)
      i -= 1
    }
    acc
  }

  def foldr1(f: (A, A) => A): Option[A] =
    if(isEmpty) None
    else{
      var i = self.length - 2
      var acc = self(self.length - 1).asInstanceOf[A]
      while(i >= 0){
        acc = f(self(i).asInstanceOf[A], acc)
        i -= 1
      }
      Some(acc)
    }

  def scanLeft[B](z: B)(f: (B, A) => B): IArray[B] = {
    val array = new Array[AnyRef](self.length + 1)
    array(0) = z.asInstanceOf[AnyRef]
    var i = 0
    val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
    while(i < self.length){
      array(i + 1) = f0(array(i), self(i))
      i += 1
    }
    new IArray(array)
  }

  def scanRight[B](z: B)(f: (A, B) => B): IArray[B] = {
    val array = new Array[AnyRef](self.length + 1)
    array(self.length) = z.asInstanceOf[AnyRef]
    var i = self.length
    val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
    while(i > 0){
      array(i - 1) = f0(self(i - 1), array(i))
      i -= 1
    }
    new IArray(array)
  }

  def scanLeft1(f: (A, A) => A): IArray[A] =
    if(self.length != 0){
      val array = new Array[AnyRef](self.length)
      array(0) = self(0).asInstanceOf[AnyRef]
      var i = 0
      val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
      val len = self.length - 1
      while(i < len){
        array(i + 1) = f0(array(i), self(i + 1))
        i += 1
      }
      new IArray(array)
    }else empty

  def scanRight1(f: (A, A) => A): IArray[A] =
    if(self.length != 0){
      val array = new Array[AnyRef](self.length)
      array(self.length - 1) = self(self.length - 1).asInstanceOf[AnyRef]
      var i = self.length - 1
      val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
      while(i > 0){
        array(i - 1) = f0(self(i - 1), array(i))
        i -= 1
      }
      new IArray(array)
    }else empty

  def startsWith(that: IArray[A], offset: Int = 0)(implicit A: Equal[A]): Boolean = {
    require(offset >= 0, "offset = " + offset  + " is invalid. offset must be positive")
    var i = offset
    var j = 0
    val thisLen = self.length
    val thatLen = that.length
    while (i < thisLen && j < thatLen && A.equal(self(i).asInstanceOf[A], that(j))) {
      i += 1
      j += 1
    }
    j == thatLen
  }

  def endsWith(that: IArray[A])(implicit A: Equal[A]): Boolean = {
    var i = length - 1
    var j = that.length - 1

    (j <= i) && {
      while (j >= 0){
        if(false == A.equal(self(i).asInstanceOf[A], that(j))){
          return false
        }
        i -= 1
        j -= 1
      }
      true
    }
  }

  def span(f: A => Boolean): (IArray[A], IArray[A]) = {
    val n = indexNot(f)
    if(n < 0){
      (this, empty[A])
    }else if(n >= self.length){
      (empty[A], this)
    }else{
      (new IArray(copyOf(self, n)), new IArray(copyOfRange(self, n, self.length )))
    }
  }

  def partition(f: A => Boolean): (IArray[A], IArray[A]) = {
    val l, r = new ArrayBuilder.ofRef[AnyRef]()
    var i = 0
    val f0 = f.asInstanceOf[AnyRef => Boolean]
    while(i < self.length){
      if(f0(self(i))){
        l += self(i)
      }else{
        r += self(i)
      }
      i += 1
    }
    (new IArray(l.result), new IArray(r.result))
  }

  @throws[IndexOutOfBoundsException]
  def updated(index: Int, elem: A): IArray[A] = {
    val array = self.clone
    array(index) = elem.asInstanceOf[AnyRef]
    new IArray(array)
  }

  def collectBy[B](implicit B: reflect.ClassTag[B]): IArray[B] = {
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    var i = 0
    while(i < self.length){
      if(B.runtimeClass.isInstance(self(i))){
        builder += self(i)
      }
      i += 1
    }
    new IArray[B](builder.result)
  }

  def collect[B](f: PartialFunction[A, B]): IArray[B] = {
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    var i = 0
    val f0 = f.asInstanceOf[PartialFunction[AnyRef, AnyRef]].runWith(builder += _)
    while(i < self.length){
      f0(self(i))
      i += 1
    }
    new IArray[B](builder.result)
  }

  def reverseMap[B](f: A => B): IArray[B] = {
    val len = self.length
    val array = new Array[AnyRef](len)
    var i = 0
    val f0 = f.asInstanceOf[AnyRef => AnyRef]
    while(i < len){
      array(len - i - 1) = f0(self(i))
      i += 1
    }
    new IArray(array)
  }

  def collectFirst[B](f: PartialFunction[A, B]): Option[B] = {
    var i = 0
    val f0 = f.asInstanceOf[PartialFunction[AnyRef, B]]
    while(i < self.length){
      if(f0 isDefinedAt self(i)){
        return Some(f0(self(i)))
      }
      i += 1
    }
    None
  }

  def collectLast[B](f: PartialFunction[A, B]): Option[B] = {
    var i = self.length - 1
    val f0 = f.asInstanceOf[PartialFunction[AnyRef, B]]
    while(0 <= i){
      if(f0 isDefinedAt self(i)){
        return Some(f0(self(i)))
      }
      i -= 1
    }
    None
  }

  def indexOfL(a: A)(implicit E: Equal[A]): Option[Int] = {
    var i = 0
    while(i < self.length){
      if(E.equal(a, this(i))){
        return Some(i)
      }
      i += 1
    }
    None
  }

  def indexOfR(a: A)(implicit E: Equal[A]): Option[Int] = {
    var i = self.length - 1
    while(0 <= i){
      if(E.equal(a, this(i))){
        return Some(i)
      }
      i -= 1
    }
    None
  }

  def sum(implicit A: Numeric[A]): A = {
    var i = 0
    var acc: A = A.zero
    while(i < self.length){
      acc = A.plus(acc, self(i).asInstanceOf[A])
      i += 1
    }
    acc
  }

  def sorted(implicit O: Order[A]): IArray[A] =
    sort0(O.toScalaOrdering)

  def sortWith(f: (A, A) => Boolean): IArray[A] =
    sort0(comparatorFromFunction(f))

  def sortBy[B](f: A => B)(implicit O: Order[B]): IArray[A] =
    sort0((O contramap f).toScalaOrdering)

  private def sort0[B](c: java.util.Comparator[B]): IArray[B] = {
    val array = self.clone
    Arrays.sort(array, c.asInstanceOf[java.util.Comparator[AnyRef]])
    new IArray(array)
  }

  def flatten[B](implicit A: A <:< IArray[B]): IArray[B] = {
    var i = 0
    var n = 0
    val length = self.length
    while(i < length){
      n += self(i).asInstanceOf[IArray[B]].length
      i += 1
    }
    val array = new Array[AnyRef](n)
    i = 0
    n = 0
    while(i < length){
      val elem = self(i).asInstanceOf[IArray[B]].self
      System.arraycopy(elem, 0, array, n, elem.length)
      n += elem.length
      i += 1
    }
    new IArray(array)
  }

  def groupBy1[B](f: A => B)(implicit O: Order[B]): B ==>> IArray1[A] =
    foldl(==>>.empty[B, OneAnd[List, A]]) { (m, a) =>
      m.alter(f(a), {
        case Some(OneAnd(h, t)) => Some(OneAnd(a, h :: t))
        case None => Some(OneAnd[List, A](a, Nil))
      })
    }.map{ case OneAnd(h, t) =>
      if(t.isEmpty){
        IArray1(h, empty[A])
      }else{
        val len = t.size
        val array = new Array[AnyRef](len)
        @tailrec
        def go(i: Int, list: List[A]): IArray1[A] = (list: @unchecked) match {
          case a :: last :: Nil =>
            array(i) = a.asInstanceOf[AnyRef]
            IArray1(last, new IArray[A](array))
          case a :: tail =>
            array(i) = a.asInstanceOf[AnyRef]
            go(i - 1, tail)
        }
        go(len - 1, h :: t)
      }
    }

  def intercalate(a: A)(implicit A: Monoid[A]): A =
    if(isEmpty){
      A.zero
    }else{
      var i = 1
      var acc = self(0).asInstanceOf[A]
      while(i < self.length){
        acc = A.append(A.append(acc, a), self(i).asInstanceOf[A])
        i += 1
      }
      acc
    }

  def intercalate1Opt(a: A)(implicit A: Semigroup[A]): Option[A] =
    if(isEmpty){
      None
    }else{
      var i = 1
      var acc = self(0).asInstanceOf[A]
      while(i < self.length){
        acc = A.append(A.append(acc, a), self(i).asInstanceOf[A])
        i += 1
      }
      Some(acc)
    }

  def intersperse(a: A): IArray[A] =
    if(isEmpty){
      empty
    }else{
      val array = new Array[AnyRef]((self.length * 2) - 1)
      var i = 0
      java.util.Arrays.fill(array, a)
      while(i < self.length){
        array(i * 2) = self(i)
        i += 1
      }
      new IArray(array)
    }

  override def toString: String =
    mkString("IArray(",", ",")")

  def mkString(sep: String = ""): String =
    mkString("", sep, "")

  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b append start

    if(self.length == 0){
      b.append(end)
    }else{
      b append self(0)
      var i = 1
      while(i < self.length){
        b append sep
        b append self(i)
        i += 1
      }
      b.append(end)
    }
  }

  def ===(that: IArray[A])(implicit A: Equal[A]): Boolean =
    (self.length == that.length) && {
      var i = 0
      while(i < self.length){
        if(! A.equal(that(i), this(i))){
          return false
        }
        i += 1
      }
      true
    }

  def widen[B](implicit ev: A <:< B): IArray[B] =
    this.asInstanceOf[IArray[B]]

  @inline private def indexNot(f: A => Boolean): Int = {
    var i = 0
    while(i < self.length){
      if(! f(this(i))){
        return i
      }
      i += 1
    }
    -1
  }

  @inline private def lastIndex(f: A => Boolean): Int = {
    var i = self.length - 1
    while(0 <= i){
      if(!f(this(i))){
        return i
      }
      i -= 1
    }
    -1
  }

}

