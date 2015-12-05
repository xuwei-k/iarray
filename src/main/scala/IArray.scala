package iarray

import annotation.tailrec
import scalaz._
import java.util.Arrays
import Arrays.{copyOf, copyOfRange}
import collection.generic.CanBuildFrom
import collection.mutable.ArrayBuilder

object IArray extends IArrayFunctions{

}

final class IArray[A] private[iarray](private[iarray] val self: Array[AnyRef]) extends AnyVal{
  import IArray._

  /**
   * @example{{{
   * scala> val x = IArray(10, 20, 30, 40)
   * scala> x(2)
   * res0: Int = 30
   * }}}
   */
  @inline def apply(i: Int): A =
    self(i).asInstanceOf[A]

  /**
   * @example{{{
   * scala> IArray("a", "b").length
   * res0: Int = 2
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray("aa", "bbb", "c").maxBy(_.size)
   * res0: Option[String] = Some(bbb)
   *
   * scala> IArray[String]().maxBy(_.size)
   * res1: Option[String] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray("aa", "bbb", "c").minBy(_.size)
   * res0: Option[String] = Some(c)
   *
   * scala> IArray[String]().minBy(_.size)
   * res1: Option[String] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(20, 30, 10).max
   * res0: Option[Int] = Some(30)
   *
   * scala> IArray[Int]().max
   * res1: Option[Int] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(20, 30, 10).min
   * res0: Option[Int] = Some(10)
   *
   * scala> IArray[Int]().min
   * res1: Option[Int] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(20, 30, 10).maxOf(- _)
   * res0: Option[Int] = Some(-10)
   *
   * scala> IArray[Int]().maxOf(- _)
   * res1: Option[Int] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(20, 30, 10).minOf(- _)
   * res0: Option[Int] = Some(-30)
   *
   * scala> IArray[Int]().minOf(- _)
   * res1: Option[Int] = None
   * }}}
   */
  def minOf[B](f: A => B)(implicit O: Order[B]): Option[B] =
    if(self.length == 0) None
    else Some(unsafeMinOf(f))

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5).find(_ > 3)
   * res0: Option[Int] = Some(4)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5).findRight(_ < 4)
   * res0: Option[Int] = Some(3)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> val a = IArray(1, 2, 3, 4)
   * scala> a.exists(_ % 3 == 0)
   * res0: Boolean = true
   *
   * scala> a.exists(_ <= 0)
   * res1: Boolean = false
   * }}}
   */
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

  /**
   * @example{{{
   * scala> val a = IArray(1, 2, 3, 4)
   * scala> a.forall(_ <= 4)
   * res0: Boolean = true
   *
   * scala> a.forall(_ % 4 < 3)
   * res1: Boolean = false
   * }}}
   */
  def forall(f: A => Boolean): Boolean =
    indexNot(f) < 0

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5).toIterator.filter(_ % 2 == 0).toList
   * res0: List[Int] = List(2, 4)
   * }}}
   */
  def toIterator: Iterator[A] =
    new IArrayIterator[A](self)

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).toIList
   * res0: scalaz.IList[Int] = [1,2,3]
   * }}}
   */
  def toIList: IList[A] = {
    var i = self.length - 1
    var acc = IList.empty[A]
    while(0 <= i){
      acc = ICons(self(i).asInstanceOf[A], acc)
      i -= 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).reverseList
   * res0: List[Int] = List(3, 2, 1)
   * }}}
   */
  def reverseList: List[A] = {
    var i = 0
    var acc: List[A] = Nil
    while(i < self.length){
      acc = new ::(self(i).asInstanceOf[A], acc)
      i += 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).reverseIList
   * res0: scalaz.IList[Int] = [3,2,1]
   * }}}
   */
  def reverseIList: IList[A] = {
    var i = 0
    var acc = IList.empty[A]
    while(i < self.length){
      acc = ICons(self(i).asInstanceOf[A], acc)
      i += 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).reverseArray
   * res0: Array[Int] = Array(3, 2, 1)
   * }}}
   */
  def reverseArray(implicit A: reflect.ClassTag[A]): Array[A] = {
    val array = new Array[A](self.length)
    var i = 0
    while(i < self.length){
      array(self.length - i - 1) = self(i).asInstanceOf[A]
      i += 1
    }
    array
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).toList
   * res0: List[Int] = List(1, 2, 3)
   * }}}
   */
  def toList: List[A] = {
    var i = self.length - 1
    var acc: List[A] = Nil
    while(0 <= i){
      acc = new ::(self(i).asInstanceOf[A], acc)
      i -= 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> import scalaz.NonEmptyList
   * scala> IArray(1, 2, 3).toNel
   * res0: Option[NonEmptyList[Int]] = Some(NonEmpty[1,2,3])
   *
   * scala> IArray[Int]().toNel
   * res1: Option[NonEmptyList[Int]] = None
   * }}}
   */
  def toNel: Option[NonEmptyList[A]] =
    if(isEmpty)
      None
    else
      Some(NonEmptyList.nel(self(0).asInstanceOf[A], toIList.tailOption.get))

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).toIArray1
   * res0: Option[IArray1[Int]] = Some(IArray1(1, 2, 3))
   *
   * scala> IArray[Int]().toIArray1
   * res1: Option[IArray1[Int]] = None
   * }}}
   */
  def toIArray1: Option[IArray1[A]] =
    if(self.length == 0)
      None
    else
      Some(IArray1(self(0).asInstanceOf[A], new IArray[A](copyOfRange(self, 1, self.length))))

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).zipperEnd.map(_.modify(_ + 10).toStream.toList)
   * res0: Option[List[Int]] = Some(List(1, 2, 13))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).oneAnd
   * res0: Option[scalaz.OneAnd[IArray, Int]] = Some(OneAnd(1,IArray(2, 3)))
   * }}}
   */
  def oneAnd: Option[OneAnd[IArray, A]] =
    if(isEmpty)
      None
    else
      Some(OneAnd(self(0).asInstanceOf[A], dropL(1)))

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).toArray
   * res0: Array[Int] = Array(1, 2, 3)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2).isEmpty
   * res0: Boolean = false
   *
   * scala> IArray[Int](1, 2).dropL(10).isEmpty
   * res1: Boolean = true
   * }}}
   */
  @inline def isEmpty: Boolean =
    self.length == 0

  /**
   * @example{{{
   * scala> IArray(1, 2).nonEmpty
   * res0: Boolean = true
   *
   * scala> IArray[Int](1, 2).dropL(10).nonEmpty
   * res1: Boolean = false
   * }}}
   */
  @inline def nonEmpty: Boolean =
    self.length != 0

  // unsafe
  @inline private[iarray] def head: A =
    self(0).asInstanceOf[A]

  // unsafe
  @inline private[iarray] def tail: IArray[A] =
    dropL(1)


  /**
   * @example{{{
   * scala> IArray(10, 20, 30).headOption
   * res0: Option[Int] = Some(10)
   * }}}
   */
  def headOption: Option[A] =
    if(isEmpty) None
    else Some(this(0))

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).headMaybe
   * res0: scalaz.Maybe[Int] = Just(10)
   * }}}
   */
  def headMaybe: Maybe[A] =
    if(isEmpty) Maybe.empty[A]
    else Maybe.Just(this(0))

  @inline private[iarray] def unsafeLast: A =
    self(self.length - 1).asInstanceOf[A]

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).lastOption
   * res0: Option[Int] = Some(30)
   * }}}
   */
  def lastOption: Option[A] =
    if(isEmpty) None
    else Some(this(length - 1))

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).lastMaybe
   * res0: scalaz.Maybe[Int] = Just(30)
   * }}}
   */
  def lastMaybe: Maybe[A] =
    if(isEmpty) Maybe.empty[A]
    else Maybe.Just(this(length - 1))

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).tailOption
   * res0: Option[IArray[Int]] = Some(IArray(20, 30))
   * }}}
   */
  def tailOption: Option[IArray[A]] =
    if(isEmpty) None
    else Some(dropL(1))

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).tailMaybe
   * res0: scalaz.Maybe[IArray[Int]] = Just(IArray(20, 30))
   * }}}
   */
  def tailMaybe: Maybe[IArray[A]] =
    if(isEmpty) Maybe.empty[IArray[A]]
    else Maybe.Just(dropL(1))

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).initOption
   * res0: Option[IArray[Int]] = Some(IArray(10, 20))
   * }}}
   */
  def initOption: Option[IArray[A]] =
    if(isEmpty) None
    else Some(dropR(1))

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).initMaybe
   * res0: scalaz.Maybe[IArray[Int]] = Just(IArray(10, 20))
   * }}}
   */
  def initMaybe: Maybe[IArray[A]] =
    if(isEmpty) Maybe.empty[IArray[A]]
    else Maybe.Just(dropR(1))

  /**
   * @example{{{
   * scala> val array = IArray("a", "b", "c", "d", "e", "f")
   * scala> array.slice(1, 3)
   * res0: IArray[String] = IArray(b, c)
   * scala> array.slice(4, 17)
   * res1: IArray[String] = IArray(e, f)
   * }}}
   */
  def slice(from: Int, until: Int): IArray[A] = {
    if(until <= from || until <= 0 || from >= self.length){
      empty[A]
    }else if(from <= 0 && self.length <= until){
      this
    }else{
      new IArray(copyOfRange(self, Math.max(from, 0), Math.min(until, self.length)))
    }
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).reverse
   * res0: IArray[Int] = IArray(3, 2, 1)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3) reverse_::: IArray(10, 11, 12)
   * res0: IArray[Int] = IArray(3, 2, 1, 10, 11, 12)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).count(_ % 2 == 1)
   * res0: Int = 4
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(10, 20, 30, 40, 50, 60, 70).splitAt(5)
   * res0: (IArray[Int], IArray[Int]) = (IArray(10, 20, 30, 40, 50),IArray(60, 70))
   * }}}
   */
  def splitAt(n: Int): (IArray[A], IArray[A]) =
    if(n <= 0){
      (empty[A], this)
    }else if(n >= self.length){
      (this, empty[A])
    }else{
      (new IArray[A](copyOf(self, n)), new IArray(copyOfRange(self, n, self.length)))
    }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).takeWhileL(_ < 5)
   * res0: IArray[Int] = IArray(1, 2, 3, 4)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).takeWhileR(_ > 2)
   * res0: IArray[Int] = IArray(3, 4, 5, 6, 7)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).dropWhileL(_ < 4)
   * res0: IArray[Int] = IArray(4, 5, 6, 7)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).dropWhileR(_ > 5)
   * res0: IArray[Int] = IArray(1, 2, 3, 4, 5)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray("a", "b", "c", "d", "e", "f").takeR(4)
   * res0: IArray[String] = IArray(c, d, e, f)
   * }}}
   */
  def takeR(n: Int): IArray[A] =
    if(n <= 0) empty[A]
    else if(n >= length) this
    else new IArray(copyOfRange(self, length - n, length))

  /**
   * @example{{{
   * scala> IArray("a", "b", "c", "d", "e", "f").takeL(2)
   * res0: IArray[String] = IArray(a, b)
   * }}}
   */
  def takeL(n: Int): IArray[A] =
    if(n <= 0) empty[A]
    else if(n >= length) this
    else new IArray(copyOf(self, n))

  /**
   * @example{{{
   * scala> import scalaz.\&/
   * scala> IArray(1, 2, 3) align IArray("a", "b", "c", "d", "e")
   * res0: IArray[Int \&/ String] = IArray(Both(1,a), Both(2,b), Both(3,c), That(d), That(e))
   * }}}
   */
  def align[B](b: IArray[B]): IArray[A \&/ B] =
    alignWith(b)(conform)

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
  def withIndex: WithIndex[A] =
    new WithIndex(self)

  /**
   * @example{{{
   * scala> IArray("a", "b", "c").zipWithIndex
   * res0: IArray[(String, Int)] = IArray((a,0), (b,1), (c,2))
   * }}}
   */
  def zipWithIndex: IArray[(A, Int)] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = (self(i), i)
      i += 1
    }
    new IArray[(A, Int)](array)
  }

  /**
   * @example{{{
   * scala> IArray("a", "b", "c", "d").zipWith(IArray("x", "y", "z"))(_ + _)
   * res0: IArray[String] = IArray(ax, by, cz)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray("a", "b", "c", "d").zipAll(IArray(1, 2), "z", 0)
   * res0: IArray[(String, Int)] = IArray((a,1), (b,2), (c,0), (d,0))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray("a", "b") zip IArray(1, 2, 3, 4)
   * res0: IArray[(String, Int)] = IArray((a,1), (b,2))
   * }}}
   */
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

  /** alias of `unzip2` */
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

  /**
   * @example{{{
   * scala> IArray(("a", 1), ("b", 2)).unzip2
   * res0: (IArray[String], IArray[Int]) = (IArray(a, b),IArray(1, 2))
   * }}}
   */
  def unzip2[B, C](implicit e: A <:< Product2[B, C]): (IArray[B], IArray[C]) = unzip[B, C]

  /**
   * @example{{{
   * scala> IArray(("a", 1, true), ("b", 2, false)).unzip3
   * res0: (IArray[String], IArray[Int], IArray[Boolean]) = (IArray(a, b),IArray(1, 2),IArray(true, false))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(("a", 1, true, 'x), ("b", 2, false, 'y)).unzip4
   * res0: (IArray[String], IArray[Int], IArray[Boolean], IArray[Symbol]) = (IArray(a, b),IArray(1, 2),IArray(true, false),IArray('x, 'y))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(("a", 1, true, 'x, 'f'), ("b", 2, false, 'y, 'g')).unzip5
   * res0: (IArray[String], IArray[Int], IArray[Boolean], IArray[Symbol], IArray[Char]) = (IArray(a, b),IArray(1, 2),IArray(true, false),IArray('x, 'y),IArray(f, g))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(("a", 1), ("b", 2)).firsts
   * res0: IArray[String] = IArray(a, b)
   * }}}
   */
  def firsts[B, C](implicit e: A <:< Product2[B, C]): IArray[B] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = self(i).asInstanceOf[Product2[AnyRef, AnyRef]]._1
      i += 1
    }
    new IArray(array)
  }

  /**
   * @example{{{
   * scala> IArray(("a", 10), ("b", 20)).seconds
   * res0: IArray[Int] = IArray(10, 20)
   * }}}
   */
  def seconds[B, C](implicit e: A <:< Product2[B, C]): IArray[C] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = self(i).asInstanceOf[Product2[AnyRef, AnyRef]]._2
      i += 1
    }
    new IArray(array)
  }

  /**
   * @example{{{
   * scala> IArray(10, 20, 30, 40).reversed[List]
   * res0: List[Int] = List(40, 30, 20, 10)
   * }}}
   */
  def reversed[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    var i = self.length - 1
    while(i >= 0){
      buf += self(i).asInstanceOf[A]
      i -= 1
    }
    buf.result
  }

  /**
   * @example{{{
   * scala> IArray(10, 20, 30, 40).to[Vector]
   * res0: Vector[Int] = Vector(10, 20, 30, 40)
   * }}}
   */
  def to[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    var i = 0
    while(i < self.length){
      buf += self(i).asInstanceOf[A]
      i += 1
    }
    buf.result
  }

  /**
   * @example{{{
   * scala> import scalaz.OneAnd
   * scala> IArray(10, 20, 30, 40).toOneAnd[Vector]
   * res0: Option[OneAnd[Vector, Int]] = Some(OneAnd(10,Vector(20, 30, 40)))
   * scala> IArray.empty[String].toOneAnd[Vector]
   * res1: Option[OneAnd[Vector, String]] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5).filter(_ % 3 != 1)
   * res0: IArray[Int] = IArray(2, 3, 5)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5).withFilter(_ % 3 != 1).map(_ * 10)
   * res0: IArray[Int] = IArray(20, 30, 50)
   * }}}
   */
  def withFilter(f: A => Boolean): WithFilter[A] =
    new WithFilter[A](self, f)

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).mapTo(_ * 10): List[Int]
   * res0: List[Int] = List(10, 20, 30)
   * }}}
   */
  def mapTo[C, B](f: A => B)(implicit C: CanBuildFrom[Nothing, B, C]): C = {
    val buf = C()
    var i = 0
    while(i < self.length){
      buf += f(self(i).asInstanceOf[A])
      i += 1
    }
    buf.result
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).map(_ * 10)
   * res0: IArray[Int] = IArray(10, 20, 30)
   * }}}
   */
  def map[B](f: A => B): IArray[B] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = f(self(i).asInstanceOf[A]).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
  def foreach[U](f: A => U): Unit = {
    var i = 0
    while(i < self.length){
      f(self(i).asInstanceOf[A])
      i += 1
    }
  }

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> val a = IArray(1, 2, 3, 4)
   * scala> a.contains(3)
   * res0: Boolean = true
   * scala> a.contains(5)
   * res1: Boolean = false
   * }}}
   */
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

  /**
   * @example{{{
   * scala> val a = IArray("a", "b", "c", "d", "e")
   * scala> a.dropL(-1)
   * res0: IArray[String] = IArray(a, b, c, d, e)
   * scala> a.dropL(2)
   * res1: IArray[String] = IArray(c, d, e)
   * scala> a.dropL(6)
   * res2: IArray[String] = IArray()
   * }}}
   */
  def dropL(n: Int): IArray[A] = {
    if(n <= 0){
      this
    }else if(n >= self.length){
      empty[A]
    }else{
      new IArray(copyOfRange(self, n, self.length))
    }
  }

  /**
   * @example{{{
   * scala> val a = IArray("a", "b", "c", "d", "e")
   * scala> a.dropR(-1)
   * res0: IArray[String] = IArray(a, b, c, d, e)
   * scala> a.dropR(2)
   * res1: IArray[String] = IArray(a, b, c)
   * scala> a.dropR(6)
   * res2: IArray[String] = IArray()
   * }}}
   */
  def dropR(n: Int): IArray[A] = {
    if(n <= 0){
      this
    }else if(n >= self.length){
      empty[A]
    }else{
      new IArray(copyOf(self, self.length - n))
    }
  }

  /**
   * @example{{{
   * scala> 100 +: IArray(1, 2, 3)
   * res0: IArray[Int] = IArray(100, 1, 2, 3)
   * }}}
   */
  def +:(a: A): IArray[A] = {
    val array = new Array[AnyRef](self.length + 1)
    System.arraycopy(self, 0, array, 1, self.length)
    array(0) = a.asInstanceOf[AnyRef]
    new IArray(array)
  }

  /**
   * @example{{{
   * scala> IArray(1, 2, 3) :+ 100
   * res0: IArray[Int] = IArray(1, 2, 3, 100)
   * }}}
   */
  def :+(a: A): IArray[A] = {
    val array = new Array[AnyRef](self.length + 1)
    System.arraycopy(self, 0, array, 0, self.length)
    array(self.length) = a.asInstanceOf[AnyRef]
    new IArray(array)
  }

  /**
   * @example{{{
   * scala> IArray("a", "b", "c") ++ IArray("x", "y")
   * res0: IArray[String] = IArray(a, b, c, x, y)
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(1, 2, 3, 4, 5).fold
   * res0: Int = 15
   * scala> import scalaz.std.string._
   * scala> IArray("a", "bc", "d").fold
   * res1: String = abcd
   * }}}
   */
  def fold(implicit A: Monoid[A]): A = {
    var i = 0
    var acc = A.zero
    while(i < self.length){
      acc = A.append(acc, self(i).asInstanceOf[A])
      i += 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> import scalaz.{\/, \/-, -\/}
   * scala> import scalaz.std.string._, scalaz.std.anyVal._
   * scala> IArray[Int \/ String](\/-("a"), -\/(1), -\/(2), \/-("b")).fold1Opt
   * res0: Option[Int \/ String] = Some(-\/(3))
   * scala> IArray.empty[Int \/ String].fold1Opt
   * res1: Option[Int \/ String] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
  def foldMap1Opt[B](f: A => B)(implicit B: Semigroup[B]): Option[B] =
    if(isEmpty){
      None
    }else{
      Some(foldMap1(f))
    }

  /**
   * @example{{{
   * scala> IArray("ab", "cde").flatMap(IArray.from(_))
   * res0: IArray[Char] = IArray(a, b, c, d, e)
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
  def foldMapR1Opt[B](z: A => B)(f: (A, B) => B): Option[B] =
    if(self.length == 0){
      None
    }else{
      Some(foldMapR1(z)(f))
    }

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
  def foldl[B](z: B)(f: (B, A) => B): B = {
    var i = 0
    var acc = z
    while(i < self.length){
      acc = f(acc, this(i))
      i += 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(123, 23, 9, 54).foldMap(_.toString.size)
   * res0: Int = 8
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
  def foldr[B](z: B)(f: (A, B) => B): B = {
    var i = self.length - 1
    var acc = z
    while(i >= 0){
      acc = f(this(i), acc)
      i -= 1
    }
    acc
  }

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala>
   * res0: =
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(1, 2, 3, 4, 5) startsWith IArray(1, 2)
   * res0: Boolean = true
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(1, 2, 3, 4, 5) endsWith IArray(3, 4, 5)
   * res0: Boolean = true
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(2, 8, 11, -2, 5, 6).span(_ % 2 == 0)
   * res0: (IArray[Int], IArray[Int]) = (IArray(2, 8),IArray(11, -2, 5, 6))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(2, 8, 11, -2, 5, 6).partition(_ % 2 == 0)
   * res0: (IArray[Int], IArray[Int]) = (IArray(2, 8, -2, 6),IArray(11, 5))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray("a", "b", "c", "d").updated(2, "z")
   * res0: IArray[String] = IArray(a, b, z, d)
   * }}}
   */
  @throws[IndexOutOfBoundsException]
  def updated(index: Int, elem: A): IArray[A] = {
    val array = self.clone
    array(index) = elem.asInstanceOf[AnyRef]
    new IArray(array)
  }

  /**
   * @example{{{
   * scala> IArray[Seq[Int]](Vector(1), List(2), Vector(3), List(4)).collectBy[Vector[Int]]
   * res0: IArray[Vector[Int]] = IArray(Vector(1), Vector(3))
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).collect{ case i if i > 3 => i * 10 }
   * res0: IArray[Int] = IArray(40, 50, 60, 70)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4).reverseMap(_ * 3)
   * res0: IArray[Int] = IArray(12, 9, 6, 3)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).collectFirst{ case i if i > 3 => i * 10 }
   * res0: Option[Int] = Some(40)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4, 5, 6, 7).collectLast{ case i if i < 3 => i * 10 }
   * res0: Option[Int] = Some(20)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(-1, 0, 1, 2, -1, 0, 1, 2).indexOfL(2)
   * res0: Option[Int] = Some(3)
   * scala> IArray(1, 2, 3, 1, 2, 3).indexOfL(5)
   * res1: Option[Int] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(1, 2, 3, 1, 2, 3).indexOfR(1)
   * res0: Option[Int] = Some(3)
   * scala> IArray(1, 2, 3, 1, 2, 3).indexOfR(5)
   * res1: Option[Int] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(10, 20, 30).sum
   * res0: Int = 60
   * }}}
   */
  def sum(implicit A: Numeric[A]): A = {
    var i = 0
    var acc: A = A.zero
    while(i < self.length){
      acc = A.plus(acc, self(i).asInstanceOf[A])
      i += 1
    }
    acc
  }

  /**
   * @example{{{
   * scala> import scalaz.std.string._
   * scala> IArray("y", "k", "f", "i", "t", "s").sorted
   * res0: IArray[String] = IArray(f, i, k, s, t, y)
   * }}}
   */
  def sorted(implicit O: Order[A]): IArray[A] =
    sort0(O.toScalaOrdering)

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(2, 7, 4, 6, 1).sortWith(_ > _)
   * res0: IArray[Int] = IArray(7, 6, 4, 2, 1)
   * }}}
   */
  def sortWith(f: (A, A) => Boolean): IArray[A] =
    sort0(comparatorFromFunction(f))

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray("aaaa", "bb", "ccccc", "d").sortBy(_.length)
   * res0: IArray[String] = IArray(d, bb, aaaa, ccccc)
   * }}}
   */
  def sortBy[B](f: A => B)(implicit O: Order[B]): IArray[A] =
    sort0((O contramap f).toScalaOrdering)

  private def sort0[B](c: java.util.Comparator[B]): IArray[B] = {
    val array = self.clone
    Arrays.sort(array, c.asInstanceOf[java.util.Comparator[AnyRef]])
    new IArray(array)
  }

  /**
   * @example{{{
   * scala> IArray(IArray(1, 2), IArray(3)).flatten
   * res0: IArray[Int] = IArray(1, 2, 3)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.==>>
   * scala> import scalaz.std.anyVal._
   * scala> IArray(1, 2, 3, 4, 5).groupBy1(_ % 3).toList.sortBy(_._1)
   * res0: List[(Int, IArray1[Int])] = List((0,IArray1(3)), (1,IArray1(1, 4)), (2,IArray1(2, 5)))
   * }}}
   */
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

  def interleave(that: IArray[A]): IArray[A] = {
    val len = Math.min(self.length, that.self.length)
    val array = new Array[AnyRef](self.length + that.self.length)
    @tailrec def loop(isThis: Boolean, i: Int): Unit = {
      if(i < len) {
        if (isThis) {
          array(i * 2) = self(i)
          loop(false, i)
        } else {
          array(i * 2 + 1) = that.self(i)
          loop(true, i + 1)
        }
      }
    }
    loop(true, 0)
    def cp(min: Array[AnyRef], max: Array[AnyRef]): Unit = {
      System.arraycopy(max, min.length, array, len * 2, max.length - min.length)
    }
    if(self.length > that.length) {
      cp(that.self, self)
    }else if(self.length < that.length){
      cp(self, that.self)
    }
    new IArray[A](array)
  }

  /**
   * @example{{{
   * scala> import scalaz.std.list._
   * scala> IArray(List("a"), List("b", "c"), Nil, List("d")).intercalate(List("z"))
   * res0: List[String] = List(a, z, b, c, z, z, d)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.list._
   * scala> IArray(List("a"), List("b", "c"), Nil, List("d")).intercalate1Opt(List("z"))
   * res0: Option[List[String]] = Some(List(a, z, b, c, z, z, d))
   * scala> IArray.empty[List[Int]].intercalate1Opt(List(7))
   * res1: Option[List[Int]] = None
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3, 4).intersperse(0)
   * res0: IArray[Int] = IArray(1, 0, 2, 0, 3, 0, 4)
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).toString
   * res0: String = IArray(1, 2, 3)
   * }}}
   */
  override def toString: String =
    mkString("IArray(",", ",")")

  /**
   * @example{{{
   * scala> IArray("a", "b", "c").mkString("_")
   * res0: String = a_b_c
   * }}}
   */
  def mkString(sep: String = ""): String =
    mkString("", sep, "")

  /**
   * @example{{{
   * scala> IArray(1, 2, 3).mkString("[", ",", "]")
   * res0: String = [1,2,3]
   * }}}
   */
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /**
   * @example{{{
   * scala> IArray("x", "y", "z").addString(new StringBuilder("aaa"), "c", "d", "e").toString
   * res0: String = aaacxdydze
   * }}}
   */
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

  /**
   * @example{{{
   * scala> import scalaz.std.anyVal._
   * scala> IArray(1, 2) === IArray(1, 2)
   * res0: Boolean = true
   * }}}
   */
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

  /**
   * @example{{{
   * scala> IArray(List(1)).widen[Seq[Int]]
   * res0: IArray[Seq[Int]] = IArray(List(1))
   * }}}
   */
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

