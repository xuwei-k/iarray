package iarray

import scalaz._
import scala.collection.mutable.ArrayBuilder
import scala.collection.generic.CanBuildFrom
import java.util.Arrays.copyOfRange

object IArray1 {

  implicit val iarray1Instance: Monad[IArray1] with Plus[IArray1] with Traverse1[IArray1] with Zip[IArray1] with Align[IArray1] with Unzip[IArray1] with Comonad[IArray1] = IArray1Instance

  implicit def iarray1Equal[A: Equal]: Equal[IArray1[A]] =
    new Equal[IArray1[A]] {
      def equal(a: IArray1[A], b: IArray1[A]) = a === b
      override def equalIsNatural = Equal[A].equalIsNatural
    }

  def fromNel[A](nel: NonEmptyList[A]): IArray1[A] =
    IArray1(nel.head, IArray.fromList(nel.tail))

  def apply[A](head: A, tail: A*): IArray1[A] =
    IArray1(head, IArray.apply(tail: _*))

  def iterate[A](start: A, size: Int)(f: A => A): IArray1[A] =
    IArray1(start, IArray.iterate(f(start), size - 1)(f))

  def zip3[A, B, C](a: IArray1[A], b: IArray1[B], c: IArray1[C]): IArray1[(A, B, C)] =
    IArray1((a.head, b.head, c.head), IArray.zip3(a.tail, b.tail, c.tail))

  def zip4[A, B, C, D](a: IArray1[A], b: IArray1[B], c: IArray1[C], d: IArray1[D]): IArray1[(A, B, C, D)] =
    IArray1((a.head, b.head, c.head, d.head), IArray.zip4(a.tail, b.tail, c.tail, d.tail))

  def zipWith3[A, B, C, D](a: IArray1[A], b: IArray1[B], c: IArray1[C])(f: (A, B, C) => D): IArray1[D] =
    IArray1(f(a.head, b.head, c.head), IArray.zipWith3(a.tail, b.tail, c.tail)(f))

  def zipWith4[A, B, C, D, E](a: IArray1[A], b: IArray1[B], c: IArray1[C], d: IArray1[D])(f: (A, B, C, D) => E): IArray1[E] =
    IArray1(f(a.head, b.head, c.head, d.head), IArray.zipWith4(a.tail, b.tail, c.tail, d.tail)(f))

}


/** Non empty immutable array
 */
final case class IArray1[A](head: A, tail: IArray[A]) {

  def alignWith[B, C](that: IArray1[B])(f: A \&/ B => C): IArray1[C] =
    IArray1(f(\&/.Both(head, that.head)), tail.alignWith(that.tail)(f))

  def zip[B](that: IArray1[B]): IArray1[(A, B)] =
    IArray1((head, that.head), tail.zip(that.tail))

  def zipWith[B, C](that: IArray1[B])(f: (A, B) => C): IArray1[C] =
    IArray1(f(head, that.head), tail.zipWith(that.tail)(f))

  def unzip[B, C](implicit e: A <:< Product2[B, C]): (IArray1[B], IArray1[C]) = {
    val h = e(head)
    val t = tail.unzip
    (IArray1(h._1, t._1), IArray1(h._2, t._2))
  }

  def unzip3[B, C, D](implicit e: A <:< Product3[B, C, D]): (IArray1[B], IArray1[C], IArray1[D]) = {
    val h = e(head)
    val t = tail.unzip3
    (IArray1(h._1, t._1), IArray1(h._2, t._2), IArray1(h._3, t._3))
  }

  def unzip4[B, C, D, E](implicit e: A <:< Product4[B, C, D, E]): (IArray1[B], IArray1[C], IArray1[D], IArray1[E]) = {
    val h = e(head)
    val t = tail.unzip4
    (IArray1(h._1, t._1), IArray1(h._2, t._2), IArray1(h._3, t._3), IArray1(h._4, t._4))
  }

  @inline def apply(i: Int): A =
    if(i == 0) head else tail(i - 1)

  def map[B](f: A => B): IArray1[B] =
    IArray1(f(head), tail map f)

  // TODO optimize?
  def traverse1[F[_], B](f: A => F[B])(implicit F: Apply[F]): F[IArray1[B]] =
    if(tail.self.length == 0)
      F.map(f(head))(x => IArray1(x, IArray.empty))
    else F.apply2(
      f(head),
      OneAnd.oneAndTraverse[IArray].traverse1(OneAnd(tail.head, tail.tail))(f)
    ){ (h, t) => IArray1(h, t.head +: t.tail) }

  def foldr[B](z: B)(f: (A, B) => B): B =
    f(head, tail.foldr(z)(f))

  def foldr1(f: (A, A) => A): A =
    tail.foldr(head)(f)

  def foldMapRight1[B](z: A => B)(f: (A, B) => B): B =
    if(tail.length == 0) z(head)
    else f(head, tail.foldMapR1(z)(f))

  def foldMapLeft1[B](z: A => B)(f: (B, A) => B): B =
    tail.foldl(z(head))(f)

  def foldMap1[B](f: A => B)(implicit B: Semigroup[B]): B =
    if(tail.self.length == 0) f(head)
    else B.append(f(head), tail.foldMap1(f))

  def flatten[B](implicit A: A <:< IArray1[B]): IArray1[B] = {
    var i = 0
    val h = head.asInstanceOf[IArray1[B]]
    var n = h.tail.length
    val len = tail.self.length
    while(i < len){
      n += tail.self(i).asInstanceOf[IArray1[B]].length
      i += 1
    }
    val array = new Array[AnyRef](n)
    System.arraycopy(h.tail.self, 0, array, 0, h.tail.length)
    i = 0
    n = h.tail.self.length
    while(i < len){
      val elem = tail.self(i).asInstanceOf[IArray1[B]]
      array(n) = elem.head.asInstanceOf[AnyRef]
      System.arraycopy(elem.tail.self, 0, array, n + 1, elem.tail.length)
      n += elem.length
      i += 1
    }
    IArray1(h.head, new IArray(array))
  }

  def flatMap[B](f: A => IArray1[B]): IArray1[B] = {
    val h = f(head)
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    builder ++= h.tail.self
    var i = 0
    while(i < tail.self.length){
      val x = f(tail(i))
      var j = 0
      val len = x.length
      while(j < len){
        builder += x(j).asInstanceOf[AnyRef]
        j += 1
      }
      i += 1
    }
    IArray1(h.head, new IArray(builder.result))
  }

  def cobind[B](f: IArray1[A] => B): IArray1[B] = {
    var i = 0
    val len = tail.self.length
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = f(IArray1(
        tail.self(i).asInstanceOf[A],
        new IArray[A](copyOfRange(tail.self, i, len))
      )).asInstanceOf[AnyRef]
      i += 1
    }
    IArray1(f(this), new IArray(array))
  }

  def cojoin: IArray1[IArray1[A]] = {
    var i = 0
    val len = tail.self.length
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = IArray1(
        tail.self(i),
        new IArray(copyOfRange(tail.self, i, len))
      ).asInstanceOf[AnyRef]
      i += 1
    }
    IArray1(this, new IArray(array))
  }

  def +:(a: A): IArray1[A] =
    IArray1(a, head +: tail)

  def :+(a: A): IArray1[A] =
    IArray1(head, tail :+ a)

  def plus(that: IArray1[A]): IArray1[A] = {
    val array = new Array[AnyRef](tail.size + that.size)
    System.arraycopy(tail.self, 0, array, 0, tail.size)
    array(tail.size) = that.head.asInstanceOf[AnyRef]
    System.arraycopy(that.tail.self, 0, array, tail.size + 1, that.tail.size)
    IArray1(head, new IArray[A](array))
  }

  def max(implicit O: Order[A]): A =
    if(tail.self.length == 0) head
    else O.max(head, tail.unsafeMax)

  def min(implicit O: Order[A]): A =
    if(tail.self.length == 0) head
    else O.min(head, tail.unsafeMin)

  def toNel: NonEmptyList[A] =
    NonEmptyList.nel(head, tail.toList)

  def toList: List[A] =
    head :: tail.toList

  def toArray(implicit A: reflect.ClassTag[A]): Array[A] = {
    val array = new Array[A](length)
    array(0) = head
    val len = tail.self.length
    if(A.runtimeClass.isPrimitive){
      var i = 0
      while(i < len){
        array(i + 1) = tail.self(i).asInstanceOf[A]
        i += 1
      }
    }else{
      System.arraycopy(tail.self, 0, array, 1, len)
    }
    array
  }

  def to[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    buf += head
    var i = 0
    while(i < tail.self.length){
      buf += tail.self(i).asInstanceOf[A]
      i += 1
    }
    buf.result
  }

  def toOneAnd[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): OneAnd[F, A] =
    OneAnd(head, tail.to[F])

  def oneAnd: OneAnd[IArray, A] =
    OneAnd(head, tail)

  def length: Int = tail.length + 1
  def size: Int = tail.length + 1

  def count(f: A => Boolean): Int =
    (if(f(head)) 1 else 0) + tail.count(f)

  def contains(a: A)(implicit E: Equal[A]): Boolean =
    E.equal(head, a) || tail.contains(a)

  def forall(f: A => Boolean): Boolean =
    f(head) && tail.forall(f)

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def find(f: A => Boolean): Option[A] =
    if(f(head)) Some(head)
    else tail.find(f)

  def indexOfL(a: A)(implicit E: Equal[A]): Option[Int] = {
    if(E.equal(head, a)) Some(0)
    else tail.indexOfL(a).map(_ + 1)
  }

  def indexOfR(a: A)(implicit E: Equal[A]): Option[Int] = {
    val t = tail.indexOfR(a)
    if(t.isDefined) t.map(_ + 1)
    else if(E.equal(head, a)) Some(0)
    else None
  }

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def init: IArray[A] = {
    if(tail.self.length == 0) IArray.empty
    else {
      val array = new Array[AnyRef](tail.self.length)
      array(0) = head.asInstanceOf[AnyRef]
      System.arraycopy(tail.self, 0, array, 1, tail.self.length - 1)
      new IArray(array)
    }
  }

  def last: A =
    if(tail.self.length == 0) head
    else tail.unsafeLast

  def reverse: IArray1[A] = {
    if(tail.self.length == 0) this
    else {
      val array = new Array[AnyRef](tail.self.length)
      array(tail.self.length - 1) = head.asInstanceOf[AnyRef]
      val len = tail.self.length - 2
      var i = len
      while(0 <= i){
        array(len - i) = tail.self(i).asInstanceOf[AnyRef]
        i -= 1
      }
      IArray1(tail.unsafeLast, new IArray(array))
    }
  }

  def intercalate1(a: A)(implicit A: Semigroup[A]): A = {
    var i = 0
    var acc = head
    while(i < tail.self.length){
      acc = A.append(A.append(acc, a), tail.self(i).asInstanceOf[A])
      i += 1
    }
    acc
  }

  def intersperse(a: A): IArray1[A] = {
    val array = new Array[AnyRef](tail.self.length * 2)
    var i = 0
    java.util.Arrays.fill(array, a)
    while(i < tail.self.length){
      array((i * 2) + 1) = tail.self(i)
      i += 1
    }
    IArray1(head, new IArray(array))
  }

  def widen[B](implicit ev: A <:< B): IArray1[B] =
    this.asInstanceOf[IArray1[B]]

  def sum(implicit A: Numeric[A]): A =
    A.plus(head, tail.sum)

  def ===(that: IArray1[A])(implicit E: Equal[A]): Boolean =
    E.equal(head, that.head) && (tail === that.tail)

  override def equals(that: Any): Boolean = that match {
    case IArray1(h, t) =>
      (head == h) && (t.self.length == tail.self.length) && java.util.Arrays.equals(t.self, tail.self)
    case _ => false
  }

  override def hashCode: Int = {
    head.## + java.util.Arrays.hashCode(tail.self)
  }

  override def toString: String =
    mkString("IArray1(", ", ", ")")

  def mkString(sep: String): String =
    mkString("", sep, "")

  def mkString(start: String, sep: String, end: String): String =
    if(tail.isEmpty) start + head + end
    else tail.mkString(start + head + sep, sep, end)
}
