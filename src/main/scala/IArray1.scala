package iarray

import scalaz._
import scala.collection.mutable.ArrayBuilder
import scala.collection.generic.CanBuildFrom
import java.util.Arrays
import java.util.Arrays.{copyOf, copyOfRange}

object IArray1 {

  implicit val iarray1Instance: Monad[IArray1] with Plus[IArray1] with Traverse1[IArray1] with Zip[IArray1] with Align[
    IArray1] with Unzip[IArray1] with Comonad[IArray1] = IArray1Instance

  /**
   * @example{{{
   * scala> import scalaz._, std.anyVal._
   * scala> val S = Show[IArray1[Int]]
   * scala> S.show(IArray1(1, 2, 3))
   * res0: Cord = IArray1(1, 2, 3)
   * scala> S.shows(IArray1(1, 2, 3))
   * res1: String = IArray1(1, 2, 3)
   * }}}
   */
  implicit def iarray1Show[A](implicit A: Show[A]): Show[IArray1[A]] =
    new Show[IArray1[A]] {
      override def shows(a: IArray1[A]) =
        a.toIterator.map(A.shows).mkString("IArray1(", ", ", ")")
    }

  val zipApply: Apply[IArray1] = IArray1ZipApply

  implicit def iarray1Equal[A: Equal]: Equal[IArray1[A]] =
    new Equal[IArray1[A]] {
      def equal(a: IArray1[A], b: IArray1[A]) = a === b
      override def equalIsNatural = Equal[A].equalIsNatural
    }

  private[this] val _single: AnyRef => IArray1[AnyRef] =
    head => IArray1(head, IArray.empty[AnyRef])

  @inline def singleF[A]: A => IArray1[A] = _single.asInstanceOf[A => IArray1[A]]

  def fromNel[A](nel: NonEmptyList[A]): IArray1[A] =
    IArray1(nel.head, IArray.fromIList(nel.tail))

  def fromOneAnd[F[_], A](a: OneAnd[F, A])(implicit F: Foldable[F]): IArray1[A] =
    IArray1(a.head, F.to[A, IArray](a.tail))

  def apply[A](head: A, tail: A*): IArray1[A] =
    IArray1(head, IArray.apply(tail: _*))

  def iterate[A](start: A, size: Int)(f: A => A): IArray1[A] =
    IArray1(start, IArray.iterate(f(start), size - 1)(f))

  def zip3[A, B, C](a: IArray1[A], b: IArray1[B], c: IArray1[C]): IArray1[(A, B, C)] =
    IArray1((a.head, b.head, c.head), IArray.zip3(a.tail, b.tail, c.tail))

  def zip4[A, B, C, D](a: IArray1[A], b: IArray1[B], c: IArray1[C], d: IArray1[D]): IArray1[(A, B, C, D)] =
    IArray1((a.head, b.head, c.head, d.head), IArray.zip4(a.tail, b.tail, c.tail, d.tail))

  def zip5[A, B, C, D, E](a: IArray1[A],
                          b: IArray1[B],
                          c: IArray1[C],
                          d: IArray1[D],
                          e: IArray1[E]): IArray1[(A, B, C, D, E)] =
    IArray1((a.head, b.head, c.head, d.head, e.head), IArray.zip5(a.tail, b.tail, c.tail, d.tail, e.tail))

  def zipWith3[A, B, C, D](a: IArray1[A], b: IArray1[B], c: IArray1[C])(f: (A, B, C) => D): IArray1[D] =
    IArray1(f(a.head, b.head, c.head), IArray.zipWith3(a.tail, b.tail, c.tail)(f))

  def zipWith4[A, B, C, D, E](a: IArray1[A], b: IArray1[B], c: IArray1[C], d: IArray1[D])(
    f: (A, B, C, D) => E): IArray1[E] =
    IArray1(f(a.head, b.head, c.head, d.head), IArray.zipWith4(a.tail, b.tail, c.tail, d.tail)(f))

  def zipWith5[A, B, C, D, E, F](a: IArray1[A], b: IArray1[B], c: IArray1[C], d: IArray1[D], e: IArray1[E])(
    f: (A, B, C, D, E) => F): IArray1[F] =
    IArray1(f(a.head, b.head, c.head, d.head, e.head), IArray.zipWith5(a.tail, b.tail, c.tail, d.tail, e.tail)(f))

}

/** Non empty immutable array
 */
final case class IArray1[A](head: A, tail: IArray[A]) { self =>

  def partition(f: A => Boolean): (IArray[A], IArray[A]) = {
    val l, r = new ArrayBuilder.ofRef[AnyRef]()
    (if (f(head)) l else r) += head.asInstanceOf[AnyRef]
    var i = 0
    while (i < tail.self.length) {
      (if (f(tail.self(i).asInstanceOf[A])) l else r) += tail.self(i)
      i += 1
    }
    (new IArray(l.result), new IArray(r.result))
  }

  def dropL(n: Int): IArray[A] =
    if (n <= 0) toIArray
    else tail.dropL(n - 1)

  def alignWith[B, C](that: IArray1[B])(f: A \&/ B => C): IArray1[C] =
    IArray1(f(\&/.Both(head, that.head)), tail.alignWith(that.tail)(f))

  def align[B](that: IArray1[B]): IArray1[A \&/ B] =
    IArray1(\&/.Both(head, that.head), tail.align(that.tail))

  def zip[B](that: IArray1[B]): IArray1[(A, B)] =
    IArray1((head, that.head), tail.zip(that.tail))

  def zipWith[B, C](that: IArray1[B])(f: (A, B) => C): IArray1[C] =
    IArray1(f(head, that.head), tail.zipWith(that.tail)(f))

  def zipAll[B](that: IArray1[B], a: A, b: B): IArray1[(A, B)] =
    IArray1((head, that.head), tail.zipAll(that.tail, a, b))

  def zipWithIndex: IArray1[(A, Int)] = {
    var i = 0
    val array = new Array[AnyRef](tail.self.length)
    while (i < tail.self.length) {
      array(i) = (tail.self(i), i + 1)
      i += 1
    }
    IArray1((head, 0), new IArray[(A, Int)](array))
  }

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

  def unzip5[B, C, D, E, F](
    implicit e: A <:< Product5[B, C, D, E, F]): (IArray1[B], IArray1[C], IArray1[D], IArray1[E], IArray1[F]) = {
    val h = e(head)
    val t = tail.unzip5
    (IArray1(h._1, t._1), IArray1(h._2, t._2), IArray1(h._3, t._3), IArray1(h._4, t._4), IArray1(h._5, t._5))
  }

  @inline def apply(i: Int): A =
    if (i == 0) head else tail(i - 1)

  def map[B](f: A => B): IArray1[B] =
    IArray1(f(head), tail map f)

  def mapTo[C, B](f: A => B)(implicit C: CanBuildFrom[Nothing, B, C]): C = {
    val buf = C()
    var i = 0
    buf += f(head)
    while (i < tail.length) {
      buf += f(tail(i))
      i += 1
    }
    buf.result
  }

  def collect[B](f: PartialFunction[A, B]): IArray[B] = {
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    var i = 0
    val f0 = f.asInstanceOf[PartialFunction[AnyRef, AnyRef]].runWith(builder += _)
    f0(head.asInstanceOf[AnyRef])
    while (i < tail.self.length) {
      f0(tail.self(i))
      i += 1
    }
    new IArray[B](builder.result)
  }

  def collectFirst[B](f: PartialFunction[A, B]): Option[B] =
    if (f isDefinedAt head) Some(f(head))
    else tail collectFirst f

  def collectLast[B](f: PartialFunction[A, B]): Option[B] = {
    val x = tail collectLast f
    if (x.isDefined) x
    else f.lift(head)
  }

  // TODO optimize?
  def traverse1[F[_], B](f: A => F[B])(implicit F: Apply[F]): F[IArray1[B]] =
    if (tail.self.length == 0)
      F.map(f(head))(x => IArray1(x, IArray.empty[B]))
    else
      F.apply2(
        f(head),
        OneAnd.oneAndTraverse[IArray].traverse1(OneAnd(tail.head, tail.tail))(f)
      ) { (h, t) =>
        IArray1(h, t.head +: t.tail)
      }

  def scanRight[B](z: B)(f: (A, B) => B): IArray1[B] = {
    var i = tail.self.length
    val array = new Array[AnyRef](i + 1)
    val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
    array(i) = z.asInstanceOf[AnyRef]
    while (0 < i) {
      array(i - 1) = f0(tail.self(i - 1), array(i))
      i -= 1
    }
    IArray1(f(head, array(0).asInstanceOf[B]), new IArray[B](array))
  }

  def scanLeft[B](z: B)(f: (B, A) => B): IArray1[B] = {
    val array = new Array[AnyRef](tail.self.length + 1)
    val f0 = f.asInstanceOf[(AnyRef, AnyRef) => AnyRef]
    array(0) = f(z, head).asInstanceOf[AnyRef]
    var i = 0
    while (i < tail.length) {
      array(i + 1) = f0(array(i), tail.self(i))
      i += 1
    }
    IArray1(z, new IArray[B](array))
  }

  def foldr[B](z: B)(f: (A, B) => B): B =
    f(head, tail.foldr(z)(f))

  def foldr1(f: (A, A) => A): A =
    tail.foldr(head)(f)

  def foldl1(f: (A, A) => A): A =
    tail.foldl(head)(f)

  def foldl[B](z: B)(f: (B, A) => B): B =
    tail.foldl(f(z, head))(f)

  def foldMapRight1[B](z: A => B)(f: (A, B) => B): B =
    if (tail.length == 0) z(head)
    else f(head, tail.foldMapR1(z)(f))

  def foldMapLeft1[B](z: A => B)(f: (B, A) => B): B =
    tail.foldl(z(head))(f)

  def foldMap1[B](f: A => B)(implicit B: Semigroup[B]): B =
    if (tail.self.length == 0) f(head)
    else B.append(f(head), tail.foldMap1(f))

  def unite1[G[_], B](implicit A: A <:< G[B], G: Foldable1[G]): IArray1[B] =
    flatMap(G.foldMap1(_)(IArray1.singleF)(IArray1Instance.semigroup[B]))

  def unite[G[_], B](implicit A: A <:< G[B], G: Foldable[G]): IArray[B] =
    foldMap1(G.foldMap(_)(IArray.singleF))

  def flatten[B](implicit A: A <:< IArray1[B]): IArray1[B] = {
    var i = 0
    val h = head.asInstanceOf[IArray1[B]]
    var n = h.tail.length
    val len = tail.self.length
    while (i < len) {
      n += tail.self(i).asInstanceOf[IArray1[B]].length
      i += 1
    }
    val array = new Array[AnyRef](n)
    System.arraycopy(h.tail.self, 0, array, 0, h.tail.length)
    i = 0
    n = h.tail.self.length
    while (i < len) {
      val elem = tail.self(i).asInstanceOf[IArray1[B]]
      array(n) = elem.head.asInstanceOf[AnyRef]
      System.arraycopy(elem.tail.self, 0, array, n + 1, elem.tail.length)
      n += elem.length
      i += 1
    }
    IArray1(h.head, new IArray[B](array))
  }

  def flatMap[B](f: A => IArray1[B]): IArray1[B] = {
    val h = f(head)
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    builder ++= h.tail.self
    var i = 0
    while (i < tail.self.length) {
      val x = f(tail(i))
      var j = 0
      val len = x.length
      while (j < len) {
        builder += x(j).asInstanceOf[AnyRef]
        j += 1
      }
      i += 1
    }
    IArray1(h.head, new IArray[B](builder.result))
  }

  def cobind[B](f: IArray1[A] => B): IArray1[B] = {
    var i = 0
    val len = tail.self.length
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = f(
        IArray1(
          tail.self(i).asInstanceOf[A],
          new IArray[A](copyOfRange(tail.self, i + 1, len))
        )).asInstanceOf[AnyRef]
      i += 1
    }
    IArray1(f(this), new IArray[B](array))
  }

  def cojoin: IArray1[IArray1[A]] = {
    var i = 0
    val len = tail.self.length
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = IArray1(
        tail.self(i),
        new IArray[AnyRef](copyOfRange(tail.self, i + 1, len))
      ).asInstanceOf[AnyRef]
      i += 1
    }
    IArray1(this, new IArray[IArray1[A]](array))
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

  private def sort0[B](c: java.util.Comparator[B]): IArray1[A] = {
    val array = copyOf(tail.self, tail.length + 1)
    array(tail.length) = head.asInstanceOf[AnyRef]
    Arrays.sort(array, c.asInstanceOf[java.util.Comparator[AnyRef]])
    IArray1(array(0).asInstanceOf[A], new IArray[A](copyOfRange(array, 1, array.length)))
  }

  def sorted(implicit O: Order[A]): IArray1[A] =
    sort0(O.toScalaOrdering)

  def sortWith(f: (A, A) => Boolean): IArray1[A] =
    sort0(IArray.comparatorFromFunction(f))

  def sortBy[B](f: A => B)(implicit O: Order[B]): IArray1[A] =
    sort0((O contramap f).toScalaOrdering)

  def max(implicit O: Order[A]): A =
    if (tail.self.length == 0) head
    else O.max(head, tail.unsafeMax)

  def min(implicit O: Order[A]): A =
    if (tail.self.length == 0) head
    else O.min(head, tail.unsafeMin)

  def maxBy[B](f: A => B)(implicit O: scalaz.Order[B]): A =
    if (tail.self.length == 0) head
    else {
      val x = tail.unsafeMaxBy(f)
      if (O.greaterThan(f(x), f(head))) x
      else head
    }

  def minBy[B](f: A => B)(implicit O: scalaz.Order[B]): A =
    if (tail.self.length == 0) head
    else {
      val x = tail.unsafeMinBy(f)
      if (O.lessThan(f(x), f(head))) x
      else head
    }

  def maxOf[B](f: A => B)(implicit O: scalaz.Order[B]): B =
    if (tail.self.length == 0) f(head)
    else O.max(f(head), tail.unsafeMaxOf(f))

  def minOf[B](f: A => B)(implicit O: scalaz.Order[B]): B =
    if (tail.self.length == 0) f(head)
    else O.min(f(head), tail.unsafeMinOf(f))

  def toNel: NonEmptyList[A] =
    NonEmptyList.nel(head, tail.toIList)

  def toList: List[A] =
    head :: tail.toList

  def toIterator: Iterator[A] = new Iterator[A] {
    private[this] var i = 0
    def hasNext: Boolean = i < self.length
    def next(): A = {
      val n = apply(i)
      i += 1
      n
    }
  }

  def toIList: IList[A] =
    head +: tail.toIList

  def toIArray: IArray[A] = {
    val array = new Array[AnyRef](length)
    array(0) = head.asInstanceOf[AnyRef]
    System.arraycopy(tail.self, 0, array, 1, tail.self.length)
    new IArray[A](array)
  }

  def toArray(implicit A: reflect.ClassTag[A]): Array[A] = {
    val array = new Array[A](length)
    array(0) = head
    val len = tail.self.length
    if (A.runtimeClass.isPrimitive) {
      var i = 0
      while (i < len) {
        array(i + 1) = tail.self(i).asInstanceOf[A]
        i += 1
      }
    } else {
      System.arraycopy(tail.self, 0, array, 1, len)
    }
    array
  }

  def to[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    buf += head
    var i = 0
    while (i < tail.self.length) {
      buf += tail.self(i).asInstanceOf[A]
      i += 1
    }
    buf.result
  }

  def toOneAnd[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): OneAnd[F, A] =
    OneAnd(head, tail.to[F])

  def oneAnd: OneAnd[IArray, A] =
    OneAnd(head, tail)

  @inline def length: Int = tail.length + 1
  @inline def size: Int = tail.length + 1

  def count(f: A => Boolean): Int =
    (if (f(head)) 1 else 0) + tail.count(f)

  def contains(a: A)(implicit E: Equal[A]): Boolean =
    E.equal(head, a) || tail.contains(a)

  def forall(f: A => Boolean): Boolean =
    f(head) && tail.forall(f)

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def find(f: A => Boolean): Option[A] =
    if (f(head)) Some(head)
    else tail.find(f)

  def findRight(f: A => Boolean): Option[A] = {
    val x = tail.findRight(f)
    if (x.isDefined) x
    else if (f(head)) Some(head)
    else None
  }

  def indexOfL(a: A)(implicit E: Equal[A]): Option[Int] = {
    if (E.equal(head, a)) Some(0)
    else tail.indexOfL(a).map(_ + 1)
  }

  def indexOfR(a: A)(implicit E: Equal[A]): Option[Int] = {
    val t = tail.indexOfR(a)
    if (t.isDefined) t.map(_ + 1)
    else if (E.equal(head, a)) Some(0)
    else None
  }

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def init: IArray[A] = {
    if (tail.self.length == 0) IArray.empty
    else {
      val array = new Array[AnyRef](tail.self.length)
      array(0) = head.asInstanceOf[AnyRef]
      System.arraycopy(tail.self, 0, array, 1, tail.self.length - 1)
      new IArray(array)
    }
  }

  def last: A =
    if (tail.self.length == 0) head
    else tail.unsafeLast

  def reverseMap[B](f: A => B): IArray1[B] =
    if (tail.self.length == 0) IArray1(f(head), IArray.empty[B])
    else {
      val array = new Array[AnyRef](tail.self.length)
      array(tail.self.length - 1) = f(head).asInstanceOf[AnyRef]
      val len = tail.self.length - 2
      var i = len
      while (0 <= i) {
        array(len - i) = f(tail.self(i).asInstanceOf[A]).asInstanceOf[AnyRef]
        i -= 1
      }
      IArray1(f(tail.unsafeLast), new IArray[B](array))
    }

  def reverse: IArray1[A] = {
    if (tail.self.length == 0) this
    else {
      val array = new Array[AnyRef](tail.self.length)
      array(tail.self.length - 1) = head.asInstanceOf[AnyRef]
      val len = tail.self.length - 2
      var i = len
      while (0 <= i) {
        array(len - i) = tail.self(i)
        i -= 1
      }
      IArray1(tail.unsafeLast, new IArray[A](array))
    }
  }

  def reversed[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    var i = tail.self.length - 1
    while (i >= 0) {
      buf += tail.self(i).asInstanceOf[A]
      i -= 1
    }
    buf += head
    buf.result
  }

  def intercalate1(a: A)(implicit A: Semigroup[A]): A = {
    var i = 0
    var acc = head
    while (i < tail.self.length) {
      acc = A.append(A.append(acc, a), tail.self(i).asInstanceOf[A])
      i += 1
    }
    acc
  }

  def intersperse(a: A): IArray1[A] = {
    val array = new Array[AnyRef](tail.self.length * 2)
    var i = 0
    java.util.Arrays.fill(array, a)
    while (i < tail.self.length) {
      array((i * 2) + 1) = tail.self(i)
      i += 1
    }
    IArray1(head, new IArray[A](array))
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
    if (tail.isEmpty) start + head + end
    else tail.mkString(start + head + sep, sep, end)
}
