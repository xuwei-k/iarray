package iarray

import annotation.tailrec
import scalaz._
import scala.collection.mutable.ArrayBuilder

private[iarray] abstract class IArrayFunctions {

  /**
   * @example{{{
   * scala> import scalaz._, std.anyVal._
   * scala> val S = Show[IArray[Int]]
   * scala> S.show(IArray(1, 2, 3))
   * res0: Cord = IArray(1, 2, 3)
   * scala> S.shows(IArray(1, 2, 3))
   * res1: String = IArray(1, 2, 3)
   * }}}
   */
  implicit final def iarrayShow[A](implicit A: Show[A]): Show[IArray[A]] =
    new Show[IArray[A]] {
      override def show(a: IArray[A]) =
        Cord(a.toIterator.map(A.shows).mkString("IArray(", ", ", ")"))
    }

  private[this] val _conform: AnyRef => AnyRef =
    x => x
  @inline private[iarray] final def conform[A]: A => A =
    _conform.asInstanceOf[A => A]

  def toListK[A]: Kleisli[List, IArray[A], A] =
    kleisli.toList.asInstanceOf[Kleisli[List, IArray[A], A]]
  def zipWithIndexK[A]: Kleisli[IArray, IArray[A], (A, Int)] =
    kleisli.zipWithIndex.asInstanceOf[Kleisli[IArray, IArray[A], (A, Int)]]
  def reverseK[A]: Kleisli[IArray, IArray[A], A] =
    kleisli.reverse.asInstanceOf[Kleisli[IArray, IArray[A], A]]
  def toIListK[A]: Kleisli[IList, IArray[A], A] =
    kleisli.toIList.asInstanceOf[Kleisli[IList, IArray[A], A]]
  def reverseListK[A]: Kleisli[List, IArray[A], A] =
    kleisli.reverseList.asInstanceOf[Kleisli[List, IArray[A], A]]
  def reverseIListK[A]: Kleisli[IList, IArray[A], A] =
    kleisli.reverseIList.asInstanceOf[Kleisli[IList, IArray[A], A]]
  def zipperEndK[A]: Kleisli[Option, IArray[A], A] =
    kleisli.zipperEnd.asInstanceOf[Kleisli[Option, IArray[A], A]]
  def cojoinK[A]: Kleisli[IArray, IArray[A], A] =
    kleisli.cojoin.asInstanceOf[Kleisli[IArray, IArray[A], A]]
  def toNelK[A]: Kleisli[Option, IArray[A], NonEmptyList[A]] =
    kleisli.toNel.asInstanceOf[Kleisli[Option, IArray[A], NonEmptyList[A]]]
  def toIArray1K[A]: Kleisli[Option, IArray[A], IArray1[A]] =
    kleisli.toIArray1.asInstanceOf[Kleisli[Option, IArray[A], IArray1[A]]]
  def oneAndK[A]: Kleisli[Option, IArray[A], OneAnd[IArray, A]] =
    kleisli.oneAnd.asInstanceOf[Kleisli[Option, IArray[A], OneAnd[IArray, A]]]
  def headOptionK[A]: Kleisli[Option, IArray[A], A] =
    kleisli.headOption.asInstanceOf[Kleisli[Option, IArray[A], A]]
  def headMaybeK[A]: Kleisli[Maybe, IArray[A], A] =
    kleisli.headMaybe.asInstanceOf[Kleisli[Maybe, IArray[A], A]]
  def lastOptionK[A]: Kleisli[Option, IArray[A], A] =
    kleisli.lastOption.asInstanceOf[Kleisli[Option, IArray[A], A]]
  def lastMaybeK[A]: Kleisli[Maybe, IArray[A], A] =
    kleisli.lastMaybe.asInstanceOf[Kleisli[Maybe, IArray[A], A]]
  def tailOptionK[A]: Kleisli[Option, IArray[A], IArray[A]] =
    kleisli.tailOption.asInstanceOf[Kleisli[Option, IArray[A], IArray[A]]]
  def tailMaybeK[A]: Kleisli[Maybe, IArray[A], IArray[A]] =
    kleisli.tailMaybe.asInstanceOf[Kleisli[Maybe, IArray[A], IArray[A]]]
  def initOptionK[A]: Kleisli[Option, IArray[A], IArray[A]] =
    kleisli.initOption.asInstanceOf[Kleisli[Option, IArray[A], IArray[A]]]
  def initMaybeK[A]: Kleisli[Maybe, IArray[A], IArray[A]] =
    kleisli.initMaybe.asInstanceOf[Kleisli[Maybe, IArray[A], IArray[A]]]

  def tailOptionEndo[A]: Endomorphic[({ type λ[α, β] = Kleisli[Option, α, β] })#λ, IArray[A]] =
    kleisli.tailOptionEndo.asInstanceOf[Endomorphic[({ type λ[α, β] = Kleisli[Option, α, β] })#λ, IArray[A]]]
  def tailMaybeEndo[A]: Endomorphic[({ type λ[α, β] = Kleisli[Maybe, α, β] })#λ, IArray[A]] =
    kleisli.tailMaybeEndo.asInstanceOf[Endomorphic[({ type λ[α, β] = Kleisli[Maybe, α, β] })#λ, IArray[A]]]
  def initOptionEndo[A]: Endomorphic[({ type λ[α, β] = Kleisli[Option, α, β] })#λ, IArray[A]] =
    kleisli.initOptionEndo.asInstanceOf[Endomorphic[({ type λ[α, β] = Kleisli[Option, α, β] })#λ, IArray[A]]]
  def initMaybeEndo[A]: Endomorphic[({ type λ[α, β] = Kleisli[Maybe, α, β] })#λ, IArray[A]] =
    kleisli.initMaybeEndo.asInstanceOf[Endomorphic[({ type λ[α, β] = Kleisli[Maybe, α, β] })#λ, IArray[A]]]

  private[iarray] def byName2[A, B, C](f: (A, => B) => C): (A, B) => C = (a, b) => f(a, b)

  private[this] val _single: AnyRef => IArray[AnyRef] =
    single(_)

  private[iarray] def comparatorFromFunction[A](f: (A, A) => Boolean): java.util.Comparator[A] =
    new java.util.Comparator[A] {
      def compare(x: A, y: A) =
        if (f(x, y)) -1 else if (f(y, x)) 1 else 0
    }

  final def singleF[A]: A => IArray[A] =
    _single.asInstanceOf[A => IArray[A]]

  final def partitionEithers[L, R](eithers: IArray[L \/ R]): (IArray[L], IArray[R]) = {
    var i = 0
    val left, right = new ArrayBuilder.ofRef[AnyRef]
    while (i < eithers.length) {
      eithers(i).asInstanceOf[L \/ R] match {
        case \/-(r) => right += r.asInstanceOf[AnyRef]
        case -\/(l) => left += l.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(left.result), new IArray(right.result))
  }

  final def partitionStdEithers[L, R](eithers: IArray[L Either R]): (IArray[L], IArray[R]) = {
    var i = 0
    val left, right = new ArrayBuilder.ofRef[AnyRef]
    while (i < eithers.length) {
      eithers(i).asInstanceOf[L Either R] match {
        case Right(r) => right += r.asInstanceOf[AnyRef]
        case Left(l) => left += l.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(left.result), new IArray(right.result))
  }

  final def partitionTry[A](array: IArray[scala.util.Try[A]]): (IArray[Throwable], IArray[A]) = {
    var i = 0
    val errors, values = new ArrayBuilder.ofRef[AnyRef]
    while (i < array.length) {
      array(i).asInstanceOf[util.Try[A]] match {
        case scala.util.Success(a) => values += a.asInstanceOf[AnyRef]
        case scala.util.Failure(e) => errors += e.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(errors.result), new IArray(values.result))
  }

  final def partitionValidations[E, A](validations: IArray[Validation[E, A]]): (IArray[E], IArray[A]) = {
    var i = 0
    val success, failure = new ArrayBuilder.ofRef[AnyRef]
    while (i < validations.length) {
      validations(i).asInstanceOf[Validation[E, A]] match {
        case Success(r) => success += r.asInstanceOf[AnyRef]
        case Failure(l) => failure += l.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(failure.result), new IArray(success.result))
  }

  final def partitionThese[A, B](these: IArray[A \&/ B]): (IArray[A], IArray[B]) = {
    var i = 0
    val as, bs = new ArrayBuilder.ofRef[AnyRef]
    while (i < these.length) {
      these(i).asInstanceOf[A \&/ B] match {
        case \&/.This(a) => as += a.asInstanceOf[AnyRef]
        case \&/.That(b) => bs += b.asInstanceOf[AnyRef]
        case \&/.Both(a, b) =>
          as += a.asInstanceOf[AnyRef]
          bs += b.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(as.result), new IArray(bs.result))
  }

  final def partitionLazyTuples[A, B](tuples: IArray[LazyTuple2[A, B]]): LazyTuple2[IArray[A], IArray[B]] = {
    lazy val a = {
      var i = 0
      val array = new Array[AnyRef](tuples.length)
      while (i < tuples.length) {
        array(i) = tuples(i)._1.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[A](array)
    }
    lazy val b = {
      var i = 0
      val array = new Array[AnyRef](tuples.length)
      while (i < tuples.length) {
        array(i) = tuples(i)._2.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[B](array)
    }
    LazyTuple2(a, b)
  }

  final def partitionLazyTuple3[A, B, C](
    tuples: IArray[LazyTuple3[A, B, C]]
  ): LazyTuple3[IArray[A], IArray[B], IArray[C]] = {
    lazy val a = {
      var i = 0
      val array = new Array[AnyRef](tuples.length)
      while (i < tuples.length) {
        array(i) = tuples(i)._1.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[A](array)
    }
    lazy val b = {
      var i = 0
      val array = new Array[AnyRef](tuples.length)
      while (i < tuples.length) {
        array(i) = tuples(i)._2.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[B](array)
    }
    lazy val c = {
      var i = 0
      val array = new Array[AnyRef](tuples.length)
      while (i < tuples.length) {
        array(i) = tuples(i)._3.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[C](array)
    }
    LazyTuple3(a, b, c)
  }

  final def fillAll[A](size: Int)(elem: A): IArray[A] = {
    val array = new Array[AnyRef](size)
    java.util.Arrays.fill(array, elem)
    new IArray(array)
  }

  final def fill[A](size: Int)(f: => A): IArray[A] = {
    val array = new Array[AnyRef](size)
    var i = 0
    while (i < size) {
      array(i) = f.asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def unfold[A, B](z: B)(f: B => Option[(B, A)]): IArray[A] = {
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    @tailrec
    def loop(next: Option[(B, A)]): Unit =
      next match {
        case Some((b, a)) =>
          builder += a.asInstanceOf[AnyRef]
          loop(f(b))
        case None =>
      }
    loop(f(z))
    new IArray(builder.result)
  }

  implicit final def iarrayEqual[A: Equal]: Equal[IArray[A]] =
    Equal.equal(_ === _)

  private[this] val _iarrayMonoid: Monoid[IArray[AnyRef]] =
    new Monoid[IArray[AnyRef]] {
      def append(a: IArray[AnyRef], b: => IArray[AnyRef]) = a ++ b
      def zero = empty
    }

  implicit final def iarrayMonoid[A]: Monoid[IArray[A]] =
    _iarrayMonoid.asInstanceOf[Monoid[IArray[A]]]

  implicit val iarrayInstance: MonadPlus[IArray]
    with IsEmpty[IArray]
    with Traverse[IArray]
    with Zip[IArray]
    with Align[IArray]
    with Unzip[IArray]
    with Cobind[IArray] = IArrayInstance

  final val zipApply: Apply[IArray] =
    IArrayZipApply

  final def empty[A]: IArray[A] =
    _empty.asInstanceOf[IArray[A]]

  private[this] val _empty: IArray[AnyRef] =
    new IArray[AnyRef](new Array[AnyRef](0))

  final def single[A](a: A): IArray[A] =
    new IArray(Array[AnyRef](a.asInstanceOf[AnyRef]))

  final def apply[A](xs: A*): IArray[A] =
    if (xs.isEmpty) empty[A]
    else {
      if (xs.isInstanceOf[scala.collection.mutable.WrappedArray[_]]) {
        new IArray[A](
          toRefArray(
            xs.asInstanceOf[scala.collection.mutable.WrappedArray[A]].array
          )
        )
      } else {
        from(xs)
      }
    }

  final def fromRefArray[A <: AnyRef](xs: Array[A]): IArray[A] =
    new IArray[A](xs.clone.asInstanceOf[Array[AnyRef]])

  final def fromArray[A](xs: Array[A]): IArray[A] =
    if (xs.getClass.getComponentType.isPrimitive) {
      new IArray[A](copyAnyValArray(xs))
    } else {
      new IArray[A](xs.clone.asInstanceOf[Array[AnyRef]])
    }

  final def fromList[A](xs: List[A]): IArray[A] = {
    val array = new Array[AnyRef](xs.size)
    var list = xs
    var i = 0
    while (!list.isEmpty) {
      array(i) = list.head.asInstanceOf[AnyRef]
      i += 1
      list = list.tail
    }
    new IArray(array)
  }

  final def fromIList[A](xs: IList[A]): IArray[A] = {
    val array = new Array[AnyRef](xs.length)
    @tailrec def loop(list: IList[A], i: Int): Unit =
      list match {
        case ICons(h, t) =>
          array(i) = h.asInstanceOf[AnyRef]
          loop(t, i + 1)
        case _ =>
      }
    loop(xs, 0)
    new IArray(array)
  }

  final def fromIndexedSeq[A](xs: IndexedSeq[A]): IArray[A] = {
    val len = xs.size
    val array = new Array[AnyRef](len)
    var i = 0
    while (i < len) {
      array(i) = xs(i).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def iterate[A](start: A, len: Int)(f: A => A): IArray[A] =
    if (len <= 0) {
      empty
    } else {
      val array = new Array[AnyRef](len)
      var i = 1
      val f0 = f.asInstanceOf[AnyRef => AnyRef]
      array(0) = start.asInstanceOf[AnyRef]
      while (i < len) {
        array(i) = f0(array(i - 1))
        i += 1
      }
      new IArray(array)
    }

  final def tabulate[A](size: Int)(f: Int => A): IArray[A] =
    if (size <= 0) {
      empty
    } else {
      val array = new Array[AnyRef](size)
      var i = 0
      while (i < size) {
        array(i) = f(i).asInstanceOf[AnyRef]
        i += 1
      }
      new IArray(array)
    }

  final def from[A](xs: Iterable[A]): IArray[A] =
    xs match {
      case list: List[A] => fromList(list)
      case ixSq: IndexedSeq[A] => fromIndexedSeq(ixSq)
      case _ => {
        val ite = xs.iterator
        val array = new Array[AnyRef](xs.size)
        var i = 0
        while (ite.hasNext) {
          array(i) = ite.next().asInstanceOf[AnyRef]
          i += 1
        }
        new IArray[A](array)
      }
    }

  final def zip3[A, B, C](a: IArray[A], b: IArray[B], c: IArray[C]): IArray[(A, B, C)] = {
    val len = Math.min(Math.min(a.length, b.length), c.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = (a(i), b(i), c(i))
      i += 1
    }
    new IArray(array)
  }

  final def zip4[A, B, C, D](a: IArray[A], b: IArray[B], c: IArray[C], d: IArray[D]): IArray[(A, B, C, D)] = {
    val len = Math.min(Math.min(a.length, b.length), Math.min(c.length, d.length))
    var i = 0
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = (a(i), b(i), c(i), d(i))
      i += 1
    }
    new IArray(array)
  }

  final def zip5[A, B, C, D, E](
    a: IArray[A],
    b: IArray[B],
    c: IArray[C],
    d: IArray[D],
    e: IArray[E]
  ): IArray[(A, B, C, D, E)] = {
    val len = Math.min(Math.min(Math.min(Math.min(a.length, b.length), c.length), d.length), e.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = (a(i), b(i), c(i), d(i), e(i))
      i += 1
    }
    new IArray(array)
  }

  final def zipWith3[A, B, C, D](a: IArray[A], b: IArray[B], c: IArray[C])(f: (A, B, C) => D): IArray[D] = {
    val len = Math.min(Math.min(a.length, b.length), c.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = f(a(i), b(i), c(i)).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def zipWith4[A, B, C, D, E](a: IArray[A], b: IArray[B], c: IArray[C], d: IArray[D])(
    f: (A, B, C, D) => E
  ): IArray[E] = {
    val len = Math.min(Math.min(a.length, b.length), Math.min(c.length, d.length))
    var i = 0
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = f(a(i), b(i), c(i), d(i)).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def zipWith5[A, B, C, D, E, F](a: IArray[A], b: IArray[B], c: IArray[C], d: IArray[D], e: IArray[E])(
    f: (A, B, C, D, E) => F
  ): IArray[F] = {
    val len = Math.min(Math.min(Math.min(Math.min(a.length, b.length), c.length), d.length), e.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while (i < len) {
      array(i) = f(a(i), b(i), c(i), d(i), e(i)).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  private def copyAnyValArray(xs: Array[_]): Array[AnyRef] = {
    var i = xs.length - 1
    val array = new Array[AnyRef](xs.length)
    while (i >= 0) {
      array(i) = xs(i).asInstanceOf[AnyRef]
      i -= 1
    }
    array
  }

  private def toRefArray[A](xs: Array[A]): Array[AnyRef] =
    if (xs.getClass.getComponentType.isPrimitive) copyAnyValArray(xs)
    else xs.asInstanceOf[Array[AnyRef]]
}
