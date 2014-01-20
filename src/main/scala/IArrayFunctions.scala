package iarray

import annotation.tailrec
import scalaz._
import collection.generic.CanBuildFrom
import collection.mutable.ArrayBuilder

private[iarray] abstract class IArrayFunctions{

  private[this] val _single: AnyRef => IArray[AnyRef] =
    single(_)

  final def singleF[A]: A => IArray[A] =
    _single.asInstanceOf[A => IArray[A]]

  final def partitionEithers[L, R](eithers: IArray[L \/ R]): (IArray[L], IArray[R]) = {
    var i = 0
    val left, right = new ArrayBuilder.ofRef[AnyRef]
    while(i < eithers.length){
      eithers(i).asInstanceOf[L \/ R] match {
        case \/-(r) => right += r.asInstanceOf[AnyRef]
        case -\/(l) => left  += l.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(left.result), new IArray(right.result))
  }

  final def partitionStdEithers[L, R](eithers: IArray[L Either R]): (IArray[L], IArray[R]) = {
    var i = 0
    val left, right = new ArrayBuilder.ofRef[AnyRef]
    while(i < eithers.length){
      eithers(i).asInstanceOf[L Either R] match {
        case Right(r) => right += r.asInstanceOf[AnyRef]
        case Left(l)  => left  += l.asInstanceOf[AnyRef]
      }
      i += 1
    }
    (new IArray(left.result), new IArray(right.result))
  }

  final def partitionTry[A](array: IArray[scala.util.Try[A]]): (IArray[Throwable], IArray[A]) = {
    var i = 0
    val errors, values = new ArrayBuilder.ofRef[AnyRef]
    while(i < array.length){
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
    while(i < validations.length){
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
    while(i < these.length){
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
      while(i < tuples.length){
        array(i) = tuples(i)._1.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[A](array)
    }
    lazy val b = {
      var i = 0
      val array = new Array[AnyRef](tuples.length)
      while(i < tuples.length){
        array(i) = tuples(i)._2.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[B](array)
    }
    LazyTuple2(a, b)
  }

  private[this] val _canBuildFrom: CanBuildFrom[Nothing, AnyRef, IArray[AnyRef]] =
    new CanBuildFrom[Nothing, AnyRef, IArray[AnyRef]] {
      import collection.mutable.Builder

      def apply() =
        new Builder[AnyRef, IArray[AnyRef]]{
          private[this] val buf: ArrayBuilder.ofRef[AnyRef] =
            new ArrayBuilder.ofRef[AnyRef]()
          def +=(elem: Object) = {
            buf += elem
            this
          }
          def clear() = buf.clear
          def result() = new IArray(buf.result)
        }
      def apply(from: Nothing) = apply()
    }

  implicit final def canBuildFrom[A]: CanBuildFrom[Nothing, A, IArray[A]] =
    _canBuildFrom.asInstanceOf[CanBuildFrom[Nothing, A, IArray[A]]]

  final def fillAll[A](size: Int)(elem: A): IArray[A] = {
    val array = new Array[AnyRef](size)
    java.util.Arrays.fill(array, elem)
    new IArray(array)
  }

  final def fill[A](size: Int)(f: => A): IArray[A] = {
    val array = new Array[AnyRef](size)
    var i = 0
    while(i < size){
      array(i) = f.asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def unfold[A, B](z: B)(f: B => Option[(B, A)]): IArray[A] = {
    val builder = new ArrayBuilder.ofRef[AnyRef]()
    @tailrec
    def loop(next: Option[(B, A)]): Unit = next match {
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

  implicit val iarrayInstance: MonadPlus[IArray] with IsEmpty[IArray] with Traverse[IArray] with Zip[IArray] with Align[IArray] with Unzip[IArray] with Cobind[IArray] =
    new MonadPlus[IArray] with IsEmpty[IArray] with Traverse[IArray] with Zip[IArray] with Align[IArray] with Unzip[IArray] with Cobind[IArray] {
      private[this] def byName2[A, B, C](f: (A, => B) => C): (A, B) => C = (a, b) => f(a, b)
      override val flip =
        super.flip
      override val applyApplicative =
        super.applyApplicative
      override def monoid[A] =
        iarrayMonoid[A]
      override def semigroup[A] =
        iarrayMonoid[A]
      override def map[A, B](fa: IArray[A])(f: A => B) =
        fa map f
      override def filter[A](fa: IArray[A])(f: A => Boolean) =
        fa filter f
      override def reverse[A](fa: IArray[A]) =
        fa.reverse
      override def traverseImpl[F[_], A, B](fa: IArray[A])(f: A => F[B])(implicit F:Applicative[F]) =
        F.map(std.list.listInstance.traverseImpl(fa.toList)(f))(IArray.fromList)
      override def traverseS[S, A, B](l: IArray[A])(f: A => State[S, B]) =
        State((s: S) => {
          var cur = s
          val result = l.map{ a =>
            val bs = f(a)(cur)
            cur = bs._1
            bs._2
          }
          (cur, result)
        })
      override def toList[A](fa: IArray[A]) =
        fa.toList
      def bind[A, B](fa: IArray[A])(f: A => IArray[B]) =
        fa flatMap f
      override def join[A](ffa: IArray[IArray[A]]) =
        ffa.flatten
      def point[A](a: => A) =
        single(a)
      def empty[A] =
        _empty.asInstanceOf[IArray[A]]
      def isEmpty[A](fa: IArray[A]) =
        fa.isEmpty
      def plus[A](a: IArray[A], b: => IArray[A]) =
        a ++ b
      override def fold[A: Monoid](fa: IArray[A]) =
        fa.fold
      override def foldMap1Opt[A, B: Semigroup](fa: IArray[A])(f: A => B) =
        fa foldMap1Opt f
      override def foldMapRight1Opt[A, B](fa: IArray[A])(z: A => B)(f: (A, => B) => B) =
        fa.foldMapR1(z)(byName2(f))
      override def foldMapLeft1Opt[A, B](fa: IArray[A])(z: A => B)(f: (B, A) => B) =
        fa.foldMapL1(z)(f)
      override def foldMap[A, B: Monoid](fa: IArray[A])(f: A => B) =
        fa foldMap f
      override def foldRight[A, B](fa: IArray[A], z: => B)(f: (A, => B) => B) =
        fa.foldr(z)(byName2(f))
      override def foldLeft[A, B](fa: IArray[A], z: B)(f: (B, A) => B) =
        fa.foldl(z)(f)
      override def foldLeft1Opt[A](fa: IArray[A])(f: (A, A) => A) =
        fa foldl1 f
      override def foldRight1Opt[A](fa: IArray[A])(f: (A, => A) => A) =
        fa.foldr1(byName2(f))
      override def empty[A](fa: IArray[A]) =
        fa.isEmpty
      override def index[A](fa: IArray[A], i: Int) =
        if(0 <= i && i < fa.length) Some(fa(i)) else None
      override def length[A](fa: IArray[A]) =
        fa.length
      def zip[A, B](a: => IArray[A], b: => IArray[B]) =
        zipApply.tuple2(a, b)
      override def zipWith[A, B, C](a: => IArray[A], b: => IArray[B])(f: (A, B) => C)(implicit F: Functor[IArray]) =
        zipApply.apply2(a, b)(f)
      override def element[A: Equal](fa: IArray[A], a: A) =
        fa contains a
      override def intercalate[A: Monoid](fa: IArray[A], a: A) =
        fa intercalate a
      def unzip[A, B](a: IArray[(A, B)]) =
        a.unzip
      override def firsts[A, B](a: IArray[(A, B)]) =
        a.firsts
      override def seconds[A, B](a: IArray[(A, B)]) =
        a.seconds
      def alignWith[A, B, C](f: A \&/ B => C) =
        (a, b) => a.alignWith(b)(f)
      override def align[A, B](a: IArray[A], b: IArray[B]) =
        a.alignWith(b)(conforms)
      def cobind[A, B](fa: IArray[A])(f: IArray[A] => B) =
        fa cobind f
      override def cojoin[A](fa: IArray[A]) =
        fa.cojoin
      override def maximum[A: Order](fa: IArray[A]) =
        fa.max
      override def minimum[A: Order](fa: IArray[A]) =
        fa.min
      override def maximumBy[A, B: Order](fa: IArray[A])(f: A => B) =
        fa maxBy f
      override def minimumBy[A, B: Order](fa: IArray[A])(f: A => B) =
        fa minBy f
      override def unite[T[_], A](value: IArray[T[A]])(implicit T: Foldable[T]) =
        bind(value)(ta => T.foldMap(ta)(singleF)(iarrayMonoid))
      override def separate[G[_, _], A, B](value: IArray[G[A, B]])(implicit G: Bifoldable[G]) = {
        if(G eq (\/.DisjunctionInstances3: Bifoldable[\/]))
          partitionEithers(value.asInstanceOf[IArray[A \/ B]])
        else if(G eq (std.tuple.tuple2Bitraverse: Bifoldable[Tuple2]))
          value.asInstanceOf[IArray[(A, B)]].unzip
        else if(G eq (Validation.ValidationInstances0: Bifoldable[Validation]))
          partitionValidations(value.asInstanceOf[IArray[Validation[A, B]]])
        else if(G eq (std.either.eitherInstance: Bifoldable[Either]))
          partitionStdEithers(value.asInstanceOf[IArray[A Either B]])
        else if(G eq (LazyTuple2.lazyTuple2Instance: Bifoldable[LazyTuple2])){
          val t = partitionLazyTuples(value.asInstanceOf[IArray[LazyTuple2[A, B]]])
          (t._1, t._2)
        }else if(G eq (\&/.TheseBitraverse: Bifoldable[\&/]))
          partitionThese(value.asInstanceOf[IArray[A \&/ B]])
        else super.separate(value)
      }
      override def all[A](fa: IArray[A])(f: A => Boolean) =
        fa forall f
      override def any[A](fa: IArray[A])(f: A => Boolean) =
        fa exists f
      override def ap(implicit F: Functor[IArray]) =
        if(F eq (this: Functor[IArray])) zipApply
        else super.ap
    }

  final val zipApply: Apply[IArray] =
    new Apply[IArray] {
      override val applyApplicative = super.applyApplicative
      def map[A, B](fa: IArray[A])(f: A => B) =
        fa map f
      def ap[A, B](fa: => IArray[A])(f: => IArray[A => B]) =
        fa.zipWith(f)((a, g) => g(a))
      override def apply2[A, B, C](fa: => IArray[A], fb: => IArray[B])(f: (A, B) => C) = {
        val _fa = fa
        if(_fa.isEmpty) empty
        else _fa.zipWith(fb)(f)
      }
      override def apply3[A, B, C, D](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C])(f: (A, B, C) => D) = {
        val _fa = fa
        if(_fa.isEmpty) empty
        else{
          val _fb = fb
          if(_fb.isEmpty) empty
          else zipWith3(_fa, _fb, fc)(f)
        }
      }
      override def apply4[A, B, C, D, E](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C], fd: => IArray[D])(f: (A, B, C, D) => E) = {
        val _fa = fa
        if(_fa.isEmpty) empty
        else{
          val _fb = fb
          if(_fb.isEmpty) empty
          else{
            val _fc = fc
            if(_fc.isEmpty) empty
            else zipWith4(_fa, _fb, _fc, fd)(f)
          }
        }
      }
      override def tuple2[A, B](fa: => IArray[A], fb: => IArray[B]) = {
        val _fa = fa
        if(_fa.isEmpty) empty
        else _fa.zip(fb)
      }
      // TODO https://github.com/scalaz/scalaz/commit/b24d595957
      override def tuple3[A, B, C](fa: => IArray[A], fb: => IArray[B], fc: IArray[C]) = {
        val _fa = fa
        if(_fa.isEmpty) empty
        else{
          val _fb = fb
          if(_fb.isEmpty) empty
          else zip3(_fa, _fb, fc)
        }
      }
      override def tuple4[A, B, C, D](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C], fd: => IArray[D]) = {
        val _fa = fa
        if(_fa.isEmpty) empty
        else{
          val _fb = fb
          if(_fb.isEmpty) empty
          else{
            val _fc = fc
            if(_fc.isEmpty) empty
            else zip4(_fa, _fb, _fc, fd)
          }
        }
      }
    }

  final def empty[A]: IArray[A] =
    _empty.asInstanceOf[IArray[A]]

  private[this] val _empty: IArray[AnyRef] =
    new IArray[AnyRef](new Array[AnyRef](0))

  final def single[A](a: A): IArray[A] =
    new IArray(Array[AnyRef](a.asInstanceOf[AnyRef]))

  final def apply[A](xs: A*): IArray[A] =
    if(xs.isEmpty) empty[A]
    else new IArray[A](
      toRefArray(
        xs.asInstanceOf[collection.mutable.WrappedArray[A]].array
      )
    )

  final def fromList[A](xs: List[A]): IArray[A] = {
    val array = new Array[AnyRef](xs.size)
    var list = xs
    var i = 0
    while(! list.isEmpty){
      array(i) = list.head.asInstanceOf[AnyRef]
      i += 1
      list = list.tail
    }
    new IArray(array)
  }

  final def fromIndexedSeq[A](xs: IndexedSeq[A]): IArray[A] = {
    val len = xs.size
    val array = new Array[AnyRef](len)
    var i = 0
    while(i < len){
      array(i) = xs(i).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def iterate[A](start: A, len: Int)(f: A => A): IArray[A] =
    if(len <= 0){
      empty
    }else{
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
    if(size <= 0){
      empty
    }else{
      val array = new Array[AnyRef](size)
      var i = 0
      while (i < size) {
        array(i) = f(i).asInstanceOf[AnyRef]
        i += 1
      }
      new IArray(array)
    }

  final def from[A](xs: Iterable[A]): IArray[A] = xs match {
    case list: List[A] => fromList(list)
    case ixSq: IndexedSeq[A] => fromIndexedSeq(ixSq)
    case _ => {
      val ite = xs.iterator
      val array = new Array[AnyRef](xs.size)
      var i = 0
      while(ite.hasNext){
        array(i) = ite.next.asInstanceOf[AnyRef]
        i += 1
      }
      new IArray[A](array)
    }
  }

  final def zip3[A, B, C](a: IArray[A], b: IArray[B], c: IArray[C]): IArray[(A, B, C)] = {
    val len = Math.min(Math.min(a.length, b.length), c.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = (a(i), b(i), c(i))
      i += 1
    }
    new IArray(array)
  }

  final def zip4[A, B, C, D](a: IArray[A], b: IArray[B], c: IArray[C], d: IArray[D]): IArray[(A, B, C, D)] = {
    val len = Math.min(Math.min(a.length, b.length), Math.min(c.length, d.length))
    var i = 0
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = (a(i), b(i), c(i), d(i))
      i += 1
    }
    new IArray(array)
  }

  final def zipWith3[A, B, C, D](a: IArray[A], b: IArray[B], c: IArray[C])(f: (A, B, C) => D): IArray[D] = {
    val len = Math.min(Math.min(a.length, b.length), c.length)
    var i = 0
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = f(a(i), b(i), c(i)).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  final def zipWith4[A, B, C, D, E](a: IArray[A], b: IArray[B], c: IArray[C], d: IArray[D])(f: (A, B, C, D) => E): IArray[E] = {
    val len = Math.min(Math.min(a.length, b.length), Math.min(c.length, d.length))
    var i = 0
    val array = new Array[AnyRef](len)
    while(i < len){
      array(i) = f(a(i), b(i), c(i), d(i)).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  private def copyAnyValArray(xs: Array[_]): Array[AnyRef] = {
    var i = xs.length - 1
    val array = new Array[AnyRef](xs.length)
    while(i >= 0){
      array(i) = xs(i).asInstanceOf[AnyRef]
      i -= 1
    }
    array
  }

  private def toRefArray[A](xs: Array[A]): Array[AnyRef] =
    if(xs.getClass.getComponentType.isPrimitive) copyAnyValArray(xs)
    else xs.asInstanceOf[Array[AnyRef]]
}

