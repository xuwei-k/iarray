package iarray

import scalaz._
import IArray._

private object IArrayInstance
    extends MonadPlus[IArray]
    with IsEmpty[IArray]
    with Traverse[IArray]
    with Zip[IArray]
    with Align[IArray]
    with Unzip[IArray]
    with Cobind[IArray] {
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
  override def traverseImpl[F[_], A, B](fa: IArray[A])(f: A => F[B])(implicit F: Applicative[F]) =
    F.map(std.list.listInstance.traverseImpl(fa.toList)(f))(IArray.fromList)
  override def traverseS[S, A, B](l: IArray[A])(f: A => State[S, B]) =
    State((s: S) => {
      var cur = s
      val result = l.map { a =>
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
    IArray.empty[A]
  def isEmpty[A](fa: IArray[A]) =
    fa.isEmpty
  def plus[A](a: IArray[A], b: => IArray[A]) =
    a ++ b
  override def mapAccumL[S, A, B](fa: IArray[A], z: S)(f: (S, A) => (S, B)) =
    fa.mapAccumL(z)(f)
  override def mapAccumR[S, A, B](fa: IArray[A], z: S)(f: (S, A) => (S, B)) =
    fa.mapAccumR(z)(f)
  override def fold[A: Monoid](fa: IArray[A]) =
    fa.fold
  override def foldMap1Opt[A, B: Semigroup](fa: IArray[A])(f: A => B) =
    fa foldMap1Opt f
  override def foldMapRight1Opt[A, B](fa: IArray[A])(z: A => B)(f: (A, => B) => B) =
    fa.foldMapR1Opt(z)(byName2(f))
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
  override def foldRightM[G[_], A, B](fa: IArray[A], z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] = {
    def go(list: List[A]): Free.Trampoline[G[B]] =
      list match {
        case h :: t => Trampoline.suspend(go(t)).map(gb => M.bind(gb)(f(h, _)))
        case Nil => Trampoline.done(M.point(z))
      }
    go(fa.toList).run
  }
  override def foldLeftM[G[_], A, B](fa: IArray[A], z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] = {
    def go(list: List[A]): Free.Trampoline[G[B]] =
      list match {
        case h :: t => Trampoline.suspend(go(t)).map(gb => M.bind(gb)(f(_, h)))
        case Nil => Trampoline.done(M.point(z))
      }
    go(fa.reverseList).run
  }
  override def empty[A](fa: IArray[A]) =
    fa.isEmpty
  override def index[A](fa: IArray[A], i: Int) =
    if (0 <= i && i < fa.length) Some(fa(i)) else None
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
    a.alignWith(b)(conform)
  override def merge[A](a1: IArray[A], a2: IArray[A])(implicit A: Semigroup[A]) = {
    val min = Math.min(a1.length, a2.length)
    val len = Math.max(a1.length, a2.length)
    val array = new Array[AnyRef](len)
    var i = 0
    while (i < min) {
      array(i) = A.append(a1(i), a2(i)).asInstanceOf[AnyRef]
      i += 1
    }
    if (min == a1.length) {
      System.arraycopy(a2.self, i, array, min, len - min)
    } else {
      System.arraycopy(a1.self, i, array, min, len - min)
    }
    new IArray(array)
  }
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
  override def maximumOf[A, B: Order](fa: IArray[A])(f: A => B) =
    fa maxOf f
  override def minimumOf[A, B: Order](fa: IArray[A])(f: A => B) =
    fa minOf f
  override def unite[T[_], A](value: IArray[T[A]])(implicit T: Foldable[T]) =
    bind(value)(ta => T.foldMap(ta)(singleF)(iarrayMonoid))
  override def separate[G[_, _], A, B](value: IArray[G[A, B]])(implicit G: Bifoldable[G]) = {
    if (G eq (\/.DisjunctionInstances2: Bifoldable[\/]))
      partitionEithers(value.asInstanceOf[IArray[A \/ B]])
    else if (G eq (std.tuple.tuple2Bitraverse: Bifoldable[Tuple2]))
      value.asInstanceOf[IArray[(A, B)]].unzip
    else if (G eq (Validation.ValidationInstances0: Bifoldable[Validation]))
      partitionValidations(value.asInstanceOf[IArray[Validation[A, B]]])
    else if (G eq (std.either.eitherInstance: Bifoldable[Either]))
      partitionStdEithers(value.asInstanceOf[IArray[A Either B]])
    else if (G eq (LazyTuple2.lazyTuple2Instance: Bifoldable[LazyTuple2])) {
      val t = partitionLazyTuples(value.asInstanceOf[IArray[LazyTuple2[A, B]]])
      (t._1, t._2)
    } else if (G eq (\&/.TheseBitraverse: Bifoldable[\&/]))
      partitionThese(value.asInstanceOf[IArray[A \&/ B]])
    else super.separate(value)
  }
  override def all[A](fa: IArray[A])(f: A => Boolean) =
    fa forall f
  override def any[A](fa: IArray[A])(f: A => Boolean) =
    fa exists f
  override def ap(implicit F: Functor[IArray]) =
    if (F eq (this: Functor[IArray])) zipApply
    else super.ap
}
