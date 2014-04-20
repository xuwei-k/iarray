package iarray

import scalaz._
import IArray._

private object IArrayInstance extends MonadPlus[IArray] with IsEmpty[IArray] with Traverse[IArray] with Zip[IArray] with Unzip[IArray] with Cobind[IArray] with Cojoin[IArray] {
  override val flip =
    super.flip
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
  override def foldMap[A, B: Monoid](fa: IArray[A])(f: A => B) =
    fa foldMap f
  override def foldRight[A, B](fa: IArray[A], z: => B)(f: (A, => B) => B) =
    fa.foldr(z)(byName2(f))
  override def foldLeft[A, B](fa: IArray[A], z: B)(f: (B, A) => B) =
    fa.foldl(z)(f)
  override def foldRightM[G[_], A, B](fa: IArray[A], z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] = {
    def go(list: List[A]): Free.Trampoline[G[B]] = list match {
      case h :: t => Trampoline.suspend(go(t)).map(gb => M.bind(gb)(f(h, _)))
      case Nil    => Trampoline.done(M.point(z))
    }
    go(fa.toList).run
  }
  override def foldLeftM[G[_], A, B](fa: IArray[A], z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] = {
    def go(list: List[A]): Free.Trampoline[G[B]] = list match {
      case h :: t => Trampoline.suspend(go(t)).map(gb => M.bind(gb)(f(_, h)))
      case Nil    => Trampoline.done(M.point(z))
    }
    go(fa.reverseList).run
  }
  override def empty[A](fa: IArray[A]) =
    fa.isEmpty
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
  def cobind[A, B](fa: IArray[A])(f: IArray[A] => B) =
    fa cobind f
  override def cojoin[A](fa: IArray[A]) =
    fa.cojoin
  override def maximum[A: Order](fa: IArray[A]) =
    fa.max
  override def minimum[A: Order](fa: IArray[A]) =
    fa.min
  override def unite[T[_], A](value: IArray[T[A]])(implicit T: Foldable[T]) =
    bind(value)(ta => T.foldMap(ta)(singleF)(iarrayMonoid))
  override def all[A](fa: IArray[A])(f: A => Boolean) =
    fa forall f
  override def any[A](fa: IArray[A])(f: A => Boolean) =
    fa exists f
  override def ap(implicit F: Functor[IArray]) =
    if(F eq (this: Functor[IArray])) zipApply
    else super.ap
}

