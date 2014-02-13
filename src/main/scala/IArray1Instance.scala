package iarray

import scalaz._

private object IArray1Instance extends
  Monad[IArray1] with
  Plus[IArray1] with
  Traverse1[IArray1] with
  Zip[IArray1] with
  Align[IArray1] with
  Unzip[IArray1] with
  Comonad[IArray1] {

  private[this] val _semigroup: Semigroup[IArray1[AnyRef]] =
    Semigroup.instance(_ plus _)

  override val flip =
    super.flip

  override val applyApplicative =
    super.applyApplicative

  override def semigroup[A] =
    _semigroup.asInstanceOf[Semigroup[IArray1[A]]]

  override def map[A, B](fa: IArray1[A])(f: A => B) =
    fa map f

  override def alignWith[A, B, C](f: A \&/ B => C): (IArray1[A], IArray1[B]) => IArray1[C] =
    { (a, b) => a.alignWith(b)(f) }

  override def align[A, B](fa: IArray1[A], fb: IArray1[B]): IArray1[A \&/ B] =
    fa align fb

  override def merge[A](a1: IArray1[A], a2: IArray1[A])(implicit A: Semigroup[A]) =
    IArray1(A.append(a1.head, a2.head), Align[IArray].merge(a1.tail, a2.tail))

  override def point[A](a: => A) =
    IArray1(a, IArray.empty[A])

  override def bind[A, B](fa: IArray1[A])(f: A => IArray1[B]) =
    fa flatMap f

  override def cobind[A, B](fa: IArray1[A])(f: IArray1[A] => B) =
    fa cobind f

  override def cojoin[A](fa: IArray1[A]) =
    fa.cojoin

  override def copoint[A](a: IArray1[A]) =
    a.head

  override def foldMapRight1[A, B](fa: IArray1[A])(z: A => B)(f: (A, => B) => B) =
    fa.foldMapRight1(z)(IArray.byName2(f))

  override def foldMapLeft1[A, B](fa: IArray1[A])(z: A => B)(f: (B, A) => B) =
    fa.foldMapLeft1(z)(f)

  override def foldMap1[A, B: Semigroup](fa: IArray1[A])(f: A => B) =
    fa foldMap1 f

  override def foldLeft[A, B](fa: IArray1[A], z: B)(f: (B, A) => B) =
    fa.foldl(z)(f)

  override def foldLeft1Opt[A](fa: IArray1[A])(f: (A, A) => A) =
    Some(fa foldl1 f)

  override def plus[A](a: IArray1[A], b: => IArray1[A]) =
    a plus b

  override def traverse1Impl[G[_]: Apply, A, B](fa: IArray1[A])(f: A => G[B]) =
    fa.traverse1(f)

  override def unzip[A, B](a: IArray1[(A, B)]) =
    a.unzip

  override def zip[A, B](a: => IArray1[A], b: => IArray1[B]) =
    a zip b

  override def length[A](fa: IArray1[A]) =
    fa.length

  override def maximum1[A: Order](fa: IArray1[A]) =
    fa.max

  override def minimum1[A: Order](fa: IArray1[A]) =
    fa.min

  override def maximumBy1[A, B: Order](fa: IArray1[A])(f: A => B) =
    fa maxBy f

  override def minimumBy1[A, B: Order](fa: IArray1[A])(f: A => B) =
    fa minBy f

  override def reverse[A](fa: IArray1[A]) =
    fa.reverse

  override def index[A](fa: IArray1[A], i: Int) =
    if(0 <= i && i < fa.length) Some(fa(i)) else None

  override def all[A](fa: IArray1[A])(f: A => Boolean) =
    fa forall f

  override def any[A](fa: IArray1[A])(f: A => Boolean) =
    fa exists f

  override def element[A: Equal](fa: IArray1[A], a: A) =
    fa contains a

  override def toList[A](fa: IArray1[A]) =
    fa.toList

  override def ap(implicit F: Functor[IArray1]) =
    if(F eq (this: Functor[IArray1])) IArray1ZipApply
    else super.ap

  override def intercalate[A: Monoid](fa: IArray1[A], a: A) =
    fa intercalate1 a

  override def intercalate1[A: Semigroup](fa: IArray1[A], a: A) =
    fa intercalate1 a
}

