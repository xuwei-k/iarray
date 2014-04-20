package iarray

import scalaz._

private object IArray1Instance extends
  Monad[IArray1] with
  Plus[IArray1] with
  Traverse1[IArray1] with
  Zip[IArray1] with
  Unzip[IArray1] with
  Comonad[IArray1] {

  private[this] val _semigroup: Semigroup[IArray1[AnyRef]] =
    Semigroup.instance(_ plus _)

  override val flip =
    super.flip

  override def semigroup[A] =
    _semigroup.asInstanceOf[Semigroup[IArray1[A]]]

  override def map[A, B](fa: IArray1[A])(f: A => B) =
    fa map f

  override def point[A](a: => A) =
    IArray1(a, IArray.empty[A])

  override def bind[A, B](fa: IArray1[A])(f: A => IArray1[B]) =
    fa flatMap f

  override def join[A](fa: IArray1[IArray1[A]]) =
    fa.flatten

  override def cobind[A, B](fa: IArray1[A])(f: IArray1[A] => B) =
    fa cobind f

  override def cojoin[A](fa: IArray1[A]) =
    fa.cojoin

  override def copoint[A](a: IArray1[A]) =
    a.head

  override def foldMap1[A, B: Semigroup](fa: IArray1[A])(f: A => B) =
    fa foldMap1 f

  override def foldLeft[A, B](fa: IArray1[A], z: B)(f: (B, A) => B) =
    fa.foldl(z)(f)

  override def foldRight1[A](fa: IArray1[A])(f: (A, => A) => A) =
    fa.foldr1(IArray.byName2(f))

  override def plus[A](a: IArray1[A], b: => IArray1[A]) =
    a plus b

  override def traverse1Impl[G[_]: Apply, A, B](fa: IArray1[A])(f: A => G[B]) =
    fa.traverse1(f)

  override def unzip[A, B](a: IArray1[(A, B)]) =
    a.unzip

  override def zip[A, B](a: => IArray1[A], b: => IArray1[B]) =
    a zip b

  override def maximum1[A: Order](fa: IArray1[A]) =
    fa.max

  override def minimum1[A: Order](fa: IArray1[A]) =
    fa.min

  override def reverse[A](fa: IArray1[A]) =
    fa.reverse

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

}

