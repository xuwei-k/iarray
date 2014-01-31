package iarray

import scalaz._

private object IArray1ZipApply extends Apply[IArray1] {

  override val applyApplicative = super.applyApplicative

  override def map[A, B](fa: IArray1[A])(f: A => B) =
    fa map f

  override def ap[A, B](fa: => IArray1[A])(f: => IArray1[A => B]) =
    fa.zipWith(f)((a, g) => g(a))

}

