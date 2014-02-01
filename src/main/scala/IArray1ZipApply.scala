package iarray

import scalaz._

private object IArray1ZipApply extends Apply[IArray1] {

  override val applyApplicative = super.applyApplicative

  override def map[A, B](fa: IArray1[A])(f: A => B) =
    fa map f

  override def ap[A, B](fa: => IArray1[A])(f: => IArray1[A => B]) =
    fa.zipWith(f)((a, g) => g(a))

  override def apply2[A, B, C](fa: => IArray1[A], fb: => IArray1[B])(f: (A, B) => C) =
    fa.zipWith(fb)(f)

  override def apply3[A, B, C, D](fa: => IArray1[A], fb: => IArray1[B], fc: => IArray1[C])(f: (A, B, C) => D) =
    IArray1.zipWith3(fa, fb, fc)(f)

  override def apply4[A, B, C, D, E](fa: => IArray1[A], fb: => IArray1[B], fc: => IArray1[C], fd: => IArray1[D])(f: (A, B, C, D) => E) =
    IArray1.zipWith4(fa, fb, fc, fd)(f)

  override def tuple2[A, B](fa: => IArray1[A], fb: => IArray1[B]) =
    fa.zip(fb)

  // TODO https://github.com/scalaz/scalaz/commit/b24d595957b
  override def tuple3[A, B, C](fa: => IArray1[A], fb: => IArray1[B], fc: IArray1[C]) =
    IArray1.zip3(fa, fb, fc)

  override def tuple4[A, B, C, D](fa: => IArray1[A], fb: => IArray1[B], fc: => IArray1[C], fd: => IArray1[D]) =
    IArray1.zip4(fa, fb, fc, fd)

}

