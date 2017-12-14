package iarray

import scalaz._
import IArray._

private object IArrayZipApply extends Apply[IArray] {
  override val applyApplicative =
    super.applyApplicative
  def map[A, B](fa: IArray[A])(f: A => B) =
    fa map f
  def ap[A, B](fa: => IArray[A])(f: => IArray[A => B]) =
    fa.zipWith(f)((a, g) => g(a))
  override def apply2[A, B, C](fa: => IArray[A], fb: => IArray[B])(f: (A, B) => C) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else _fa.zipWith(fb)(f)
  }
  override def apply3[A, B, C, D](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C])(f: (A, B, C) => D) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else {
      val _fb = fb
      if (_fb.isEmpty) empty
      else zipWith3(_fa, _fb, fc)(f)
    }
  }
  override def apply4[A, B, C, D, E](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C], fd: => IArray[D])(
    f: (A, B, C, D) => E) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else {
      val _fb = fb
      if (_fb.isEmpty) empty
      else {
        val _fc = fc
        if (_fc.isEmpty) empty
        else zipWith4(_fa, _fb, _fc, fd)(f)
      }
    }
  }
  override def apply5[A, B, C, D, E, F](
    fa: => IArray[A],
    fb: => IArray[B],
    fc: => IArray[C],
    fd: => IArray[D],
    fe: => IArray[E])(f: (A, B, C, D, E) => F) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else {
      val _fb = fb
      if (_fb.isEmpty) empty
      else {
        val _fc = fc
        if (_fc.isEmpty) empty
        else {
          val _fd = fd
          if (_fd.isEmpty) empty
          else zipWith5(_fa, _fb, _fc, _fd, fe)(f)
        }
      }
    }
  }
  override def tuple2[A, B](fa: => IArray[A], fb: => IArray[B]) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else _fa.zip(fb)
  }
  override def tuple3[A, B, C](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C]) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else {
      val _fb = fb
      if (_fb.isEmpty) empty
      else zip3(_fa, _fb, fc)
    }
  }
  override def tuple4[A, B, C, D](fa: => IArray[A], fb: => IArray[B], fc: => IArray[C], fd: => IArray[D]) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else {
      val _fb = fb
      if (_fb.isEmpty) empty
      else {
        val _fc = fc
        if (_fc.isEmpty) empty
        else zip4(_fa, _fb, _fc, fd)
      }
    }
  }
  override def tuple5[A, B, C, D, E](
    fa: => IArray[A],
    fb: => IArray[B],
    fc: => IArray[C],
    fd: => IArray[D],
    fe: => IArray[E]) = {
    val _fa = fa
    if (_fa.isEmpty) empty
    else {
      val _fb = fb
      if (_fb.isEmpty) empty
      else {
        val _fc = fc
        if (_fc.isEmpty) empty
        else {
          val _fd = fd
          if (_fd.isEmpty) empty
          else zip5(_fa, _fb, _fc, _fd, fe)
        }
      }
    }
  }
}
