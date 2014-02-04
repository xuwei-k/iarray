package iarray

import scala.collection.{mutable, immutable, generic}
import scala.collection.generic.CanBuildFrom
import scala.annotation.unchecked.uncheckedVariance

object WrappedIArray extends generic.SeqFactory[WrappedIArray] {
  def newBuilder[A]: mutable.Builder[A, WrappedIArray[A]] =
    new mutable.Builder[A, WrappedIArray[A]] {
      val buf = IArray.canBuildFrom[A].apply
      def +=(a: A) = {
         buf += a
         this
      }
      def clear(): Unit = buf.clear()

      def result = new WrappedIArray(buf.result)
    }

  private[this] val cbf = new CanBuildFrom[Nothing, AnyRef, WrappedIArray[AnyRef]] {
    def apply = newBuilder
    def apply(from: Nothing) = newBuilder
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Nothing, A, WrappedIArray[A]] =
    cbf.asInstanceOf[CanBuildFrom[Nothing, A, WrappedIArray[A]]]
}

final class WrappedIArray[A](val self: IArray[A])
  extends immutable.IndexedSeq[A]
  with generic.GenericTraversableTemplate[A, WrappedIArray]
  with collection.IndexedSeqOptimized[A, WrappedIArray[A]] {

  override def companion = WrappedIArray

  override def apply(i: Int): A =
    self(i)

  override def length: Int =
    self.length

  override def size: Int =
    self.length

  override def filter(f: A => Boolean): WrappedIArray[A] =
    new WrappedIArray(self.filter(f))

  override def foreach[U](f: A => U): Unit =
    self foreach f

  override def span(f: A => Boolean): (WrappedIArray[A], WrappedIArray[A]) = {
    val x = self span f
    (new WrappedIArray(x._1), new WrappedIArray(x._2))
  }

  override def splitAt(i: Int): (WrappedIArray[A], WrappedIArray[A]) = {
    val x = self splitAt i
    (new WrappedIArray(x._1), new WrappedIArray(x._2))
  }

  override def toArray[B >: A](implicit B: reflect.ClassTag[B]): Array[B] =
    self.toArray

  override def toList: List[A] =
    self.toList

  override def to[C[_]](implicit c: CanBuildFrom[Nothing, A, C[A @uncheckedVariance]]): C[A @uncheckedVariance] =
    self.to[C]

  override def foldLeft[B](b: B)(f: (B, A) => B): B =
    self.foldl(b)(f)

  override def foldRight[B](b: B)(f: (A, B) => B): B =
    self.foldr(b)(f)

  override def reverse: WrappedIArray[A] =
    new WrappedIArray(self.reverse)

  override def drop(n: Int): WrappedIArray[A] =
    new WrappedIArray(self dropL n)

  override def dropRight(n: Int): WrappedIArray[A] =
    new WrappedIArray(self dropR n)

  override def take(n: Int): WrappedIArray[A] =
    new WrappedIArray(self takeL n)

  override def takeWhile(f: A => Boolean): WrappedIArray[A] =
    new WrappedIArray(self takeWhileL f)

  override def dropWhile(f: A => Boolean): WrappedIArray[A] =
    new WrappedIArray(self dropWhileL f)

  override def takeRight(n: Int): WrappedIArray[A] =
    new WrappedIArray(self takeR n)

  override def count(f: A => Boolean): Int =
    self count f

  override def forall(f: A => Boolean): Boolean =
    self forall f

  override def exists(f: A => Boolean): Boolean =
    self exists f
}

