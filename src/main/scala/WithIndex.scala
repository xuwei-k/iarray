package iarray

import scala.collection.generic.CanBuildFrom

final class WithIndex[A](private val self: Array[AnyRef]) extends AnyVal{
  def map[B](f: (A, Int) => B): IArray[B] = {
    var i = 0
    val array = new Array[AnyRef](self.length)
    while(i < self.length){
      array(i) = f(self(i).asInstanceOf[A], i).asInstanceOf[AnyRef]
      i += 1
    }
    new IArray(array)
  }

  def foreach[U](f: (A, Int) => U): Unit = {
    var i = 0
    while(i < self.length){
      f(self(i).asInstanceOf[A], i)
      i += 1
    }
  }

  def to[F[_]](implicit C: CanBuildFrom[Nothing, (A, Int), F[(A, Int)]]): F[(A, Int)] = {
    var i = 0
    val buf = C()
    while(i < self.length){
      buf += ((self(i).asInstanceOf[A], i))
      i += 1
    }
    buf.result
  }

}


