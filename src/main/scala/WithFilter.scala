package iarray

import collection.mutable.ArrayBuilder
import collection.generic.CanBuildFrom

final class WithFilter[A] private[iarray] (self: Array[AnyRef], f: A => Boolean){

  def to[F[_]](implicit C: CanBuildFrom[Nothing, A, F[A]]): F[A] = {
    val buf = C()
    var i = 0
    while(i < self.length){
      if(f(self(i).asInstanceOf[A])){
        buf += self(i).asInstanceOf[A]
      }
      i += 1
    }
    buf.result
  }

  def map[B](g: A => B): IArray[B] = {
    val buf = new ArrayBuilder.ofRef[AnyRef]
    var i = 0
    val f1 = f.asInstanceOf[AnyRef => Boolean]
    val f2 = g.asInstanceOf[AnyRef => AnyRef]
    while(i < self.length){
      if(f1(self(i))){
        buf += f2(self(i))
      }
      i += 1
    }
    new IArray(buf.result)
  }

  def flatMap[B](g: A => IArray[B]): IArray[B] = {
    val buf = new ArrayBuilder.ofRef[AnyRef]
    var i = 0
    val f1 = f.asInstanceOf[AnyRef => Boolean]
    val f2 = g.asInstanceOf[AnyRef => IArray[AnyRef]]
    while(i < self.length){
      if(f1(self(i))){
        buf ++= f2(self(i)).self
      }
      i += 1
    }
    new IArray(buf.result)
  }

  def foreach[U](g: A => U): Unit = {
    var i = 0
    val f1 = f.asInstanceOf[AnyRef => Boolean]
    val f2 = g.asInstanceOf[AnyRef => U]
    while(i < self.length){
      if(f1(self(i))){
        f2(self(i))
      }
      i += 1
    }
  }

  def withFilter(g: A => Boolean): WithFilter[A] =
    new WithFilter[A](self, x => f(x) && g(x))
}

