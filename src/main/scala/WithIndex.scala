package iarray

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

  // TODO more methods ?
}


