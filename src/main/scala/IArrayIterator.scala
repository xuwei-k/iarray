package iarray

private final class IArrayIterator[A](array: Array[AnyRef]) extends Iterator[A]{

  private[this] var i = 0
  def hasNext = i < array.length
  def next(): A =
    if(hasNext){
      val r = array(i).asInstanceOf[A]
      i += 1
      r
    }else Iterator.empty.next
}

