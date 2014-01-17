package iarray

private final class IArrayIterator[+A](array: IArray[A]) extends Iterator[A]{

  private[this] var i = 0
  def hasNext = i < array.length
  def next(): A = {
    val r = array(i)
    i += 1
    r
  }
}

