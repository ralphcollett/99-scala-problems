package problems

object P01 {
  def last(input: List[Int]): Int = input match {
    case head :: Nil => head
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }
}
