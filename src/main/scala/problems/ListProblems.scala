package problems

object ListProblems {

  // P01
  def last(input: List[Int]): Int = input match {
    case last :: Nil => last
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }

  // P02
  def penultimate(input: List[Int]): Int = input match {
    case penultimate :: _ :: Nil => penultimate
    case Nil => throw new NoSuchElementException
    case last :: Nil => throw new NoSuchElementException
    case _ :: tail => penultimate(tail)
  }
}
