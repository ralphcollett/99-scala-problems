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

  // P03
  def nth(n: Int, input: List[Int]): Int = n match {
    case 0 => input.head
    case _ if n < 0 => throw new NoSuchElementException
    case _ => nth(n - 1, input.tail)
  }

  // P04
  def length(input: List[Int]): Int = {
    input match {
      case Nil => 0
      case _ :: tail => length(tail) + 1
    }
  }
}
