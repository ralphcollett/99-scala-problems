package problems

object ListProblems {

  // P01
  def last[A](input: List[A]): A = input match {
    case last :: Nil => last
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }

  // P02
  def penultimate[A](input: List[A]): A = input match {
    case penultimate :: _ :: Nil => penultimate
    case Nil => throw new NoSuchElementException
    case last :: Nil => throw new NoSuchElementException
    case _ :: tail => penultimate(tail)
  }

  // P03
  def nth[A](n: Int, input: List[A]): A = n match {
    case 0 => input.head
    case _ if n < 0 => throw new NoSuchElementException
    case _ => nth(n - 1, input.tail)
  }

  // P04
  def length[A](input: List[A]): Int = input.foldLeft(0)((count, _) => count + 1)

  // P05
  def reverse[A](input: List[A]): List[A] = input.foldLeft(List[A]())((acc, element) => element :: acc)
}
