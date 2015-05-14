package problems

import scala.util.Random

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
  def length[A](input: List[A]) = input.foldLeft(0)((count, _) => count + 1)

  // P05
  def reverse[A](input: List[A]) = input.foldLeft(List[A]())((acc, element) => element :: acc)

  // P06
  def isPalindrome[A](input: List[A]) = input == reverse(input)

  // P07
  def flatten(input: List[Any]): List[Any] = input match {
    case Nil => Nil
    case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  // P08
  def compress[A](input: List[A]): List[A] = input match {
    case Nil => Nil
    case _ :: Nil => input
    case head :: tail if head == tail.head => compress(tail)
    case head :: tail => head :: compress(tail)
  }

  // P09
  def pack[A](input: List[A]): List[List[A]] = input match {
    case Nil => Nil
    case l@(head :: tail) =>
      val (matching, theRest) = l.span(_ == head)
      List(matching) ::: pack(theRest)
  }

  // P10
  def encode[A](input: List[A]): List[(Int, A)] =
    pack(input).map(packed => (packed.size, packed.head))

  // P11
  def encodeModified[A](input: List[A]): List[Any] =
    pack(input).map(packed => if (packed.size == 1) packed.head else (packed.size, packed.head))

  // P12
  def decode[A](input: List[(Int, A)]): List[A] =
    input.flatMap { case (n, a) => List.fill(n)(a) }

  // P13
  def encodeDirect[A](input: List[A]): List[(Int, A)] = input match {
    case Nil => Nil
    case l@(head :: tail) =>
      val (matching, theRest) = l.span(_ == head)
      List((matching.size, head)) ::: encodeDirect(theRest)
  }

  // P14
  def duplicate[A](input: List[A]): List[A] = input.flatMap(a => List(a, a))

  // P15
  def duplicateN[A](n: Int, input: List[A]) = input.flatMap(List.fill(n)(_))

  // P16
  def drop[A](n: Int, input: List[A]) = input.zipWithIndex.filter(i => (i._2 + 1) % n != 0).map(_._1)

  // P17
  def split[A](n: Int, input: List[A]): (List[A], List[A]) = input match {
    case Nil => (Nil, Nil)
    case list if n == 0 => (Nil, list)
    case head :: tail =>
      val (start, end) = split(n - 1, tail)
      (head :: start, end)
  }

  // P18
  def slice[A](i: Int, k: Int, input: List[A]): List[A] = i match {
    case 0 => input.take(k)
    case _ => slice(i - 1, k - 1, input.tail)
  }

  // P19
  def rotate[A](n: Int, input: List[A]) = n match {
    case 0 => input
    case _ if n > 1 =>
      val (start, end) = split(n, input)
      end ::: start
    case _ =>
      val (start, end) = split(input.length + n, input)
      end ::: start
  }

  // P20
  def removeAt[A](n: Int, input: List[A]) = {
    val (start, endHead :: endTail) = split(n, input)
    (start ::: endTail, endHead)
  }

  // P21
  def insertAt[A](a: A, n: Int, input: List[A]): List[A] =
    if (n == 0) a :: input else input.head :: insertAt(a, n - 1, input.tail)

  // P22
  def range(i: Int, k: Int): List[Int] = if (i > k) Nil else i :: range(i + 1, k)

  // P23
  def randomSelect[A](n: Int, input: List[A]): List[A] = {
    val random = Random

    List.fill(n)(input(random.nextInt(input.length)))
  }

  // P24
  def lotto(n: Int, m: Int) = randomSelect(n, range(1, m))

  // P25
  def randomPermute[A](input: List[A]) = {
    def loop(r: Random, source: List[A]): List[A] = source match {
      case Nil => Nil
      case _ =>
        val (remainder, removed) = removeAt(r.nextInt(source.length), source)
        removed :: loop(r, remainder)
    }

    loop(Random, input)
  }

  // P26
  def combinations[A](n: Int, input: List[A]): List[List[A]] = {
    n match {
      case 1 => input.map(List(_))
      case _ => input.zipWithIndex.flatMap { case (a, i) => combinations(n - 1, input.drop(i + 1)).map(a :: _) }
    }
  }
}
