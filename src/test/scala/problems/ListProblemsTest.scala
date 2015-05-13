package problems

import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{FunSuite, Matchers}
import problems.ListProblems._

class ListProblemsTest extends FunSuite with Matchers {

  test("P01: Find the last element of a list") {
    last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }

  test("P02: Find the last but one element of a list") {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be(5)
  }

  test("P03: Find the nth element of a list") {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be(2)
  }

  test("P04: Find the number of elements of a list") {
    ListProblems.length(List(1, 1, 2, 3, 5, 8)) should be(6)
  }

  test("P05: Reverse a list") {
    reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }

  test("P06: Find out whether a list is a palindrome") {
    isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
    isPalindrome(List(1, 2, 2, 1)) should be(true)
    isPalindrome(List(1, 2, 3, 2, 4)) should be(false)
  }

  test("P07: Flatten a nested list structure") {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }

  test("P08: Eliminate consecutive duplicates of list elements") {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("P09: Pack consecutive duplicates of list elements into sublists") {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("P10: Run-length encoding of a list") {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("P11: Modified run-length encoding") {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  test("P12: Decode a run-length encoded list") {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("P13: Run-length encoding of a list (direct solution)") {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  test("P14: Duplicate the elements of a list") {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("P15: Duplicate the elements of a list a given number of times") {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("P16: Drop every Nth element from a list") {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("P17: Split a list into two parts") {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("P18: Extract a slice from a list") {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g))
  }

  test("P19: Rotate a list N places to the left") {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("P20: Remove the Kth element from a list") {
    removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd), 'b))
  }

  test("P21: Insert an element at a given position into a list") {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

  test("P22: Create a list containing all integers within a given range") {
    range(4, 9) should be(List(4, 5, 6, 7, 8, 9))
  }

  def beASubListOf[A](parent: List[A]) = new Matcher[List[A]] {
    override def apply(left: List[A]): MatchResult = MatchResult(
      left.forall(parent.contains(_)),
      s"$left was not a sublist of $parent (elements not contained: ${left.filterNot(parent.contains(_))})",
      s"$left was a sublist of $parent"
    )
  }

  test("P23: Extract a given number of randomly selected elements from a list") {
    val input = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val randomSelection = randomSelect(3, input)

    randomSelection should have length 3
    randomSelection should beASubListOf(input)
  }

  test("P24: Lotto: Draw N different random numbers from the set 1..M") {
    val lottoResults = lotto(6, 49)

    lottoResults should have length 6
    lottoResults should beASubListOf(range(1, 49))
  }
}
