package problems

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
}
