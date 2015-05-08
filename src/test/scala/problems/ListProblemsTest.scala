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
}