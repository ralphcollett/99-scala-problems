package problems

import org.scalatest.{FunSuite, Matchers}
import problems.ListProblems._

class ListProblemsTest extends FunSuite with Matchers {

  test("Find the last element of a list") {
    last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }

  test("Find the last but one element of a list") {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be(5)
  }

}
