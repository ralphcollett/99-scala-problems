package problems

import org.scalatest.{FunSuite, Matchers}

class P01Test extends FunSuite with Matchers {

  test("Find the last element of a list") {
    P01.last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }
}
