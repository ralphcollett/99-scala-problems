package problems

import org.scalatest.{Matchers, FunSuite}
import S99Int._

class ArithmeticProblemsTest extends FunSuite with Matchers {

  test("P31: Determine whether a given integer number is prime") {
    4.isPrime should be(false)

    7.isPrime should be(true)
  }
}
