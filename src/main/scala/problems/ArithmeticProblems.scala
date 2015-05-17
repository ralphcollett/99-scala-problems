package problems

class S99Int(val i: Int) {

  // P31
  def isPrime = Range(2, i - 1).forall(i % _ != 0)
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}
