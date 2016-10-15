package week2

/**
  * Created by engin on 07.10.2016.
  */
object test {

  import math.abs

  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) =
  //fixedPoint(y => x / y)(1)
    fixedPoint(averageDamp(y => x / y))(1)

  def main(args: Array[String]) {
    sqrt(2)
  }

}
