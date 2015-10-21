/**
 * Created by zhhan on 10/12/2015.
 */

import scala.math._

object FixedPoints {
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      println(next)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  // exercise 5.3.1
  def cubeRoots(x: Double) = fixedPoint(averageDamp(y => x/(y*y)))(1.0d)

  def main(args: Array[String]): Unit = {
    println("Start")
    //println(sqrt(2))
    println(cubeRoots(27))
    println("End")
  }
}
