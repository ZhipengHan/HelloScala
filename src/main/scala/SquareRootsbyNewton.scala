/**
 * Created by zhhan on 10/10/2015.
 */
import scala.math._

object SquareRootsbyNewton {


  def sqrt(x: Double) = {

    def square(x: Double): Double = x * x

    def sqrtIter(guess: Double, x: Double): Double = {
      println(guess, x)
      if(isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    }

    def isGoodEnough(guess: Double, x: Double): Boolean = {
      abs(square(guess) - x) < 0.001
    }

    def improve(guess: Double, x: Double): Double = {
      (guess + x/guess)/2
    }
    sqrtIter(1, x)
  }

  def main(args: Array[String]): Unit = {
    println(sqrt(2))

    println(sqrt(4))

    println(sqrt(0.5))
  }
}
