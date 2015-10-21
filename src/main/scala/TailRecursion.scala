/**
 * Created by zhhan on 10/12/2015.
 */
object TailRecursion {
  // tail-recursive
  def gcd(a: Int, b: Int): Int = if(b == 0)a else gcd(b, a%b)

  // not tail-recursive
  def factorial(n: Int): Int = if(n == 0) 1 else n*factorial(n - 1)

  // Exercise 4.6.1 tail-recursive
  def factorial2(n: Int, result: Int = 1): Int = {
    if(n == 0) result else factorial2(n - 1, result * n)
  }

  def main(args: Array[String]): Unit = {
    println("Start")

    println(gcd(14, 21))
    println(factorial(5))
    println(factorial2(5))

    println("End")
  }
}
