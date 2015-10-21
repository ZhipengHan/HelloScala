/**
 * Created by zhhan on 10/12/2015.
 */
object FirstClassFunctions {
  // sum all integers between two given numbers a and b
  def sumInts(a: Int, b: Int): Int = if(a > b) 0 else a + sumInts(a + 1, b)

  // sum all x*x that {x | a<=x<=b}
  def square(x: Int): Int = x * x

  def sumSquares(a: Int, b: Int): Int = if(a > b) 0 else square(a) + sumSquares(a + 1, b)

  // sum 2^x that {x | a<=x<=b}
  def powerOfTwo(x: Int): Int = if(x == 0) 1 else 2 * powerOfTwo(x - 1)

  def sumPowersOfTwo(a: Int, b: Int): Int = if(a > b) 0 else powerOfTwo(a) + sumPowersOfTwo(a + 1, b)

  // sum with function
  def sum(f: Int => Int, a: Int, b: Int): Int = if(a > b) 0 else f(a) + sum(f, a + 1, b)

  def id(x: Int): Int = x
  def sumInts2(a: Int, b: Int): Int = sum(id, a, b)

  def sumSquares2(a: Int, b: Int): Int = sum(square, a, b)

  def sumPowersOfTwo2(a: Int, b: Int): Int = sum(powerOfTwo, a, b)

  // anonymous functions
  def sumInts3(a: Int, b: Int): Int = sum(x => x, a, b)

  def sumSquares3(a: Int, b: Int): Int = sum(x => x*x, a, b)

  // currying
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = if(a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }

  def sumInts4 = sum(x => x)
  def sumSquares4 = sum(x => x*x)
  def sumPowersOfTwo4 = sum(powerOfTwo)
  // equivarently
//  val sumInts = sum(x => x)
//  val sumSquares = sum(x => x*x)
//  val sumPowersOfTwo = sum(powerOfTwo)

  //exercise 5.2.1
  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
    def iter(a: Int, result: Int): Int = {
      if(a>b) result
      else iter(a + 1, result + f(a))
    }
    iter(a, 0)
  }

  def main(args: Array[String]): Unit = {
    println("Start")

    println(sumInts(3, 7))
    println(sumInts2(3, 7))

    println(sumSquares(3, 7))
    println(sumSquares2(3, 7))

    println(sumPowersOfTwo(3, 7))
    println(sumPowersOfTwo2(3, 7))

    println(sumSquares4(1, 10) + sumPowersOfTwo4(10, 20))

    // test exercise 5.2.1
    println(sum2(x => x)(1, 10))

    println("End")
  }

}
