package ImplicitConversion

/**
 * Created by zhhan on 2015/10/28.
 */
abstract class SemiGroup[A]{
  def add(x: A, y: A): A
}
abstract class Monoid[A] extends SemiGroup[A]{
  def unit: A
}
object stringMonoid extends Monoid[String]{
  def add(x: String, y: String): String = x.concat(y)
  def unit: String = ""
}
object intMonoid extends Monoid[Int]{
  def add(x: Int, y: Int): Int = x + y
  def unit: Int = 0
}

// implicit conversions


object ImplicitConversionTest {
  def sum[A](xs: List[A])(implicit m: Monoid[A]): A = if(xs.isEmpty) m.unit
  else m.add(xs.head, sum(xs.tail))

  implicit def int2ordered(x: Int): Ordered[Int]  = new Ordered[Int]{
    def compare(y: Int): Int =
      if(x < y) -1
      else if(x > y) 1
      else 0
  }
  def sort[A <% Ordered[A]](xs: List[A]): List[A] =
  if(xs.isEmpty || xs.tail.isEmpty) xs
  else {
    val (ys, zs) = xs.splitAt(xs. length / 2)
      merge(ys, zs)
  }

    def merge[A](xs: List[A], ys: List[A])
                (implicit c: A => Ordered[A]): List[A] =
      if (xs.isEmpty) ys
      else if (ys.isEmpty) xs
      else if (c(xs.head) < ys.head) xs.head :: merge(xs.tail, ys)
      else ys.head :: merge(xs, ys.tail)(c)

  def main (args: Array[String]) {
    println(sum(List("a","bc","def"))(stringMonoid))
    println(sum(List(1, 2, 3))(intMonoid))
    //println(sum(List(1, 2, 3, 4)))

  }
}
