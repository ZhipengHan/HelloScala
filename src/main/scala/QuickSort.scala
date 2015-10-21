/**
 * Created by zhhan on 10/10/2015.
 */
object QuickSort {

  def sort(xs: Array[Int]): Unit = {

    def swap(i: Int, j: Int): Unit = {
      val t = xs(i); xs(i) = xs(j); xs(j) = t;
    }

    def sortl(l: Int, r: Int): Unit ={
      val pivot = xs((l+r)/2)
      var i = l; var j = r;
      while(i <= j){
        while(xs(i) < pivot) i += 1
        while(xs(j) > pivot) j -= 1

        if(i <= j){
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if(l < j) sortl(l, j)
      if(j < r) sortl(i, r)
    }

    sortl(0, xs.length - 1)
  }

  def sort2(xs: Array[Int]): Array[Int] = {
    if(xs.length <= 1) xs
    else{
      val pivot = xs(xs.length / 2)
      Array.concat(
        sort2(xs filter(pivot >)),
      xs filter(pivot ==),
      sort2(xs filter(pivot <)))
    }
  }

  def main(args: Array[String]): Unit = {
    var xs = Array[Int](1,5,6,8,2);


    println("Array:")
    xs.foreach(print)

    println()
    println("Sort2 result:")
    sort2(xs).foreach(print);

    println()
    println("The old array not changed:")
    xs.foreach(print)

    sort(xs);
    println()
    println("Sort1 result, the old array changed:")
    xs.foreach(print)
  }
}
