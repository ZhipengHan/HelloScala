package Concurrency

/**
 * Created by zhhan on 10/28/2015.
 */
class BondedBuffer[A](N: Int) {
  var in = 0
  var out = 0
  var n = 0
  val elems = new Array[A](N)

  def put(x: A) = synchronized{
    while(n >= N) wait()
    elems(in) = x; in = (in + 1)%N; n = n + 1
    if(n == 1) notifyAll()
  }

  def get: A = synchronized{
    while(n == 0) wait()
    val x = elems(out); out = (out + 1)%N; n = n - 1
    if(n == N - 1) notifyAll()
    x
  }
}
