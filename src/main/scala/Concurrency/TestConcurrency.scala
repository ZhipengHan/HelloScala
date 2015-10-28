package Concurrency

import scala.concurrent.SyncVar
import scala.concurrent.ops._
import scala.util.Random

/**
 * Created by zhhan on 10/28/2015.
 */
object TestConcurrency {
  def main(args: Array[String]): Unit ={
    val buf = new BondedBuffer[String](10)

    spawn{ while(true) { val s = produceString; buf.put(s)}}
    spawn{ while(true) { val s = buf.get; consumeString(s)}}
  }

  def produceString:String = {
    val s = Random.nextString(10)
    s
  }

  def consumeString(s: String) = {
    print(s)
  }

  def spawn(p: => Unit): Unit ={
    val t = new Thread(){override def run() = p }
    t.start()
  }

//  def future[A](p: => A): Unit => A = {
//    val result = new SyncVar[A]
//    fork(result.set(p))
//    (() => result.get)
//  }

  def par[A, B](xp: => A, yp: => B): (A, B) = {
    val y = new SyncVar[B]
    spawn{ y set yp}
    (xp, y.get)
  }

  def replicate(start: Int, end: Int)(p: Int => Unit): Unit ={
    if(start == end)()
    else if(start + 1 == end)
      p(start)
    else{
      val mid = (start + end) / 2
      spawn{ replicate(start, mid)(p)}
      replicate(mid, end)(p)
    }
  }

  def parMap[A,B](f: A => B, xs: Array[A]): Array[B] = {
    val results = new Array[B](xs.length)
    replicate(0, xs.length){i => results(i) = f(xs(i))}
    results
  }
}

class Lock{
  var available = true
  def acquire = synchronized{
    while(!available) wait()
    available = false
  }
  def release = synchronized{
    available = true
    notify()
  }
}

class ReadersWriters{
  val m = new MailBox
  private case class Writers(n: Int);
  private case class Readers(n: Int){m send this}
  Writers(0); Readers(0)
  def

}