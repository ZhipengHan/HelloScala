/**
 * Created by zhhan on 10/13/2015.
 */
abstract class IntStack{
  def push(x: Int): IntStack = new IntNonEmptyStack(x, this)
  def isEmpty: Boolean
  def top: Int
  def pop: IntStack
}

class IntEmptyStack extends IntStack{
  def isEmpty: Boolean = true
  def top: Int = error("EmptyStack.top")
  def pop: IntStack = error("EmptyStack.pop")
}

class IntNonEmptyStack(elem: Int, rest: IntStack) extends IntStack{
  def isEmpty: Boolean = false
  def top: Int = elem
  def pop: IntStack = rest
}

// generic type
abstract class Stack[A]{
  def push(x: A): Stack[A] = new NonEmptyStack(x, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}

class EmptyStack[A] extends Stack[A]{
  def isEmpty: Boolean = true
  def top: A = error("EmptyStack.top")
  def pop: Stack[A] = error("EmptyStack.pop")
}

class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A]{
  def isEmpty: Boolean = false
  def top: A = elem
  def pop: Stack[A] = rest
}

abstract class Set[A]{
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}

object GenericTypesandMethods {
  def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean = {
    p.isEmpty ||
      p.top == s.top && isPrefix[A](p.pop, s.pop)
  }

  def main(args: Array[String]): Unit = {
    val x = new EmptyStack[Int]
    val y = x.push(1).push(2)
    println(y.pop.pop)

    val s1 = new EmptyStack[String].push("abc")
    val s2 = new EmptyStack[String].push("abc").push(s1.top)
    println(isPrefix(s1, s2))
  }
}
