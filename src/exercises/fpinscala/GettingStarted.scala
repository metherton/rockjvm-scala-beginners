package exercises.fpinscala

object GettingStarted extends App {


  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int =
      if (n == 0) prev
      else go(n - 1, curr, prev + curr)

    go(n, 0, 1)
  }

  println(fib(5))

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(index: Int): Boolean = {
      if (index >= as.length - 1) true
      else if (gt(as(index), as(index + 1))) false
      else loop(index + 1)
    }
    loop(0)
  }


  val isIntegerGreaterThan = (a: Int, b: Int) => a > b

  println(isSorted(Array(7, 2, 3, 4, 5), isIntegerGreaterThan))

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def curryMore[A, B, C, D](f:(A, B, C) => D): A => (B => (C => D)) =
    (a: A) => (b: B) => (c: C) => f(a, b, c)

  val addA = curry((a: Int, b: Int) => a + b)
  val addB = addA(1)
  val res = addB(5)
  println(res)

  val timesTwo: Int => Int = (a: Int) => a * 2
  val timesThree: Int => Int = (a: Int) => a * 3
  val timesFour: Int => Int = (a: Int) => a * 4


  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
