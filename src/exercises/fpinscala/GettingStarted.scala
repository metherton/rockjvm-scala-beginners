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

}
