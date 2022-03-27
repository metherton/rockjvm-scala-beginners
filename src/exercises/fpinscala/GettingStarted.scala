package exercises.fpinscala

object GettingStarted extends App {


  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, prev: Int): Int =
      if (i == 0) 0
      else if (i == 1) 1
      else if (i == n ) prev
      else go(i + 1, prev + i)

    go(0, 0)
  }

  println(fib(0))


}
