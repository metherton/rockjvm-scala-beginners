package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {


  def factorial(n: Int): Int =
    if (n <= 1) 1
    else n * factorial(n - 1)

  def anotherFactorial(n: Int): BigInt = {
    @tailrec
    def factHelper(x: Int, accumulator: BigInt): BigInt =
      if (x <= 1) accumulator
      else factHelper(x - 1, x * accumulator) // TAIL RECURSION

    factHelper(n, 1)
  }

  /*
    anotherFactorial(10) = factHelper(10,1)
    = factHelper(9, 10 * 1)
    = factHelper(8, 9 * 10 * 1)
    = factHelper(7, 8 * 9 * 10 * 1)
      ..
    = factHelper(2, 3 * 4.. 10 * 1)
    = factHelper(1, 2 * 3.. 10 * 1)
   */

  println(anotherFactorial(5000))

  /**
    *   1. Concatenates a string n times
    *   2. isPrime function tail recursive
    *   3. Fibonacci - tail recursive
    */



  @tailrec
  def concatenateTailrec(aString: String, n: Int, accumulator: String): String = {
    if (n <= 0) accumulator
    else concatenateTailrec(aString, n - 1, aString + accumulator)
  }
  println(concatenateTailrec("hello", 3, ""))


  def isPrime(n: Int): Boolean = {
    def isPrimeTailrec(t: Int, isStillPrime: Boolean): Boolean =
      if (!isStillPrime) false
      else if (t <= 1) true
      else isPrimeTailrec(t - 1, n % t != 0 && isStillPrime)
    isPrimeTailrec(n / 2, true)
  }

  println(isPrime(2003))

  def fibannaci(n: Int): Int = {

    def fibTailrec(i: Int, last: Int, nextToLast: Int): Int = {
      if (i >= n) last
      else fibTailrec(i + 1, last + nextToLast, last)
    }
    if (n <= 2) 1
    else fibTailrec(2, 1, 1)
  }


  println(fibannaci(8))

  val accountIdMapping = Map()

  case class Connection(accountId: Long)

  val conn1 = Connection(1)
  val conn2 = Connection(2)

  val listConns = List(conn1, conn2)



}
