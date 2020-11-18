package lectures.part2oop

object Exceptions extends App {

  val x: String = null
  //println(x.length)
  // this will crash with NullPointerException
  // 1. throwing and catching exceptions

//  val aWeirdValue = throw new NullPointerException

  // throwable classes extend the Throwable class
  // Exception and Error are the major Throwable subtypes

  // 2. How to catch exceptions
  def getInt(withExceptions: Boolean): Int =
    if (withExceptions) throw new RuntimeException("no int for you")
    else 42

  val potentialFail = try {
    getInt(true)
  } catch {
    case e: RuntimeException => println("caught a runtime exception")
  } finally {
    // code that will get executed no matter what
    // optional
    // does not influence the return type of this expression
    // use finally only for side effects, e.g logging

    println("finally")

    // 3. how to define your own exception
    class MyException extends Exception
    val exception = new MyException
    class OverflowException extends RuntimeException
    class UnderflowException extends RuntimeException
    class MathCalculationException extends RuntimeException("Division by 0")

  //  throw exception


    /*
        1. Crash your program with OutOfMemoryException
        2. Crash with StackOverflowError
        3. PocketCalculator -
            - add(x, y)
            - subtract(x, y)
            - multiply(x, y)
            - divide(x, y)

            Throw
              - OverflowException if add(x,y) exceeds Int.MAX_VALUE
              - UnderflowException if sutract(x,y) exceeds Int.MIN_VALUE
              - MathCalculationException for division by 0
     */

    // Outofmemory exception
//    val longArray: Array[Long] = new Array(Int.MaxValue)

    // stackoverflowexception
//    def fib(count: Int): Int =
//      if (count == Int.MaxValue) 1
//      else fib(count + 1) + count
//    fib(1)

    object PocketCalculator {
      def add(x: Int, y: Int) = {
        val result = x + y
        if (x > 0 && y > 0 && result < 0) throw new OverflowException
        else if (x < 0 && y < 0 && result > 0) throw new UnderflowException
        else result
      }

      def subtract(x: Int, y: Int) = {
        val result = x - y
        if (x > 0 && y < 0 && result < 0) throw new OverflowException
        else if (x < 0 && y > 0 && result > 0) throw new UnderflowException
        else result
      }


      def multiply(x: Int, y: Int) = {
        val result = x * y
        if (x > 0 && y > 0 && result < 0) throw new OverflowException
        else if (x < 0 && y < 0 && result < 0) throw new OverflowException
        else if (x > 0 && y < 0 && result > 0) throw new UnderflowException
        else if (x < 0 && y > 0 && result > 0) throw new UnderflowException
        else result
      }

      def divide(x: Int, y: Int) = {
        if (y == 0) throw new MathCalculationException
        else x / y
      }

    }

    val pc = PocketCalculator
    println(pc.add(5, 15))
    println(pc.divide(5, 0))

  }

}
