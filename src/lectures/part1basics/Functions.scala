package lectures.part1basics

object Functions extends App {

  def aFunction(a: String, b: Int): String =
    a + " " + b

  println(aFunction("hello", 3))

  def aParameterlessFunction(): Int = 42

  println(aParameterlessFunction)

  def aRepeatedFunction(aString: String, n: Int): String =
    if (n == 1) aString
    else aString + aRepeatedFunction(aString, n - 1)

  println(aRepeatedFunction("hello", 3))

  // WHEN YOU NEED LOOPS, USE RECURSION

  def aBigFunction(n: Int): Int = {
    def aSmallerFunction(a: Int, b: Int): Int = a + b
    aSmallerFunction(n, n - 1)
  }

  /*
    1. A greeting function for kids (name, age) => "Hi my name is $name and I am $age years old"
    2. Factorial function 1 * 2 * 3.. * n .. a recursive function
    3. Fibannaci function f(1) = 1 , f(2) = 1, f(n) = f(n - 1) + f(n - 2)
   */

  def fibonacci(n: Int): Int = {
    if (n <= 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  println("fibonaci(6) is " + fibonacci(6))

  // 1 1 2 3 5 8 13

  def factorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n - 1)
  }

  println("factorial 3 is: " + factorial(3))

  def greeting(name: String, age: Int): Unit =
    println(f"Hi my name is $name and I am $age years old")

  greeting("martin", 57)

  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t - 1)

    isPrimeUntil(n / 2)
  }

  println(isPrime(2003))
  println(isPrime(17))
  println(isPrime(17 * 37))
}
