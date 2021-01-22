package lectures.part2afp

object CurriesAndPaf extends App {

  // curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int  => Int = y => 3 + y

  println(add3(4))
  println(superAdder(3)(4))

  // METHOD !
  def curriedAdder(x: Int)(y: Int): Int = x + y

  println(curriedAdder(4)(5))

  // lifting .. converting method to functions
  val add4: Int => Int = curriedAdder(4)
  // lifting - ETA expansion

  // functions != methods (JVM limitation)


  def inc(x: Int): Int = x + 1

  List(1,2,3).map(inc) // the compiler actually changes this to
  List(1,2,3).map(x => inc(1)) // ETA expansion


  println(add4(10))


  // PARTIAL function applications

  val add5 = curriedAdder(5) _ // the _ tells the compiler to do the ETA expansion so i don't need to specify the type now


  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y  - add7 is a function value
  // as many different implementations of add7 using the above
  // be creative

  val add7 = (x: Int) => simpleAddFunction(x, 7)


  val add7_2 = (x: Int) => simpleAddFunction.curried(7)


  val add7_6 = (x: Int) => simpleAddFunction(7, _: Int)


  val add7_3 = (x: Int) => curriedAddMethod(7) _ // PAF



  val add7_4 = (x: Int) => curriedAddMethod(7)(_) // PAF - alternative syntax

  val add7_5 = (x: Int) => simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
    // COMPILER rewritest this as y => simpleAddMethod(7,y)

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c // x: String => concatenator(hello, x, how are you)

  val insertName = concatenator("Hello, I'm ", _: String, ", how are you ?")

  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello ", _: String, _: String) // (x,y) => concatenator(Hello, x, y)

  println(fillInTheBlanks("Daniel, ", "Scala is awesome"))

  /*

      EXERCISES
      1. Process a list of numbers and return their string representations with different formats
        Use the %4.2f , %8.6f and %14.12f with a curried function
   */

  def curriedFormatter(fmt: String)(number: Double): String = fmt.format(number)

  val fourTwoFormatter = curriedFormatter("%4.2f") _
  val eightSixFormatter = curriedFormatter("%8.6f") _
  val fourteenTwelveFormatter = curriedFormatter("%14.12f") _  // lift
  val formatters = List(fourTwoFormatter, eightSixFormatter, fourteenTwelveFormatter)


  val newFormats = List(23.12, 24242.121212, 3.32323, 5533.232323, 3423423423.2342)
    .flatMap(x => formatters.map(n => n(x))) // compiler does sweet eta exmpansion

  println(newFormats)

  println("%4.2f".format(Math.PI))
  println("%8.6f".format(Math.PI))
  println("%14.12f".format(Math.PI))

  // curried formatter function takes 2 params..a format and a number..
  // apply that curried formatter function on a list of numbers as a higher order function


  /*

      EXERCISES
      2. differences between
          - functions vs methods
          - parmeters: by-name vs 0-lamda
   */

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1
  def method: Int = 42
  def parenMethod(): Int = 42

  /*
      calling byName and byFunction
      - int
      - method
      - parenMethod
      - lamda
      - PAF

   */

  println(byName(23)) // ok
  println(byName(method))
  println(byName(parenMethod()))
  println(byName(parenMethod)) // ok but beware ==> byName(parenMethod())
 // byName(() => 42) // not ok
  byName((() => 42)()) // ok

  // byName(parenMethod _) // not ok .. function value not a substitue for by name parameter

  // byFunction(45) // not ok

  // byFunction(method) // not ok - does not do eta expansion here !

  byFunction(parenMethod) // compiler does eta expansion
  byFunction(() => 46) // works
  byFunction(parenMethod _) // also works - unnecessary
}

