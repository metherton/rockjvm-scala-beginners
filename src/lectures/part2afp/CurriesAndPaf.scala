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

  val add7a = (x: Int) => simpleAddFunction(x, 7)
  println(add7a(5))

  val add7b = (x: Int) => simpleAddMethod(x, 7)
  println(add7b(6))


  val add7c = (x: Int) => curriedAddMethod(7)(x)
  println(add7c(7))

  val add7d = (x: Int) => simpleAddFunction.curried(7)
  println(add7d(81))

  val add7e = (x: Int) => curriedAddMethod(7) _ // PAF
  println(add7e(10))


  val add7f = (x: Int) => curriedAddMethod(7)(_) // PAF - alternative syntax

  val add7g = (x: Int) => simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
    // COMPILER rewritest this as y => simpleAddMethod(7,y)

}
