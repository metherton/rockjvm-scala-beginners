package lectures.part3fp

object AnonymousFunctions extends App {

//  val doubler = new Function1[Int,Int] {
//    override def apply(x: Int) = 2 * x
//  }

  // anonymous function LAMDA
  val doubler = (x: Int) => x * 2

  // multiple params in a lambda
  val adder: (Int, Int) => Int = (a: Int, b: Int) => a + b

  // no params
  val justDoSomething: () => Int = () => 3

  val anInvocation = justDoSomething()
  // careful
  println(justDoSomething) // function itself
  println(justDoSomething()) // call

  // curly braces with lamdas
  val stringToInt = { (str: String) =>
    str.toInt
  }

  println(stringToInt("55"))


  // type inference
  val doubler_v3: Int => Int = x => x * 2

  val adder1: (Int, Int) => Int = (x, y) => x + y

  // shortest lamdas
  val doubler_v4: Int => Int = _ * 2

  // MORE syntactic sugar
  val niceIncrementer: Int => Int = _ + 1 // equivalent to x => x + 1
  val niceAdder: (Int, Int) => Int = _ + _ // equivalent to (a, b) => a + b

  // each underscore is a different argument, you can't reuse them

  /*
      1. MyList: replace all FunctionX calls with lambdas
      2. Rewrite the "special" adder as an anonymous function
   */

}
