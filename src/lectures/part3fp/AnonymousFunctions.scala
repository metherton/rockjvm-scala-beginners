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

  // careful
  println(justDoSomething) // function itself
  println(justDoSomething()) // call

  // curly braces with lamdas
  val stringToInt = { (str: String) =>
    str.toInt
  }

  println(stringToInt("55"))

  // MORE syntactic sugar
  val niceIncrementer: Int => Int = _ + 1 // equivalent to x => x + 1
  val niceAdder: (Int, Int) => Int = _ + _ // equivalent to (a, b) => a + b

  /*
      1. MyList: replace all FunctionX calls with lambdas
      2. Rewrite the "special" adder as an anonymous function
   */

}
