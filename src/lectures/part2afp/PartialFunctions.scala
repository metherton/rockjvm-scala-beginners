package lectures.part2afp

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new RuntimeException("no suitable cases")


  val aFussyFunction_v2 = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }


  // partial function
  val aPartialFunction_v2: PartialFunction[Int, Int] = { // x match {...}
      case 1 => 42
      case 2 => 56
      case 5 => 999
  }

  println(aPartialFunction_v2(1))

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // {1, 2, 5} => Int

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value

  println(aPartialFunction(2))

  // PF utilities
  println(aPartialFunction.isDefinedAt(67))

  // lift
  val liftedPf = aPartialFunction.lift // Int => Option[Int]
  println(liftedPf(2))
  println(liftedPf(96))


  val anotherPf: PartialFunction[Int, Int] = {
    case 45 => 86
  }

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  val pfChain2 = aPartialFunction.orElse[Int, Int](anotherPf)

  println(pfChain(2))
  println(pfChain(45))

  // PF extend normal functions

  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  val aMappedList = List(1,2,3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }

  println(aMappedList)

  /*
      NOTE: PF can only have one PARAMETER type

   */

  /**
    *   Exercises
    *
    *   1 - construct a PF instance yourself (anonymouns class)
    *   2 - dumb chatbot as a PF
    */


  val aManualFussyFunction = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 42
      case 2 => 65
      case 5 => 999
    }
    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  println(aManualFussyFunction(1))

  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi my name is HAL9000"
    case "goodbye" => "Once you start talking there is no way out"
    case "call mom" => "unable to find your phone without your credit card"
  }

  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)

}
