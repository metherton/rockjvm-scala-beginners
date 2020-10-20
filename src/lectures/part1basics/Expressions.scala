package lectures.part1basics

object Expressions extends App {

  val x = 1 + 2 // EXPRESSION
  println(x)


  println(2 + 3 * 4)

  // + / - * & | << >> >>> (right shift with zero extension)

  println(1 == x)
  // == != > < <= >=

  println(!(1 == x))

  // ! || &&

  var aVariable = 2
  aVariable +=3 // also works with *= -= /= .... side effects

  println(aVariable)

  // Instructions (DO) vs Expressions (VALUE)

  // IF expression
  val aCondition = true
  val aConditionedValue = if (aCondition) 5 else 3
  println(aConditionedValue)


  // EVERYTHING in scala is an expression

  val aWeirdValue = (aVariable = 3)
  println(aWeirdValue)

  // side effects.. printlin(), whiles, reassigning

  // Code blocks
  val aCodeBlock = {
    val y = 2
    val z = y + 1
    if (z > 2) "Hello" else "Goodbye"
  }



}
