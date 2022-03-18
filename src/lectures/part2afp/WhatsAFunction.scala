package lectures.part2afp

object WhatsAFunction {

  // FP functions are first class citizens
  // JVM

  trait MyFunction[A, B] {
    def apply(arg: A): B
  }

  val doubler: MyFunction[Int, Int] = new MyFunction[Int, Int] {
    override def apply(arg: Int): Int = arg * 2
  }

  val meaningOfLife = 42

  doubler(meaningOfLife) // same as doubler.apply

  val doublerStandard = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 * 2
  }

  val meaningDoubled_v2 = doublerStandard(meaningOfLife)

  // all functions are instances of FunctionX


  val adder = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  println(adder(5,6))

  val anAddition = adder(2, 67)

  // (Int, String, Double, Boolean) => Int === Function4[Int, String, Double, Boolean, Int]
  val aFourArgFunction = new Function4[Int, String, Double, Boolean, Int] {
    override def apply(v1: Int, v2: String, v3: Double, v4: Boolean): Int = ???
  }

  /*
      Exercises
      1. A function which takes 2 strings as arguments and concatenates them
      2. Replace Predicate and Transformer with appropriate function types if necessary
      3. Define a function which takes an Int as an argument and returns ANOTHER FUNCTION as a result
   */


  // 1
  val concatenator: (String, String) => String = new Function2[String, String, String] {
    override def apply(a: String, b: String): String = a + b
  }


  println(concatenator("Martin ", "Etherton"))


  //2
  // yes : Predicate[T] equivalent with Function1[T, Boolean] === T => Boolean
  // yes : Transformer[A, B] equivalent with Function1[A, B] === A => B

  //3

  val curryF = new Function1[Int, (Int => Int)] {
    override def apply(v1: Int): Int => Int = {
      (v2) => v1 + v2
    }
  }
  val add1 = curryF(1)
  println(add1(4))

  def main(args: Array[String]): Unit = {

  }

}
