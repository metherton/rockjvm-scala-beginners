package lectures.part4implicits

object PimpMyLibrary extends App {

  // 2.isPrime

  implicit class RichInt(val value: Int) {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times(function: () => Unit): Unit = {
      def timesAux(n: Int): Unit =
        if (n <= 0) ()
        else {
          function()
          timesAux(n - 1)
        }
      timesAux(value)
    }
    def *[T](list: List[T]): List[T] = {
      def concatenate(n: Int): List[T] =
        if (n <= 0) List()
        else concatenate(n - 1) ++ list

      concatenate(value)
    }
  }



  new RichInt(42).sqrt

  42.isEven // new RichInt(42).isEven

  // type enrichment = pimping

  import scala.concurrent.duration._
  3.seconds

  // compiler doesn't do multiple implicit conversions
  // 42.isOdd

  /*
      Enrich the String class
      - asInt
      - encrypt
        "John" -> Lnjp

      Keep enriching the Int class
      - times(function)
      3.times(() => ...)
      - * method
        3 * List(1,2) => List(1,2,1,2,1,2)

   */

  implicit class RichString(val string: String) {
    def asInt: Int = Integer.valueOf(string) // java.lang.Integer -> Int
    def encrypt(cypherDistance: Int): String = string.map(c => (c + cypherDistance).asInstanceOf[Char])
  }

  println("213".asInt)
  println("abcd".encrypt(2))

  3.times(() => println("scala rocks"))

  println(4.*(List(1,2)))

  // "3" / 4
  implicit def stringToInt(string: String): Int = Integer.valueOf(string)

  println("6" / 2)  // stringToInt("6") / 2

  // equivalent: implicit class RichAltInt(value)
  class RichAltInt(value: Int)
  implicit def enrich(value: Int) = new RichAltInt(value)

  // danger zone with implicit conversions with methods
  implicit def intToBoolean(i: Int): Boolean = i == 1

  /*
      if (n) do something
      else do something else
   */

  val aConditionedValue = if (3) "OK" else "Something wrong"
  println(aConditionedValue)

  /*
       Tips

       keep type enrichment to implicit classes and type classes
       avoid implicit defs as much as possible
       package implicits clearly, bring into scope only what you need
       IF you need conversions, make them specific
   */
  //
}
