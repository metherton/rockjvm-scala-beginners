package lectures.part2oop

object MethodNotations extends App {

  import scala.language.postfixOps

  class Person(val name: String, favoriteMovie: String, val age: Int = 0) {

    def likes(movie: String): Boolean = movie == favoriteMovie
    def +(person: Person): String = s"${this.name} is hanging out with ${person.name}"
    def +(nickname: String): Person = new Person(s"$name ($nickname)", favoriteMovie, age)
    def unary_+ : Person = new Person(name, favoriteMovie, age + 1)
    def unary_! : String = s"$name, what the heck ?!"
    def isAlive: Boolean = true
    def learns(subject: String): String = s"$name learns $subject"
    def learnScala  = this learns "Scala"
    def apply(): String = s"Hi my name is $name and I like $favoriteMovie"
    def apply(times: Int): String = s"$name watched $favoriteMovie $times times"


    override def toString: String = s"Name: ${name}, Favorite film: ${favoriteMovie}, Age: ${age}"
  }

  val mary = new Person("Mary", "Inception", 25)

  println(+mary)

  println(mary.likes("Inception"))
  println(mary likes "Inception") // equivalent
  // infix notation = operator notation

  println(mary.learns("scala"))

  println(mary learnScala)

  println(mary(5))

  // "operators" in scala
  val tom = new Person("Tom", "Fight Club", 28)
  println(mary + tom)

  println((mary + "the bloody one")())

  println(mary.+(tom))
  println(!mary)

  println(mary.unary_!)
  // prefix notation
  val x = -1
  val y = 1.unary_-
  // unary_ prefix only works with - + ~ !

  // postfix notation
  println(mary.isAlive)
  println(mary isAlive)

  // apply
  println(mary.apply())
  println(mary())

  /*
      1. Overload the + operator
        mary + "the rockstar" => new person "Mary (the rockstar)"
      2. Add an age to the person class
        Add a unary + operator => new person with the age + 1
        +mary => mary with age incremented

        ++

      3. Add a learns method in the Person class takes a string parameter.. => "mary learns scala"
          Add a learnsScala method (no params), calls learns method with "Scala"
          use it in prefix notation

      4. Overload the apply method..
        mary.apply(2)..takes a number.. => "Mary watched Inception 2 times"

   */

}
