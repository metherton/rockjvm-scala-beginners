package lectures.part2oop

object MethodNotations extends App {

  import scala.language.postfixOps

  class Person(val name: String, favoriteMovie: String) {

    def likes(movie: String): Boolean = movie == favoriteMovie
    def hangOutWith(person: Person): String = s"${this.name} is hanging out with ${person.name}"
    def unary_! : String = s"$name, what the heck ?!"
    def isAlive: Boolean = true
    def apply(): String = s"Hi my name is ${name} and I like ${favoriteMovie}"
  }

  val mary = new Person("Mary", "Inception")
  println(mary.likes("Inception"))
  println(mary likes "Inception") // equivalent
  // infix notation = operator notation

  // "operators" in scala
  val tom = new Person("Tom", "Fight Club")
  println(mary hangOutWith tom)

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

}
