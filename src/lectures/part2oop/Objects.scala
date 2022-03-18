package lectures.part2oop

object Objects extends App {

  // objects do not receive paramterts


  // SCALA DOES NOT HAVE CLASS LEVEL FUNCTIONALITY
  object Person { // type + its only instance
    // "static" / "class" - level functionality
    val N_EYES = 2

    // can have method definitions
    def canFly: Boolean = false

    // factory methods
    def from(mother: Person, father: Person): Person = new Person("Bobbie2", 1)

    // pattern widely used in practise

    def apply(mother: Person, father: Person): Person = new Person("Bobbie", 26)
  }

  // scala object is a singleton instance

  // use val to create class fields..without val it is just a class parameter
  class Person(val name: String, val age: Int) {
    // instance level functionality

    // body

    val x = 2 // x becomes a field

    println(1 + 3)

    // method
    def greet(name: String): Unit = println(s"${this.name} says hi $name")

  }
  // COMPANIONS


  println(Person.N_EYES)
  println(Person.canFly)

  // Scala object = SINGLETON INSTANCE
  val mary = new Person("Mary", 30)
  //val mary = null
  mary match {
    case p: Person => println(p)
    case _ => println("no match found")
  }

  val john = new Person("John", 45)
  println(john == mary)

  val person1 = Person
  val person2 = Person

  // THESE 2 point to same single instance of Person object
  println(person1 == person2)

  val bobbie = Person(mary, john)

  // Scala applications = Scala objects with
  // def main(args: Array[String]): Unit

  println(s"Bobbie: $bobbie")

  val first = List(1,2,3)
  val second = List(4,5,6)

  println(first.concat(second))

  println(mary.x)
  mary.greet("Daniel")

}
