package lectures.part2oop

object OOBasics extends App {

  val person = new Person("john", 26)
  println(person.age)
  person.greet("daniel")
  person.greet()
}

// constructor

class Person(name: String, val age: Int) {
  //body
  val x = 2
  println(1 + 2)

  def greet(name: String): Unit =
    println(s"${this.name} says Hi, $name")

  // overloading
  def greet(): Unit =
    println(s"Hi I am $name")

  def this(name: String) = this(name, 0)
  def this() = this("John Doe")

}
// class parameters are NOT FIELDS