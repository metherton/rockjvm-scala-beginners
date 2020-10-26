package lectures.part2oop

object OOBasics extends App {

  val person = new Person("john", 26)
  println(person.age)
  person.greet("daniel")
  person.greet()

  val counter = new Counter

  counter.inc(10).print

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


/*
      Novel and a writer

      Writer: firstName, surname, year
      - method fullname

      Novel: name, year of release, author
      - authorAge (at year of release)
      - isWrittenBy(author)
      - copy (new year of release) = new instance of novel
 */

class Writer(firstName: String, surname: String, val year: Int) {

  def fullName(): String =
    s"$firstName $surname"
}

class Novel(val name: String, val yearOfRelease: Int, val author: Writer) {

  def authorAge(): Int =
    yearOfRelease - author.year

  def isWrittenBy(author: Writer): Boolean =
    author == this.author

  def copy(newYearOfRelease: Int): Novel =
    new Novel(name, newYearOfRelease, author)
}

/*

    Counter class
    - receives an int value
    - method to increment/decrement => new counter
    - overload inc/dec to receive an amount

 */

class Counter(val count: Int = 0) {

  def inc(): Counter =
    new Counter(count + 1) // immutability

  def dec(): Counter =
    new Counter(count - 1)

  def inc(amount: Int): Counter =
    if (amount <= 0) this
    else inc.inc(amount - 1)

  def dec(amount: Int): Counter =
    if (amount <= 0) this
    else dec.dec(amount - 1)

  def print = println(count)

}