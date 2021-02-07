package exercises

import lectures.part4implicits.TypeClasses.{User, john}

object EqualityPlayground extends App {

  /**
    *  Equality
    */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }
  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.apply(a, b)
  }
  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }
  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  implicit class TypeSafeEqual[T](value: T) {
    def ===(other: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.apply(value, other)
    def !==(other: T)(implicit equalizer: Equal[T]): Boolean =
      ! equalizer.apply(value, other)

  }



  /*
      Exercise:implement the TC pattern for the Equality tc
 */
  val john = User("john", 23, "john@rockthejvm.com")
  val anotherJohn = User("john", 45, "anotherjohn@rockthejvm.com")
  println(Equal(john, anotherJohn))
  // above is an example of what is called AD HO polymorhpism

  /*
      Exercise

      - improve the Equal TC with an implicit conversion class, which will have 2 new methods
      === (another value: T)
      !== (anotherValue: T)

   */

  println(john === anotherJohn)
  /*

      john.===(anotherJohn)
      new TypeSafeEqual[User](john).===(anotherJohn)
      new TypeSafeEqual[User](john).===(anotherJohn)(NameEquality)
   */

  /*
      TYPE SAFE

   */


  println(john !== anotherJohn)



}
