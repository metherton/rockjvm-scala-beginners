package lectures.part4implicits

object TypeClasses extends App {

  trait HTMLWritable {
    def toHtml: String
  }
  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String =
      s"<div>$name ($age yo) <a href=$email </></div>"
  }
  /*
      1. Only works for the types we write
      2. One implementation out of quite a number
   */
  // option 2 - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any): Unit = value match {
      case User(n, a, e) =>
      case _ =>
    }
  }
  /**
    *  1. Lost type safety
    *  2. need to modify code every time
    *  3. still one implementation for one type..e.g if user is logged in or not
    */
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }
  implicit object UserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email} </></div>"
  }
  val john = User("john", 23, "john@rockthejvm.com")
  println(UserSerializer.serialize(john))
  // 1 We can define serializers for other types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString()}</div>"
  }
  // 2 We can define MULTIPLE serializers for the same type
  object PartialUserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name}</></div>"
  }
  // TYPE class
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }
  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }
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
  // PART 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)
    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }
  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style='color: blue'>$value</div>"
  }
  println(HTMLSerializer.serialize(42))
  println(HTMLSerializer.serialize(User("fred", 56, "fred@rockthejvm.com")))
  // access to the entire type class interface
  println(HTMLSerializer[User].serialize(User("bill", 45, "bill@rockthejvm.com")))
  /*
        Exercise:implement the TC pattern for the Equality tc
   */
  val anotherJohn = User("john", 45, "anotherjohn@rockthejvm.com")
  println(Equal(john, anotherJohn))
  // above is an example of what is called AD HO polymorhpism
}
