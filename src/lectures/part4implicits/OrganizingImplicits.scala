package lectures.part4implicits

object OrganizingImplicits extends App {

  println("\"teststring\"")

  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _) // this takes precedence over sorted in predef
  println(List(1,4,5,3,2).sorted)

  // scala.Predef is where sorted is...which is automatically imported when you write code

  /*

        Implicits (used as implicit parameters)

        - val / var
        - object
        - accessor methods - defs with no parentheses...very important...no parentheses
   */

  // Exercise
  case class Person(name: String, age: Int)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  object AlphabeticNameOrdering {

    implicit val alphabeticalOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }


  object AgeOrdering {

    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)
  }

  import AgeOrdering._

  println(persons.sorted)

  /*
      Implicit scope
      - normal scope = LOCAL SCOPE
      - imported scope
      - companions of all types involved in the method signature
          - List
          - Ordering
          - all the types involved = A or any supertype
   */

  // override def sorted[B >: A](implicit ord : scala.Ordering[B]) : C = { /* compiled code */ }


  /**
    *   Exercise
    *   - totalPrice - most used 50%
    *   - by unit count - 25%
    *   - by unit price - 25%
    *
    */



  case class Purchase(nUnits: Int, unitPrice: Double)

//  object Purchase {
//    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
//  }

  val purchases = List(Purchase(5, 10), Purchase(15, 2), Purchase(7, 11))


  object UnitOrdering {
    implicit val unitsOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits < b.nUnits)
  }

  object PriceOrdering {
    implicit val priceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.unitPrice < b.unitPrice)
  }

  import PriceOrdering._
  println(purchases.sorted)


//  import UnitOrdering._
//  println(purchases.sorted)



}
