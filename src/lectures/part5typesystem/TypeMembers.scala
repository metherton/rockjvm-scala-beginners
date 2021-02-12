package lectures.part5typesystem

object TypeMembers extends App {

  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    type AnimalType // abstract Type member
    type BoundedAnimal <: Animal
    type SuperBoundedAnimal >: Dog <: Animal
    type AnimalC = Cat // alias for cat
  }

  val ac = new AnimalCollection
//  val dog: ac.AnimalType = ???

  // val cat: ac.BoundedAnimal = new Cat

  val pup: ac.SuperBoundedAnimal = new Dog
  val cat: ac.AnimalC = new Cat


  type CatAlias = Cat
  val anotherCat: CatAlias = new Cat


  // alternative to generics
  trait MyList {
    type T
    def add(element: T): MyList
  }

  class NonEmptyList(value: Int) extends MyList {
    override type T = Int

    def add(element: Int): MyList = ???
  }

  val aCat = new Cat
  // .type
  type CatsType = aCat.type
  val newCat: CatsType = aCat

  /*
      Exercise - enforce a type to be applicable to some types only

   */

  // LOCKED
  trait MList {
    type A
    def head: A
    def tail: MList
  }

  trait ApplicableToNumbers {
    type A <: Number
  }

  // should not compile
//  class CustomList(hd: String, tl: CustomList) extends MList with ApplicableToNumbers {
//    type A = String
//    def head = hd
//    def tail = tl
//  }

  // should compile
  class IntList(hd: Int, tl: IntList) extends MList {
    type A = Int
    def head = hd
    def tail = tl
  }


  // Number
  // type members and type member constraints (bounds)



}
