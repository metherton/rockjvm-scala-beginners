package lectures.part2oop

object Generics extends App {

  trait RandomSelector[A] {

    def randomElement[A](seq: Seq[A]): A = {
      val randomNum = util.Random.nextInt(seq.length)
      seq(randomNum)
    }
  }

  class MyRandomSelector extends RandomSelector[String] {


  }

  val mrs = new MyRandomSelector
  println(s"random selelcted : ${mrs.randomElement(List("1","2","3","4"))}")

  abstract class MyList[A] { // "generic" list
    def head: A
    def tail: MyList[A]
  }

  class Empty[A] extends MyList[A] {
    override def head: A = throw new NoSuchElementException

    override def tail: MyList[A] = throw new NoSuchElementException
  }

  class NonEmpty[A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  val listOfIntegers = new NonEmpty[Int](1, new NonEmpty[Int](2, new Empty[Int]))

  val firstNumber = listOfIntegers.head
  val adding = firstNumber + 3

//  class MyList[+A] {
//    // use the type A
//
//    def add[B >: A](element: B): MyList[B] = ???
//    /*
//        A = Cat
//        B = Dog = Animal
//
//     */
//
//
//  }

  // multiple generic types
  trait MyMap[Key, Value]
///  class MyMap[Key, Value]



  // generic methods
  object MyList {
    def from2Elements[A](elem1: A, elem2: A): MyList[A] =
      new NonEmpty[A](elem1, new NonEmpty[A](elem2, new Empty[A]))

    def empty[A]: MyList[A] = ???
  }

  val first2Numbers = MyList.from2Elements[Int](1, 2)

  val first2Numbers_v2 = MyList.from2Elements(1, 2)

  val first2Numbers_v3 = new NonEmpty(1, new NonEmpty(2, new Empty))

  val emptyListOfIntegers = MyList.empty[Int]

  /**
    * Exercise = generisice linked list implementation
    */

  // variance problem
  class Animal
  class Cat extends Animal
  class Dog extends Animal

  // 1. yes List[Cat] extends List[Animal] = COVARIANCE
  class CovariantList[+A]
  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  // animalList.add(new Dog) ??? HARD QUESTION => we return a list of animals


  // 2. No = INVARIANCE
  class InvariantList[A]
  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Animal]

  // 3. Hell no - CONTRAVARIANCE
  class Trainer[-A]
  val trainer: Trainer[Cat] = new Trainer[Animal]

  // bounded types
  class Cage[A <: Animal](animal: A)
  val cage = new Cage(new Dog)

//  class Car
//  val newCage = new Cage(new Car)

  // expand MyList to be generic

}
