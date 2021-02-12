package lectures.part5typesystem

import lectures.part5typesystem.Variance.InvariantParking

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  case class House[A >: Animal](animal: A)
  House(new Dog)

  // what is variance
  // "inheritance" type substitution of generics

  class Cage[T]
  val animalCage: Cage[Animal] = new Cage[Animal]

  // yes - covariance
  class CCage[+T]
  val ccage: CCage[Animal] = new CCage[Cat]

  // no - invariance
  class ICage[T]
  //val icage: ICage[Animal] = new ICage[Cat]  // does not compile
  // val x: Int = "hello" // actually same to compiler as this

  // hell no - opposite = contravariance
  class XCage[-T]
  val xcage: XCage[Cat] = new XCage[Animal]

  class InvariantCage[T](val animal: T) // invariant

  // covariant positions
  class CovariantCage[+T](val animal: T) // COVARIANT POSITION

  // class ContravariantCage[-T](val animal: T)
  /*

      val catCage: XCage[Cat] = new XCage[Animal](new Crocodile)

   */

  // class CovariantVariableCage[+T](var animal: T) // types of vars are in CONTRAVARIANT POSITION
  /*
      val ccage: CCage[Animal] = new CCage[Cat](new Cat)
      ccage.animal = new Crocodile

   */

  // class ContravariantVariableCage[-T](var animal: T) // ALSO in COVARIANT position
  /*
      val catCage: XCage[Cat] = new XCage[Animal](new Crocodile)
   */

  class InvariantVariableCage[T](var animal: T) // ok

//  trait AnotherCovariantCage[+T] {
//    def addAnimal(animal: T) // CONTRAVARIANT position
//  }
  /*

      val ccage: CCage[Animal] = new CCage[Dog]
      ccage.add(new Cat)

   */

  class AnotherContravariantCage[-T] {
    def addAnimal(animal: T) = true
  }

  val acc: AnotherContravariantCage[Cat] = new AnotherContravariantCage[Animal]
  acc.addAnimal(new Cat)
  class Kitty extends Cat
  acc.addAnimal(new Kitty)

  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = new MyList[B]// widening the type
  }

  val emptyList = new MyList[Kitty]
  val animals = emptyList.add(new Kitty)
  val moreAnimals = animals.add(new Cat)
  val evenMoreAnimals = moreAnimals.add(new Dog)


  // METHOD ARGUMENTS ARE IN CONTRAVARIANT POSITION


  // return types
  class PetShop[-T] {
    // def get(isItAPuppy: Boolean): T // METHOD RETURN TYPES ARE IN COVARIANT POSITION
    /*
        val catShop = new PetShop[Animal] {
          def get(isItAPuppy: Boolean): Animal = new Cat
        }

        val dogShop: PetShop[Dog] = catShop
        dogShop.get(true) // EVIL CAT

     */

    def get[S <: T](isItAPuppy: Boolean, defaultAnimal: S): S = defaultAnimal
  }

  val shop: PetShop[Dog] = new PetShop[Animal]
  // val evilCat = shop.get(true, new Cat)

  class TerraNova extends Dog
  val bigFurry = shop.get(true, new TerraNova)

  /*
      Big RULE

      method arguments are in CONTRAVARIANT position
      return types are in COVARIANT position

   */

  /*

      1. Design Invariant, Covariant, Contravariant versions

          Parking[T](things: List[T] {

            park(vehicle: T)
            impound(vehicles: List[T])
            checkVehicles(conditions: String): List[T]
          }

      2. Used someone elses API: IList[T]

      3. Parking = monad!
          - flatMap

   */

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle

  class IList[T]

  class InvariantParking[T](vehicles: List[T]) {
    def park(vehicle: T): InvariantParking[T] = ???
    def impound(vehicles: List[T]): InvariantParking[T] = ???
    def checkVehicles(conditions: String): List[T] = vehicles

    def flatMap[S](f: T => InvariantParking[S]): InvariantParking[S] = ???
  }

  val vip = new InvariantParking[Vehicle](List[Vehicle]())
  vip.park(new Vehicle)

  class CovariantParking[+T](vehicles: List[T]) {
    def park[S >: T](vehicle: S): CovariantParking[S] = ???
    def impound[S >: T](vehicles: List[S]): CovariantParking[S] = ???
    def checkVehicles(conditions: String): List[T] = ???

    def flatMap[S](f: T => CovariantParking[S]): CovariantParking[S] = ???
  }


  class ContravariantParking[-T](vehicles: List[T]) {
    def park(vehicle: T): ContravariantParking[T] = ???
    def impound(vehicles: List[T]): ContravariantParking[T] = ???
    def checkVehicles[S <: T](conditions: String): List[S]  = ???

    // check the Variance exercise video for explanation about this..basically double contravariance = covariance
    def flatMap[R <: T, S](f: R => ContravariantParking[S]): ContravariantParking[S] = ???
  }

  /*
      Rule of Thumb
      - use covariance = COLLECTION OF THINGS
      - use contravariance = GROUP OF ACTIONS


   */

  // Exercise 2

  class CovariantParking2[+T](vehicles: IList[T]) {
    def park[S >: T](vehicle: S): CovariantParking2[S] = ???
    def impound[S >: T](vehicles: IList[S]): CovariantParking2[S] = ???
    def checkVehicles[S >: T](conditions: String): IList[S] = ???
  }


  class ContravariantParking2[-T](vehicles: IList[T]) {
    def park(vehicle: T): ContravariantParking2[T] = ???
    def impound[S <: T](vehicles: IList[S]): ContravariantParking2[S] = ???
    def checkVehicles[S <: T](conditions: String): IList[S]  = ???
  }

  // Exercise 3

  // flatMap



}
