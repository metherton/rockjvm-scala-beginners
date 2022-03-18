package lectures.part2oop

object AbstractDataTypes extends App {

  // abstract
  abstract class Animal {
    val creatureType: String
    def eat: Unit
  }

  class MeatMeal extends Animal {
    override val creatureType: String = "croc bait"

    override def eat: Unit = "I donot eat..i am eaten by others"
  }

  class Dog extends Animal {
    override val creatureType: String = "Canine"

    override def eat: Unit = println("crunch crunch")
  }

  // traits
  trait Carnivore {
    def eat(animal: Animal): Unit
    val preferredMeal: String = "fresh meat"
    val otherAnimalFood: Animal
  }


  trait LizardTypeCarnivore {
    lazy val otherAnimalFood: MeatMeal = new MeatMeal
  }

  trait ColdBlooded

  class Crocodile extends Animal with Carnivore with ColdBlooded with LizardTypeCarnivore {
    override val creatureType: String = "croc"

    override def eat: Unit = println("nom nom nom")
    override def eat(animal: Animal): Unit = {

      println(s"I'm a croc and I'm eating ${animal.creatureType}")

      println(s"I like other animals like $otherAnimalFood")
    }



  }

  val dog = new Dog
  val croc = new Crocodile
  croc.eat(dog)


  println(s"crocodile favourite meat is ${croc.preferredMeal}")

  // traits vs abstract
  // 1 - traits do not have constructor parameters
  // 2 - multiple traits may be inherited by the same class
  // 3 - traits describe behaviour.. abstract class = "thing"
}
