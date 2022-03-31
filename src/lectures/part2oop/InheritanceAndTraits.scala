package lectures.part2oop

object InheritanceAndTraitsm extends App {

  // single class inheritance
  sealed class Animal {

    val creatureType = "wild"

    def eat = println("eat eat")
  }

  class Cat extends Animal {
    def crunch = {
      eat
      println("crunch crunch")
    }
  }

  trait Something extends Cat {
    def doLog(): Unit = {
      println("hey from do log")
    }
  }


  val cat = new Cat
//  cat.crunch



  class Person(name: String, age: Int) {
    def this(name: String) = this(name, 0)
  }
  class Adult(name: String, age: Int, idCard: String) extends Person(name)

  // overriding
  class Dog(override val creatureType: String) extends Animal {

//    override val creatureType = "domestic"

    override def eat = {
      super.eat
      println("crunch2 crunch2")
    }
  }

  val dog = new Dog("K9")
  dog.eat
  println(dog.creatureType)

  // type substitution .. polymorphism
  val unknownAnimal: Animal = new Dog("K9")


  //overriding vs overrloading

  class Crocodile extends Animal {
    override val creatureType = "very wild"

    override def eat: Unit = println("I can eat anything, I am a croc")

    // overloading: multiple methods with the same name, different signatures
    // different signature =
    // different argument list (different number of arguments +different arg type)
    // + different return type (optional)
    def eat(animal: Animal): Unit = println("I'm eating this poor fella")

    /*
        def eat(dog: Dog): Unit = println("eating a dog")
        def eat(person: Person): Unit = println("I'm eating a person with name ${person.name}")
        def eat(person: Person, dog: Dog): Unit = println("I'm eating a person with name ${person.name} and a dog")
        def eat(): Int = 45 // this is not a sufficient overload..just changing the return type is not enough
        def eat(dog: Dog, person: Person): Unit = println("I'm eating a person with name ${person.name} and a dog")

     */
    def eat(dog: Dog): Unit = println("eating a dog")
    def eat(person: Person): Unit = println("I'm eating a person with name ${person.name}")
    def eat(person: Person, dog: Dog): Unit = println("I'm eating a person with name ${person.name} and a dog")
    //def eat(): Int = 45 // this is not a sufficient overload..just changing the return type is not enough
    def eat(dog: Dog, person: Person): Unit = println("I'm eating a dog and a person with name ${person.name}")

  }

  // super


  // preventing overrides
  // 1 - use final on member
  // 2 - use final on class
  // 3 - use seal the class - extend classes in THIS FILE, prevent extention in other files


  val croc = new Crocodile
  val daniel = new Person("daniel", 99)
  croc.eat(daniel, dog)
  croc.eat(dog, daniel)

}
