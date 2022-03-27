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


  // super


  // preventing overrides
  // 1 - use final on member
  // 2 - use final on class
  // 3 - use seal the class - extend classes in THIS FILE, prevent extention in other files


}
