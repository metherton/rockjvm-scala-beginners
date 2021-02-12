package lectures.part5typesystem

object Bounds extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  case class House[+A](animal: A) {
    def get = animal
    def put[B >: A](newAnimal: B) = animal
  }
  House(new Dog)


  println(House(new Cat).get)

}
