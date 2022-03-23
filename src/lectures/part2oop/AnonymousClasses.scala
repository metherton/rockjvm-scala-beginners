package lectures.part2oop

object AnonymousClasses extends App {

  abstract class Animal {
    def eat: Unit
  }



  // anonymous class
  val funnyAnimal: Animal = new Animal {
    override def eat: Unit = println("aHahanan")
  }

  /*
      equivalent with
        class AnonymousClasses$$anon$1 extends Animal {
          override def eat: Unit = println("aHahanan")
        }

        val funnyAnimal: Animal = new AnonymousClasses$$anon$1
   */

  class Person(name: String) {
    def sayHi: Unit = println(s"Hi my name is $name, how can i help ?")
  }

  val jim = new Person("Jim") {
    override def sayHi: Unit = println(s"Hi my name is Jim, how can i help ?")
  }

  /*
      1. Generic trait MyPredicate[-T] with a little method test(T) => Boolean
      2. Generic trait MyTransformer[-A, B] with a method transform(A) => B
      3. MyList:
          map(transformer) => MyList
          filter(predicate) => MyList
          flatMap(transformer from A to MyList[B] => MyList[B])

          class EvenPredicate extends MyPredicate[Int]
          class StringToIntTransformer extends MyTransformer[String, Int]

        [1, 2, 3].map(n * 2) = [2, 4, 6]
        [1, 2, 3, 4].filter(n % 2) = [2, 4]
        [1, 2, 3].flatMap(n => [n, n + 1]) => [1, 2, 2, 3, 3, 4]

   */


  trait MyPredicate[-T] {
    def test[T](f: T => Boolean): Boolean
  }

  trait MyTransformer[-A, B] {
    def transform[A, B](el: A): B
  }


}
