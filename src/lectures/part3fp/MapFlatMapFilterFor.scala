package lectures.part3fp

import exercises.{Cons, Empty, MyList}


object MapFlatMapFilterFor extends App {

  val list = List(1,2,3)
  println(list)
  println(list.head)
  println(list.tail)

  // map
  println(list.map(_ + 1))
  println(list.map(_ + " is a number"))

  // filter
  println(list.filter(_ % 2 == 0))

  // flatMap
  val toPair = (x: Int) => List(x, x + 1)
  println(list.flatMap(toPair))

  // print all combinations between two lists
  val numbers = List(1, 2, 3, 4)
  val chars = List('a', 'b', 'c', 'd')
  val colors = List("black", "white")

  // List("a1", "a2"...."d4")

  // iterating
  val combinations = numbers.flatMap(n => chars.flatMap(c => colors.map(color => "" + c + n + "-" + color)))
  println(combinations)

  // foreach
  list.foreach(println)

  // for-comprehensions
  val forCombinations = for {
    number <- numbers if number % 2 == 0
    char <- chars
    color <- colors
  } yield "" + char + number + "-" + color

  println(forCombinations)

  for {
    n <- numbers
  } println(n)

  // syntax overload
  list.map { x =>
    x * 2
  }

  /*
      1. MyList supports for comprehensions ?
        map(f: A => B) => MyList[B]
        filter(p: A => Boolean) => MyList[A]
        flatMap(f: A => MyList[B]) => MyList[B]
      2. A small collection of at most ONE element - Maybe[+T]
        map, flatMap, filter


   */

  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  println(listOfIntegers)

  for {
    n <- listOfIntegers
  } println(n)

//  abstract class Maybe[+T] {
//
//  }
//  case object Empty extends Maybe[Nothing]
//
//  case class

}
