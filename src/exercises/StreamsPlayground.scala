package exercises

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate 2 streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of this stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  /*
      [1 2 3].toList([]) =
      [2 3].toList([1]) =
      [3].toList([2 1]) =
      [].toList([3 2 1]) // reverse it
      [1 2 3]


   */
  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc
    else tail.toList(head :: acc)

}


object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException

  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)

  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()

  def map[B](f: Nothing => B): MyStream[B] = this

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this

}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream[A] = tl // call by need

  /*
      val s = Cons(1, EmptyStream)
      val prepended = 1 #:: s = new Cons(1, s)
   */
  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  /*
      val s = new Cons(1, ?)
      mapped = s.map(_ + 1) = new Cons(2, s.tail.map(_ + 1))
        ...mapped.tail

   */
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f)) // preserves lazy evaluation

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate) // preserves lazy eval
  }

  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons(start, MyStream.from(generator(start))(generator))
}


object StreamsPlayground extends App {

  val f1 = (x: Int) => List(x * 2)
  val fs = (x: Int) => Some(x * 2)

  val s1 = Some(2)
  println(s1.flatMap(fs))
  println(fs(2))



  println(f1(2) ++ Nil.flatMap(f1))

  val aList = List(2)

  println(f1(2) == aList.flatMap(f1))
  println(aList.flatMap(f1))
  println(f1(2))

  val f2 = (x: List[Int]) => List(x.map(y => y * 2))

  val aList2 = List(List(1, 2, 3))

  println(f2(List(1, 2, 3)) == aList2.flatMap(f2))

  val aMonadInstance = List(4,5,6)
  println(aMonadInstance.flatMap(x => List.apply(x)) == aMonadInstance)

  val fg = (x: Int) => List(x * 3)
  val fh = (x: Int) => List(x * 4)

  println(aMonadInstance.flatMap(fg).flatMap(fh))
  println(aMonadInstance.flatMap(x => fg(x).flatMap(fh)))

  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.::#(0)
  println(startFrom0.head)

 // startFrom0.take(10000).foreach(println)

  // map , flatMap
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())

  println(startFrom0.filter(_ < 10).take(10).toList())

  // Exercises on streams
  // 1 stream of fibonacci numbers
  // 2 stream of prime numbers with eratosthenes sieve
  /*
      [2 3 4 ...]
      filter out all numbers divisible by 2
      [2 3 5 7 9 11 ...]
      filter out all numbers divisible by 3
      [2 3 5 7 11 ...]
      filter out all numbers divisible by 5
        ...
   */

  /*
      [first, [... ]
      [first, fibo(second, first + second)

   */
  def fibonnaci(first: Int, second: Int): MyStream[Int] =
    new Cons(first, fibonnaci(second, first + second))

  println(fibonnaci(1, 1).take(100).toList())


  /*
        [2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ...
        [2, 3, 5, 7, 9, 11...
        [ 2 erathusthenus applied to numbers filtered by n % 2 != 0
        [ 2 3 erathusthens applied to 5, 7, 9 filtered by n % 3 !=0
        [2 3 5 erathusthenus applied to 7 11

   */
  def eratusthenus(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new Cons(numbers.head, eratusthenus(numbers.tail).filter(_ % numbers.head != 0))


  println(eratusthenus(MyStream.from(2)(_ + 1)).take(100).toList())
}

