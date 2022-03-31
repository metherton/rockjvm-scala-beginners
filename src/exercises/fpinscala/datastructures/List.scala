package exercises.fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Cons is short for construct

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // _* allows us to pass a Seq to a variadic method

  def fill[A](n: Int, a: A): List[A] = {
    def go(n: Int, acc: List[A]): List[A] = {
      if (n <= 0 ) acc
      else go(n - 1, Cons(a, acc))
    }
    go(n, Nil)
  }

  def tail[A](as: List[A]): List[A] = as match {
      case Nil => throw new UnsupportedOperationException("tail of empty list")
      case Cons(_, t) => t
  }

  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(x, Nil)
    case Cons(h, t) => Cons(x, t)
  }

}

object Runner extends App {

  import List._
  val listDevil: List[Int] = fill(3, 6)
  println(listDevil)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y

  }

  println(x)
  println(tail(List(1,2,3,4,5)))
  //println(tail(List())) throws exception

  println(setHead(9, List()))
  println(setHead(8, List(1,2)))
}
