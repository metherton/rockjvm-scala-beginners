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
    else Cons(as.head, apply(as.tail: _*))

  def fill[A](n: Int, a: A): List[A] = {
    def go(n: Int, acc: List[A]): List[A] = {
      if (n <= 0 ) acc
      else go(n - 1, Cons(a, acc))
    }
    go(n, Nil)
  }

}

object Runner extends App {

  import List._
  val listDevil: List[Int] = fill(3, 6)
  println(listDevil)
}
