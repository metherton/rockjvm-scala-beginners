package exercises.fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Cons is short for construct

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)((a, b) => a + b)


  def sumFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)((a, b) => a + b)

  def productFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 1)((a, b) => a * b)

  def product2(ints: List[Int]): Int =
    foldRight(ints, 1)((a, b) => a * b)


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
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

  def tailWithoutDrop[A](as: List[A]): List[A] = as match {
      case Nil => throw new UnsupportedOperationException("tail of empty list")
      case Cons(_, t) => t
  }

  def tail[A](as: List[A]): List[A] = drop(as, 1)

  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(x, Nil)
    case Cons(h, t) => Cons(x, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, t) => drop(t, n - 1)
    }
  }

//  def drop[A](l: List[A], n: Int): List[A] = {
//    def go[A](l: List[A], n: Int): List[A] = {
//      if (n <= 0) l
//      else l match {
//        case Nil => l
//        case Cons(x, t) => go(t, n - 1)
//      }
//    }
//    go(l, n)
//  }



//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
//    case Cons(h, t) if f(h) => dropWhile(t, f)
//    case _ => l
//  }

  /*
      If we group arguments into two argument lists we don't need to specify type
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](l: List[A], seed: B)(fn: (A, B) => B): B = l match {
    case Nil => seed
    case Cons(x, xs) => fn(x, foldRight(xs, seed)(fn))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h))(f)
  }

  def lengthFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)


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
  println(tailWithoutDrop(List(1,2,3,4,5)))
  println(tail(List(1,2,3,4,5)))
  //println(tail(List()))
  println(tail(List()))
  //println(tail(List())) throws exception

  println(setHead(9, List()))
  println(setHead(8, List(1,2)))

  println(drop(List(), 3))
  println(drop(List(1,2,3, 4), 3))

  println(dropWhile(List(1,2,3, 4))( (a) => a < 3 ))

  println(init(List(1,2,3,4,5)))
  println(foldRight(List(1,2,3,4,5), 0)((a, b) => a + b))
  println(sum2(List(1,2,3,4,5)))
  println(sumFoldLeft(List(1,2,3,4,5)))
  println(product2(List(1,2,3,4,5)))

  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))

  println(length(List(1,2,3)))
  println(foldLeft(List(1,2,3,4,5), 0)((a, b) => a + b))

}
