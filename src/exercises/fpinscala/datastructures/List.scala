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

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)((a2, a) => Cons(a, a2))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](l: List[A], seed: B)(fn: (A, B) => B): B = l match {
    case Nil => seed
    case Cons(x, xs) => fn(x, foldRight(xs, seed)(fn))
  }

  def foldRightAsFoldLeft[A, B](l: List[A], acc: B)(fn: (A, B) => B): B = {
    foldLeft(l, acc)((b, a) => fn(a, b))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))


  def lengthFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  def concatenate2[A](lOfl: List[List[A]]): List[A] =
    foldLeft(lOfl, List[A]())((acc, h) => append(acc, h))
  def concatenate[A](lOfl: List[List[A]]): List[A] =
    foldLeft(lOfl, List[A]())(append)
//  def concat[A](l: List[List[A]]): List[A] =
//    foldRight(l, Nil: List[A], append)

  def addOne(l: List[Int]): List[Int] =
    reverse(foldLeft(l, Nil : List[Int])((acc, h) => Cons(h + 1, acc)))

  def convertDoubleToString(l: List[Double]): List[String] =
    foldLeft(l, Nil : List[String])((acc, h) => Cons(h.toString, acc))


//  def map[A,B](as: List[A])(f: A => B): List[B] =
//    foldLeft(as, Nil : List[B])((acc, h) => Cons(f(h), acc))
  def map[A,B](as: List[A])(f: A => B): List[B] =
  foldRightAsFoldLeft(reverse(as), Nil : List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

//  def zip[A](l1: List[A], l2: List[A]): List[A] = {
//    foldRight(List((l1, l2)), Nil: List[(A, A)])((xs, acc) => Cons((xs._1 + xs._2), acc ))
//  }

  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
  }

  def zipWith[A](l1: List[A], l2: List[A], f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
    case _ => false
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
  println(lengthFoldLeft(List(1,2,3)))
  println(foldLeft(List(1,2,3,4,5), 0)((a, b) => a + b))

  println(appendViaFoldLeft(List(4,5,6), List(1,2,3)))

  println(reverse(List(1,2,3)))

  println(concatenate(List(List(1,2,3), List(4,5,6))))
  println(addOne(List(1,2,3)))
  println(map(List(1,2,3))(_ * 2))
  println(filter(List(1,2,3))((x) => x < 3))
  println(flatMap(List(1,2,3))(x => List(x,x)))
  println(filterViaFlatMap(List(1,2,3))((x) => x < 3))

  println(zipWith(List(1,2,3), List(4, 5, 6), (a: Int, b: Int) => a + b))
}
