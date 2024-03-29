package exercises.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def sizeUsingFold[A](t: Tree[A]): Int =  {
    fold(t, (a: A) => 1, (a: Int, b: Int) => 1 + a + b)
  }

  def depthUsingFold[A](t: Tree[A]): Int =  {
    fold(t, (a: A) => 1, (a: Int, b: Int) => 1 + a.max(b))
  }

  def fold[A, B](t: Tree[A], f: A => B, g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l, f, g), fold(r, f, g))
  }

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] =  {
    fold(t, (a: A) => Leaf(f(a)), (a: Tree[B], b: Tree[B]) => Branch(a, b))
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }



}

object RunnerTree extends App {

  import Tree._


  val b: Tree[Int] = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))

  println(size(b))
  println(sizeUsingFold(b))
  println(maximum(b))
  println(depth(b))
  println(depthUsingFold(b))
  println(map(b)(a => a * 2))
  println(mapUsingFold(b)(a => a * 2))

}

