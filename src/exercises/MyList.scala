package exercises

abstract class MyList[+A] {

  /*
      head = first element of list
      tail = remainder of list
      isEmpty = is this list empty
      add = new list with this element added
      tostring = a string representation of the list
   */

  def head: A
  def tail: MyList[A]
  def add[B >: A](el: B): MyList[B]
  def isEmpty: Boolean

  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"
}

// objects can extend classes
object Empty extends MyList[Nothing] {

  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def add[B >: Nothing](el: B): MyList[B] = new Cons(el, Empty)
  def isEmpty: Boolean = true


  def printElements: String = ""
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {

  def head: A = h
  def tail: MyList[A] = t
  def add[B >: A](el: B): MyList[B] = new Cons(el, this)
  def isEmpty: Boolean = false
  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}

object ListTest extends App {

  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] = new Cons("one", new Cons("two", new Cons("three", Empty)))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)
}