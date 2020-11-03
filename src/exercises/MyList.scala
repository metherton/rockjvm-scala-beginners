package exercises

abstract class MyList {

  /*
      head = first element of list
      tail = remainder of list
      isEmpty = is this list empty
      add = new list with this element added
      tostring = a string representation of the list
   */

  def head: Int
  def tail: MyList
  def add(el: Int): MyList
  def isEmpty: Boolean

  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"
}

// objects can extend classes
object Empty extends MyList {

  def head: Int = throw new NoSuchElementException
  def tail: MyList = throw new NoSuchElementException
  def add(el: Int): MyList = new Cons(el, Empty)
  def isEmpty: Boolean = true


  def printElements: String = ""
}

class Cons(h: Int, t: MyList) extends MyList {

  def head: Int = h
  def tail: MyList = t
  def add(el: Int): MyList = new Cons(el, this)
  def isEmpty: Boolean = false
  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}

object ListTest extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list.tail.head)
  println(list.add(4).head)
  println(list.isEmpty)

  println(list.toString)
}