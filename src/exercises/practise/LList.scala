package exercises.practise

import scala.annotation.tailrec

// singly linked list
// [1, 2, 3] = [1] -> [2] -> [3] -> |
abstract class LList {
  def head: Int
  def tail: LList
  def isEmpty: Boolean
  override def toString() = super.toString
  def add(element: Int): LList = new Cons(element, this)
}

class Empty extends LList {
  override def head: Int = throw new NoSuchElementException("no element found")

  override def tail: LList = throw new NoSuchElementException("no element found")

  override def isEmpty: Boolean = true

  override def toString(): String = "[]"
}

class Cons(override val head: Int, override val tail: LList) extends LList {

  override def isEmpty: Boolean = false

  override def toString(): String = {
    @tailrec
    def concatenateElements(remainder: LList, acc: String): String = {
      if (remainder.isEmpty) acc
      else concatenateElements(remainder.tail, s"$acc ${remainder.head}")
    }
    s"[${concatenateElements(this, "")}]"
  }

}

//class Node {
//  val value: Int
//  val next: Node
//}


object LListTest {

  def main(args: Array[String]): Unit = {

    val empty = new Empty
    println(empty.isEmpty)
    println(empty)

    val first3Numbers = new Cons(1, new Cons(2, new Cons(3, empty)))
    val first3Numbers_v2 = empty.add(1).add(2).add(3)
    println(first3Numbers)
    println(first3Numbers_v2)
  }
}