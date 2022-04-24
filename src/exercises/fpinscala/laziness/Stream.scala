package exercises.fpinscala.laziness

import exercises.fpinscala.datastructures.{List, None, Option, Some}

sealed trait Stream[+A] {
  def headOption: Option[A]
  def toList: List[A]
}
case object Empty extends Stream[Nothing] {
  override def headOption: Option[Nothing] = None

  override def toList: List[Nothing] = List()
}
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def headOption: Option[A] = this match {
    case StreamCons(h, t) => {
      println("head option")
      Some(h())
    }
    case _ => None
  }

//  override def toList: List[A] = this match {
//
//  }
  override def toList: List[A] = ???
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    StreamCons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream [A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

object StreamRunner extends App {

  def expensive(i: Int) = {
    println("in expensive")
    Thread.sleep(2000)
  }

//  println(Stream(1,2,3,4,5).headOption)
//  val x = StreamCons(() => expensive(5), () => Empty)
//  println(x.headOption)
//  println(x.headOption)
//  println("++++++++++++++++++++++++++++++++++")
  val y = Stream.cons(() => expensive(6), Empty)
  println(y.headOption)
  println(y.headOption)

}
