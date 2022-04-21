package exercises.fpinscala.datastructures

sealed trait Stream[+A] {
  def headOption: Option[A]
}
case object Empty extends Stream[Nothing] {
  override def headOption: Option[Nothing] = None
}
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def headOption: Option[A] = this match {
    case StreamCons(h, t) => Some(h())
    case _ => None
  }
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


  println(Stream(1,2,3,4,5).headOption)

}
