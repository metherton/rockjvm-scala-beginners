package exercises.fpinscala.datastructures

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[ B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]


}

case class Some[+A](get: A) extends Option[A] {
  override def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  override def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  override def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

  override def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }
}
case object None extends Option[Nothing] {
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = None[B]

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = None

  override def filter(f: Nothing => Boolean): Option[Nothing] = None

  override def map[B](f: Nothing => B): Option[B] = None
}