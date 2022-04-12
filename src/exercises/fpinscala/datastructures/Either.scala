package exercises.fpinscala.datastructures

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)

  }
  def map2[EE >: E,B,C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      bb <- b
    } yield f(a, b)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object EitherRunner extends App {




}