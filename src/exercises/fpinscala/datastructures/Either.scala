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


  def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Cons(h, t) => h.flatMap( hh => sequence(t).map(x => Cons(hh, x)))
  }

//  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
//    as match
//  case Nil => Right(Nil)
//  case h :: t => (f(h).map2(traverse(t)(f)))(_ :: _)

//  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
//    as match
//  case Nil => Right(Nil)
//  case h :: t => (f(h).map2(traverse(t)(f)))(_ :: _)
//def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
//  for
//    a <- this
//  b1 <- b
//  yield f(a,b1)

  def map2[EE >: E,B,C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
  }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case Cons(h, t) => (f(h).map2(traverse(t)(f)))(Cons(_, _))
    }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object EitherRunner extends App {


  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}


  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 13

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
   for {
     a <- Try{age.toInt}
     tickets <- Try{numberOfSpeedingTickets.toInt}
   } yield insuranceRateQuote(a, tickets)
  }

  println(s"parseInsurnace result is" + parseInsuranceRateQuote("23","33"))
  println(s"parseInsurnace result is" + parseInsuranceRateQuote("hello","33"))


  val first = Right(1)

  println(first.traverse(Cons(Right(2), Cons(Right(3), Cons(Right(4), Nil))))(a => Right(a)))

}