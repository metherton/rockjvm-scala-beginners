package exercises.fpinscala.datastructures

import exercises.fpinscala.datastructures.OptionRunner.{Try}

sealed trait Option[+A] {

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }


  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(v => Some(v)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

  def filterViaMap(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.size)


  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  //  override def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  //    case (None, _) => None
  //    case (_, None) => None
  //    case (aOpt, bOpt) => Some(f(aOpt, bOpt))
  //  }
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {

    val optAge: Option[Int] = Try { age.toInt }
    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Cons(h, t) => h.flatMap( hh => sequence(t).map( x => Cons(hh, x)))
  }

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => map2(f(h), traverse(t)(f))((a, b) => Cons(a, b))

  }
}



case class Some[+A](get: A) extends Option[A]



case object None extends Option[Nothing]

object OptionRunner extends App {



  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}






}