package exercises.fpinscala.datastructures

import exercises.fpinscala.datastructures.OptionRunner.{Try}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[ B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def variance(xs: Seq[Double]): Option[Double]
  def lift[A,B](f: A => B): Option[A] => Option[B]
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
  def sequence[A](a: List[Option[A]]): Option[List[A]]
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
}



case class Some[+A](get: A) extends Option[A] {
  override def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  override def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }


  override def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(v => Some(v)).getOrElse(ob)

  override def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

  def filterViaMap(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  override def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.size)


  override def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  override def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

//  override def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
//    case (None, _) => None
//    case (_, None) => None
//    case (aOpt, bOpt) => Some(f(aOpt, bOpt))
//  }
  override def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {

    val optAge: Option[Int] = Try { age.toInt }
    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  override def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Cons(h, t) => h.flatMap( hh => sequence(t).map( x => Cons(hh, x)))
  }

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  override def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => map2(f(h), traverse(t)(f))((a, b) => Cons(a, b))

  }
}



case object None extends Option[Nothing] {
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = None[B]

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = None

  override def filter(f: Nothing => Boolean): Option[Nothing] = None

  override def map[B](f: Nothing => B): Option[B] = None

  override def variance(xs: Seq[Double]): Option[Double] = None

  override def lift[A, B](f: A => B): Option[A] => Option[B] = None[B]

  override def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = None

  override def sequence[A](a: List[Option[A]]): Option[List[A]] = None

  override def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = None
}

object OptionRunner extends App {



  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}






}