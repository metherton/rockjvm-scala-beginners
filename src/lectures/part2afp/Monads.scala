package lectures.part2afp

object Monads extends App {

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  // our own Try monad

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      }
  }
  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
      left-identity

      unit.flatMap(f) = f(x)
      Attempt(x).flatMap(f) = f(x) // Success case !
      Success(x).flatMap(f) = f(x) // proved

      right-identity

      attempt.flatMap(unit) = attempt
      Success(x).flatMap(x => Attempt(x)) = Attempt(x) = Success(x)
      Fail(e).flatMap(...) = Fail(e)

      associativity

      attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
      Fail(e).flatMap(f).flatMap(g) = Fail(e)
      Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

      Success(v).flatMap(f).flatMap(g) =
        f(v).flatMap(g) OR Fail(e)

      Success(v).flatMap(x => f(x).flatMap(g)) =
        f(v).flatMap(g) OR Fail(e)
   */

  val attempt = Attempt {
    throw new RuntimeException("My own monad, yes !")
  }

  println(attempt)

  /*

      EXERCISE:

      1} Implement a Lazy[T] monad = computation which will only be executed when its needed.

        unit / apply // in companion object
        flatMap

      2} Monads = unit + flatMap
         Monads = unit + Map + flatten

          Monad[T] {

            def flatMap[B](f: T => Monad[B]): Monad[B] = ... (implemented)

            def map[B](f: T => B): Monad[B] = ???
            def flatten(m: Monad[Monad[T]]): Monad[T] = ???

            // have List in mind

   */


  class Lazy[+A](value: => A) {
    def use: A = value
    def flatMap[B](f: A => Lazy[B]): Lazy[B] =
      f(value)
  }

  object Lazy {
    def apply[A](a: => A): Lazy[A] =
      new Lazy(a)
  }

  val lazyInstance = Lazy {
    println("today i don't feel like doing anything")
    42
  }

  println(lazyInstance.use)

}
