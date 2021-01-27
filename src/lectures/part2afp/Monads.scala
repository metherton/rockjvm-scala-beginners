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

            def map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x))) // Monad[B]
            def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap((x: Monad[T]) => x)

            // have List in mind
          List(1,2,3).map(_ * 2) = List(1,2,3).flatMap(x => List(x * 2))
          List(List(1,2),List(3,4)).flatten = List(List(1,2), List(3,4)).flatMap(x => x) = List(1,2,3,4)
   */


  class Lazy[+A](value: => A) {
    // call by need
    private lazy val internalValue = value
    def use: A = internalValue
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] =
      f(internalValue)

  }

  object Lazy {
    def apply[A](a: => A): Lazy[A] =
      new Lazy(a)
  }

  val lazyInstance = Lazy {
    println("today i don't feel like doing anything")
    42
  }

  val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  flatMappedInstance.use
  flatMappedInstance2.use

  /*
      left-identity

      unit.flatMap(f) = f(x)
      Lazy(x).flatMap(f) = f(x)

      right-identity

      l.flatMap(unit) = l
      Lazy(x).flatMap(x => Lazy(x)) = Lazy(x)

      associativity

      l.flatMap(f).flatMap(g) == l.flatMap(x => f(x).flatMap(g))

      Lazy(v).flatMap(f).flatMap(g) =
        f(v).flatMap(g)

      Lazy(v).flatMap(x => f(x).flatMap(g)) =
        f(v).flatMap(g)
   */


}
