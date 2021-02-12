package lectures.part5typesystem

object StructuralTypes extends App {


  // structural types

  type JavaCloseable = java.io.Closeable

  class HipsterCloseable {
    def close(): Unit = println("yeah yeah i'm closing")
  }

  // def closeQuietly(closeable: JavaCloseable OR HipsterCloseable)

  type UnifiedCloseable = {
    def close(): Unit
  } //  STRUCTURAL TYPE

  def closeQuietly(unifiedCloseable: UnifiedCloseable): Unit = unifiedCloseable.close()

  closeQuietly(new JavaCloseable {
    override def close(): Unit = ???
  })

  closeQuietly(new HipsterCloseable)

  // TYPE REFINEMENTS


  type AdvancedCloseable = JavaCloseable {
    def closeSilently(): Unit
  }

  class AdvancedJavaCloseable extends JavaCloseable {
    override def close(): Unit = println("java closes")
    def closeSilently(): Unit = println("java closes silently")
  }

  def closeShh(advCloseable: AdvancedCloseable): Unit = advCloseable.closeSilently()

  closeShh(new AdvancedJavaCloseable)
  // closeShh(new HipsterCloseable)


  // using structural types as standalone types
  def altClose(closeable: {def close(): Unit}): Unit = closeable.close()

  // type checking => duck checking

  type SoundMaker = {
    def makeSound(): Unit
  }

  class Dog {
    def makeSound(): Unit = println("bark")
  }

  class Car {
    def makeSound(): Unit = println("vroom")
  }

  val dog: SoundMaker = new Dog
  val car: SoundMaker = new Car

  // static duck typing


  // CAVEAT - based on reflection

  /*
      Exercises

   */

  trait CBL[+T] {
    def head: T
    def tail: CBL[T]
  }

  class Human {
    def head: Brain = new Brain

  }

  class Brain {
    override def toString: String = "BRAINZ!"
  }

  def f[T](somethingWithAHead: {def head: T}): Unit = println(somethingWithAHead.head)

  /*
      f is compatible with a CBL and with a Human - YES

   */

  case object CBNil extends CBL[Nothing] {
    def head: Nothing = ???

    def tail: CBL[Nothing] = ???
  }

  case class CBCons[T](override val head: T, override val tail: CBL[T]) extends CBL[T]

  f(CBCons(2, CBNil))
  f(new Human) // ? T = Brain !!

  // 2.
  object HeadEqualizer {
    type Headable[T] = {def head: T}
    def ===[T](a: Headable[T], b: Headable[T]): Boolean = a.head == b.head
  }

  /*
      is compatible with a CBL and with a Human - YES

   */

  val brainzList = CBCons(new Brain, CBNil)
  val stringsList = CBCons("Brainz", CBNil)

  HeadEqualizer.===(brainzList, new Human)
  // problem
  HeadEqualizer.===(new Human, stringsList) // NOT TYPE SAFE

}
