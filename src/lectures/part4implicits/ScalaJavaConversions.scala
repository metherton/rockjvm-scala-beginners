package lectures.part4implicits

import java.{util => ju}

object ScalaJavaConversions extends App {

  import collection.JavaConverters._

  val javaSet: ju.Set[Int] = new ju.HashSet[Int]()
  (1 to 5).foreach(javaSet.add)
  println(javaSet)

  val scalaSet = javaSet.asScala

  val javalist = new ju.ArrayList[String]()
  javalist.add("first")

  javalist.asScala.toList match {
    case Seq(l) => println("is a scala sqe")
    case _ => println("is not a scala seq")
  }


  /*
      Iterator
      Iterable
      ju.List - collection.mutable.Buffer
      ju.Set - collection.mutable.Set
      ju.Map - collection.mutable.Map

   */

  import collection.mutable._
  val numbersBuffer = ArrayBuffer[Int](1,2,3)
  val juNumbersBuffer = numbersBuffer.asJava

  println(juNumbersBuffer.asScala eq numbersBuffer)

  val numbers = List(1,2,3)
  val juNumbers = numbers.asJava
  val backToScala = juNumbers.asScala

  println(backToScala eq numbers)
  println(backToScala == numbers)

  /*

      Exercise
      create a Scala-Java Optional-Option
        .asScala
   */

  class ToScala[T](value: => T) {
    def asScala: T = value
  }

  implicit def asScalaOptional[T](o: ju.Optional[T]): ToScala[Option[T]] = new ToScala[Option[T]](
    if (o.isPresent) Some(o.get) else None
  )

  val juOptional: ju.Optional[Int] = ju.Optional.of(2)
  val scalaOption = juOptional.asScala
  println(scalaOption)

}
