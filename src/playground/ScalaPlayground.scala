package playground

object ScalaPlayground extends App {

  println("Hello scala")


  println(List(1,2,3).scanLeft(0)(_ + _))
}
