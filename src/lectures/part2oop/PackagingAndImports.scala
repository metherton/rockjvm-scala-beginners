package lectures.part2oop

import java.sql

import playground.{PrinceCharming, Cinderella => Princess}
import java.util.Date
import java.sql.{Date => SqlDate}

object PackagingAndImports extends App {


  // package members are accessible via their simple name
  val writer = new Writer("martin", "etherton", 34)

  // import the package
  val princess = new Princess

  // packages are in hierarchy
  // matches folder structure


  // package object
  sayHello
  println(SPEED_OF_LIGHT)

  // imports
  val prince = new PrinceCharming

  // 1. use fully qualified name
//  val date = new Date
//  val sqlDate = new sql.Date(2018, 5, 4)

  // use aliasing
  val sqlDate = new SqlDate(2018, 5, 4)

  // default imports
  // java.lang - String Object Exception
  // scala - Int, Nothing Function
  // scala.Predef - println ???

}
