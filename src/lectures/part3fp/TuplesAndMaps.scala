package lectures.part3fp

object TuplesAndMaps extends App {


  // tuples = finite ordered lists
  val aTuple = new Tuple2(2, "hello scala") // Tuple2[Int, String] = (Int, String)

  println(aTuple._1) // 2

  println(aTuple.copy(_2 = "goodby java"))
  println(aTuple.swap) // ("hello scala",2)

  // Maps - keys -> values

  val aMap: Map[String, Int] = Map()

  //val phoneBook = Map(("Jim", 555), "Daniel" -> 789).withDefaultValue(-1)
  val phoneBook: Map[String, Int] = Map(("Jim", 555), "Daniel" -> 789).withDefaultValue(-1)

  // a -> b is sugar for (a, b)
  println(phoneBook)


  // map ops
  println(phoneBook.contains("Jim"))

  println(phoneBook("Jim"))
  println(phoneBook("Mary"))

  // add a pairing
  val newPairing = ("Mary", 678)
  val newPhonebook = phoneBook + newPairing
  println(newPhonebook)

  // Functionals on maps
  // map, flatMap, filter

  println(phoneBook.map(pair => pair._1.toLowerCase -> pair._2))

  // filterKeys
  val filteredBook = phoneBook.view.filterKeys(x => x.startsWith("J")).toMap
  println(filteredBook)

  // mapValues
  println(phoneBook.view.mapValues(number => number * 10).toMap)
  println(phoneBook.view.mapValues(number => "02345-" + number).toMap)

  // conversions
  println(phoneBook.toList)

  println(List(("Daniel", 555)).toMap)

  val names = List("Bob", "James", "Angela", "Mary", "Daniel", "Jim")
  println(names.groupBy(name => name.charAt(0)))
}
