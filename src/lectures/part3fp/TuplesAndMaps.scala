package lectures.part3fp

import scala.annotation.tailrec

object TuplesAndMaps extends App {


  val myM = scala.collection.mutable.Map(("one" -> ("oneP", "oneS")), ("two" -> ("twoP", "twoS")))
  myM += ("three" -> ("threeP", "threeS"))
  println(myM)
  myM -= "one"
  println(myM)

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

  /**
    *   1. What would happen if I had two original entries "Jim" -> 555 and "JIM" -> 900
    *
    *   !!! careful with mapping keys
    *
    *   2. Overly simplified social network based on maps
    *     Person = String
    *     - add a person to the network
    *     - remove a person
    *     - friend (mutual) a person adds another person as a friend
    *     - unfriend
    *
    *     - number of friends of a person
    *     - person with most friends
    *     - how many people have no friends
    *     - if there is a social connection between two people (direct or not)
    */


    def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
      network + (person -> Set())

  val dupes: Map[String, Int] = Map(("Jim", 555), "JIM" -> 789, "jIm" -> 898).withDefaultValue(-1)
  println(dupes.map(pair => pair._1.toLowerCase -> pair._2))

  val network: Map[String, Set[String]] = Map()
  val newPerson1 = ("John", Set[String]("Willie", "Time", "Jim"))
  val newPerson2 = ("Fred", Set[String]("Bill", "Rick"))
  val newNetwork = network + newPerson1 + newPerson2
  println(newNetwork)
  val removedNetwork = newNetwork - "John"
  println(removedNetwork)

  def friend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)
    network + (a -> (friendsA + b)) + (b -> (friendsB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)
    network + (a -> (friendsA - b)) + (b -> (friendsB - a))
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))

    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  val empty: Map[String, Set[String]] = Map()
  val network1 = add(add(empty, "Bob"), "Mary")
  println(network1)
  println(friend(network1, "Bob", "Mary"))
  println(unfriend(friend(network1, "Bob", "Mary"), "Bob", "Mary"))

  println(remove(friend(network1, "Bob", "Mary"), "Bob"))

  println(friend(newNetwork, "John", "Fred"))

  def numberOfFriends(person: String, network: Map[String, Set[String]]): Int =
    if (!network.contains(person)) 0
    else
      network(person).size

  println(numberOfFriends("Jim", newNetwork))

  val networkCount: Map[String, Int] = newNetwork.map((pair) => (pair._1, pair._2.size))

  println(networkCount.maxBy(_._2)._1)


  println(networkCount.filter((pair) => pair._2 == 0))

  // Jim, Bob, Mary
  val people = add(add(add(empty, "Bob"), "Mary"), "Jim")
  val jimBob = friend(people, "Bob", "Jim")
  val testNet = friend(jimBob, "Bob", "Mary")
  println(testNet)


  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  println(mostFriends(testNet))

  def nPeopleNoFriends(network: Map[String, Set[String]]): Int =
    network.count(_._2.isEmpty)

  println(nPeopleNoFriends(testNet))


  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {

    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }

    bfs(b, Set(), network(a) + a)
  }

  println(socialConnection(testNet, "Mary", "Jim"))
}
