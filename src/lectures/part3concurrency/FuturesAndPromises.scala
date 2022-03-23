package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromises extends App {


  case class Person(name: String, age: Int)

  val youngGuy = Person("youngie", 20)
  val oldGuy = Person("oldie", 50)

  val futureGuys = Future {
    List(youngGuy, oldGuy)
  }

  val onlyYoungGuys = futureGuys.map(guys => guys.filter(g => g.age < 40))
  val onlyYoungGuysComplete = futureGuys.map(guys => guys.filter(g => g.age < 40))


  val result = onlyYoungGuys andThen {
      case Success(guys) => println(guys)
      case Failure(ex) => println("failed to get guys")
    }

  val resultq = onlyYoungGuysComplete onComplete  {
    case Success(guys) => println(guys)
    case Failure(ex) => println("failed to get guys")
  }

  Thread.sleep(300)
  println(s"result$result")
  println(s"resultq$resultq")

//  onlyYoungGuys.onComplete {
//    case Success(guys) => println(guys)
//    case Failure(ex) => println("failed to get guys")
//  }


  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife
  } // (global) which is passed by the compiler

  println(aFuture.value) // Option[Try[Int]]

  println("waiting on the future")

  aFuture.onComplete { // partial function so we can leave out the Try[Int] => stuff
    case Success(meaningOfLife) => println(s"meaning of life is $meaningOfLife")
    case Failure(e) => println(s"I have failed with $e")
  } // SOME thread

  Thread.sleep(3000)


  // mini social network

  case class Profile(id: String, name: String) {
    def sendMessage(anotherProfile: Profile, message: String) =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    // database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-jane" -> "Jane"
    )
    val friends = Map(
      "fb.id.0-jane" -> "fb.id.2-bill"
    )

    val random = new Random()

    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetching from DB
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }
    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }

    def sendMessageToBestFriend(accountId: String, message: String): Unit = {
      // 1 call fetchProfile
      val profileFuture = SocialNetwork.fetchProfile(accountId)
      profileFuture.onComplete {
        case Success(profile) =>
          val friendProfileFuture = SocialNetwork.fetchBestFriend(profile)
          friendProfileFuture.onComplete {
            case Success(friendProfile) => profile.sendMessage(friendProfile, message)
            case Failure(e) => e.printStackTrace()
          }
        case Failure(exception) => print(exception.printStackTrace)
      }
      // 2 call fetchBestFriend
      // 3 call profile.sendMessage(bestFriend)
    }


    // onComplete is a hassle
    // solution is functional composition

    val janeProfileFuture = SocialNetwork.fetchProfile("fb.id.0-jane")
    val janeFuture: Future[String] = janeProfileFuture.map(profile => profile.name) // map transforms value contained inside, asynchronously
    val janesBestFriend: Future[Profile] = janeProfileFuture.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
    val janesBestFriendFilter: Future[Profile] = janesBestFriend.filter(profile => profile.name.startsWith("Z"))

    def sendMessageToBestFriend_v2(accountId: String, message: String): Unit = {
      val profileFuture = SocialNetwork.fetchProfile(accountId)
      // TODO:
      val action: Future[Unit] = profileFuture.flatMap { profile =>
        SocialNetwork.fetchBestFriend(profile).map { bestFriend => // Future[Unit]
          profile.sendMessage(bestFriend, message) // unit
        }
      }
    }

  }

  // client; mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
//  mark.onComplete {
//    case Success(markProfile) => {
//      val bill = SocialNetwork.fetchBestFriend(markProfile)
//      bill.onComplete {
//        case Success(billProfile) => markProfile.poke(billProfile)
//        case Failure(e) => e.printStackTrace()
//      }
//    }
//    case Failure(ex) => ex.printStackTrace()
//  }



  // functional composition of futures
  // map, flatMap, filter
  val nameOnTheWall = mark.map(profile => profile.name)
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  // for comprehensions
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.sendMessage(bill, "hey bill")

  Thread.sleep(1000)

  // fallbacks patterns
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // online banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM Banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some process
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch user from DB
      // create a transaction
      // WAIT for transaction to finish

      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      Await.result(transactionStatusFuture, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(BankingApp.purchase("Daniel", "iphone 12", "rock the jvm store", 3000))

  // promises
  val promise = Promise[Int]() // controller over a future
  val future = promise.future

  // thread 1 - consumer
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
  }

  // thread 2 - producer
  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(1000)
    // fulfilling the promise
    promise.success(42)
    println("[producer] done")
  })

  producer.start()

  Thread.sleep(1000)

  /*
      1. fulfill a future immediately with a value
      2. write function inSequence(fa, fb) // 2 futures in sequence
      3. function first(fa, fb) -> future with the first value of the two futures
      4. function last(fa, fb) -> future with the last value of the two futures
      5. retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]

   */

  // 1
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2
  def insequence[A, B](first: Future[A], second: Future[B]): Future[B] =
    first.flatMap(_ => second)

  // 3 - first out of two futures
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {

    val promise = Promise[A]()

    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future

  }

  // 4 - last out of two futures
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {

    // 1 promise which both will try to complete
    // 2 promise which the LAST future will complete
    val bothPromise = Promise[A]()
    val lastPromise = Promise[A]()
    val checkAndComplete = (result: Try[A]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future

  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }

  first(fast, slow).foreach(f => println("FIRST: " + f))
  last(fast, slow).foreach(l => println("LAST: " + l))

  Thread.sleep(1000)

  // retry until

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil(action, condition)
      }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated: " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 5).foreach(result => println("result is " + result))

  Thread.sleep(2000)


  implicit class ScheduleMapper(schedules: Schedules) {
    def mapper(): Map[String, Int] = {
      schedules.listSchedules.map(sched => (sched.name -> sched.age)).toMap
    }
  }

  case class Schedule(recType: String, name: String, age: Int)
  case class Schedules(listSchedules: List[Schedule])

  val s1 = Schedule("1", "compliance", 5)
  val s2 = Schedule("2", "selective", 3)
  val scheds = Schedules(List(s1, s2))

  def getSchedules(): Future[Schedules] = Future {
    Thread.sleep(200)
    scheds
  }

  val myRules: Future[Map[String, Int]] = for {
    myScheds <- getSchedules()
  } yield myScheds.mapper()


  val list = List(List(1),List(2),List(3))
  val newList = list.flatMap(x => List(x))

  //println(myRules)

  def getRules(): Future[Map[String, Int]] = myRules

  def getExpirationTime(): Future[Int] = {
    myRules.map(rule => rule("compliance"))
  }

  val exp = getExpirationTime()
  exp onComplete {
    case Success(l) => println(s"value: $l")
    case Failure(e) => println(e)
  }

  Thread.sleep(300)

  val doub = (x: Int) => x * 2

  val myInt: List[Int] = List(5)
  val myIntMapped = myInt.map(x => doub(x))
  val myIntFlMp = myInt.flatMap( x => List (doub(x)))

  println(myInt)

  val myIntFutureMapped = myInt.map(s => Future { (s * 2).toString } )

//  println(myIntFlatmapped)




//  myIntFlatmapped.andThen {
//    case Success(bl) => println(bl)
//    case Failure(e) => print(e)
//  }

  val times2AndToString = (x: Int) => (x * 2).toString

  val flatMapFn = (x: Int) => Future {times2AndToString(x)}

//  val r = myInt.flatMap(x => flatMapFn(x))

//  println(r)


  val bla = Future {
    3
  }

  val r = bla onComplete {
    case Success(s) => s
    case Failure(e) => println(e)
  }
  Thread.sleep(500)
  println(r)

  def fallbackF = {
    Future { 56 }
  }

  val failingFuture = Future {
    42 / 1
  }


  val failed = failingFuture recoverWith {
    case e => {
      println(e)
      fallbackF
    }

  }

  val bal = for {
    res <- failed
  } yield res


  for {
    b <- bal
  } yield {
    println("hi" + b)
  }

  val n42 = Future {
    Thread.sleep(500)
    42
  }

  val n42DoubleString = n42 map {
    num => (num * 2).toString
  }

  n42DoubleString foreach {v => println(v)}

  Thread.sleep(1000)

  val bla5 = Future { 42 / 0}
  bla5.map {
    v => v * 2
  } recoverWith {
    case e => Future {3}
  }
}


