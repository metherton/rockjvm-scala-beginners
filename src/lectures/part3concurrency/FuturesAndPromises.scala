package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromises extends App {

  val excludedSourcesInConfig = List("")
  if (excludedSourcesInConfig(0) == "") {
    println("empty list")
  }

  val excludedVideoSourcesMain = if (excludedSourcesInConfig(0) != "" && excludedSourcesInConfig.contains("screen")) excludedSourcesInConfig else List()
  val excludedVideoSourcesRow = if (excludedSourcesInConfig(0) != "") "screen" :: excludedSourcesInConfig else List("screen")

  println(excludedVideoSourcesMain)
  println(excludedVideoSourcesRow)


  val width = "1024".toDouble
  val height = "576".toDouble
  val prop: Double = height / width
  println(prop)
  println(576/1024)
  val numcol = 5
  val rowh = ((height / width) * (width / numcol)).toInt
  println(rowh)
  val screenHeight = (height - rowh).toInt
  println(screenHeight)
  val screenWidth = ((screenHeight / height) * width).toInt
  println(screenWidth)

  val videoLayout: String =
    f"""
      |{
      |    "main": {
      |       "xpos: ${((width - screenWidth)/2).toInt}
      |       "row": 1,
      |       "column": 2,
      |       "y_pos": ${screenHeight}
      |       "width": ${width.toInt}
      |    }
      |}
    """.stripMargin

  val videoLayoutConc1: String =
    f"""
       |{
       |    "main": {
       |       "row": 1,
     """

  val videoSources: String = if (excludedVideoSourcesMain.length > 0)
    f"""|       "video": screen,
     """
  else ""

  val videoLayoutConc2: String =
    f"""|       "column": 2,
       |    }
       |}
    """

  val combined = if (true) videoLayoutConc1 + videoSources + videoLayoutConc2 else videoLayoutConc1  + videoLayoutConc2

  println(videoLayout)
  println(combined.stripMargin)

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
    def poke(anotherProfile: Profile) =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    // database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )
    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
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
  } mark.poke(bill)

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
}


