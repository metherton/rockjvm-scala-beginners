package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {

  /*
      interface Runnable {
        public void run()
      }
   */
  // JVM threads
//  val aThread = new Thread(new Runnable {
//    def run(): Unit = println("running in parallel")
//  })

 // aThread.start() // gives the signal to the JVM to start a JVM thread
  // create a JVM thread => OS thread

//  aThread.join()

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
//
  threadHello.start()
  threadGoodbye.start()

  // executors
  val pool = Executors.newFixedThreadPool(10)

  pool.execute(() => println("the something in the thread pool"))
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("done after 1 second")
//  })
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("almost done")
//    Thread.sleep(1000)
//    println("done after 2 seconds")
//  })
//
  pool.shutdown()


//  pool.execute(() => println("should not appear")) // throws exception in calling thread

//  pool.shutdownNow()

//  println(pool.isShutdown)

  def runInParallel() = {

    var x = 0
    val thread1 = new Thread(() => {
      x = 1
    })
    val thread2 = new Thread(() => {
      x = 2
    })
    thread1.start()
    thread2.start()
    println(x)
  }

//  for (_ <- 1 to 100) runInParallel

  class BankAccount(@volatile var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price  // account.amount = account.amount - price
//    println("I've bought " + thing)
//    println("my account is now " + account)
  }

//  for (x <- 1 to 10000) {
//    val account = new BankAccount(50000)
//    val thread1 = new Thread(() => buy(account, "shoes", 3000))
//    val thread2 = new Thread(() => buy(account, "iphone", 4000))
//
//    thread1.start()
//    thread2.start()
//    Thread.sleep(10)
//    if (account.amount != 43000) println("AHA: " + account.amount)
//  }

  /*
      thread1 (shoes) 50000
        account = 50000 - 3000 = 47000
      thread2 (phone) 50000
        account = 50000 - 4000 = 46000 overwrites the memory of account.amount


   */

  // option 1 : use synchronised()
//  def buySafe(account: BankAccount, thing: String, price: Int) = {
//    account.synchronized {
//      // no 2 threads can evaluate this at the same time
//      account.amount -= price  // account.amount = account.amount - price
//      println("I've bought " + thing)
//      println("my account is now " + account)
//    }
//  }


  // option 2 - use volatile


  /*

      Exercises

      1: Construct 50 inception threads
          thread1 -> thread2 -> thread3 -> ...
          println("hello from thread #3)

          IN REVERSE ORDER

   */

    def inceptionThreads(maxThreads: Int, i: Int = 1): Thread =
      new Thread(() => {
        if (i < maxThreads) {
          val newThread = inceptionThreads(maxThreads, i + 1)
          newThread.start()
          newThread.join()
        }
        println(s"hello from thread ${i}")
      })

//    inceptionThreads(50).start()
  /*

    Exercises

    2

 */
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  threads.foreach(_.join())
  println(x)
  /*
      1. what is the biggest value possible for x ?
      2. what is the SMALLEST value possible for x ?

      thread1: x = 0
      thread2: x = 0
        ...
      thread100: x = 0

      for all threads x = 1 and write it back to x
   */



  /*

      3. sleep fallacy


   */

  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "scala is awesome"
  })

  message = "scala sucks"
  awesomeThread.start()


  Thread.sleep(1001)

  // fix with join
  awesomeThread.join() // wait for awesome thread to join


  println(message)

  /*
      whats the value of message
      is it guaranteed
      why ? why not ?   NOT

      main thread...
        scala sucks
        awesome thread.start
        sleep // relieves execution
        awesome thread
          sleep // relieves execution
        OS gives CPU to some important thread - takes CPU more than 2 seconds
        OS gives CPU back to main thread
        println(scala sucks)

        OS gives CPU to auwesome thread
          message = scala is awesome // too late


   */
}
