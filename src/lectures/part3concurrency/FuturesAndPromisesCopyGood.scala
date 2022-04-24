package lectures.part3concurrency

import lectures.part3concurrency.FuturesAndPromises.Profile

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}


// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromisesCopyGood extends App {

 // val failedF = Future.failed(new UnsupportedOperationException("I am failed future"))
  val failedF = Future.successful("successful future")

  val failedFMap = failedF.map(s => println(s" in map $s"))
  failedFMap recoverWith {
    case _ => Future.successful(1)

  }

  val intF = Future {
    println("in printf")
    1
  }

  val doF = Future {
    println("in dof")
    42
    //throw new UnsupportedOperationException("dof is not supported")
  }

  val doF2 = Future {
    println("in dof2")

  }


  val dolResult = doF.andThen {
    case Success(i) => {
      println(s"hopefully not doing anything $i")
      throw new UnsupportedOperationException("dof throwing exception")
    }
    case Failure(e) => println(s"should throw exception $e")
  }

  dolResult.andThen {
    case Success(i) => {
      println(s"hopefully not doing anything second andThen")
    }
    case Failure(e) => println(s"should come in second andThen failure $e")
  }


  def createAndSendEvents(events: List[String], recordingType: String): Future[Unit] = {
    println(s"create and send events $recordingType")
    if (events.size > 0) {
      events.foreach(recType => {
        sendEvent(recType)
      })
    }
    Future.successful(())
  }



  def transferRecording() = {
    val transferData = for {
      recordingType <- Future {
        println("Getting recording")
        "recording"
      }
      _ <- createAndSendEvents(List("event1", "event2", "event3"), recordingType)
    } yield ()

    transferData.andThen {
      case Success(_) => println("transfer data succeeded")
      case Failure(_) => println("transfer data failed")
    }

    Thread.sleep(3000)

  }

  //transferRecording()


//  val f1 = Future {
//    Thread.sleep(500)
//    "event1"
//  }
//  val f2 = Future {
//    Thread.sleep(500)
//    "event2"
//  }

//  val lifted: List[Future[Try[String]]] = List(f1, f2).map(_.map(Success(_)).recover {case t => Failure(t)})

  def sendEvent(data: String) = {
    val p = Promise[Unit]()
    try {
      if (data.equals("event1")) {
        Thread.sleep(500)
        p.failure(throw new Exception(s"failed for $data"))
      } else {
        Thread.sleep(500)
        p.success(println(s"Succeeded for $data"))
      }
    } catch {
      case e: Exception => {
        println(s"${e.getMessage}")
        p.failure(e)
      }
    } finally {
      p.future
    }
    p.future
    //    Future.successful(())
  }

  val liftedFuture: List[Future[Try[Unit]]] = List("event1", "event2", "event3")
    .map(e => {
      sendEvent(e)
    }
    .map(g => {
      Success(g)
    })
    .recover {
      case t => Failure(t)
    }
    )

 // Future.sequence(liftedFuture).foreach(f => println(f))


  val fRecording = Future {
    "recording"
  }
  val fFail = Future.failed(new IllegalArgumentException("wrong param"))
  val fAccount = Future {
    println("in future account")
    "account"
  }
  val fCall = Future {
    println("in future call")
    "call"
  }

  val details = (for {
    fail <- fFail
    //recording <- fRecording
    account <- fAccount
    call <- fCall
  //  fail <- fFail
 // } yield (recording, account, call))
  } yield (fail, account, call))

  println(details)

  details.andThen {
    case Success(e) => println(e)
    case _ => println("failed")
  }

//  details.andThen {
//    case Success(d) => println(d._2)
//    case Failure(e) => println(e)
//  }.andThen {
//    case Success(d) => Future.failed(new IllegalArgumentException("bad argument"))
//    case Failure(e) => println(e)
//  }.andThen {
//    case Success(d) => println(d._1)
//    case Failure(e) => println(e)
//  }.recover {
//    case e: Throwable => println(e.getMessage)
//  }

}


