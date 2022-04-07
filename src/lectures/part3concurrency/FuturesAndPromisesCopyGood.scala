package lectures.part3concurrency

import lectures.part3concurrency.FuturesAndPromises.Profile

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}


// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromisesCopyGood extends App {

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

  Future.sequence(liftedFuture).foreach(f => println(f))


  val fRecording = Future {
    "recording"
  }
  val fFail = Future.failed(new IllegalArgumentException("wrong param"))
  val fAccount = Future {
    "account"
  }
  val fCall = Future {
    "call"
  }

  val details = (for {
    recording <- fRecording
    account <- fAccount
    call <- fCall
  //  fail <- fFail
  } yield (recording, account, call))

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


