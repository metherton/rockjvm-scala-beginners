package lectures.part3concurrency

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}


// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromisesCopyGood extends App {

  def createAndSendEvents(events: List[String]): Future[Unit] = {
    if (events.size > 0) {
      events.foreach(recType => {
        sendEvent(recType)
      })
    }
    Future.successful(())
  }

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

  def transferRecording() = {
    val transferData = for {
      recording <- Future { "recording" }
      _ <- createAndSendEvents(List("event1", "event2", "event3"))
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

  Future.sequence(liftedFuture).foreach(f => {
    case Success(e) => println("All futures succeeded")
    case Failure(e) => println(s"Something failed $e")
  })

}


