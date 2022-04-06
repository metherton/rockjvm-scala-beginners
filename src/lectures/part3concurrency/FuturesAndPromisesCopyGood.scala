package lectures.part3concurrency

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}


// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromisesCopyBad extends App {

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
        println(s"unable to send event $data to GRP")
        p.failure(e)
      }
    } finally {
      p.future
    }

    Future.successful(())
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

  transferRecording()
}


