package uk.co.odinconsultants.fp.cats.cancellation

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.FiniteDuration._
import scala.concurrent.duration._
import cats.implicits._

object WhoWins extends IOApp {

  def javaSleep(ms: Long = 2000) = IO {
    println(s"About to sleep $ms ms")
    Thread.sleep(ms)
    println(s"Finished sleeping $ms ms")
  }.onCancel(IO.println(s"Sleeping $ms cancelled")).onError(t => IO { t.printStackTrace() })

  def print(x: Any): IO[Unit] =
    IO.println(s"$x").onCancel(IO.println(s"$x cancelled")).onError(t => IO {t.printStackTrace()})

  def doSomething(x: Int): IO[Unit] =
    print(s"Starting $x") >>
    IO.sleep(1.seconds).onCancel(IO.println(s"$x sleep cancelled")) >>
    print(s"Finished $x")

  override def run(args: List[String]): IO[ExitCode] = {
    val something: IO[Any] = for {
      first <- doSomething(1).start
      _ <- first.cancel
      javaSleepFibre <- javaSleep(1999).start
      interruptible <- IO.interruptible(many = true)(javaSleep()).start
      second <- doSomething(2)
      _ <- javaSleepFibre.cancel
//      _ <- interruptible.cancel
    } yield {
      println("finished")
    }
    something.as(ExitCode.Success)
  }
}
