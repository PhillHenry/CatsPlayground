package uk.co.odinconsultants.fp.cats.cancellation

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.FiniteDuration._
import scala.concurrent.duration._
import cats.implicits._

object WhoWins extends IOApp {

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
      interruptible <- IO.interruptible(many = false)(doSomething(3)).start
      second <- doSomething(2)
//      _ <- interruptible.cancel
    } yield {
      println("finished")
    }
    something.as(ExitCode.Success)
  }
}
