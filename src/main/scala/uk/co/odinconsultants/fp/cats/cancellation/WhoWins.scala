package uk.co.odinconsultants.fp.cats.cancellation

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.FiniteDuration._
import scala.concurrent.duration._
import cats.implicits._

object WhoWins extends IOApp {

  def doSomething(x: Int): IO[Unit] = IO.println(s"Starting x = $x") >>
    IO.sleep(1.seconds) >>
    IO.println(s"Finished with $x")

  override def run(args: List[String]): IO[ExitCode] = {
    val something = for {
      first <- doSomething(1).start
      _ <- first.cancel
      second <- doSomething(2)
    } yield {
      println("finished")
    }
    something.as(ExitCode.Success)
  }
}
