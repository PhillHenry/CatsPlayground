package uk.co.odinconsultants.fp.cats.effects
import cats.effect
import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp, OutcomeIO, Poll, Resource}

import scala.concurrent.duration._

object MyCancellation extends IOApp.Simple {

  def logging(x: String): IO[String] = IO.println(s"${Thread.currentThread()}: $x") *> IO(x)

  val errorHandler: Throwable => IO[Unit] = x => IO.println(s"Failed with '${x.getMessage}''")

  type Resource = String

  def failingAt(x: String): IO[Resource] = IO.raiseError[Resource](new Throwable(x))

  type Acquire = Poll[IO] => IO[Resource]
  val happyPoll: Acquire      = poll => logging(s"acquire: '$poll''")
  val failingAcquire: Acquire = poll => logging(s"acquire: $poll") *> failingAt("acquire")

  type Use = Resource => IO[Resource]
  val happyUse: Use                         = x => logging(s"use '$x''")
  val failingUse: Use                       = x => logging(s"use: $x") *> failingAt("use")
  def waitingUse(wait: FiniteDuration): Use = x =>
    logging(s"use (waiting): $x") *> IO.sleep(wait) *> IO.println(s"finished waiting $wait") *> IO(
      x
    )

  type Release = (Resource, OutcomeIO[Resource]) => IO[Unit]
  val happyRelease: Release   = (x, outcome) => logging(s"release $x $outcome").void
  val failingRelease: Release = (x, outcome) =>
    logging(s"release $x $outcome") *> failingAt("release").void

  override def run: IO[Unit] = {
    val happyPath =
      IO.bracketFull(happyPoll)(happyUse)(happyRelease)

    val badRelease =
      IO.bracketFull(happyPoll)(happyUse)(failingRelease)

    val badUse =
      IO.bracketFull(happyPoll)(failingUse)(happyRelease)

    val badAcquire =
      IO.bracketFull(failingAcquire)(happyUse)(happyRelease)

    val semaphore                      = Semaphore[IO](1)
    val res: effect.Resource[IO, Unit] =
      Resource.make(IO.println("resource acquire"))(x => happyPath *> IO(x))

    for {
      happy       <- happyPath
      _           <- IO.println("")
      acquire     <- badAcquire.handleErrorWith(errorHandler)
      _           <- IO.println("")
      misuse      <- badUse.handleErrorWith(errorHandler)
      _           <- IO.println("")
      releasing   <- badRelease.handleErrorWith(errorHandler)
      _           <- IO.println("")
      interrupted <- IO.bracketFull(happyPoll)(waitingUse(10.seconds))(happyRelease).start
      _           <- IO.sleep(1.second)
      _           <- interrupted.cancel
    } yield ()
  }
}
