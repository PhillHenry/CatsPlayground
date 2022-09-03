package uk.co.odinconsultants.fp.cats.effects
import cats.effect
import cats.effect.IO.Blocking
import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp, OutcomeIO, Poll, Resource, Sync}

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
    logging(s"use (waiting $wait): $x") *> IO {
      javaWaitFor(wait)
      x
    }

  private def javaWaitFor(
      wait: FiniteDuration
  ) = {
    println(s"About to wait $wait")
    Thread.currentThread().synchronized(Thread.currentThread().wait(wait.toMillis))
    println(s"finished waiting $wait")
  }
  type Release = (Resource, OutcomeIO[Resource]) => IO[Unit]
  val happyRelease: Release                 = (x, outcome) => logging(s"release $x $outcome").void
  val failingRelease: Release               = (x, outcome) =>
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

    val sleepingUse = IO.bracketFull(happyPoll)(waitingUse(3.seconds))(happyRelease)

    val semaphore: IO[Semaphore[IO]]       = Semaphore[IO](1)
    val res: effect.Resource[IO, Resource] =
      Resource.makeFull(happyPoll) { x: Resource =>
        IO.println(s"Releasing $x").void
      }

    val blockingHappyPath: IO[Resource] = Sync[IO]
      .blocking {
        javaWaitFor(3.seconds)
        "producer.beginTransaction()"
      }
      .bracketCase(happyUse)(happyRelease)

    for {
      happy       <- happyPath
      _           <- IO.println("")
      acquire     <- badAcquire.handleErrorWith(errorHandler)
      _           <- IO.println("")
      misuse      <- badUse.handleErrorWith(errorHandler)
      _           <- IO.println("")
      releasing   <- badRelease.handleErrorWith(errorHandler)
      _           <- IO.println("")
      interrupted <- sleepingUse.start
      _           <- IO.sleep(1.second)
      _           <- interrupted.cancel // interrupting makes no difference to the IO
      _           <- IO.println("")
      sem         <- Semaphore[IO](1)
      permit      <- sem.permit.surround(sleepingUse)
      _           <- IO.println("")
      blocking    <- blockingHappyPath
    } yield ()
  }
}
