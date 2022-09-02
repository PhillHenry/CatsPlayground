package uk.co.odinconsultants.fp.cats.effects
import cats.effect.{IO, IOApp, OutcomeIO, Poll}

object MyCancellation extends IOApp.Simple {

  def logging(x: String): IO[String] = IO.println(s"${Thread.currentThread()}: $x") *> IO(x)

  val errorHandler: Throwable => IO[Unit] = x => IO.println(s"Failed with '${x.getMessage}''")

  type Resource = String

  def failingAt(x: String): IO[Resource] = IO.raiseError[Resource](new Throwable(x))

  type Acquire = Poll[IO] => IO[Resource]
  val happyPoll: Acquire      = poll => logging(s"acquire: '$poll''")
  val failingAcquire: Acquire = poll => logging(s"acquire: $poll") *> failingAt("acquire")

  type Use = Resource => IO[Resource]
  val happyUse: Use   = x => logging(s"use '$x''")
  val failingUse: Use = x => logging(s"use: $x") *> failingAt("use")

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

    for {
      happy     <- happyPath
      _         <- IO.println("")
      acquire   <- badAcquire.handleErrorWith(errorHandler)
      _         <- IO.println("")
      misuse    <- badUse.handleErrorWith(errorHandler)
      _         <- IO.println("")
      releasing <- badRelease.handleErrorWith(errorHandler)
    } yield ()
  }
}
