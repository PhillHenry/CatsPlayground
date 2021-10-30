package uk.co.odinconsultants.fp.cats.effects

import cats.effect.{ExitCode, IO, IOApp}

object MyPure extends IOApp {
  /**
   * @see cats.MonadError#ensure
   */
  override def run(args: List[String]): IO[ExitCode] = {
    val fa = IO.println("hi") // .as(true)
    val asImpure = fa.flatMap(a => fa) // this prints out twice (obviously)
    val asPure = fa.flatMap(a => IO.pure(a)) // this avoids a second print out
    asImpure.as(ExitCode.Success)
  }
}
