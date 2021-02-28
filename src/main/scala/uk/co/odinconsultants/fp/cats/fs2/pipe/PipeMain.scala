package uk.co.odinconsultants.fp.cats.fs2.pipe

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{Pipe, Stream}

object PipeMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val printEffect:  Int => IO[Unit]     = x => IO { println(x) }
    val printEach:    Pipe[IO, Int, Unit] = { s =>
      val result: Stream[IO, IO[Unit]] = s.map { x =>
        val printed: IO[Unit] = printEffect(x)
        printed
      }
      result.void
    }
    printEach(Stream(1, 2, 3, 4, 5)).compile.toList.as(ExitCode.Success)
  }

}
