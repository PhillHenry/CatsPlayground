package uk.co.odinconsultants.fp.cats.fs2.pipe

import cats.effect.{ExitCode, IO, IOApp, Ref}
import fs2.{INothing, Pipe, Stream, io}
import fs2.io.file.Files

import java.io.InputStream
import java.net.URL
import java.nio.file.Paths

object DownloadMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    download("https://isitchristmas.com/", "/tmp/christmas.txt")
      .compile.drain.as(ExitCode.Success)
  }

  def printTick(i: Int): IO[Unit] = if (i % 100 == 0) IO {
    print(".")
  } else IO.unit

  def download(spec: String, filename: String): fs2.Stream[IO, Unit] = {
    val input: InputStream = new URL(spec).openConnection.getInputStream
    val output: Pipe[IO, Byte, INothing] = Files[IO].writeAll(Paths.get(filename))
    val stream: fs2.Stream[IO, Byte] = io.readInputStream[IO](IO(input), 4096, closeAfterUse = true)
    val ticks = Stream.emit(0).repeat.covary[IO].evalMapAccumulate(0){ (acc: Int, _: Any) => printTick(acc) >> IO((acc + 1, acc)) }
    stream.zipWith(ticks.repeat){case (byte, _) => byte}.through(output)
  }

}
