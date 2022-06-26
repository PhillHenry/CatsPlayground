package uk.co.odinconsultants.fp.cats.structures

import cats.data.NonEmptyList
import cats.effect.kernel.Sync
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import cats.{Applicative, ApplicativeThrow}

import scala.util.control.NoStackTrace

sealed trait UserCommand
case class DownloadCommand(urls: NonEmptyList[String]) extends UserCommand
case class BuildCommand(file: List[String])            extends UserCommand
case class DeployCommand(image: String)                extends UserCommand

sealed trait CommandResult
case class DownloadsResult[T[_]](file: NonEmptyList[String]) extends CommandResult
case class BuildResult[T[_]](image: String)                  extends CommandResult
case class DeployResult[T[_]](message: String)               extends CommandResult

sealed trait DownloadResult        extends CommandResult
case class DownloadSuccessResult() extends DownloadResult
case class DownloadFailureResult() extends DownloadResult

object CommandError extends NoStackTrace

abstract class Actions[T[_]] {
  def download(url: String): T[String]
  def build(files: List[String]): T[String]
  def deploy(image: String): T[String]
}

class Prod[T[_]: Sync] extends Actions[T] {
  val BAD_URL   = "bad_url"
  val BAD_FILE  = "bad_file"
  val BAD_IMAGE = "bad_image"

  override def download(url: String): T[String] = delay(url, BAD_URL)

  def merge(x: T[String], y: T[String]): T[String] = for {
    a <- x
    b <- y
  } yield s"$a\n$b"

  override def build(files: List[String]): T[String] = {
    val actions = for { file <- files } yield delay(file, BAD_FILE)
    actions.reduce((x, y) => merge(x, y))
  }

  override def deploy(image: String): T[String] = delay(image, BAD_IMAGE)

  private def delay(x: String, except: String): T[String] =
    implicitly[Sync[T]].blocking(
      realWork(x, except)
    ) // in the real world, we'd probably use blocking over delay

  private def realWork(x: String, except: String): String =
    if (x == except) {
      println(s"Blowing up on $x")
      throw CommandError
    } else {
      println(s"Returning $x")
      x
    }
}

abstract class Interpreter[T[_]] {
  def interpret(actions: Actions[T]): UserCommand => T[CommandResult]
}

object SequencedInterpreter {
  type DownloadStrategy[T[_]] = NonEmptyList[T[String]] => T[NonEmptyList[String]]
}

class SequencedInterpreter[T[_]: Applicative](downloading: SequencedInterpreter.DownloadStrategy[T])
    extends Interpreter[T] {

  override def interpret(actions: Actions[T]): UserCommand => T[CommandResult] = {
    case DownloadCommand(urls) =>
      val downloads = for {
        url <- urls
      } yield actions.download(url)
      handleDownloads(downloads).map(DownloadsResult(_))
    case BuildCommand(files)   => actions.build(files).map(BuildResult(_))
    case DeployCommand(image)  => actions.deploy(image).map(DeployResult(_))
  }

  def handleDownloads(downloads: NonEmptyList[T[String]]): T[NonEmptyList[String]] = downloading(
    downloads
  )
}

object InterpreterOps {

  def retrying[T[_]: ApplicativeThrow, A](fa: T[A], remaining: Int, f: Throwable => T[A]): T[A] =
    if (remaining <= 0) fa else retrying(fa.handleErrorWith(f), remaining - 1, f)

  def withRetries[T[_]: ApplicativeThrow](
      actions: NonEmptyList[T[String]]
  ): T[NonEmptyList[String]] =
    retryingEach(actions).sequence

  def retryingEach[T[_]: ApplicativeThrow](
      actions: NonEmptyList[T[String]]
  ): NonEmptyList[T[String]] = for {
    action <- actions
  } yield retrying(action, 3, _ => action)

}

object MyFlow extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val prod                                          = new Prod[IO]
    val fn: SequencedInterpreter.DownloadStrategy[IO] = _.sequence
    val interpreter                                   = new SequencedInterpreter[IO](fn)
    val commands                                      = List(DownloadCommand(NonEmptyList.of("x", "y", prod.BAD_URL)))

    execute(prod, interpreter, commands).map(_ => ExitCode.Success)
  }

  private def execute[T[_]: Applicative](
      actions: Actions[T],
      interpreter: Interpreter[T],
      commands: List[DownloadCommand],
  ): T[Unit] = {
    val interpret                       = interpreter.interpret(actions)
    val results: List[T[CommandResult]] = for {
      command <- commands
    } yield interpret(command)
    results.sequence.map { xs =>
      println(xs.mkString("\n"))
    }
  }
}
