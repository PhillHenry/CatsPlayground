package uk.co.odinconsultants.fp.cats.structures

import cats.{Applicative, ApplicativeError, FlatMap, Id, Monad, MonadError}
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Sync

import scala.util.control.NoStackTrace

sealed trait UserCommand
case class DownloadCommand(urls: List[String]) extends UserCommand
case class DockerCommand(file: List[String]) extends UserCommand
case class DeployCommand(image: String) extends UserCommand

sealed trait CommandResult
case class DownloadResult[T[_]](file: List[String]) extends CommandResult
case class DockerResult[T[_]](image: String) extends CommandResult
case class DeployResult[T[_]](message: String) extends CommandResult

object CommandError extends NoStackTrace

abstract class Actions[T[_]] {
  def download(url: String): T[String]
  def docker(files: List[String]):  T[String]
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
  } yield s"$a, $b"

  override def docker(files: List[String]): T[String]  = {
    val actions = for { file <- files } yield delay(file, BAD_FILE)
    actions.reduce((x, y) => merge(x, y))
  }

  override def deploy(image: String): T[String] = delay(image, BAD_IMAGE)

  private def delay(x: String, except: String): T[String] =
    implicitly[Sync[T]].delay(realWork(x, except))

  private def realWork(x: String, except: String): String =
    if (x == except) {
      println(s"Blowing up on $x")
      throw new Exception()
    } else {
      println(s"Returning $x")
      x
    }
}

abstract class Interpreter[T[_]] {
  def interpret(actions: Actions[T]): UserCommand => T[CommandResult]
}

class SingleThreadedInterpreter[T[_]: Applicative] extends Interpreter[T] {

  override def interpret(actions: Actions[T]): UserCommand => T[CommandResult] = _ match {
    case DownloadCommand(urls) =>
      val downloads: List[T[String]] = for {
        url <- urls
      } yield actions.download(url)
      downloads.sequence.map(DownloadResult(_))
    case DockerCommand(files)  => actions.docker(files).map(DockerResult(_))
    case DeployCommand(image)  => actions.deploy(image).map(DeployResult(_))
  }
}

object MyFlow  extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val prod = new Prod[IO]
    val interpret = new SingleThreadedInterpreter[IO].interpret(prod)
    val commands = List(DownloadCommand(List("x", "y", prod.BAD_URL)))
    val results: List[IO[CommandResult]] = for {
      command <- commands
    } yield interpret(command)
    results.sequence.map { xs =>
      println(xs.mkString("\n"))
      ExitCode.Success
    }
  }

}
