package uk.co.odinconsultants.fp.cats.structures

import cats.{Applicative, ApplicativeError, FlatMap, Id, Monad, MonadError}
import cats.implicits._
import cats.effect.IO
import cats.effect.kernel.Sync

import scala.util.control.NoStackTrace

sealed trait UserCommand
case class DownloadCommand(urls: List[String]) extends UserCommand
case class DockerCommand(file: List[String]) extends UserCommand
case class DeployCommand(image: String) extends UserCommand

sealed trait CommandResult
case class DownloadResult[T[_]](file: T[List[String]]) extends CommandResult
case class DockerResult[T[_]](image: T[String]) extends CommandResult
case class DeployResult[T[_]](message: T[String]) extends CommandResult

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
    if (x == except)
      throw new Exception()
    else
      x
}

object MyFlow {

  def interpreter[T[_]: Applicative](actions: Actions[T]): UserCommand => CommandResult = _ match {
    case DownloadCommand(urls) =>
      val downloads: List[T[String]] = for {
        url <- urls
      } yield actions.download(url)
      DownloadResult(downloads.sequence)
    case DockerCommand(files)  => DockerResult(actions.docker(files))
    case DeployCommand(image)  => DeployResult(actions.deploy(image))
  }

  def main(args: Array[String]): Unit = {
    val prod = new Prod[IO]
    val interpret = interpreter(prod)
    val commands = List(DownloadCommand(List("x", "y", prod.BAD_URL)))
    val results = for {
      command <- commands
    } yield interpret(command)
    println(results.mkString("\n"))
  }

}
