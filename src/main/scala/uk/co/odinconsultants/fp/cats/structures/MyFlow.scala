package uk.co.odinconsultants.fp.cats.structures

import cats.{Applicative, ApplicativeError, Id, Monad, MonadError}
import cats.implicits._
import cats.effect.IO
import cats.effect.kernel.Sync

import scala.util.control.NoStackTrace

sealed trait UserCommand
case class DownloadCommand(url: String) extends UserCommand
case class DockerCommand(file: String) extends UserCommand
case class DeployCommand(image: String) extends UserCommand

sealed trait CommandResult
case class DownloadResult(file: String) extends CommandResult
case class DockerResult(image: String) extends CommandResult
case class DeployResult(message: String) extends CommandResult

object CommandError extends NoStackTrace

abstract class Actions[T[_]] {
  def download(url: String): T[String]
  def docker(file: String):  T[String]
  def deploy(image: String): T[String]
}

class Prod[T[_]: MonadError[*[_], Throwable]: Sync] extends Actions[T] {
  val BAD_URL   = "bad_url"
  val BAD_FILE  = "bad_file"
  val BAD_IMAGE = "bad_image"

  implicit val T = implicitly[Sync[T]]

  override def download(url: String): T[String] = delay(url, BAD_URL)

  override def docker(file: String): T[String]  = delay(file, BAD_FILE)

  override def deploy(image: String): T[String] = delay(image, BAD_IMAGE)

  private def delay(x: String, except: String): T[String] = T.delay(realWork(x, except))

  private def realWork(x: String, except: String): String = {
    if (x == except)
      throw new Exception()
    else
      x
  }
}

object MyFlow {

  def interpreter[T[_]](actions: Actions[T]): UserCommand => T[String] = _ match {
    case DownloadCommand(url) => actions.download(url)
    case DockerCommand(file)  => actions.download(file)
    case DeployCommand(image) => actions.download(image)
  }

  def main(args: Array[String]): Unit = {

    val prod = new Prod[IO]
    println(prod.download("good url"))
  }

}
