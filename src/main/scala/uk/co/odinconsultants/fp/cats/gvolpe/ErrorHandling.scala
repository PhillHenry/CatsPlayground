package uk.co.odinconsultants.fp.cats.gvolpe

import cats.MonadError
import cats.effect._
import cats.implicits._

import scala.util.control.NoStackTrace

trait Random[F[_]] {
  def bool: F[Boolean]
  def int: F[Int]
}

object Random {
  /**
   * This is needed for syntax like:
  val F = Random[F]
   */
  def apply[F[_]](implicit instance : Random[F]) : Random[F] = instance

  /**
   * This is needed if we want to instantiate Category with, say, IO that knows nothing about Random.
   * Note that the object `Random` must be the same name as the trait `Random` or you'll get:
could not find implicit value for evidence parameter of type uk.co.odinconsultants.fp.cats.gvolpe.Random_[cats.effect.IO]
    val liveCategories: LiveCategories[IO] = new LiveCategories[IO]()
   * Plus, they must be in the same file or you'll get:
Companions 'trait Random' and 'object Random' must be defined in same file:
   */
  implicit def syncInstance[F[_]: Sync]: Random[F] =
    new Random[F] {
      def bool: F[Boolean] = int.map(_ % 2 === 0)
      def int: F[Int]      = Sync[F].delay(scala.util.Random.nextInt(100))
    }
}

case class Category(name: String)
sealed trait BusinessError extends NoStackTrace
case object RandomError extends BusinessError

class LiveCategories[
  F[_]: MonadError[*[_], Throwable]: Random
] {

  /**
   * Fabio Labella @SystemFw 13:34
there is a plugin that lets you write that, but I don't know if I'd recommend it
without it, you can write Sync[F].delay], or name F as an implicit param
the plugin is context-applied
   */
  val F = Random[F]

  def findAll: F[List[Category]] =
    F.bool.ifM(
      List.empty[Category].pure[F],
      RandomError.raiseError[F, List[Category]]
    )

  def maybeFindAll: F[Either[BusinessError, List[Category]]] =
    F.bool.map {
      case true  => List.empty[Category].asRight[BusinessError]
      case false => RandomError.asLeft[List[Category]]
    }

}


object ErrorHandling extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val liveCategories: LiveCategories[IO] = new LiveCategories[IO]()
    liveCategories.maybeFindAll.map(x => println(x)).as(ExitCode.Success)
  }
}
