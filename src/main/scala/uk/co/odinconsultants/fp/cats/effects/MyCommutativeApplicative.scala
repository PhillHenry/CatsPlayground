package uk.co.odinconsultants.fp.cats.effects
import cats.effect.{IO, IOApp}
import cats.implicits._
//
//import scala.Predef.refArrayOps
//import scala.collection.SortedMap
//import scala.collection.compat.immutable.ArraySeq

/** armanbilge — Yesterday at 11:24 AM
  * to do an unorderedTraverse you need a CommutativeApplicative
  * "commutative" means that the order of the arguments does not matter e.g. f(x, y) === f(y, x)
  * Either is not commutative, because it short-circuits: when you hit the first Left, it will stop.
  * so whichever left comes first wins, and the order matters
  * I think if you use a SortedMap then you can use traverse instead of unorderedTraverse, which
  * only needs Applicative (not commutative)
  */
object MyCommutativeApplicative extends IOApp.Simple {

  def foo(s: String): Either[String, Int] =
    if (s.getBytes.forall(_.toChar.isDigit)) Right(s.toInt) else Left("Only digits allowed")

  override def run: IO[Unit] = {
    val aMap = Map("aaa" -> "123", "bbb" -> "321")
    /*
could not find implicit value for parameter ev$1: cats.CommutativeApplicative[[+B]Either[String,B]]
     */
//    aMap.unorderedTraverse(foo) >>
//    val sortedMap: SortedMap[String, String] = SortedMap(aMap.toArray: _*)
    IO.println(
      aMap.toList.traverse { case (k, v) => foo(v) }
    ) *>
//    sortedMap.parUnorderedTraverse(foo)
      IO.unit
  }
}
