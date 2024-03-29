package uk.co.odinconsultants.fp.cats.structures

import cats.data._
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import cats.{Applicative, _}
import munit.FunSuite

import scala.collection.immutable.SortedSet

class ListSpec extends FunSuite {
  test("traversing sets") {
    import ListSpec._

    val listOfSetInts = List(Set(1, 2, 3), Set(4, 5, 6, 7))
    val setOfLists    = Set(List(1, 2, 3), List(4, 5, 6, 7))
    val result3       = listOfSetInts.parTraverse(x => List(x))
    val result4       = listOfSetInts.parTraverse(x => Id(x.size))
    println(result3)
    println(result4)
  }
}

object ListSpec {
  implicit val pAu: Parallel.Aux[List, Set] = new Parallel[List] {

    override def monad: Monad[List] = ???
    override type F[x] = Set[x]

    override def applicative: Applicative[Set] = new Applicative[Set] {
      override def pure[A](x: A): Set[A] = Set(x)
      override def ap[A, B](ff: Set[A => B])(
          fa: Set[A]
      ): Set[B] = for {
        fn <- ff
        a  <- fa
      } yield fn(a)
    }
    override def sequential: Set ~> List       = new (Set ~> List) {
      override def apply[A](
          fa: Set[A]
      ): List[A] = fa.toList
    }
    override def parallel: List ~> Set         = new (List ~> Set) {
      override def apply[A](
          fa: List[A]
      ): Set[A] = fa.toSet
    }
  }
}
