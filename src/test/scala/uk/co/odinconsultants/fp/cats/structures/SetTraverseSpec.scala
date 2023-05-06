package uk.co.odinconsultants.fp.cats.structures
import cats.syntax.all._
import cats.{Applicative, _}
import munit.FunSuite

class SetTraverseSpec extends FunSuite {
  test("traversing sets") {
    implicit val pAu: Parallel.Aux[Set, List] = new Parallel[Set] {
      override def monad: Monad[Set] = ???
      override type F[x] = List[x]

      override def applicative: Applicative[List] = new Applicative[List] {
        override def pure[A](x: A): List[A] = List(x)
        override def ap[A, B](ff: List[A => B])(
            fa: List[A]
        ): List[B] = for {
          fn <- ff
          a  <- fa
        } yield fn(a)
      }

      override def sequential: List ~> Set = new (List ~> Set) {
        override def apply[A](
            fa: List[A]
        ): Set[A] = fa.toSet
      }
      override def parallel: Set ~> List   = new (Set ~> List) {
        override def apply[A](
            fa: Set[A]
        ): List[A] = fa.toList
      }
    }

    implicit val commAppList = new CommutativeApplicative[List] {
      override def pure[A](x: A): List[A]                           = List(x)
      override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = for {
        a <- fa
        f <- ff
      } yield f(a)
    }

    val setOfLists = Set(List(1, 2, 3), List(4, 5, 6, 7))
    val results    = setOfLists.parUnorderedTraverse(_.toSet)
    println(results)
  }
}
