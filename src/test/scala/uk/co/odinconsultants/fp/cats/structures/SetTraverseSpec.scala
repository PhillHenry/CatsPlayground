package uk.co.odinconsultants.fp.cats.structures
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import cats.{Applicative, _}
import munit.FunSuite

class SetTraverseSpec extends FunSuite {
  test("traversing sets") {
    implicit val pAuxSetList: Parallel.Aux[Set, List] = new Parallel[Set] {
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

    implicit val unorderedTraverseList = new UnorderedTraverse[List] {
      override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](
          sa: List[A]
      )(f: A => G[B]): G[List[B]] = ???
      override def unorderedFoldMap[A, B: CommutativeMonoid](
          fa: List[A]
      )(f: A => B): B = ???
    }

    implicit val trav = new UnorderedTraverse[Set] {
      override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: Set[A])(
          f: A => G[B]
      ): G[Set[B]] = {
        val xs: Set[G[B]] = sa.map(f)
        var g: G[Set[B]]  = Applicative[G].pure(Set.empty[B])

        xs.foreach { gb: G[B] =>
          g = g.map2(gb)((setB, b) => setB + b)
        }

        g // foreach is yucky but works. I note there are similar uses of var in Cats itself
      }
      override def unorderedFoldMap[A, B: CommutativeMonoid](fa: Set[A])(f: A => B): B =
        CommutativeMonoid[B].combineAll(fa.map(f))
    }

    implicit val commApp = new CommutativeApplicative[Set] {
      override def pure[A](x: A): Set[A]                         = Set(x)
      override def ap[A, B](ff: Set[A => B])(fa: Set[A]): Set[B] = for {
        a <- fa
        f <- ff
      } yield f(a)
    }

    val setOfLists = Set(List(1, 2, 3), List(4, 5, 6, 7))
    implicit val aux = ListSpec.pAu
    val results2    = setOfLists.parUnorderedSequence //(x => List(x))
    println(results2)
  }
}
