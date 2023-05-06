package uk.co.odinconsultants.fp.cats.structures

import cats.data._
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import cats.{Applicative, _}
import munit.FunSuite

import scala.collection.immutable.SortedSet

class SetSpec extends FunSuite {
  test("traversing sets") {
    implicit val pAu: Parallel.Aux[List, Set] = new Parallel[List] {

      override def monad: Monad[List] = ???
      override type F[x] = Set[x]

      override def applicative: Applicative[Set] = ???
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
    implicit val commApp        = new CommutativeApplicative[Set] {
      override def pure[A](x: A): Set[A]                         = Set(x)
      override def ap[A, B](ff: Set[A => B])(fa: Set[A]): Set[B] = for {
        a <- fa
        f <- ff
      } yield f(a)
    }

    val setOfInts               = Set(1, 2, 3)
    val setOfListOfInts         = Set(List(1, 2, 3), List(4, 5, 6))
    val result: List[Set[Int]]  = setOfInts.parUnorderedTraverse(x => List(x))
    val result2: List[Set[Int]] = setOfListOfInts.parUnorderedSequence
    println(result)
    println(result2)
  }
  test("traversing non empty sets") {
    val set                              = SortedSet(1, 2, 3)
    val nesOpt: Option[NonEmptySet[Int]] = NonEmptySetImpl.fromSet(set)
    nesOpt.flatMap { (nes: NonEmptySet[Int]) =>
      implicit val pAu: Parallel.Aux[List, NonEmptySet] = new Parallel[List] {

        override def monad: Monad[List] = ???
        override type F[x] = NonEmptySet[x]

        override def applicative: Applicative[NonEmptySet] = ???
        override def sequential: NonEmptySet ~> List       = ???
        override def parallel: List ~> NonEmptySet         = ???
      }
      implicit val commApp = new CommutativeApplicative[NonEmptySet] {
        override def pure[A](x: A): NonEmptySet[A]                                         = ???
        override def ap[A, B](ff: NonEmptySet[A => B])(fa: NonEmptySet[A]): NonEmptySet[B] = ???
      }
      implicit val trav    = new UnorderedTraverse[NonEmptySet] {
        override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: NonEmptySet[A])(
            f: A => G[B]
        ): G[NonEmptySet[B]] = ???
        override def unorderedFoldMap[A, B: CommutativeMonoid](fa: NonEmptySet[A])(f: A => B): B =
          ???
      }

      val result = nes.parUnorderedTraverse(x => List(x))
      println(result)
      Some(result)
    }
  }
}
