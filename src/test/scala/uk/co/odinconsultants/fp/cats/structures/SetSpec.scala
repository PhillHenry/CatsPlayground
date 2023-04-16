package uk.co.odinconsultants.fp.cats.structures

import cats.Parallel.Aux
import cats.{Applicative, _}
import cats.data._
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import cats.syntax.set._
import munit.{CatsEffectSuite, FunSuite}

import java.util
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet


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
    implicit val commApp = new CommutativeApplicative[Set] {
      override def pure[A](x: A): Set[A]                                         = Set(x)
      override def ap[A, B](ff: Set[A => B])(fa: Set[A]): Set[B] = for {
        a <- fa
        f <- ff
      } yield f(a)
    }
    implicit val trav = new UnorderedTraverse[Set] {
      override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: Set[A])(
          f: A => G[B]
      ): G[Set[B]] = {
        val alo                   = Applicative[G].compose[Set]
        val x: G[Set[(Int, Int)]] = alo.product(Applicative[G].pure(Set(1)), Applicative[G].pure(Set(2)))

        val xs: Set[G[B]] = sa.map(f)
        var g : G[Set[B]] = Applicative[G].pure(Set[B]())

        xs.foreach { gb: G[B] =>
          g = g.map2(gb) { (b1, b2) => b1.+(b2) }
        }

        g // TODO
      }
      override def unorderedFoldMap[A, B: CommutativeMonoid](fa: Set[A])(f: A => B): B =
        CommutativeMonoid[B].combineAll(fa.map(f))
    }
    val xs                                                                                         = Set(1, 2, 3)
    val result: Seq[Set[Int]] = xs.parUnorderedTraverse(x => List(x))
    print(result)
  }
  test("traversing non empty sets") {
    val set = SortedSet(1,2,3)
    val nesOpt: Option[NonEmptySet[Int]] = NonEmptySetImpl.fromSet(set)
    nesOpt.flatMap { (nes: NonEmptySet[Int]) =>
      implicit val pAu: Parallel.Aux[List, NonEmptySet] = new Parallel[List] {

        override def monad: Monad[List] = ???
        override type F[x] = NonEmptySet[x]

        override def applicative: Applicative[NonEmptySet] = ???
        override def sequential: NonEmptySet ~> List = ???
        override def parallel: List ~> NonEmptySet = ???
      }
      implicit val commApp = new CommutativeApplicative[NonEmptySet] {
        override def pure[A](x: A): NonEmptySet[A] = ???
        override def ap[A, B](ff:  NonEmptySet[A => B])(fa:  NonEmptySet[A]): NonEmptySet[B] = ???
      }
      implicit val trav = new UnorderedTraverse[NonEmptySet] {
        override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: NonEmptySet[A])(f: A => G[B]): G[NonEmptySet[B]] = ???
        override def unorderedFoldMap[A, B: CommutativeMonoid](fa:  NonEmptySet[A])(f:  A => B): B = ???
      }

      val result = nes.parUnorderedTraverse(x => List(x))
      println(result)
      Some(result)
    }
  }
}
