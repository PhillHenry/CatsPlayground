package uk.co.odinconsultants.fp.cats.structures

import cats.Parallel.Aux
import cats._
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
