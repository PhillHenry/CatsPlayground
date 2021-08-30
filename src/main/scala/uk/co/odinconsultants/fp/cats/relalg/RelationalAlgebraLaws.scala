package uk.co.odinconsultants.fp.cats.relalg

import cats.{Applicative, Traverse}

case class Join[F[_], C, X, Y](xs: F[X], ys: F[Y], conditions: List[C])

trait RelationalAlgebra[F[_], C] {

  def innerJoin[X, Y](join: Join[F, C, X, Y]): F[(X, Y)]
  def outerJoin[X, Y](join: Join[F, C, X, Y]): F[(X, Option[Y])]
  def aggregate[X, Y](xs: F[X], f: X => Y): F[Y]

}
