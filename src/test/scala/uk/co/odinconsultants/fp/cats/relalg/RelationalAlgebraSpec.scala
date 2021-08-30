package uk.co.odinconsultants.fp.cats.relalg

import cats.effect.IO
import munit.CatsEffectSuite
import org.scalatest.WordSpec

class RelationalAlgebraSpec extends CatsEffectSuite {

  case class X(id: Int, value: String)

  case class Y(id: Int, key: Long)

  val Xs = List(X(1, "a"), X(2, "b"), X(4, "d"))
  val Ys = List(Y(1, 101), Y(2, 102), Y(3, 103))

  type C = (X, Y) => Boolean
//  case class C(f: C)

  val primaryKeysMatch: C = (x, y) => x.id == y.id

  val selectX = IO(Xs)
  val selectY = IO(Ys)

  def innerJoinTest(db: RelationalAlgebra[IO, C]): IO[(List[X], List[Y])] = {
    val join = Join(selectX, selectY, List(primaryKeysMatch))
    db.innerJoin(join)
  }

  def innerListJoinTest(db: RelationalAlgebra[List, C]): List[(X, Y)] = {
    val join: Join[List, C, X, Y] = Join(Xs, Ys, List(primaryKeysMatch))
    db.innerJoin(join)
  }

  test("nothing in particular") {
    assertIO(IO("test"), "test")
  }

}
