package uk.co.odinconsultants.fp.cats.relalg

import cats.effect.IO
import munit.CatsEffectSuite
import org.scalatest.WordSpec

class RelationalAlgebraSpec extends CatsEffectSuite {

  case class X(id: Int, value: String)

  case class Y(id: Int, key: Long)

  val Xs = List(X(1, "a"), X(2, "b"), X(4, "d"))
  val Ys = List(Y(1, 101), Y(2, 102), Y(3, 103))

  trait C[X, Y] {
    def apply(x: X, y: Y): Boolean
  }

  val primaryKeysMatch: C[X, Y] = (x: X, y: Y) => x.id == y.id

  val selectX: IO[List[X]] = IO(Xs)
  val selectY: IO[List[Y]] = IO(Ys)

  def innerJoinTest(db: RelationalAlgebra[IO, C[X, Y]]): IO[(List[X], List[Y])] = { // TODO bad signature
    val join: Join[IO, C[X, Y], List[X], List[Y]] = Join(selectX, selectY, List(primaryKeysMatch))
    db.innerJoin(join)
  }

  def innerListJoinTest(db: RelationalAlgebra[List, C[X, Y]]): List[(X, Y)] = {
    val join: Join[List, C[X, Y], X, Y] = Join(Xs, Ys, List(primaryKeysMatch))
    db.innerJoin(join)
  }

  test("nothing in particular") {
    assertIO(IO("test"), "test")
  }

}
