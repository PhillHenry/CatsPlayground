package uk.co.odinconsultants.fp.cats.structures

class OddEquals(val x: Int, val y: String) {
  override def equals(a: Any): Boolean = {
    if (a.isInstanceOf[OddEquals]) {
      val other = a.asInstanceOf[OddEquals]
      x == other.x
    } else false
  }

  override def hashCode(): Int = {
    x
  }
}

object MySets {

  type Output = String
  type F = String => OddEquals
  type G = OddEquals => Output

  def functorLaw(xs: Iterable[String], f: F, g: G): (Iterable[Output], Iterable[Output]) = {
    (xs.map(x => g(f(x))), xs.map(f).map(g))
  }

  val f: F = x => new OddEquals(x.length, x)
  val g: G = _.y

  def main(args: Array[String]): Unit = {
    checkFunctorLaws(Set("hello", "world"))
    checkFunctorLaws(List("hello", "world"))
  }

  private def checkFunctorLaws(iterable: Iterable[Output]) = {
    val (xs, ys) = functorLaw(iterable, f, g)
    println(s"xs = ${xs}")
    println(s"ys = ${ys}")
    println(s"Are functor laws obeyed? ${xs == ys}")
    println()
  }
}
