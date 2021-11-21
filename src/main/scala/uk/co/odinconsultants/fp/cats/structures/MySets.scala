package uk.co.odinconsultants.fp.cats.structures

class OddEquals(val id: Int, val label: String) {
  override def equals(a: Any): Boolean =
    if (a.isInstanceOf[OddEquals]) {
      val other = a.asInstanceOf[OddEquals]
      id == other.id
    } else false

  override def hashCode(): Int = id
}

object MySets {

  type F = String => OddEquals
  type G = OddEquals => String

  val f: F = x => new OddEquals(x.length, x)
  val g: G = _.label

  def functorLaw(xs: Iterable[String], f: F, g: G): (Iterable[String], Iterable[String]) =
    (xs.map(x => g(f(x))),
      xs.map(f).map(g))


  def main(args: Array[String]): Unit = {
    checkFunctorLaws(Set("hello", "world"))
    checkFunctorLaws(List("hello", "world"))
  }

  private def checkFunctorLaws(iterable: Iterable[String]) = {
    val (xs, ys) = functorLaw(iterable, f, g)
    println(s"xs = ${xs}")
    println(s"ys = ${ys}")
    println(s"Are functor laws obeyed? ${xs == ys}")
    println()
  }
}
