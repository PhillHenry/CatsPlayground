package uk.co.odinconsultants.fp.cats.gvolpe

trait Implication[F] {
  def helloWorld: String
}

object Implication {

  def apply[T](implicit instance : Implication[T]) : Implication[T] = instance

  implicit def newImplication[F]: Implication[F] = new Implication[F] {
    override def helloWorld: String = "hello world"
  }
}

object ImplicitsDemoMain {

  /**
   * No matter what type T is, it is given the extension method.
   */
  def needsImplication[T: Implication](t: T): String = {
    Implication[T].helloWorld
  }

  def main(args: Array[String]): Unit = {
    println(needsImplication(1))
    println(needsImplication(Option(1)))
  }

}
