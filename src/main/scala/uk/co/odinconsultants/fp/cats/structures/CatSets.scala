package uk.co.odinconsultants.fp.cats.structures

import cats.{Applicative, Apply, Foldable, UnorderedFoldable}
import cats.implicits._
import cats.kernel.CommutativeMonoid

/**
 * Note that this doesn't work for Set because Applicative implies Monad which Set is not.
 */
class UnorderedExamples[T[_]: Applicative: UnorderedFoldable] {
  def xs: T[Int] = Applicative[T].pure(1)
  def folded(implicit ev: CommutativeMonoid[Int]) = {
    val folder = UnorderedFoldable[T]
    folder.unorderedFold(xs)
  }
}

object CatSets {

  def main(args: Array[String]): Unit = {
    unorderedSetFolding
    orderedListFolding
    val unorderedList = new UnorderedExamples[List]
    println(unorderedList.xs)
    println(unorderedList.folded)
  }


  private def orderedListFolding = {
    val listFolder: Foldable[List] = Foldable[List]
    val integers = List(1, 2, 3)
    val strings = List("hello", "world")
    println(listFolder.fold(integers))
    println(listFolder.fold(strings))
  }

  private def unorderedSetFolding = {
    val setFolder: UnorderedFoldable[Set] = UnorderedFoldable[Set]
    val integers = Set(1, 2, 3)
    val strings = Set("hello", "world")
    print(setFolder.unorderedFold(integers))
    //    print(setFolder.unorderedFold(strings)) // doesn't compile because no CommutativeMonoid
    //    for strings
  }
}
