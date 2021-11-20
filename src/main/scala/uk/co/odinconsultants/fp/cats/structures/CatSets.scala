package uk.co.odinconsultants.fp.cats.structures

import cats.{Foldable, UnorderedFoldable}
import cats.implicits._

object CatSets {

  def main(args: Array[String]): Unit = {
    unorderedSetFolding

    orderedListFolding
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
