package uk.co.odinconsultants.fp.cats.structures

import cats.implicits._
import cats.kernel.{CommutativeMonoid, Monoid}
import cats.{Foldable, UnorderedFoldable}

class UnorderedExamples[T[_]: UnorderedFoldable, A: CommutativeMonoid](xs: T[A]) {
  def folded: A = {
    val folder = UnorderedFoldable[T]
    folder.unorderedFold(xs)
  }
  override def toString: String = s"${xs} unordered foldable -> ${folded}"
}

class OrderedExamples[T[A]: Foldable, A: Monoid](xs: T[A]) {
  def folded: A = {
    val folder = Foldable[T]
    folder.fold(xs)
  }

  override def toString: String = s"${xs} ordered foldable -> ${folded}"
}

object CatSets {

  def main(args: Array[String]): Unit = {
    unorderedSetFolding
    orderedListFolding
    println(new UnorderedExamples(List(1, 2, 3)))
    println(new UnorderedExamples(Set(1, 2, 3)))
    println(new OrderedExamples(List(1, 2, 3)))
//    println(new OrderedExamples(Set(1, 2, 3))) // "No Foldable[Set]"
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
