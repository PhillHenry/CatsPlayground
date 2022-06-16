package uk.co.odinconsultants.fp.cats.relalg

import cats.effect.IO
import munit.CatsEffectSuite
import fs2.Stream
import scala.concurrent.duration._
import cats.implicits._

class TestSuiteSpec extends CatsEffectSuite {
  test("testcontrol") {
    import cats.effect.testkit.TestControl

    val stream = Stream.constant("hello").take(2).covary[IO].metered(5.second)

    val expected = Vector(
      0.seconds -> Vector(),
      5.seconds -> Vector("hello"),
      10.seconds -> Vector("hello", "hello")
    )

    // typically this is more complex and has `Ref` and concurrency
    // it should be able to give you the output you assert on without knowing
    // about TestControl
    val harness =
      IO.monotonic.flatMap { t0 =>
        stream
        .scanMap(_.pure[Vector])
        .evalMap(x => IO.monotonic.map(t1 => t1 - t0).tupleRight(x))
        .compile
        .toVector
    }

    TestControl.executeEmbed(harness).assertEquals(expected)
  }
}
