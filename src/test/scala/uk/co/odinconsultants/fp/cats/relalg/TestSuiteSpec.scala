package uk.co.odinconsultants.fp.cats.relalg

import cats.effect.IO
import munit.CatsEffectSuite
import fs2.Stream
import scala.concurrent.duration._
import cats.implicits._

/**
SystemFw — 02/27/2022
note that the details of harness don't really matter
you could have put each Stream element in Ref and then write a for or whatever
the idea behind it is quite simple though
write a test as if TestControl didn't exist
then, make it deterministic by running it with TestControl.executeEmbed
I tend to write them in a style where I write the program under test, the expected result, the
harnessing code (sometimes this is mixed with the program under test), and then the assertion
in this case, how would you test your code if you didn't have TestControl?
what I did was accumulate each intermediate result in the stream, and then tagging it with the time
at which it was produced
the program works without TestControl, but it's going to be slow (as in, wait 10 seconds), and fail
because it's not going to produce an output after exactly 5 seconds due to clock imprecision
then, you just run it with TestControl, and you're done
bonus point: you don't really need to learn anything specific to TestControl except that one function
 */
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
      IO.monotonic.flatMap { t0 =>  // for monotonic, see https://typelevel.org/cats-effect/docs/2.x/datatypes/clock
        stream
          .scanMap(_.pure[Vector])
          .evalMap(x => IO.monotonic.map(t1 => t1 - t0).tupleRight(x))
          .compile
          .toVector
    }
    /*
    PH: "System.nanoTime() is monotonic, if and only if the underlying platform supports CLOCK_MONOTONIC "
    https://stackoverflow.com/questions/2978598/will-system-currenttimemillis-always-return-a-value-previous-calls
     */

    TestControl.executeEmbed(harness).assertEquals(expected)
  }
}
