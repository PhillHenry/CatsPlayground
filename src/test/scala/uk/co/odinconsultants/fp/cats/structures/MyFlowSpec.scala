package uk.co.odinconsultants.fp.cats.structures

import cats.effect.IO
import munit.CatsEffectSuite

import java.util.concurrent.atomic.AtomicInteger

class MyFlowSpec extends CatsEffectSuite {

  val PASSED = "passed"

  test("First failure then retry succeeds") {
    val interpreter    = new RetryingInterpreter[IO]
    val externalSystem = new AtomicInteger(0)
    val download       = mockCallToExternalSystem(externalSystem, _ == 1)
    for {
      _ <- interpreter.handleDownloads(List(download))
    } yield assertEquals(externalSystem.get(), 2)
  }

  private def mockCallToExternalSystem(
      externalSystem: AtomicInteger,
      when: Int => Boolean,
  ): IO[String] = IO {
    val count = externalSystem.incrementAndGet()
    print(s"count = $count")
    if (when(count)) throw CommandError else PASSED
  }
}
