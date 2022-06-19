package uk.co.odinconsultants.fp.cats.structures

import cats.effect.IO
import munit.CatsEffectSuite

import java.util.concurrent.atomic.AtomicInteger

class MyFlowSpec extends CatsEffectSuite {

  val PASSED = "passed"

  test("First failure then retry succeeds") {
    val interpreter = new RetryingInterpreter[IO]
    val externalSystem = new AtomicInteger(0)
    val download = IO {
      val count = externalSystem.incrementAndGet()
      print(s"count = $count")
      if (count == 1) throw CommandError else PASSED
    }
    val results = interpreter.handleDownloads(List(download)).unsafeRunSync()
    assertEquals(results, List(PASSED))
    assertEquals(externalSystem.get(), 2)
  }

}
