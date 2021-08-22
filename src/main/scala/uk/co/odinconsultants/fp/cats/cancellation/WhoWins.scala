package uk.co.odinconsultants.fp.cats.cancellation

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.FiniteDuration._
import scala.concurrent.duration._
import cats.implicits._

import java.io.{OutputStream, PrintStream}

object WhoWins extends IOApp {

  val START = System.currentTimeMillis()

  System.setOut(new PrintStream(scala.Console.out) {
    val OLD_STREAM = System.out
    override def println(x: String): Unit = OLD_STREAM.println(message(x))
  })

  def message(x: Any): String = "%5d: %s".format(System.currentTimeMillis() - START, x)

  def javaLog(x: Any): Unit = {
    println(message(x))
  }

  def javaCode(ms: Long = 2000): Unit = {
    javaLog(s"About to Thread.sleep($ms)")
    Thread.sleep(ms)
    javaLog(s"Finished Thread.sleep($ms)")
  }

  def javaSleep(ms: Long): IO[Unit] = logTermination(IO(javaCode(ms)), s"Thread.sleep($ms) cancelled")

  def javaSleepInterruptible(ms: Long): IO[Unit]
    = logTermination(IO.interruptible(many=true)(javaCode(ms)), s"IO.interruptible Thread.sleep($ms) cancelled")

  def logTermination(io: IO[Unit], msg: String): IO[Unit]
    = io.onCancel(IO.println(msg)).onError(t => IO { t.printStackTrace() })

  def print(x: Any): IO[Unit] =
    IO.println(s"$x").onCancel(IO.println(s"$x cancelled")).onError(t => IO {t.printStackTrace()})

  def doSomething(x: Any): IO[Unit] = {
    val io = print(s"Starting '$x'") >>
      IO.sleep(1.seconds).onCancel(IO.println(s"IO.sleep($x) cancelled")) >>
      print(s"Finished '$x'")

    io.onCancel(print(s"'$x' cancelled")) // note the onCancel needs to be on the last IO in the chain
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val something: IO[Any] = for {
      first <- doSomething("IO.start.cancel").start
      _ <- first.cancel
      javaSleepFibre <- javaSleep(1999).start
      javaInterruptibleFibre <- javaSleepInterruptible(2001).start
      interruptible <- IO.interruptible(many=true)(javaSleep(2000)).start // this never seems to start
      _ <- doSomething("plain IO.sleep")
      _ <- javaSleepFibre.cancel // can't seem to cancel this
      _ <- interruptible.cancel
      _ <- javaInterruptibleFibre.cancel
    } yield {
      javaLog("Terminating...")
    }
    something.as(ExitCode.Success)
  }
}
