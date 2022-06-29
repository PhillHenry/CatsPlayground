package uk.co.odinconsultants.fp.cats.akka
import cats.effect.IOApp

/** See https://gist.github.com/Swoorup/1ac9b69e0c0f1c0925d1397a94b0a762
  * daniel — Yesterday at 5:14 PM
  * Couple thoughts…
  *
  * Are you trying to use types to enforce that all ask messages receive a response? If you don't do this, then I think you can simplify things out quite a bit. In particular, the choice of ask vs send becomes a call-site thing that the receiver is oblivious to, and the receiver can send responses regardless. Typing the response of an ask message does make sense to me.
  *
  * I think ask can probably have a simpler signature. In particular, rather than making the indirection of the Deferred part of the message (something which every message would then have to encode), you could do it behind the scenes for the user and just allow ask and send to be almost exactly identical (except that send always produces F[Unit] and always returns immediately).
  *
  * As for the handler, I would be thinking in terms of GADTs, particularly if we're typing ask messages differently. I would use phantom types to encode the expected response type of an ask message, and the handler case for that message within the actor would need to produce an F[_] containing that type. send messages would of course produce F[Unit].
  *
  * For the record, I still think actors are a bad abstraction. 🙂 But they're a fun space to play in, if nothing else.
  */
object AkkaFake extends IOApp.Simple {
  import cats.effect._
  import cats.effect.std.Queue
  import cats.effect.syntax.all._
  import fs2.Stream

  case class FSM[F[_], S, I, O](run: (S, I) => F[(S, O)])
  case class Ping[F[_]](who: String, replyTo: Deferred[F, String])

  def makeActor[S, I, O](initialState: S, fsm: FSM[IO, S, I, O]): ResourceIO[I => IO[O]] =
    for {
      ref   <- Ref.of[IO, S](initialState).toResource
      queue <- Queue.unbounded[IO, (I, Deferred[IO, O])].toResource
      _     <- Stream
                 .fromQueueUnterminated(queue)
                 .evalScan(initialState) { case (state, (msg, replyTo)) =>
                   fsm
                     .run(state, msg)
                     .flatMap { case (newState, output) =>
                       replyTo.complete(output).as(newState)
                     }
                 }
                 .compile
                 .drain
                 .background
      ask    = (input: I) =>
                 for {
                   deferred <- Deferred[IO, O]
                   _        <- queue.offer((input, deferred))
                   output   <- deferred.get
                 } yield output
    } yield ask

  val pongBehavior = FSM[IO, Unit, Ping[IO], Unit] { case (_, Ping(who, replyTo)) =>
    replyTo
      .complete(s"Hello ${who} from pong")
      .as(() -> ())
  }

  val run =
    makeActor((), pongBehavior)
      .use(pongActor =>
        (IO.println("Press enter to get response:\n") *> IO.readLine)
          .flatMap(_ =>
            for {
              promise  <- Deferred[IO, String]
              _        <- pongActor(Ping("Sytherax", promise))
              response <- promise.get
              _        <- IO.println(s"Got response: $response")
            } yield ()
          )
          .foreverM
      )
}
