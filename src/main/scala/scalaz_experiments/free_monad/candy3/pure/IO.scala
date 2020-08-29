package scalaz_experiments.free_monad.candy3.pure

import cats.InjectK
import cats.free.Free

/* Handles user interaction */
sealed trait IOA[A]

case class Receive[A](response: A) extends IOA[A]

class IO[F[_]](implicit I: InjectK[IOA, F]) {
  def receive[A](request: A): Free[F, A] = Free.inject[IOA, F](Receive(request))
}

object IO {
  implicit def io[F[_]](implicit I: InjectK[IOA, F]): IO[F] = new IO[F]
}
