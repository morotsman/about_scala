package scalaz_experiments.free_monad.candy3.pure.algebra

import cats.InjectK
import cats.free.Free

/* Handles user interaction */
sealed trait IOA[A]

case class Write[A](message: A) extends IOA[Unit]

case class Read[A]() extends IOA[A]

class IO[F[_]](implicit I: InjectK[IOA, F]) {
  def write[A](message: A): Free[F, Unit] = Free.inject[IOA, F](Write(message))

  def read[A](): Free[F, A] = Free.inject[IOA, F](Read())
}

object IO {
  implicit def io[F[_]](implicit I: InjectK[IOA, F]): IO[F] = new IO[F]
}
