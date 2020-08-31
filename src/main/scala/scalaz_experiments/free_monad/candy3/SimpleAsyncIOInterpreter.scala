package scalaz_experiments.free_monad.candy3

import cats.~>
import scalaz_experiments.free_monad.candy3.pure.algebra.{IOA, Receive}

import scala.concurrent.Future
import Types.ProgramResult
import cats.data.EitherT


object SimpleAsyncIOInterpreter extends (IOA ~> ProgramResult) {
  def apply[A](i: IOA[A]): ProgramResult[A] = i match {
    case Receive(request) => EitherT(Future.successful(Right(request): Either[Exception, A]))
  }
}
