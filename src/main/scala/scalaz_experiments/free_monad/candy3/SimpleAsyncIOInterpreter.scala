package scalaz_experiments.free_monad.candy3

import cats.~>
import scalaz_experiments.free_monad.candy3.pure.{IOA, Receive}

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object SimpleAsyncIOInterpreter extends (IOA ~> Future) {
  def apply[A](i: IOA[A]): Future[A] = i match {
    case Receive(request) => for {
      i <- Future.successful(request)
    } yield i
  }
}
