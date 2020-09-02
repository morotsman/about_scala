package scalaz_experiments.free_monad.candy3

import cats.data.EitherT

import scala.concurrent.Future

object Types {

  type ProgramResult[A] = Future[A]// Future[Either[Exception, A]]

}
