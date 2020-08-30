package scalaz_experiments.free_monad.candy3

import cats.data.EitherT

import scala.concurrent.Future

object Types {

  type ProgramResult[A] = EitherT[Future, Exception,A]// Future[Either[Exception, A]]

}
