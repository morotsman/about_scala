package scalaz_experiments.free_monad.candy3

import cats.data.{EitherT, State}
import scalaz_experiments.free_monad.candy3.pure.MachineState

import scala.concurrent.Future

object Types {

  type ProgramResult[A] = Future[A]// Future[Either[Exception, A]]

  case class InternalState[A](in: List[Either[Throwable,A]], out: List[Either[Throwable,A]], machine: Either[Throwable, MachineState])

  type CandyState[A] = State[InternalState[Any], A]

}
