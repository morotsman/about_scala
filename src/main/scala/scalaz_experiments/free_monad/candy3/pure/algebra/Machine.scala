package scalaz_experiments.free_monad.candy3.pure.algebra

import cats.InjectK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.MachineState

/* Represents state operations */
sealed trait MachineOp[A]

case class InitialState(m: MachineState) extends MachineOp[Either[Exception, MachineState]]

case class UpdateState(id: Long, f: MachineState => Either[Exception,MachineState]) extends MachineOp[Either[Exception, MachineState]]

case class CurrentState(id: Long) extends MachineOp[Either[Exception, MachineState]]

class Machine[F[_]](implicit I: InjectK[MachineOp, F]) {
  def updateState(id: Long, f: MachineState => Either[Exception, MachineState]): Free[F, Either[Exception, MachineState]] = Free.inject[MachineOp, F](UpdateState(id, f))

  def currentState(id: Long): Free[F, Either[Exception, MachineState]] = Free.inject[MachineOp, F](CurrentState(id))

  def initialState(m: MachineState): Free[F, Either[Exception, MachineState]] = Free.inject[MachineOp, F](InitialState(m))
}

object Machine {
  implicit def machine[F[_]](implicit I: InjectK[MachineOp, F]): Machine[F] = new Machine[F]
}
