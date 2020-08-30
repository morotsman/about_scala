package scalaz_experiments.free_monad.candy3.pure.algebra

import cats.InjectK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.MachineState

import scala.util.Try

/* Represents state operations */
sealed trait MachineOp[A]

case class InitialState(m: MachineState) extends MachineOp[Try[MachineState]]

case class UpdateState(id: Long, f: MachineState => Try[MachineState]) extends MachineOp[Try[MachineState]]

case class CurrentState(id: Long) extends MachineOp[Try[MachineState]]

class Machine[F[_]](implicit I: InjectK[MachineOp, F]) {
  def updateState(id: Long, f: MachineState => Try[MachineState]): Free[F, Try[MachineState]] = Free.inject[MachineOp, F](UpdateState(id, f))

  def currentState(id: Long): Free[F, Try[MachineState]] = Free.inject[MachineOp, F](CurrentState(id))

  def initialState(m: MachineState): Free[F, Try[MachineState]] = Free.inject[MachineOp, F](InitialState(m))
}

object Machine {
  implicit def machine[F[_]](implicit I: InjectK[MachineOp, F]): Machine[F] = new Machine[F]
}
