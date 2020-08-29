package scalaz_experiments.free_monad.candy3.pure

import cats.InjectK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.CandyRule.Result

/* Represents state operations */
sealed trait MachineOp[A]

case class InitialState(m: MachineState) extends MachineOp[MachineState]

case class UpdateState(id: Long, f: MachineState => (MachineState, Result)) extends MachineOp[Result]

case class CurrentState(id: Long) extends MachineOp[MachineState]

class Machine[F[_]](implicit I: InjectK[MachineOp, F]) {
  def updateState(id: Long, f: MachineState => (MachineState, Result)): Free[F, Result] = Free.inject[MachineOp, F](UpdateState(id, f))

  def currentState(id: Long): Free[F, MachineState] = Free.inject[MachineOp, F](CurrentState(id))
}

object Machine {
  implicit def machine[F[_]](implicit I: InjectK[MachineOp, F]): Machine[F] = new Machine[F]
}
