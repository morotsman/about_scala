package scalaz_experiments.free_monad.candy3.pure

import cats.InjectK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.CandyRule.Result

/* Represents state operations */
sealed trait MachineOp[A]

case class UpdateState(f: MachineState => (MachineState, Result)) extends MachineOp[Result]

case class CurrentState() extends MachineOp[MachineState]

class Machine[F[_]](implicit I: InjectK[MachineOp, F]) {
  def updateState(f: MachineState => (MachineState, Result)): Free[F, Result] = Free.inject[MachineOp, F](UpdateState(f))

  def currentState: Free[F, MachineState] = Free.inject[MachineOp, F](CurrentState())
}

object Machine {
  implicit def machine[F[_]](implicit I: InjectK[MachineOp, F]): Machine[F] = new Machine[F]
}
