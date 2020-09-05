package scalaz_experiments.free_monad.candy3.interpreter

import cats.data.State
import cats.~>
import scalaz_experiments.free_monad.candy3.Types.{CandyState, InternalState}
import scalaz_experiments.free_monad.candy3.pure.MachineState
import scalaz_experiments.free_monad.candy3.pure.algebra.{CurrentState, InitialState, MachineOp, UpdateState}

object StateMachineInterpreter {

  def stateMachineInterpreter(): (MachineOp ~> CandyState) = new (MachineOp ~> CandyState) {
    override def apply[A](fa: MachineOp[A]): CandyState[A] = fa match {
      case UpdateState(id, f) => State { s =>
        val updatedMachine = for {
          machine <- s.machine
          newMachine <- f(machine)
        } yield newMachine
        (updateMachine(updatedMachine, s), updatedMachine)
      }
      case CurrentState(id) =>  for {
        s <- State.get
      } yield s.machine
      case InitialState(machine) => State { s =>
        (s, s.machine)
      }
    }
  }

  def updateMachine[A](m: Either[Throwable, MachineState], s: InternalState[Any]): InternalState[Any] =
    s.copy(machine = m)
}
