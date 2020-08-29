package scalaz_experiments.free_monad.candy3

import cats.~>
import scalaz_experiments.free_monad.candy3.pure.{CurrentState, InitialState, MachineOp, MachineState, UpdateState}

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

object SimpleAsyncMachineInterpreter extends (MachineOp ~> Future) {
  // really ugly, I know. Will replace with an actor later
  private[this] var id = 0L
  private val machines = scala.collection.mutable.Map[Long, MachineState]()

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(id, f) => Future.successful {
      val (newMachine, output) = f(machines.get(id).get)
      machines(id) = newMachine
      output
    }
    case CurrentState(id) => Future.successful {
      machines.get(id).get
    }
    case InitialState(machine) => Future {
      val newMachine = machine.copy(id = Some(id))
      machines(id) = newMachine
      id = id + 1
      newMachine
    }
  }
}
