package scalaz_experiments.free_monad.candy3

import cats.~>
import scalaz_experiments.free_monad.candy3.pure.algebra.{CurrentState, InitialState, MachineOp, UpdateState}
import scalaz_experiments.free_monad.candy3.pure.MachineState

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object SimpleAsyncMachineInterpreter extends (MachineOp ~> Future) {
  // really ugly, I know. Will replace with an actor later
  private[this] var id = 0L
  private val machines = scala.collection.mutable.Map[Long, MachineState]()

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(id, f) => Future.successful {
      val newMachine = for {
        om <- Try(machines(id))
        nm <- f(om)
      } yield nm

      newMachine.foreach(nm => machines(id) = nm)
      newMachine
    }
    case CurrentState(id) => Future.successful {
      Try(machines(id))
    }
    case InitialState(machine) => Future {
      val newMachine = machine.copy(id = Some(id))
      machines(id) = newMachine
      id = id + 1
      Try(newMachine)
    }
  }
}
