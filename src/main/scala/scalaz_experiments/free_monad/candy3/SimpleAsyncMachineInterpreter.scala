package scalaz_experiments.free_monad.candy3

import cats.~>
import scalaz_experiments.free_monad.candy3.pure.algebra.{CurrentState, InitialState, MachineOp, UpdateState}
import scalaz_experiments.free_monad.candy3.pure.MachineState

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import Types.ProgramResult
import cats.data.EitherT

object SimpleAsyncMachineInterpreter extends (MachineOp ~> ProgramResult) {
  // This is really ugly, I know. Will replace with an actor later
  private var id = 0L
  private val machines = scala.collection.mutable.Map[Long, MachineState]()

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(id, f) => EitherT(Future.successful {
      val newMachine = for {
        oldM <- machines.get(id).toRight(new NoSuchElementException(s"Could not find a machine with id: $id"))
        newM <- f(oldM)
      } yield newM
      newMachine.foreach(m => machines(id) = m)
      newMachine
    })
    case CurrentState(id) => EitherT(Future.successful {
      machines
        .get(id)
        .toRight(new NoSuchElementException(s"Could not find a machine with id: $id"))
    })
    case InitialState(machine) => EitherT(Future {
      val newMachine = machine.copy(id = Some(id))
      machines(id) = newMachine
      id = id + 1
      Right(newMachine): Either[Exception, MachineState]
    })
  }
}
