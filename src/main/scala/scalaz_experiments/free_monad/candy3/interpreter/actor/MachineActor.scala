package scalaz_experiments.free_monad.candy3.interpreter.actor

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.util.Timeout
import cats.~>
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.interpreter.actor.MachineActor._
import scalaz_experiments.free_monad.candy3.pure.MachineState
import scalaz_experiments.free_monad.candy3.pure.algebra.{CurrentState, InitialState, MachineOp, UpdateState}

import scala.concurrent.ExecutionContextExecutor

object MachineActor {

  sealed trait MachineRequest

  case class UpdateStateRequest(id: Long, f: MachineState => Either[Throwable, MachineState], replyTo: ActorRef[UpdateStateReply]) extends MachineRequest

  case class CurrentStateRequest(id: Long, replyTo: ActorRef[CurrentStateReply]) extends MachineRequest

  case class InitialStateRequest(m: MachineState, replyTo: ActorRef[InitialStateReply]) extends MachineRequest

  sealed trait MachineReply

  case class UpdateStateReply(result: Either[Throwable, MachineState]) extends MachineReply

  case class CurrentStateReply(result: Either[Throwable, MachineState]) extends MachineReply

  case class InitialStateReply(result: Either[Throwable, MachineState]) extends MachineReply

  def apply(): Behavior[MachineRequest] = {
    behave(0L, Map[Long, MachineState]())
  }

  def behave(currentId: Long, machines: Map[Long, MachineState]): Behavior[MachineRequest] = Behaviors.receive { (context, message) =>
    message match {
      case UpdateStateRequest(id, f, replyTo) => {
        val newMachine = for {
          oldM <- machines.get(id).toRight(new NoSuchElementException(s"Could not find a machine with id: $id"))
          newM <- f(oldM)
        } yield newM

        replyTo ! UpdateStateReply(newMachine)
        newMachine match {
          case Right(m) =>
            behave(currentId, machines + (id -> m))
          case Left(_) =>
            Behaviors.same
        }
      }
      case CurrentStateRequest(id, replyTo) => {
        replyTo ! CurrentStateReply(machines.get(id).toRight(new NoSuchElementException(s"Could not find a machine with id: $id")))
        Behaviors.same
      }
      case InitialStateRequest(m, replyTo) => {
        val newMachine = m.copy(id = Some(currentId))
        replyTo ! InitialStateReply(Right(newMachine))
        behave(currentId + 1, machines + (currentId -> newMachine))
      }
    }
  }
}
