package scalaz_experiments.free_monad.candy3

import cats._
import cats.implicits._
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.pure.MachineState
import scalaz_experiments.free_monad.candy3.pure.algebra.{CurrentState, InitialState, MachineOp, UpdateState}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import cats.{Id, ~>}
import cats.instances.future._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import cats.data.EitherT
import scalaz_experiments.free_monad.candy3.MachineActor.{CurrentStateReply, InitialStateReply, Machine, UpdateStateReply}
import scalaz_experiments.free_monad.candy3.SimpleAsyncMachineInterpreter.machines


object MachineActor {

  sealed trait Machine

  case class UpdateState(id: Long, f: MachineState => Either[Exception, MachineState], replyTo: ActorRef[UpdateStateReply]) extends Machine

  case class CurrentState(id: Long, replyTo: ActorRef[CurrentStateReply]) extends Machine

  case class InitialState(m: MachineState, replyTo: ActorRef[InitialStateReply]) extends Machine

  sealed trait MachineReply

  case class UpdateStateReply(result: Either[Exception, MachineState]) extends MachineReply

  case class CurrentStateReply(result: Either[Exception, MachineState]) extends MachineReply

  case class InitialStateReply(result: Either[Exception, MachineState]) extends MachineReply

  def apply(): Behavior[Machine] = {
    behave(0L, Map[Long, MachineState]())
  }

  def behave(currentId: Long, machines: Map[Long, MachineState]): Behavior[Machine] = Behaviors.receive { (context, message) =>
    message match {
      case UpdateState(id, f, replyTo) => {
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
      case CurrentState(id, replyTo) => {
        replyTo ! CurrentStateReply(machines.get(id).toRight(new NoSuchElementException(s"Could not find a machine with id: $id")))
        Behaviors.same
      }
      case InitialState(m, replyTo) => {
        val newMachine = m.copy(id = Some(currentId))
        replyTo ! InitialStateReply(Right(newMachine))
        behave(currentId + 1, machines + (currentId -> newMachine))
      }
    }
  }
}

object SystemInitializer {

  case class Setup(replyTo: ActorRef[SystemContext])

  case class SystemContext(machineActor: ActorRef[Machine])

  def setup(): Behavior[Setup] =
    Behaviors.setup { context =>
      val ref: ActorRef[Machine] = context.spawn(MachineActor(), "machine")

      Behaviors.receiveMessage { message =>
        message.replyTo ! SystemContext(ref)
        Behaviors.same
      }
    }
}

object ActorMachineInterpreter {
  def actorMachineInterpreter(ref: ActorRef[Machine])(implicit timeout: Timeout, scheduler: Scheduler): (MachineOp ~> ProgramResult) = new (MachineOp ~> ProgramResult) {
    override def apply[A](fa: MachineOp[A]): ProgramResult[A] = fa match {
      case UpdateState(id, f) =>
        val result = ref
          .ask((ref: ActorRef[UpdateStateReply]) => MachineActor.UpdateState(id, f, ref))
          .map(r => r.result)
        EitherT(result)
      case CurrentState(id) =>
        val result = ref
          .ask((ref: ActorRef[CurrentStateReply]) => MachineActor.CurrentState(id, ref))
          .map(r => r.result)
        EitherT(result)
      case InitialState(machine) =>
        val result = ref
          .ask((ref: ActorRef[InitialStateReply]) => MachineActor.InitialState(machine, ref))
          .map(r => r.result)
        EitherT(result)
    }
  }
}
