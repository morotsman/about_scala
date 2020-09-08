package scalaz_experiments.free_monad.candy3.interpreter.actor

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.util.Timeout
import cats.~>
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.interpreter.actor.MachineActor._
import scalaz_experiments.free_monad.candy3.pure.algebra.{CurrentState, InitialState, MachineOp, UpdateState}

import scala.concurrent.ExecutionContextExecutor


object ActorMachineInterpreter {
  def actorMachineInterpreter(ref: ActorRef[MachineRequest])(implicit timeout: Timeout, scheduler: Scheduler, ec: ExecutionContextExecutor): (MachineOp ~> ProgramResult) = new (MachineOp ~> ProgramResult) {
    override def apply[A](fa: MachineOp[A]): ProgramResult[A] = fa match {
      case UpdateState(id, f) => ref
        .ask((ref: ActorRef[UpdateStateReply]) => UpdateStateRequest(id, f, ref))
        .map(r => r.result)
      case CurrentState(id) => ref
        .ask((ref: ActorRef[CurrentStateReply]) => CurrentStateRequest(id, ref))
        .map(r => r.result)
      case InitialState(machine) => ref
        .ask((ref: ActorRef[InitialStateReply]) => InitialStateRequest(machine, ref))
        .map(r => r.result)
    }
  }
}
