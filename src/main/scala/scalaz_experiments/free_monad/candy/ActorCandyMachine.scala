package scalaz_experiments.free_monad.candy

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
import scalaz_experiments.free_monad.candy.MachineActor.{CurrentStateReply, Machine, MachineReply, UpdateStateReply}
import scalaz_experiments.free_monad.candy.pure.CandyRule.Result
import scalaz_experiments.free_monad.candy.pure.{CurrentState, IOA, MachineOp, MachineState, Read, UpdateState, Write}

import scala.annotation.tailrec

object MachineActor {

  sealed trait Machine

  case class UpdateState(f: MachineState => (MachineState, Result), replyTo: ActorRef[UpdateStateReply]) extends Machine

  case class CurrentState(replyTo: ActorRef[CurrentStateReply]) extends Machine

  sealed trait MachineReply

  case class UpdateStateReply(result: Result) extends MachineReply

  case class CurrentStateReply(machine: MachineState) extends MachineReply

  def apply(m: MachineState): Behavior[Machine] = {
    behave(m)
  }

  def behave(m: MachineState): Behavior[Machine] = Behaviors.receive { (context, message) =>
    message match {
      case UpdateState(f, replyTo) => {
        val (newMachine, result) = f(m)
        replyTo ! UpdateStateReply(result)
        behave(newMachine)
      }
      case CurrentState(replyTo) => {
        replyTo ! CurrentStateReply(m)
        Behaviors.same
      }
    }
  }
}

object ActorIOInterpreter extends (IOA ~> Future) {
  def apply[A](i: IOA[A]) = i match {
    case Read() => for {
      _ <- Future.successful(System.out.print("> "))
      i <- Future.successful(StdIn.readLine())
    } yield i
    case Write(msg) =>
      Future.successful(System.out.println(msg))
  }
}

object ActorMachineInterpreter {
  def actorMachineInterpreter(ref: ActorRef[Machine])(implicit timeout: Timeout, scheduler: Scheduler): (MachineOp ~> Future) = new (MachineOp ~> Future) {
    override def apply[A](fa: MachineOp[A]): Future[A] = fa match {
      case UpdateState(f) => ref
        .ask((ref: ActorRef[UpdateStateReply]) => MachineActor.UpdateState(f, ref))
        .map(r => r.result)
      case CurrentState() => ref
        .ask((ref: ActorRef[CurrentStateReply]) => MachineActor.CurrentState(ref))
        .map(r => r.machine)
    }
  }
}

object SystemInitializer {

  case class Setup(replyTo: ActorRef[SystemContext])

  case class SystemContext(machineActor: ActorRef[Machine])

  def setup(m: MachineState): Behavior[Setup] =
    Behaviors.setup { context =>
      val ref: ActorRef[Machine] = context.spawn(MachineActor(m), "machine")

      Behaviors.receiveMessage { message =>
        message.replyTo ! SystemContext(ref)
        Behaviors.same
      }
    }
}

object ActorCandyMachine {

  import scalaz_experiments.free_monad.candy.pure.Machine._, scalaz_experiments.free_monad.candy.pure.IO._, scalaz_experiments.free_monad.candy.pure.CandyMachine._, SystemInitializer._, ActorMachineInterpreter._

  println("ASync")
  val initialMachine = new MachineState(true, 50, 0)

  val system: ActorSystem[Setup] =
    ActorSystem(SystemInitializer.setup(initialMachine), "candy")

  implicit val timeout: Timeout = 3.seconds
  implicit val ec: ExecutionContextExecutor = system.executionContext
  implicit val scheduler: Scheduler = system.scheduler

  def main(args: Array[String]): Unit = {
    val result: Future[Unit] = for {
      r <- setupSystem()
      _ <- runProgram(r)
    } yield ()

    result.onComplete(_ => system.terminate())
  }

  def setupSystem(): Future[SystemContext] = system.ask((ref: ActorRef[SystemContext]) => Setup(ref))

  def runProgram(r: SystemContext): Future[Unit] = {
    val interpreter: CandyMachine ~> Future = actorMachineInterpreter(r.machineActor) or ActorIOInterpreter
    program.foldMap(interpreter)
  }
}
