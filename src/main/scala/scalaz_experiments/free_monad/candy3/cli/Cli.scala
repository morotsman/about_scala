package scalaz_experiments.free_monad.candy3.cli

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import cats._
import cats.data.EitherT
import cats.implicits._
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.interpreter.{ActorMachineInterpreter, PromptAsyncIOInterpreter, SystemInitializer}
import scalaz_experiments.free_monad.candy3.interpreter.SystemInitializer.{Setup, SystemContext}
import scalaz_experiments.free_monad.candy3.pure.{CandyProgram, MachineState}
import scalaz_experiments.free_monad.candy3.pure.CandyProgram.{CandyMachine, Program}
import scalaz_experiments.free_monad.candy3.pure.Request.CreateMachine
import scalaz_experiments.free_monad.candy3.server.CandyServer.system

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

object Cli {

  private implicit val system: ActorSystem[Setup] =
    ActorSystem(SystemInitializer.setup, "candy")

  private implicit val timeout: Timeout = 3.seconds
  private implicit val ec: ExecutionContextExecutor = system.executionContext

  def main(args: Array[String]): Unit = {
    val initialMachine = MachineState(locked = true, candies = 20, coins = 0)

    val asyncProgram = for {
      i <- setupActorSystem().map(createInterpreter)
      _ <- runProgram(i, initialMachine)
    } yield ()

    asyncProgram.onComplete(_ => system.terminate())

  }

  def runProgram[A](interpreter: CandyMachine ~> ProgramResult, initialMachine: MachineState) = {
    val program = for {
      _ <- createMachine(initialMachine)
      _ <- CandyProgram.cliProgram
    } yield ()

    program.value.foldMap(interpreter)
  }

  private def createMachine(initialMachine: MachineState): Program[MachineState] =
    CandyProgram.machineProgram(CreateMachine(initialMachine))

  private def setupActorSystem(): Future[SystemContext] = system.ask((ref: ActorRef[SystemContext]) => Setup(ref))

  private def createInterpreter(context: SystemContext) =
    ActorMachineInterpreter.actorMachineInterpreter(context.machineActor) or PromptAsyncIOInterpreter

}
