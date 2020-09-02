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
import scalaz_experiments.free_monad.candy3.pure.CandyProgram
import scalaz_experiments.free_monad.candy3.pure.CandyProgram.CandyMachine
import scalaz_experiments.free_monad.candy3.server.CandyServer.system

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

object Cli {

  implicit val system: ActorSystem[Setup] =
    ActorSystem(SystemInitializer.setup, "candy")

  implicit val timeout: Timeout = 3.seconds
  implicit val ec: ExecutionContextExecutor = system.executionContext

  def main(args: Array[String]): Unit = {

    val asyncProgram = for {
      i <- setupActorSystem().map(createInterpreter)
      _ <- runProgram(i)
    } yield ()

    asyncProgram.onComplete(_ => system.terminate())

  }

  def runProgram(interpreter: CandyMachine ~> ProgramResult): ProgramResult[Unit] = {
    CandyProgram.cliProgram.foldMap(interpreter)
  }

  def setupActorSystem(): Future[SystemContext] = system.ask((ref: ActorRef[SystemContext]) => Setup(ref))

  def createInterpreter(context: SystemContext) =
    ActorMachineInterpreter.actorMachineInterpreter(context.machineActor) or PromptAsyncIOInterpreter

}
