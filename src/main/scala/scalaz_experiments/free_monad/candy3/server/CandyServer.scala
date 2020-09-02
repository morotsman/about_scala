package scalaz_experiments.free_monad.candy3.server

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives
import akka.util.Timeout
import cats._
import cats.implicits._
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.interpreter.{ActorMachineInterpreter, NoopAsyncIOInterpreter, SystemInitializer}
import scalaz_experiments.free_monad.candy3.pure.CandyProgram.CandyMachine
import scalaz_experiments.free_monad.candy3.pure.Request._
import scalaz_experiments.free_monad.candy3.pure._
import scalaz_experiments.free_monad.candy3.interpreter.SystemInitializer.{Setup, SystemContext}
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.{Failure, Success}

// DTO
final case class Machines(machines: List[MachineState])

final case class Error(message: String)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val machineFormat = jsonFormat4(MachineState)
  implicit val machinesFormat = jsonFormat1(Machines)
  implicit val errorFormat = jsonFormat1(Error)
}

class CandyServer(val interpreter: CandyMachine ~> ProgramResult) extends Directives with JsonSupport {
  val route =
    concat(
      post {
        path("candy") {
          entity(as[MachineState]) { machine =>
            onComplete(handler(CreateMachine(machine))) {
              case Success(value) => toResponse(value)
              case Failure(ex) => internalServerError(ex)
            }
          }
        }
      }, get {
        path("candy" / LongNumber) { (id) => {
          onComplete(handler(GetMachineState(id))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => internalServerError(ex)
          }
        }
        }
      }, put {
        path("candy" / LongNumber / "coin") { (id) => {
          onComplete(handler(InsertCoin(id))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => internalServerError(ex)
          }
        }
        }
      }, put {
        path("candy" / LongNumber / "turn") { (id) => {
          onComplete(handler(Turn(id))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => internalServerError(ex)
          }
        }
        }
      }
    )

  private def internalServerError(ex: Throwable) =
    complete(StatusCodes.InternalServerError, Error(ex.getMessage))

  private def toResponse(value: Either[Exception, MachineState]) = value match {
    case Right(v) => complete(v)
    case Left(ex) => mapError(ex)
  }

  private def mapError(ex: Exception) = {
    ex match {
      case e: IllegalStateException => complete(StatusCodes.BadRequest, Error(e.getMessage))
      case e: NoSuchElementException => complete(StatusCodes.NotFound, Error(e.getMessage))
      case e => complete(StatusCodes.InternalServerError, Error(e.getMessage))
    }
  }

  private def handler(r: Request): Future[Either[Exception, MachineState]] =
    CandyProgram.machineProgram(r).foldMap(interpreter).value

}

object CandyServer {

  implicit val system: ActorSystem[Setup] =
    ActorSystem(SystemInitializer.setup, "candy")

  implicit val timeout: Timeout = 3.seconds
  implicit val ec: ExecutionContextExecutor = system.executionContext

  def main(args: Array[String]): Unit = {
    def setupActorSystem(): Future[SystemContext] = system.ask((ref: ActorRef[SystemContext]) => Setup(ref))

    def createInterpreter(context: SystemContext) =
      ActorMachineInterpreter.actorMachineInterpreter(context.machineActor) or NoopAsyncIOInterpreter

    def bindingFuture(interpreter: CandyMachine ~> ProgramResult) = Http().newServerAt("localhost", 8090).bind(new CandyServer(interpreter).route)

    val server = for {
      interpreter <- setupActorSystem().map(createInterpreter)
      bf <- bindingFuture(interpreter)
    } yield bf

    println(s"Server online at http://localhost:8090/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    server
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
