package scalaz_experiments.free_monad.candy3

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import cats._
import cats.implicits._
import scalaz_experiments.free_monad.candy3.pure.CandyProgram.CandyMachine
import scalaz_experiments.free_monad.candy3.pure.Request._
import scalaz_experiments.free_monad.candy3.pure._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._
import Types._

import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

// DTO
final case class Machines(machines: List[MachineState])

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val machineFormat = jsonFormat4(MachineState)
  implicit val machinesFormat = jsonFormat1(Machines)
}

class CandyServer extends Directives with JsonSupport {
  val route =
    concat(
      post {
        path("candy") {
          onComplete(handler(CreateMachine(MachineState(None, true, 10, 0)))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
      }, get {
        path("candy" / LongNumber) { (id) => {
          onComplete(handler(GetMachineState(id))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
        }
      }, put {
        path("candy" / LongNumber / "coin") { (id) => {
          onComplete(handler(InsertCoin(id))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
        }
      }, put {
        path("candy" / LongNumber / "turn") { (id) => {
          onComplete(handler(Turn(id))) {
            case Success(value) => toResponse(value)
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
        }
      }
    )

  private def toResponse(value: Either[Exception, MachineState]) = value match {
    case Right(v) => complete(v)
    case Left(ex) => ex match {
      case e: IllegalStateException => complete(StatusCodes.BadRequest, e.getMessage)
      case e: NoSuchElementException => complete(StatusCodes.NotFound, e.getMessage)
      case e => complete(StatusCodes.InternalServerError, e.getMessage)
    }
  }

  val interpreter: CandyMachine ~> ProgramResult = SimpleAsyncMachineInterpreter or SimpleAsyncIOInterpreter

  def handler(r: Request): Future[Either[Exception, MachineState]] = {
    val tmp: ProgramResult[MachineState] = CandyProgram.program(r).foldMap(interpreter)
    tmp.value
  }


}

object CandyServer {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext


    val bindingFuture = Http().newServerAt("localhost", 8090).bind(new CandyServer().route)

    println(s"Server online at http://localhost:8090/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
