package scalaz_experiments.free_monad.candy2

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import cats.~>
import scalaz_experiments.free_monad.candy2.ToFutureMonad._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}

import CandyMachine._

object CandyServer {

  val route =
    concat (
      post {
        path("candy") {
          onComplete(handler(CreateMachine())) {
            case Success(value) => complete(s"Echo: $value")
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
      }, get {
        path("candy" / LongNumber) { (id) => {
          onComplete(handler(GetMachineState(id))) {
            case Success(value) => complete(s"Echo: $value")
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
        }
      }, put {
        path("candy" / LongNumber / "coin") { (id) => {
          onComplete(handler(InsertCoin(id))) {
            case Success(value) => complete(s"Echo: $value")
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
      }}, put {
        path("candy" / LongNumber / "turn") { (id) => {
          onComplete(handler(Turn(id))) {
            case Success(value) => complete(s"Echo: $value")
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
        }}
  )

  def toFutureCompiler: IOA ~> CandyType =
    new (IOA ~> CandyType) {
      override def apply[A](fa: IOA[A]): CandyType[A] = fa match {
        case Read() =>
          (a: Request[Response]) =>
            Future {
              a.asInstanceOf[A]
            }
      }
    }

  val handler: (Request[Response]) => Future[Response] = CandyProgram.program.foldMap(toFutureCompiler)

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext


    val bindingFuture = Http().newServerAt("localhost", 8090).bind(route)

    println(s"Server online at http://localhost:8090/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }


}
