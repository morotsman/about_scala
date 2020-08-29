package scalaz_experiments.free_monad.candy3

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import cats.~>
import cats.instances.future._
import scalaz_experiments.free_monad.candy3.pure.CandyProgram.CandyMachine
import scalaz_experiments.free_monad.candy3.pure._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}

object CandyServer {

  val route =
    concat(
      post {
        path("candy") {
          onComplete(handler(CreateMachine(MachineState(None, true, 10, 0)))) {
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
        }
      }, put {
        path("candy" / LongNumber / "turn") { (id) => {
          onComplete(handler(Turn(id))) {
            case Success(value) => complete(s"Echo: $value")
            case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
          }
        }
        }
      }
    )

  object AsyncIOInterpreter extends (IOA ~> Future) {
    def apply[A](i: IOA[A]): Future[A] = i match {
      case Receive(request) => for {
        i <- Future.successful(request)
      } yield i
    }
  }

  object AsyncInpureMachineInterpreter extends (MachineOp ~> Future) {
    // really ugly, I know. Will replace with an actor later
    private[this] var id = 0L
    private val machines = scala.collection.mutable.Map[Long, MachineState]()

    def apply[A](fa: MachineOp[A]) = fa match {
      case UpdateState(id, f) => Future.successful {
        val (newMachine, output) = f(machines.get(id).get)
        machines(id) = newMachine
        output
      }
      case CurrentState(id) => Future.successful {
        machines.get(id).get
      }
      case InitialState(machine) => Future {
        val newMachine = machine.copy(id = Some(id))
        machines(id) = newMachine
        id = id + 1
        newMachine
      }
    }
  }

  val interpreter: CandyMachine ~> Future = AsyncInpureMachineInterpreter or AsyncIOInterpreter

  def handler(r: Request): Future[Response] =
    CandyProgram.program(r).foldMap(interpreter)

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
