package scalaz_experiments.free_monad.candy3.server

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.Timeout
import cats.~>
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.interpreter.SystemInitializer.{Setup, SystemContext}
import scalaz_experiments.free_monad.candy3.interpreter.{ActorMachineInterpreter, NoopAsyncIOInterpreter, StateIOInterpreter, StateMachineInterpreter, SystemInitializer}
import scalaz_experiments.free_monad.candy3.pure.MachineState
import spray.json.DefaultJsonProtocol

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val machineFormat = jsonFormat4(MachineState)
  implicit val errorFormat = jsonFormat1(Error)
}

class CandyServerTest extends AnyFlatSpec with Matchers with ScalatestRouteTest with JsonSupport {

  val locked = true
  val unlocked = false

  def unitUnderTest = {
    implicit val mySystem: ActorSystem[Setup] =
      ActorSystem(SystemInitializer.setup(), "candy")
    implicit val timeout: Timeout = 3.seconds

    def setupActorSystem(): Future[SystemContext] = mySystem.ask((ref: ActorRef[SystemContext]) => Setup(ref))

    def createInterpreter(context: SystemContext) =
      ActorMachineInterpreter.actorMachineInterpreter(context.machineActor)

    val futureInterpreter = setupActorSystem().map(createInterpreter)

    val interpreter = Await.result(futureInterpreter, 3.seconds)

    (new CandyServer(interpreter), mySystem)
  }

  "A User" should "be able to create a new machine" in {
    val machine = MachineState(None, locked, 20, 0)
    val (server, mySystem) = unitUnderTest

    Post("/candy", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 20, 0)
    }
    mySystem.terminate()
  }

  "A User" should "be able to create two new machines" in {
    val machine = MachineState(None, locked, 20, 0)
    val (server, mySystem) = unitUnderTest

    Post("/candy", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 20, 0)
    }

    Post("/candy", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(1L), locked, 20, 0)
    }
    mySystem.terminate()
  }

  "A User" should "be able to check the current state of the machine" in {
    val machine = MachineState(None, locked, 20, 0)
    val (server, mySystem) = unitUnderTest

    Post("/candy", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 20, 0)
    }

    Get("/candy/0", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 20, 0)
    }

    mySystem.terminate()
  }

  "A User" should "get an error message if trying to get the status of a non existing machine" in {
    val machine = MachineState(None, locked, 20, 0)
    val (server, mySystem) = unitUnderTest

    Get("/candy/0", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.NotFound
      responseAs[Error] shouldEqual Error("Could not find a machine with id: 0")
    }

    mySystem.terminate()
  }


  "A User" should "be able to dispose a coin" in {
    val machine = MachineState(None, locked, 20, 0)
    val (server, mySystem) = unitUnderTest

    Post("/candy", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 20, 0)
    }

    Put("/candy/0/coin", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), unlocked, 20, 1)
    }

    mySystem.terminate()
  }

  "A User" should "get an error message if trying to dispose a coin in a non existing machine" in {
    val (server, mySystem) = unitUnderTest

    Put("/candy/0/coin") ~> server.route ~> check {
      status shouldEqual StatusCodes.NotFound
      responseAs[Error] shouldEqual Error("Could not find a machine with id: 0")
    }

    mySystem.terminate()
  }

  "A User" should "get an error message if trying to turn a non existing machine" in {
    val (server, mySystem) = unitUnderTest

    Put("/candy/0/turn") ~> server.route ~> check {
      status shouldEqual StatusCodes.NotFound
      responseAs[Error] shouldEqual Error("Could not find a machine with id: 0")
    }

    mySystem.terminate()
  }

  "A User" should "be able to turn" in {
    val machine = MachineState(None, locked, 20, 0)
    val (server, mySystem) = unitUnderTest

    Post("/candy", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 20, 0)
    }

    Put("/candy/0/coin", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), unlocked, 20, 1)
    }

    Put("/candy/0/turn", machine) ~> server.route ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[MachineState] shouldEqual MachineState(Some(0L), locked, 19, 1)
    }

    mySystem.terminate()
  }
}
