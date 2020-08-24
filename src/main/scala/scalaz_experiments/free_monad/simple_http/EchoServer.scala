package scalaz_experiments.free_monad.simple_http

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

object EchoServer {


  def handler(a: String) = Future {
    a
  }

  val route =
    path("echo" / Segment) { (a) => {
      onComplete(handler(a)) {
        case Success(value) => complete(s"Echo: $value")
        case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
      }
    }}

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext


    val bindingFuture = Http().newServerAt("localhost", 8090).bind(route)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }


}
