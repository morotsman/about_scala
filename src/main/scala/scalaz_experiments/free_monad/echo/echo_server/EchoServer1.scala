package scalaz_experiments.free_monad.echo.echo_server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{complete, onComplete, path}
import cats.{Monad, ~>}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

import scala.concurrent.{Await, Future}
import scala.io.StdIn
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

object EchoServer1 {
  type EchoType[A] = Function1[String, Future[A]]

  implicit def toFuture: Monad[EchoType] = new Monad[EchoType] {

    override def flatMap[A, B](fa: EchoType[A])(f: A => EchoType[B]): EchoType[B] = (s: String) => for {
      a <- fa(s)
      r <- f(a)(s)
    } yield r

    override def tailRecM[A, B](ia: A)(f: A => EchoType[Either[A, B]]): EchoType[B] = (s: String) => {
      // Oops, not tailrec
      def go(a: A): Future[B] = {
        for {
          eab <- f(a)(s)
          r <- eab match {
            case Right(b) => Future {
              b
            }
            case Left(nextA) => go(nextA)
          }
        } yield r
      }

      def ugly(a: A): Future[B] = {
        var result: Option[Future[B]] = Option.empty
        var currentA = a
        do {
          val feab: Future[Either[A, B]] = f(currentA)(s)
          val eab: Either[A, B] = Await.result(feab, 1000 millis)
          eab match {
            case Right(b) =>
              result = Option(Future {
                b
              })
            case Left(nextA) =>
              currentA = nextA
          }
        } while (result.isEmpty)

        result.get
      }

      ugly(ia)
    }

    override def pure[A](a: A): EchoType[A] = (_) =>
      Future {
        a
      }
  }

  def toFutureCompiler: EchoA ~> EchoType =
    new (EchoA ~> EchoType) {
      override def apply[A](fa: EchoA[A]): EchoType[A] = fa match {
        case Read() =>
          (a: String) =>
            Future {
              a.asInstanceOf[A]
            }
      }
    }

  val handler: (String) => Future[String] = PureEcho.program.foldMap(toFutureCompiler)

  val route =
    path("echo" / Segment) { (a) => {
      onComplete(handler(a)) {
        case Success(value) => complete(s"Echo: $value")
        case Failure(ex) => complete(StatusCodes.InternalServerError, s"An error occurred: ${ex.getMessage}")
      }
    }
    }

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
