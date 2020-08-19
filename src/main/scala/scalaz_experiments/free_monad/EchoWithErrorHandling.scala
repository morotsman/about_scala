package scalaz_experiments.free_monad

import scalaz._
import Scalaz._
import scalaz_experiments.free_monad.Echo.{Echo, print, printLn, read}

import scala.io.StdIn
import scala.util.{Try, Failure, Success}

object EchoProgramWithErrorHandling {
  def program: Echo[Try[Unit]] = for {
    _ <- greet
    s <- loop
  } yield (s)

  private def greet= for {
    _ <- printLn("The great echo program!")
    r <- printLn("'q' to quit")
  } yield r

  def loop: Echo[Try[Unit]] = {
    for {
      in <- readFromPrompt
      _ <- echo(in)
      s <- if (quit(in)) goodbye else loop
    } yield s
  }

  private def readFromPrompt: Echo[Try[String]] = {
    for {
      _ <- print("> ")
      s <- read[String]()
    } yield s
  }

  private def echo[A](in: Try[A]): Echo[Try[Unit]] = {
    for {
      r <- printLn(in match {
        case Failure(e) => "Oops, something has gone wrong: " + e
        case Success(v) => s"You wrote: '$v'"
      })
      _ <- printLn("")
    } yield r
  }

  private def quit(st: Try[String]): Boolean = {
    st match {
      case Failure(e) => true
      case Success(v) => v == "q"
    }
  }


  private def goodbye: Echo[Try[Unit]]  = for {
    r <- printLn("Hope to see you again soon, goodbye!")
  } yield (r)
}

object EchoEchoEcho {

  import EchoProgram._

  def compilerWithSideEffects: EchoA ~> Id.Id =
    new (EchoA ~> Id) {
      def apply[A](fa: EchoA[A]): Id[A] = fa match {
        case Read() =>
          Try(StdIn.readLine)
        case PrintLn(s) =>
          Try(System.out.println(s))
        case Print(s) =>
          Try(System.out.print(s))
      }
    }

  def main(args: Array[String]): Unit = {
    program.foldMap(compilerWithSideEffects)
  }

}
