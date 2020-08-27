package scalaz_experiments.free_monad.echo.simple_echo

import cats._
import cats.free.Free
import scalaz_experiments.free_monad.echo.simple_echo.Echo.{Echo, print, printLn, read}

import scala.io.StdIn
import scala.util.Try

sealed trait EchoA[A]

case class Read[A]() extends EchoA[A]

case class PrintLn[A](a: A) extends EchoA[Unit]

case class Print[A](a: A) extends EchoA[Unit]

object Echo {
  type Echo[A] = Free[EchoA, A]

  def read[A](): Echo[A] =
    Free.liftF[EchoA, A](Read[A]())

  def printLn[A](a: A): Echo[Unit] =
    Free.liftF[EchoA, Unit](PrintLn[A](a))

  def print[A](a: A): Echo[Unit] =
    Free.liftF[EchoA, Unit](Print[A](a))
}

object EchoProgram {
  def program: Echo[Unit] = for {
    _ <- greet
    s <- loop
  } yield (s)

  private def greet= for {
    _ <- printLn("The great echo program!")
    r <- printLn("'q' to quit")
  } yield r

  def loop: Echo[Unit] = for {
    in <- readFromPrompt
    _ <- echo(in)
    s <- if (quit(in)) goodbye else loop
  } yield s

  private def readFromPrompt: Echo[String] = for {
    _ <- print("> ")
    s <- read[String]()
  } yield s

  private def echo[A](in: A): Echo[Unit] = for {
    _ <- printLn(s"You wrote: '${in}'")
    r <- printLn("")
  } yield r

  private def quit(st: String): Boolean =
    st == "q"

  private def goodbye: Echo[Unit]  = for {
    r <- printLn("Hope to see you again soon, goodbye!")
  } yield (r)
}

object EchoEchoEcho {

  import EchoProgram._

  def compilerWithSideEffects: EchoA ~> Id =
    new (EchoA ~> Id) {
      def apply[A](fa: EchoA[A]): Id[A] = fa match {
        case Read() =>
          StdIn.readLine.asInstanceOf[A]
        case PrintLn(s) =>
          System.out.println(s)
        case Print(s) =>
          System.out.print(s)
      }
    }

  def main(args: Array[String]): Unit = {
    program.foldMap(compilerWithSideEffects)
  }

}
