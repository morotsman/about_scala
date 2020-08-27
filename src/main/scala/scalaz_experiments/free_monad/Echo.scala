package scalaz_experiments.free_monad

import cats._
import cats.free.Free
import cats.implicits._
import scalaz_experiments.free_monad.Echo.{Echo, print, printLn, read}

import scala.io.StdIn
import scala.util.Try

sealed trait EchoA[A]

case class Read[A]() extends EchoA[Try[A]]

case class PrintLn[A](a: A) extends EchoA[Try[Unit]]

case class Print[A](a: A) extends EchoA[Try[Unit]]

object Echo {
  type Echo[A] = Free[EchoA, A]

  def read[A](): Echo[Try[A]] =
    Free.liftF[EchoA, Try[A]](Read[A]())

  def printLn[A](a: A): Echo[Try[Unit]] =
    Free.liftF[EchoA, Try[Unit]](PrintLn[A](a))

  def print[A](a: A): Echo[Try[Unit]] =
    Free.liftF[EchoA, Try[Unit]](Print[A](a))
}

object EchoProgram {
  def program: Echo[Try[Unit]] = for {
    _ <- greet
    s <- loop
  } yield (s)

  private def greet= for {
    _ <- printLn("The great echo program!")
    r <- printLn("'q' to quit")
  } yield r

  def loop: Echo[Try[Unit]] = for {
    in <- readFromPrompt
    _ <- echo(in)
    s <- if (quit(in)) goodbye else loop
  } yield s

  private def readFromPrompt: Echo[Try[String]] = for {
    _ <- print("> ")
    s <- read[String]()
  } yield s

  private def echo[A](in: Try[A]): Echo[Try[Unit]] = for {
    _ <- printLn(s"You wrote: '${in.get}'")
    r <- printLn("")
  } yield r

  private def quit(st: Try[String]): Boolean =
    st.filter(_ == "q").isSuccess

  private def goodbye: Echo[Try[Unit]]  = for {
    r <- printLn("Hope to see you again soon, goodbye!")
  } yield (r)
}

object EchoEchoEcho {

  import EchoProgram._

  def compilerWithSideEffects: EchoA ~> Id =
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
