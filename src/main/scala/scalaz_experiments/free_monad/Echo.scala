package scalaz_experiments.free_monad

import scalaz._
import Scalaz._

import scala.io.StdIn
import scala.util.Try

sealed trait EchoA[A]

case class Read[A]() extends EchoA[Try[A]]

case class PrintLn[A](a: A) extends EchoA[Try[Unit]]

case class Print[A](a: A) extends EchoA[Try[Unit]]

object Echo {
  type Echo[A] = Free[EchoA, A]

  def read[A](): Echo[Try[A]] =
    Free.liftF[EchoA, Try[A]](Read[A])

  def printLn[A](a: A): Echo[Try[Unit]] =
    Free.liftF[EchoA, Try[Unit]](PrintLn[A](a))

  def print[A](a: A): Echo[Try[Unit]] =
    Free.liftF[EchoA, Try[Unit]](Print[A](a))

  def program: Echo[Try[Unit]] = for {
    _ <- greet
    s <- loop
  } yield (s)

  def greet: Echo[Try[Unit]] = for {
    r <- printLn("The great echo program!")
    r <- printLn("'q' to quit")
  } yield (r)

  def loop: Echo[Try[Unit]] = for {
    i <- echo
    s <- if (quit(i)) goodbye else loop
  } yield (s)

  def echo: Echo[Try[String]] = for {
    _ <- print("> ")
    s <- read
    _ <- printLn(s"You wrote: '${s.get}'")
    _ <- printLn("")
  } yield (s)

  def quit(st: Try[String]): Boolean =
    st.filter(_ == "q").isSuccess

  def goodbye: Echo[Try[Unit]] = for {
    r <- printLn("Hope to see you again soon, goodbye!")
  } yield (r)

}

object EchoEchoEcho {

  import Echo._


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
