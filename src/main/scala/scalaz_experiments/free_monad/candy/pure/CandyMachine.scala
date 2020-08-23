package scalaz_experiments.free_monad.candy.pure

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import scalaz_experiments.free_monad.candy.pure.CandyRule._

import scala.language.postfixOps

/* Handles user interaction */
sealed trait IOA[A]

case class Read() extends IOA[String]

case class Write(msg: String) extends IOA[Unit]

class IO[F[_]](implicit I: InjectK[IOA, F]) {
  def write(msg: String): Free[F, Unit] = Free.inject[IOA, F](Write(msg))

  def read(): Free[F, String] = Free.inject[IOA, F](Read())
}

object IO {
  implicit def io[F[_]](implicit I: InjectK[IOA, F]): IO[F] = new IO[F]
}

/* Represents persistence operations */
sealed trait MachineOp[A]

case class UpdateState(f: MachineState => (MachineState, String)) extends MachineOp[String]

case class CurrentState() extends MachineOp[MachineState]

class Machine[F[_]](implicit I: InjectK[MachineOp, F]) {
  def updateState(f: MachineState => (MachineState, String)): Free[F, String] = Free.inject[MachineOp, F](UpdateState(f))

  def currentState: Free[F, MachineState] = Free.inject[MachineOp, F](CurrentState())
}

object Machine {
  implicit def machine[F[_]](implicit I: InjectK[MachineOp, F]): Machine[F] = new Machine[F]
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case object Quit extends Input

case class Invalid(s: String) extends Input {
  override def toString: String = s"Sorry, $s is not a valid input"
}

object CandyMachine {
  type CandyMachine[A] = EitherK[MachineOp, IOA, A]

  type Program[A] = Free[CandyMachine, A]

  def program(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Unit] = {
    import D._
    import I._

    def main(): Program[Unit] =
      for {
        _ <- welcome
        _ <- showPossibleInputs
        _ <- doWhileM(programLoop)(_ != Quit)
      } yield ()

    def welcome: Program[Unit] = for {
      _ <- write("Welcome to the fabulous candy machine!")
    } yield ()

    def showPossibleInputs: Program[Unit] = for {
      _ <- write("Valid commands")
      _ <- write("c: insert coin")
      _ <- write("t: turn")
      _ <- write("q: quit")
      _ <- newLine
    } yield ()

    def getInput: Program[Input] = for {
      input <- read().map(toInput)
      result <- if (validInput(input)) pure(input) else handleInvalidInput(input)
    } yield result

    def newLine: Program[Unit] = write("")

    def pure[A](i: A): Program[A] = Free.pure[CandyMachine, A](i)

    def validInput(i: Input): Boolean = i == Turn || i == Coin || i == Quit

    def handleInvalidInput(i: Input): Program[Input] = for {
      _ <- write(i.toString)
      - <- showPossibleInputs
      r <- getInput
    } yield r

    def doWhileM[A](p: Program[A])(expr: => A => Boolean): Program[Unit] = for {
      a <- p
      _ <- if (expr(a)) doWhileM(p)(expr) else pure(())
    } yield ()

    def programLoop: Program[Input] = for {
      _ <- showCurrentStatus
      input <- getInput
      _ <- if (input != Quit) updateMachine(input) else pure(())
    } yield input

    def showCurrentStatus: Program[Unit] = for {
      current <- currentState
      _ <- write(statusMessage(current))
    } yield ()

    def statusMessage(m: MachineState): String = m match {
      case MachineState(locked, candies, coins) if locked => s"The machine is locked and has $candies candies left"
      case MachineState(locked, candies, coins) => s"The machine is unlocked and has $candies candies left"
    }

    def updateMachine(input: Input): Program[Unit] = for {
      feedback <- updateState(applyRule(input))
      _ <- write(feedback)
    } yield ()

    main()
  }

  def toInput(s: String): Input =
    if (s == "c")
      Coin
    else if (s == "t")
      Turn
    else if (s == "q") {
      Quit
    } else
      Invalid(s)
}

