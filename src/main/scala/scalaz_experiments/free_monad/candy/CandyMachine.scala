package scalaz_experiments.free_monad.candy

import cats.data.{EitherK, State}
import cats.free.Free
import cats.{Id, InjectK, ~>}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

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

case class MachineState(locked: Boolean, candies: Int, coins: Int)

case class UpdateState(f: MachineState => (MachineState, String)) extends MachineOp[String]

case class CurrentState() extends MachineOp[MachineState]

class Machine[F[_]](implicit I: InjectK[MachineOp, F]) {
  def updateState(f: MachineState => (MachineState, String)): Free[F, String] = Free.inject[MachineOp, F](UpdateState(f))

  def currentState(): Free[F, MachineState] = Free.inject[MachineOp, F](CurrentState())
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
    import I._, D._

    def welcome(): Program[Unit] = for {
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
      _ <- newLine
      result <- if (validInput(input)) pure(input) else handleInvalidInput(input)
    } yield result

    def handleInvalidInput(i: Input): Program[Input] = for {
      _ <- write(i.toString)
      - <- showPossibleInputs
      - <- newLine
      r <- getInput
    } yield r

    def newLine: Program[Unit] = write("")

    def pure[A](i: A): Program[A] =
      Free.pure[CandyMachine, A](i)

    def validInput(i: Input): Boolean =
      i == Turn || i == Coin || i == Quit

    for {
      _ <- welcome()
      _ <- showPossibleInputs
      current <- currentState()
      _ <- write(current.toString)
      input <- getInput
      feedback <- updateState(applyRule(input))
      _ <- write(feedback)
      current <- currentState()
      _ <- write(current.toString)
    } yield ()

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

  def applyRule(input: Input)(machine: MachineState): (MachineState, String) = input match {
    case Coin =>
      if (machine.candies == 0) {
        (machine, "No candies Left")
      } else if (machine.locked) {
        val unlocked = false
        (MachineState(unlocked, machine.candies, machine.coins + 1), "Unlocked, turn to get your candy")
      } else {
        (machine, "Could not accept coin, turn to get a candy")
      }
    case Turn =>
      if (machine.candies == 0) {
        (machine, "No candies Left")
      } else if (!machine.locked) {
        val locked = true
        (MachineState(locked, machine.candies - 1, machine.coins), "Here is your candy")
      } else {
        (machine, "You need to dispose a coin to get a candy")
      }
  }
}

object IOInterpreter extends (IOA ~> Id) {
  def apply[A](i: IOA[A]) = i match {
    case Read() =>
      StdIn.readLine()
    case Write(msg) =>
      System.out.println(msg)
  }
}

object StateMachine {
  type StateMachine[A] = State[MachineState, A]
}

import StateMachine._

object InpureMachineInterpreter extends (MachineOp ~> Id) {
  private[this] var machine = new MachineState(true, 10, 0)

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(f) =>
      val (newMachine, output) = f(machine)
      machine = newMachine
      output
    case CurrentState() => machine
  }
}



object PureMachineInterpreter extends (MachineOp ~> StateMachine) {
  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(f) => for {
      m <- State.get[MachineState]
      (newState, output) = f(m)
      _ <- State.set(newState)
    } yield output
    case CurrentState() => for {
      m <- State.get
    } yield m
  }
}

object Test {

  import Machine._, IO._, CandyMachine._

  val interpreter1: CandyMachine ~> Id = InpureMachineInterpreter or IOInterpreter
  // val interpreter2: CandyMachine ~> Id =  IOInterpreter or PureMachineInterpreter

  def main(args: Array[String]): Unit = {
    val evaled: Unit = program.foldMap(interpreter1)
  }
}
