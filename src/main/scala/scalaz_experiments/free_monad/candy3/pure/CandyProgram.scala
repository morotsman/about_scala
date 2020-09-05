package scalaz_experiments.free_monad.candy3.pure

import cats.data.{EitherK, EitherT}
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.Request._
import scalaz_experiments.free_monad.candy3.pure.algebra.{IO, IOA, Machine, MachineOp}

object Request {

  trait Request

  trait CliRequest extends Request

  case class HelpRequest() extends CliRequest

  case class QuitRequest() extends CliRequest

  trait MachineRequest extends Request

  case class CreateMachine(m: MachineState) extends MachineRequest

  case class GetMachineState(id: Long) extends MachineRequest

  case class InsertCoin(id: Long) extends MachineRequest

  case class Turn(id: Long) extends MachineRequest

}

object CandyProgram {
  type CandyMachine[A] = EitherK[MachineOp, IOA, A]

  type FreeProgram[A] = Free[CandyMachine, A]

  type Program[A] = EitherT[FreeProgram, Throwable, A]

  def cliProgram(initialMachine: MachineState)(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Unit] = {

    def main(): Program[Unit] = (for {
      _ <- write("Welcome to the candy machine")
      _ <- showCommands
      _ <- createMachine
      _ <- doWhileM(processInput)(input => input != QuitRequest())
    } yield ())

    def write[A](s: A): Program[Unit] = EitherT(I.write(s))

    def showCommands: Program[Unit] = for {
      _ <- write("Available commands")
      _ <- write("s - get current state of machine")
      _ <- write("c - insert a coin")
      _ <- write("t - turn")
      _ <- write("h - help")
      _ <- write("q - quit")
    } yield ()

    def createMachine: Program[Unit] =
      handleRequest(CreateMachine(initialMachine))

    def doWhileM[A](p: Program[A])(expr: => A => Boolean): Program[Unit] = for {
      a <- p
      _ <- if (expr(a)) doWhileM(p)(expr) else noop
    } yield ()

    def noop: Program[Unit] = EitherT(pure(Right(()): Either[Throwable, Unit]))

    def processInput: Program[Request] = for {
      request <- getRequest
      _ <- handleRequest(request)
    } yield request

    def getRequest: Program[Request] = (for {
      input <- read[String]()
      request <- EitherT(pure(toRequest(input)))
    } yield request).recoverWith(e => handleInvalidRequest(e))

    def read[A](): Program[A] = EitherT(I.read[A]())

    def pure[A](i: A) = Free.pure[CandyMachine, A](i)

    def toRequest(s: String): Either[Throwable, Request] =
      if (s == "c")
        Right(InsertCoin(0L))
      else if (s == "t")
        Right(Turn(0L))
      else if (s == "q")
        Right(QuitRequest())
      else if (s == "s")
        Right(GetMachineState(0))
      else if (s == "h")
        Right(HelpRequest())
      else
        Left(new IllegalArgumentException(s"Invalid request: $s"))

    def handleInvalidRequest(e: Throwable): Program[Request] = for {
      _ <- write(e.getMessage)
      r <- getRequest
    } yield r

    def handleRequest(request: Request): Program[Unit] = (request match {
      case QuitRequest() => noop
      case HelpRequest() => showCommands
      case CreateMachine(_) => for {
        m <- machineProgram(request)
        _ <- write(m.toString)
      } yield ()
      case GetMachineState(_) => for {
        m <- machineProgram(request)
        _ <- write(m.toString)
      } yield ()
      case InsertCoin(_) => for {
        m <- machineProgram(request)
        _ <- write("Coin disposed, turn to get your candy!")
      } yield ()
      case Turn(_) => for {
        m <- machineProgram(request)
        _ <- write("Here is your candy!")
      } yield ()
    }).recoverWith(e => write(s"Error when handling request: ${e.getMessage}"))

    main()
  }

  def machineProgram(request: Request)(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[MachineState] = {
    import D._
    import I._

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    def applyRule(input: Input)(machine: MachineState): Either[Throwable, MachineState] = input match {
      case Coin =>
        if (machine.candies == 0) {
          Left(new IllegalStateException("No candies left"))
        } else if (machine.locked) {
          Right(machine.copy(locked = false, coins = machine.coins + 1))
        } else {
          Left(new IllegalStateException("A coin has already been disposed"))
        }
      case Turn =>
        if (machine.candies == 0) {
          Left(new IllegalStateException("No candies left"))
        } else if (!machine.locked) {
          Right(machine.copy(locked = true, candies = machine.candies - 1))
        } else {
          Left(new IllegalStateException("No coin has been disposed"))
        }
    }

    val result = for {
      response <- request match {
        case CreateMachine(m) => initialState(m)
        case GetMachineState(id) => currentState(id)
        case InsertCoin(id) => updateState(id, applyRule(Coin))
        case Request.Turn(id) => updateState(id, applyRule(Turn))
      }
    } yield response

    EitherT(result)
  }
}
