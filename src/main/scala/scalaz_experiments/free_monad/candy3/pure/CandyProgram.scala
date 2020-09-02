package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherK
import cats.free.Free
import scalaz_experiments.free_monad.candy.pure.CandyMachine.Program
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

  type Program[A] = Free[CandyMachine, A]

  def cliProgram()(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Unit] = {
    import D._
    import I._

    def main(): Program[Unit] = for {
      _ <- write("Welcome to the candy machine")
      _ <- showCommands
      _ <- createMachine
      _ <- doWhileM(processInput)(input => input != QuitRequest())
    } yield()

    def processInput: Program[Request] = for {
      request <- getRequest
      _ <- handleRequest(request)
    } yield request

    def handleRequest(request: Request): Program[Unit] = request match {
      case QuitRequest() => pure(())
      case HelpRequest() => showCommands
      case _ => for {
        m <- machineProgram(request)
        _ <- write(m)
      } yield ()
    }

    def doWhileM[A](p: Program[A])(expr: => A => Boolean): Program[Unit] = for {
      a <- p
      _ <- if (expr(a)) doWhileM(p)(expr) else pure(())
    } yield ()

    def showCommands: Program[Unit] = for {
     _ <- write("Available commands")
     _ <- write("s - get current state of machine")
     _ <- write("c - insert a coin")
     _ <- write("t - turn")
     _ <- write("h - help")
     _ <- write("q - quit")
    } yield ()

    def getRequest: Program[Request] = for {
      request <- read().map(toRequest)
      result <- request match {
        case Left(e) => handleInvalidRequest(e)
        case Right(v) => pure(v)
      }
    } yield result

    def pure[A](i: A): Program[A] = Free.pure[CandyMachine, A](i)

    def handleInvalidRequest(e: Exception): Program[Request] = for {
      _ <- write(e.getMessage)
      r <- getRequest
    } yield r

    def toRequest(s: String): Either[Exception, Request] =
      if (s == "c")
        Right(InsertCoin(0L))
      else if (s == "t")
        Right(Turn(0L))
      else if (s == "q")
        Right(QuitRequest())
      else if(s == "s")
        Right(GetMachineState(0))
      else if(s == "h")
        Right(HelpRequest())
      else
      Left(new IllegalArgumentException(s"Invalid request: $s"))

    main()
  }

  private def createMachine =
    machineProgram(CreateMachine(MachineState(locked = true, candies = 100, coins = 0)))

  def machineProgram(request: Request)(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[MachineState] = {
    import D._
    import I._

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    def applyRule(input: Input)(machine: MachineState): Either[Exception, MachineState] = input match {
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

    for {
      response <- request match {
        case CreateMachine(m) => initialState(m)
        case GetMachineState(id) => currentState(id)
        case InsertCoin(id) => updateState(id, applyRule(Coin))
        case Request.Turn(id) => updateState(id, applyRule(Turn))
      }
    } yield response
  }
}
