package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.Request._
import scalaz_experiments.free_monad.candy3.pure.algebra.{IO, IOA, Machine, MachineOp}

import scala.util.Try

object Request {
  trait Request

  case class CreateMachine[A](m: MachineState) extends Request

  case class GetMachineState[A](id: Long) extends Request

  case class InsertCoin[A](id: Long) extends Request

  case class Turn[A](id: Long) extends Request
}

object CandyProgram {
  type CandyMachine[A] = EitherK[MachineOp, IOA, A]

  type Program[A] = Free[CandyMachine, A]

  def program(request: Request)(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Try[MachineState]] = {
    import D._
    import I._

    def handleRequest(r: Request): Program[Try[MachineState]] = r match {
      case CreateMachine(m) => initialState(m)
      case GetMachineState(id) => currentState(id)
      case InsertCoin(id) => updateState(id, applyRule(Coin))
      case Request.Turn(id) => updateState(id, applyRule(Turn))
    }

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    def applyRule(input: Input)(machine: MachineState): Try[MachineState] = input match {
      case Coin =>
        if (machine.candies == 0) {
          Try(throw new IllegalStateException("No candies left"))
        } else if (machine.locked) {
          Try(machine.copy(locked = false, coins = machine.coins + 1))
        } else {
          Try(throw new IllegalStateException("A coin has already been disposed"))
        }
      case Turn =>
        if (machine.candies == 0) {
          Try(throw new IllegalStateException("No candies left"))
        } else if (!machine.locked) {
          Try(machine.copy(locked = true, candies = machine.candies - 1))
        } else {
          Try(throw new IllegalStateException("No coin has been disposed"))
        }
    }

    for {
      r <- receive(request)
      response <- handleRequest(r)
    } yield response
  }
}
