package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherT
import scalaz_experiments.free_monad.candy3.pure.Request.{CreateMachine, GetMachineState, InsertCoin, Request}
import scalaz_experiments.free_monad.candy3.pure.algebra.{Machine, MachineOp}

trait RequestHandlerProgram extends Program {

  def requestHandler(request: Request)(implicit D: Machine[CandyMachine]): Program[MachineState] = {
    import D._

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

object RequestHandler extends RequestHandlerProgram {
  type CandyMachine[A] = MachineOp[A]
}

