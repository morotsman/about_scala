package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.Request._
import scalaz_experiments.free_monad.candy3.pure.Result._

object Request {
  trait Request

  case class CreateMachine[A](m: MachineState) extends Request

  case class GetMachineState[A](id: Long) extends Request

  case class InsertCoin[A](id: Long) extends Request

  case class Turn[A](id: Long) extends Request
}

trait Response

case class SimpleResponse(s: String) extends Response

object Result {
  sealed trait Result

  case object NO_CANDIES_LEFT extends Result {
    override def toString: String = "No candies Left"
  }

  case object UNLOCKED extends Result {
    override def toString: String = "Unlocked, turn to get your candy"
  }

  case object ALREADY_UNLOCKED extends Result {
    override def toString: String = "Could not accept coin, turn to get a candy"
  }

  case object CANDY_DISPOSED extends Result {
    override def toString: String = "Here is your candy"
  }

  case object DISPOSE_A_COIN extends Result {
    override def toString: String = "You need to dispose a coin to get a candy"
  }

}

object CandyProgram {
  type CandyMachine[A] = EitherK[MachineOp, IOA, A]

  type Program[A] = Free[CandyMachine, A]

  def program(request: Request)(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Response] = {
    import D._
    import I._

    def handleRequest(r: Request): Program[Response] = r match {
      case CreateMachine(m) => initialState(m).map(m => SimpleResponse(m.toString))
      case GetMachineState(id) => currentState(id).map(m => SimpleResponse(m.toString))
      case InsertCoin(id) => updateState(id, applyRule(Coin)).map(m => SimpleResponse(m.toString))
      case Request.Turn(id) => updateState(id, applyRule(Turn)).map(m => SimpleResponse(m.toString))
    }


    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    def applyRule(input: Input)(machine: MachineState): (MachineState, Result) = input match {
      case Coin =>
        if (machine.candies == 0) {
          (machine, NO_CANDIES_LEFT)
        } else if (machine.locked) {
          (machine.copy(locked = false, coins = machine.coins + 1), UNLOCKED)
        } else {
          (machine, ALREADY_UNLOCKED)
        }
      case Turn =>
        if (machine.candies == 0) {
          (machine, NO_CANDIES_LEFT)
        } else if (!machine.locked) {
          (machine.copy(locked = true, candies = machine.candies - 1), UNLOCKED)
        } else {
          (machine, DISPOSE_A_COIN)
        }
    }

    for {
      r <- receive(request)
      response <- handleRequest(r)
    } yield response
  }
}
