package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherK
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.CandyRule.Coin

trait Request

case class CreateMachine[A](m: MachineState) extends Request

case class GetMachineState[A](id: Long) extends Request

case class InsertCoin[A](id: Long) extends Request

case class Turn[A](id: Long) extends Request

trait Response

case class SimpleResponse(s: String) extends Response

object CandyProgram {
  type CandyMachine[A] = EitherK[MachineOp, IOA, A]

  type Program[A] = Free[CandyMachine, A]

  def program(request: Request)(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Response] = {
    import D._
    import I._

    def handleRequest(r: Request): Program[Response] = r match {
      case CreateMachine(m) => initialState(m).map(m => SimpleResponse(m.toString))
      case GetMachineState(id) => currentState(id).map(m => SimpleResponse(m.toString))
      case InsertCoin(id) => updateState(id, CandyRule.applyRule(CandyRule.Coin)).map(m => SimpleResponse(m.toString))
      case Turn(id) => updateState(id, CandyRule.applyRule(CandyRule.Turn)).map(m => SimpleResponse(m.toString))
    }

    for {
      r <- receive(request)
      response <- handleRequest(r)
    } yield response
  }
}
