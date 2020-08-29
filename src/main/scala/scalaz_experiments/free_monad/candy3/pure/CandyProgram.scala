package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherK
import cats.free.Free

trait Request

case class CreateMachine[A]() extends Request

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

    for {
      r <- receive(request)
    } yield SimpleResponse(r.toString)
  }
}
