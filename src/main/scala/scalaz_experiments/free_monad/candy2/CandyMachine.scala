package scalaz_experiments.free_monad.candy2

import cats.free.Free
import cats.free.Free.liftF

object CandyMachine {
  trait IOA[A]

  case class Read[A]() extends IOA[A]

  trait Request[A]

  case class CreateMachine[A]() extends Request[A]

  case class GetMachineState[A](id: Long) extends Request[A]

  case class InsertCoin[A](id: Long) extends Request[A]

  case class Turn[A](id: Long) extends Request[A]

  trait Response

  case class SimpleResponse(s: String) extends Response


  object IO {
    type IO[A] = Free[IOA, A]

    def read[A](): IO[A] =
      liftF[IOA, A](Read())
  }

  object CandyProgram {
    import IO._

    def program: IO[Response] =
      read[Request[Response]]().map {
        case CreateMachine() => SimpleResponse("Create machine")
        case GetMachineState(id) => SimpleResponse("GetMachineState")
        case InsertCoin(id) => SimpleResponse("InsertCoin")
        case Turn(id) => SimpleResponse("Turn")
      }





  }

}

