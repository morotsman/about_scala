package scalaz_experiments.free_monad.candy2

import cats.free.Free
import cats.free.Free.liftF

object CandyMachine {
  trait IOA[A]

  case class Read[A]() extends IOA[A]

  trait Command

  case class CreateMachine() extends Command

  case class GetMachineState(id: Long) extends Command

  case class InsertCoin(id: Long) extends Command

  case class Turn(id: Long) extends Command

  trait Response

  case class SimpleResponse(s: String) extends Response


  object IO {
    type IO[A] = Free[IOA, A]

    def read[A](): IO[A] =
      liftF[IOA, A](Read())
  }

  object CandyProgram {
    import IO._

    def program[A]: IO[Response] =
      read[Command]().map {
        case CreateMachine() => SimpleResponse("Create machine")
        case GetMachineState(id) => SimpleResponse("GetMachineState")
        case InsertCoin(id) => SimpleResponse("InsertCoin")
        case Turn(id) => SimpleResponse("Turn")
      }





  }

}

