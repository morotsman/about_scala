package scalaz_experiments.free_monad.candy

import cats.data.State
import cats.{Id, ~>}

object Test {
  case class Buffers[A](in: List[A], out: List[A])

  type CandyState[A] = State[Buffers[Any], A]

  object IOInterpreterState extends (IOA ~> CandyState) {
    def apply[A](i: IOA[A]) = i match {
      case Read() => for {
        old <- State.get
        _ <- State.modify(addToOutput(old.in.head))
        _ <- State.modify(addToInput(old.in.tail))
      } yield (old.in.head.asInstanceOf[A])
      case Write(msg) => for {
        _ <- State.modify(addToOutput(msg))
      } yield ()
    }

    def addToOutput[A](i: A)(s: Buffers[Any]): Buffers[Any] =
      s.copy(out = i :: s.out)

    def addToInput[A](i: List[A])(s: Buffers[Any]): Buffers[Any] =
      s.copy(in = i)
  }

  object InpureMachineInterpreter extends (MachineOp ~> CandyState) {
    private[this] var machine = new MachineState(true, 10, 0)

    def apply[A](fa: MachineOp[A]) = fa match {
      case UpdateState(f) => State { s =>
        val (newMachine, output) = f(machine)
        machine = newMachine
        (s, output)
      }
      case CurrentState() => State { s =>
        (s, machine)
      }
    }
  }
}

object TestCandyMachine {

  import Machine._, IO._, CandyMachine._, Test._

  val interpreter: CandyMachine ~> CandyState = InpureMachineInterpreter or IOInterpreterState

  def main(args: Array[String]): Unit = {
    val myInput = List("c", "t", "a", "c", "t", "q")

    val result = program.foldMap(interpreter).run(Buffers(myInput, List()))
    result.value._1.out.reverse.foreach(println)
  }
}


