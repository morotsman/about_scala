package scalaz_experiments.free_monad.candy

import cats.data.State
import cats.{Id, ~>}

object Test {

  case class TestState[A](in: List[A], out: List[A], machine: MachineState)

  type CandyState[A] = State[TestState[Any], A]

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

    def addToOutput[A](i: A)(s: TestState[Any]): TestState[Any] =
      s.copy(out = i :: s.out)

    def addToInput[A](i: List[A])(s: TestState[Any]): TestState[Any] =
      s.copy(in = i)
  }

  object PureMachineInterpreter extends (MachineOp ~> CandyState) {
    def apply[A](fa: MachineOp[A]) = fa match {
      case UpdateState(f) => State { s =>
        val (newMachine, output) = f(s.machine)
        (updateMachine(newMachine)(s), output)
      }
      case CurrentState() => State { s =>
        (s, s.machine)
      }
    }

    def updateMachine[A](m: MachineState)(s: TestState[Any]): TestState[Any] =
      s.copy(machine = m)
  }

}

object TestCandyMachine {

  import Machine._, IO._, CandyMachine._, Test._

  val interpreter: CandyMachine ~> CandyState = PureMachineInterpreter or IOInterpreterState

  def main(args: Array[String]): Unit = {
    val myInput = List[Any]("c", "t", "a", "c", "t", "q")
    val initialMachine = new MachineState(true, 50, 0)
    val initialState = TestState(myInput, List(), initialMachine)
    
    val result = program.foldMap(interpreter).run(initialState).value
    result._1.out.reverse.foreach(println)
    println("machine: " + result._1.machine)
  }
}


