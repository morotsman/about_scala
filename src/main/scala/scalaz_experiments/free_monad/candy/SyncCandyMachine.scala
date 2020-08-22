package scalaz_experiments.free_monad.candy

import cats.{Id, ~>}
import scalaz.Id

import scala.io.StdIn


object IOInterpreter extends (IOA ~> Id) {
  def apply[A](i: IOA[A]) = i match {
    case Read() =>
      System.out.print("> ")
      StdIn.readLine()
    case Write(msg) =>
      System.out.println(msg)
  }
}

object Test {
  def InpureMachineInterpreter(initialMachine: MachineState): (MachineOp ~> Id) = new (MachineOp ~> Id){
    var currentMachine = initialMachine

    def apply[A](fa: MachineOp[A]) = fa match {
      case UpdateState(f) =>
        val (newMachine, output) = f(currentMachine)
        currentMachine = newMachine
        output
      case CurrentState() => currentMachine
    }
  }
}

object SyncCandyMachine {
  import Machine._, IO._, CandyMachine._, Test._

  val initialMachine = new MachineState(true, 20, 0)

  val interpreter: CandyMachine ~> Id = InpureMachineInterpreter(initialMachine) or IOInterpreter

  def main(args: Array[String]): Unit = {
    program.foldMap(interpreter)
  }
}


