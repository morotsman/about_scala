package scalaz_experiments.free_monad.candy

import cats.{Id, ~>}
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

object InpureMachineInterpreter extends (MachineOp ~> Id) {
  private[this] var machine = new MachineState(true, 10, 0)

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(f) =>
      val (newMachine, output) = f(machine)
      machine = newMachine
      output
    case CurrentState() => machine
  }
}

object SyncCandyMachine {
  import Machine._, IO._, CandyMachine._

  val interpreter: CandyMachine ~> Id = InpureMachineInterpreter or IOInterpreter

  def main(args: Array[String]): Unit = {
    program.foldMap(interpreter)
  }
}


