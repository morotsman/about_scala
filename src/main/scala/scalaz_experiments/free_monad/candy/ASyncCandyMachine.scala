package scalaz_experiments.free_monad.candy

import cats.{Id, ~>}
import cats.instances.future._
import scalaz_experiments.free_monad.candy.pure.{CurrentState, IOA, MachineOp, MachineState, Read, UpdateState, Write}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

object AsyncIOInterpreter extends (IOA ~> Future) {
  def apply[A](i: IOA[A]) = i match {
    case Read() => for {
      _ <- Future.successful(System.out.print("> "))
      i <- Future.successful(StdIn.readLine())
    } yield i
    case Write(msg) =>
      Future.successful(System.out.println(msg))
  }
}

object AsyncInpureMachineInterpreter extends (MachineOp ~> Future) {
  private[this] var machine = new MachineState(true, 10, 0)

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(f) => Future.successful {
      val (newMachine, output) = f(machine)
      machine = newMachine
      output
    }
    case CurrentState() => Future.successful {
      machine
    }
  }
}

object AsyncCandyMachine {
  import scalaz_experiments.free_monad.candy.pure.Machine._, scalaz_experiments.free_monad.candy.pure.IO._, scalaz_experiments.free_monad.candy.pure.CandyMachine._

  println("ASync")
  implicit val ec = ExecutionContext.global

  val interpreter: CandyMachine ~> Future = AsyncInpureMachineInterpreter or AsyncIOInterpreter

  def main(args: Array[String]): Unit = {
    val eval: Future[Unit] = program.foldMap(interpreter)

    Await.result(eval, 20000 millis)
  }
}
