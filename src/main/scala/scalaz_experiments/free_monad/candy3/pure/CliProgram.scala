package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherK
import scalaz_experiments.free_monad.candy3.pure.algebra.{IO, IOA, Machine, MachineOp}

object CliProgram extends CliCandyProgram with RequestHandlerProgram {
  type CandyMachine[A] = EitherK[MachineOp, IOA, A]

  def cliProgram(implicit I: IO[CandyMachine], D: Machine[CandyMachine]): Program[Unit] = cliProgram(requestHandler)
}
