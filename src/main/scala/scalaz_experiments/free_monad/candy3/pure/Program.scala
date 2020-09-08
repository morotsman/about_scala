package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherT
import cats.free.Free

trait Program {
  type CandyMachine[A]

  type FreeProgram[A] = Free[CandyMachine, A]

  type Program[A] = EitherT[FreeProgram, Throwable, A]
}
