package scalaz_experiments.free_monad.candy3.pure

import scalaz_experiments.free_monad.candy3.pure.algebra.MachineOp

object RequestHandler extends RequestHandlerProgram {
  type CandyMachine[A] = MachineOp[A]
}
