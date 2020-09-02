package scalaz_experiments.free_monad.candy3.interpreter

import cats.~>
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.pure.algebra.IOA

object NoopAsyncIOInterpreter extends (IOA ~> ProgramResult) {

  // not used in the context of this web service
  def apply[A](i: IOA[A]): ProgramResult[A] = ???
}
