package scalaz_experiments.free_monad.candy3.interpreter

import cats.data.State
import cats.~>
import scalaz_experiments.free_monad.candy3.Types._
import scalaz_experiments.free_monad.candy3.pure.algebra.{IOA, Read, Write}

object StateIOInterpreter extends (IOA ~> CandyState) {
  def apply[A](i: IOA[A]): CandyState[A] = i match {
    case Read() => for {
      old <- State.get
      _ <- State.modify(addToOutput(old.in.head))
      _ <- State.modify(addToInput(old.in.tail))
    } yield (old.in.head.asInstanceOf[A])
    case Write(msg) => for {
      _ <- State.modify(addToOutput(msg))
    } yield ()
  }

  def addToOutput[A](i: A)(s: InternalState[Any]): InternalState[Any] =
    s.copy(out = i :: s.out)

  def addToInput[A](i: List[A])(s: InternalState[Any]): InternalState[Any] =
    s.copy(in = i)
}
