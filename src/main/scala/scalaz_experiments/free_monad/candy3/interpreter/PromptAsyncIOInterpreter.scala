package scalaz_experiments.free_monad.candy3.interpreter

import cats.data.EitherT
import cats.~>
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.pure.algebra.{IOA, Read, Write}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object PromptAsyncIOInterpreter extends (IOA ~> ProgramResult) {

  override def apply[A](i: IOA[A]): ProgramResult[A] = i match {
    case Write(message) => Future {
      Try(System.out.println(message)).toEither
    }
    case Read() => Future {
      Try(StdIn.readLine()).toEither
    }
  }

}
