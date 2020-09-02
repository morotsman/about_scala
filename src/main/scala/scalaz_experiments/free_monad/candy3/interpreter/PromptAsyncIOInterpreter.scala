package scalaz_experiments.free_monad.candy3.interpreter

import cats.data.EitherT
import cats.~>
import scalaz_experiments.free_monad.candy3.Types.ProgramResult
import scalaz_experiments.free_monad.candy3.pure.algebra.{IOA, Read, Write}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

import scala.concurrent.ExecutionContext.Implicits.global

object PromptAsyncIOInterpreter extends (IOA ~> ProgramResult) {

  override def apply[A](i: IOA[A]): ProgramResult[A] = i match {
    case Write(message) =>
      val result = Future {
        Right(System.out.println(message)): Either[Exception, Unit]
      }
      EitherT(result)
    case Read() =>
      val result = Future {
        Right(StdIn.readLine()).asInstanceOf[Either[Exception, A]]
      }
      EitherT(result)
  }

}
