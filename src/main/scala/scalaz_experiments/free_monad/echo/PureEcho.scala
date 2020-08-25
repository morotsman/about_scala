package scalaz_experiments.free_monad.echo

import cats.free.Free
import cats.free.Free.liftF
import cats.arrow.FunctionK
import cats.{Id, ~>}

import scala.collection.mutable
import scala.io.StdIn

sealed trait EchoA[A]
case class Read[T]() extends EchoA[T]


object PureEcho {
  type Echo[A] = Free[EchoA, A]

  def read[T](): Echo[T] =
    liftF[EchoA, T](Read[T]())


  def program: Echo[String] = for {
    s <- read[String]()
  } yield s
}

object Test {
  def impureCompiler: EchoA ~> Id  =
    new (EchoA ~> Id) {
      override def apply[A](fa: EchoA[A]): Id[A] = fa match {
        case Read() =>
          val s = StdIn.readLine()
          println("Echo: " + s)
          s.asInstanceOf[A]
      }
    }

  def main(args: Array[String]): Unit = {
    PureEcho.program.foldMap(impureCompiler)
  }
}
