package scalaz_experiments.free_monad.candy2

import cats.Monad
import scalaz_experiments.free_monad.candy2.CandyMachine.{Request, Response}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.{Await, Future}

object ToFutureMonad {
  type CandyType[A] = Function1[Request[Response], Future[A]]

  implicit def toFuture: Monad[CandyType] = new Monad[CandyType] {

    override def flatMap[A, B](fa: CandyType[A])(f: A => CandyType[B]): CandyType[B] = (s: Request[Response]) => for {
      a <- fa(s)
      r <- f(a)(s)
    } yield r

    override def tailRecM[A, B](ia: A)(f: A => CandyType[Either[A, B]]): CandyType[B] = (s: Request[Response]) => {
      // Oops, not tailrec
      def go(a: A): Future[B] = {
        for {
          eab <- f(a)(s)
          r <- eab match {
            case Right(b) => Future {
              b
            }
            case Left(nextA) => go(nextA)
          }
        } yield r
      }

      def ugly(a: A): Future[B] = {
        var result: Option[Future[B]] = Option.empty
        var currentA = a
        do {
          val feab: Future[Either[A, B]] = f(currentA)(s)
          val eab: Either[A, B] = Await.result(feab, 1000 millis)
          eab match {
            case Right(b) =>
              result = Option(Future {
                b
              })
            case Left(nextA) =>
              currentA = nextA
          }
        } while (result.isEmpty)

        result.get
      }

      go(ia)
    }

    override def pure[A](a: A): CandyType[A] = (_) =>
      Future {
        a
      }
  }
}
