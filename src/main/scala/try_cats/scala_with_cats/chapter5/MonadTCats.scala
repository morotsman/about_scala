package try_cats.scala_with_cats.chapter5

import cats.data.{EitherT, OptionT}
import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object ComposingMonads {

  type ListOption[A] = OptionT[List, A]

  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A] = Either[String, A]

  // Build our final monad stack using OptionT:
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  type FutureEither[A] = EitherT[Future, String, A]

  type FutureEitherOption[A] = OptionT[FutureEither, A]

  def main(args: Array[String]): Unit = {
    val result1: ListOption[Int] = OptionT(List(Option(10), None, Option(20)))

    val result2: ListOption[Int] = 32.pure[ListOption]

    val tmp1: ListOption[Int] = result1.flatMap { (x: Int) =>
      result2.map { (y: Int) =>
        x + y
      }
    }
    val tmp2: List[Option[Int]] = tmp1.value

    val tmp3 = for {
      x <- result1
      y <- result2
    } yield x + y

    println(tmp2)
    println(tmp3.value)

    val a = 10.pure[ErrorOrOption]
    // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))

    val b = 32.pure[ErrorOrOption]
    // b: ErrorOrOption[Int] = OptionT(Right(Some(32)))

    val c = a.flatMap(x => b.map(y => x + y))
    // c: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(42)))

    import cats.instances.future._ // for Monad
    import scala.concurrent.Await
    import scala.concurrent.duration._

    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b

    futureEitherOr
    // res14: FutureEitherOption[Int] = OptionT(EitherT(Future(Success(Right(Some(42))))))

    val intermediate = futureEitherOr.value
    // intermediate: FutureEither[Option[Int]] = EitherT(Future(Success(Right(Some(42)))))

    val stack = intermediate.value
    // stack: scala.concurrent.Future[Either[String,Option[Int]]] = Future(Success(Right(Some(42))))

    println(Await.result(futureEitherOr.value.value, Duration.fromNanos(100000000)))

    // Create using apply:
    val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
    // errorStack1: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(10)))

    // Create using pure:
    val errorStack2 = 32.pure[ErrorOrOption]
    // errorStack2: ErrorOrOption[Int] = OptionT(Right(Some(32)))

    // Extracting the untransformed monad stack:
    errorStack1.value
    // res11: ErrorOr[Option[Int]] = Right(Some(10))

    // Mapping over the Either in the stack:
    errorStack2.value.map(_.getOrElse(-1))
    // res13: scala.util.Either[String,Int] = Right(32)
  }

}

object UsagePattern {

  // super stack
  sealed abstract class HttpError

  final case class NotFound(item: String) extends HttpError

  final case class BadRequest(msg: String) extends HttpError

  // etc...

  type FutureEither[A] = EitherT[Future, HttpError, A]


  // glue code

  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result: OptionT[Logged, Int] = for {
      a: Int <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  // This approach doesn't force OptionT on other users' code:
  val result1 = addAll("1", "2", "3")
  // result1: Logged[Option[Int]] = WriterT((List(Read 1, Read 2, Read 3),Some(6)))

  val result2 = addAll("1", "a", "3")
  // result2: Logged[Option[Int]] = WriterT((List(Read 1, Failed on a),None))
}

object TransformAndRollout {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = EitherT(Future {
    powerLevels.get(autobot).toRight(s"No autobot with that name: $autobot")
  })

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    a1 <- getPowerLevel(ally1)
    a2 <- getPowerLevel(ally2)
  } yield a1 + a2 > 15

  def report(ally1: String, ally2: String)(specialMove: Boolean): String =
    s"$ally1 and $ally2 can ${if (specialMove) "" else "not "}execute special move";

  def tacticalReport(ally1: String, ally2: String): String = {
    val futureResult = canSpecialMove(ally1, ally2).map(report(ally1, ally2)).value
    Await.result(futureResult, Duration.fromNanos(100000)) match {
      case Right(r) => r
      case Left(e) => e
    }
  }

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz", "Bumblebee"))

    println(tacticalReport("Bumblebee", "Hot Rod"))

    println(tacticalReport("Jazz", "Ironhide"))
  }
}
