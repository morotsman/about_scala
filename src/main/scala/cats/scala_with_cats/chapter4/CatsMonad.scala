package cats.scala_with_cats.chapter4

import cats.{Id, Monad}
import cats.data.{Reader, WriterT}
import cats.instances.option._
import cats.instances.list._
import cats.scala_with_cats.chapter4.HackingOnReader.DbReader

import scala.annotation.tailrec // for Monad

class CatsMonad {

  trait MonadExample[F[_]] {
    def pure[A](value: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

  object MonadExamples {
    val opt1 = Monad[Option].pure(3)
    // opt1: Option[Int] = Some(3)

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    // opt2: Option[Int] = Some(5)

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    // opt3: Option[Int] = Some(500)

    val list1 = Monad[List].pure(3)
    // list1: List[Int] = List(3)

    val list2 = Monad[List].
      flatMap(List(1, 2, 3))(a => List(a, a * 10))
    // list2: List[Int] = List(1, 10, 2, 20, 3, 30)

    val list3 = Monad[List].map(list2)(a => a + 123)
    // list3: List[Int] = List(124, 133, 125, 143, 126, 153)

    Monad[Option].flatMap(Option(1))(a => Option(a * 2))
    // res0: Option[Int] = Some(2)

    Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    // res1: List[Int] = List(1, 10, 2, 20, 3, 30)

    import cats.instances.vector._ // for Monad

    Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))
    // res2: Vector[Int] = Vector(1, 10, 2, 20, 3, 30)

    import cats.instances.future._ // for Monad
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
    Await.result(future, 1.second)
  }

  object MonadSyntaxExample1 {

    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad
    import cats.syntax.applicative._ // for pure

    1.pure[Option]
    // res4: Option[Int] = Some(1)

    1.pure[List]
    // res5: List[Int] = List(1)
  }


  object MonadSyntaxExample2 {

    import cats.Monad
    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x * x + y * y))

    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad

    sumSquare(Option(3), Option(4))
    // res8: Option[Int] = Some(25)

    sumSquare(List(1, 2, 3), List(4, 5))
    // res9: List[Int] = List(17, 26, 20, 29, 25, 34)

    def sumSquare2[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
      for {
        x <- a
        y <- b
      } yield x * x + y * y

    sumSquare2(Option(3), Option(4))
    // res10: Option[Int] = Some(25)

    sumSquare2(List(1, 2, 3), List(4, 5))
    // res11: List[Int] = List(17, 26, 20, 29, 25, 34)

    import cats.Id

    sumSquare2(3: Id[Int], 4: Id[Int])

    "Dave": Id[String]
    // res3: cats.Id[String] = Dave

    123: Id[Int]
    // res4: cats.Id[Int] = 123

    List(1, 2, 3): Id[List[Int]]
    // res5: cats.Id[List[Int]] = List(1, 2, 3)

    val a = Monad[Id].pure(3)
    // a: cats.Id[Int] = 3

    val b = Monad[Id].flatMap(a)(_ + 1)
    // b: cats.Id[Int] = 4

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    for {
      x <- a
      y <- b
    } yield x + y
    // res6: cats.Id[Int] = 7

  }

  object MyIdExample {

    trait Monad[F[_]] {
      def pure[A](value: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

      def map[A, B](value: F[A])(func: A => B): F[B] =
        flatMap(value)(a => pure(func(a)))
    }

    type MyId[A] = A

    implicit val myIdMonad: Monad[MyId] = new Monad[MyId] {
      override def flatMap[A, B](fa: MyId[A])(f: A => MyId[B]): MyId[B] = f(fa)

      override def pure[A](x: A): MyId[A] = x
    }

    val tmp: MyId[Int] = myIdMonad.pure(1)
  }

  object EitherExample {
    val either1: Either[String, Int] = Right(10)
    val either2: Either[String, Int] = Right(32)

    for {
      a <- either1
      b <- either2
    } yield a + b
    // res0: scala.util.Either[String,Int] = Right(42)

    import cats.syntax.either._ // for asRight

    val a = 3.asRight[String]
    // a: Either[String,Int] = Right(3)

    val b = 4.asRight[String]
    // b: Either[String,Int] = Right(4)

    for {
      x <- a
      y <- b
    } yield x * x + y * y
    // res4: scala.util.Either[String,Int] = Right(25)

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
        if (num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        }
      }

    countPositive(List(1, 2, 3))
    // res5: Either[String,Int] = Right(3)

    countPositive(List(1, -2, 3))
    // res6: Either[String,Int] = Left(Negative. Stopping!)

    Either.catchOnly[NumberFormatException]("foo".toInt)
    // res7: Either[NumberFormatException,Int] = Left(java.lang.NumberFormatException: For input string: "foo")

    Either.catchNonFatal(sys.error("Badness"))
    // res8: Either[Throwable,Nothing] = Left(java.lang.RuntimeException: Badness)

    Either.fromTry(scala.util.Try("foo".toInt))
    // res9: Either[Throwable,Int] = Left(java.lang.NumberFormatException: For input string: "foo")

    Either.fromOption[String, Int](None, "Badness")
    // res10: Either[String,Int] = Left(Badness)

    import cats.syntax.either._

    "Error".asLeft[Int].getOrElse(0)
    // res11: Int = 0

    "Error".asLeft[Int].orElse(2.asRight[String])
    // res12: Either[String,Int] = Right(2)

    (-1).asRight[String].ensure("Must be non-negative!")(_ > 0)
    // res13: Either[String,Int] = Left(Must be non-negative!)

    "error".asLeft[Int].recover {
      case str: String => -1
    }
    // res14: Either[String,Int] = Right(-1)

    "error".asLeft[Int].recoverWith {
      case str: String => Right(-1)
    }
    // res15: Either[String,Int] = Right(-1)

    "foo".asLeft[Int].leftMap(_.reverse)
    // res16: Either[String,Int] = Left(oof)

    6.asRight[String].bimap(_.reverse, _ * 7)
    // res17: Either[String,Int] = Right(42)

    "bar".asLeft[Int].bimap(_.reverse, _ * 7)
    // res18: Either[String,Int] = Left(rab)

    123.asRight[String]
    // res19: Either[String,Int] = Right(123)

    123.asRight[String].swap
    // res20: scala.util.Either[Int,String] = Left(123)
  }

  object ExampleErrorHandling {

    import cats.syntax.either._

    for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "DIV0".asLeft[Int]
      else (a / b).asRight[String]
    } yield c * 100
  }

  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String)
    extends LoginError

  final case class PasswordIncorrect(username: String)
    extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError =>
        println(s"Unexpected error")
    }

}

object MonadErrorDefExample {

  trait MonadError[F[_], E] extends Monad[F] {
    // Lift an error into the `F` context:
    def raiseError[A](e: E): F[A]

    // Handle an error, potentially recovering from it:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

}

object MonadErrorExample {

  import cats.MonadError
  import cats.instances.either._ // for MonadError

  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  // success: ErrorOr[Int] = Right(42)

  val failure = monadError.raiseError("Badness")
  // failure: ErrorOr[Nothing] = Left(Badness)

  monadError.handleError(failure) {
    case "Badness" =>
      monadError.pure("It's ok")

    case other =>
      monadError.raiseError("It's not ok")
  }

  monadError.ensure(success)("Number too low!")(_ > 1000)

  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._ // for ensure

  val success1 = 42.pure[ErrorOr]
  // success: ErrorOr[Int] = Right(42)

  val failure1 = "Badness".raiseError[ErrorOr, Int]
  // failure: ErrorOr[Int] = Left(Badness)

  success1.ensure("Number to low!")(_ > 1000)
  // res4: Either[String,Int] = Left(Number to low

  import scala.util.Try
  import cats.instances.try_._ // for MonadError

  val exn: Throwable =
    new RuntimeException("It's all gone wrong")

  exn.raiseError[Try, Int]
  // res6: scala.util.Try[Int] = Failure(java.lang.RuntimeException: It's all gone wrong)

}

object EvaluationExamples {
  val x = {
    println("Computing X")
    math.random
  }

  def y = {
    println("Computing Y")
    math.random
  }

  lazy val z = {
    println("Computing Z")
    math.random
  }

  def main(args: Array[String]): Unit = {
    println("main")
    x // first access
    // res0: Double = 0.013533499657218728
    println(x)
    x // second access
    println(x)
    // res1: Double = 0.013533499657218728

    println(y)
    println(y)

    println(z)
    println(z)
  }
}

object EvalExample {

  import cats.Eval

  val now = Eval.now(math.random + 1000)
  // now: cats.Eval[Double] = Now(1000.337992547842)

  val later = Eval.later(math.random + 2000)
  // later: cats.Eval[Double] = cats.Later@37f34fd2

  val always = Eval.always(math.random + 3000)
  // always: cats.Eval[Double] = cats.Always@486516b

  now.value
  // res6: Double = 1000.337992547842

  later.value
  // res7: Double = 2000.863079768816

  always.value
  // res8: Double = 3000.710688646907

  val x = Eval.now {
    println("Computing X")
    math.random
  }

  val y = Eval.always {
    println("Computing Y")
    math.random
  }

  val z = Eval.later {
    println("Computing Z")
    math.random
  }

  val greeting = Eval.
    always {
      println("Step 1");
      "Hello"
    }.
    map { str => println("Step 2"); s"$str world" }

  val ans = for {
    a <- Eval.now {
      println("Calculating A");
      40
    }
    b <- Eval.always {
      println("Calculating B");
      2
    }
  } yield {
    println("Adding A and B")
    a + b
  }

  val saying = Eval.
    always {
      println("Step 1");
      "The cat"
    }.
    map { str => println("Step 2"); s"$str sat on" }.
    memoize.
    map { str => println("Step 3"); s"$str the mat" }

  def factorial(n: BigInt): BigInt =
    if (n == 1) n else n * factorial(n - 1)

  def factorialT(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      // factorialT(n - 1).map(_ * n)
      Eval.defer(factorialT(n - 1).map(_ * n))
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightT[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldRightT(tail, acc)(fn).map(b => fn(head, b)))
      case Nil =>
        Eval.now(acc)
    }

  def main(args: Array[String]): Unit = {
    println("main")
    println(x.value)
    println(x.value)

    println(y.value)
    println(y.value)

    println(z.value)
    println(z.value)

    println(greeting.value)

    println("**************")

    println(ans.value)
    println(ans.value)

    println("**************")
    println(saying.value)
    println(saying.value)


    // factorial(50000) stackoverflow

    println(factorialT(50000).value)

    // println(foldRight((1 to 100000).toList, 0L)(_ + _)) stackoverflow
    println(foldRightT((1 to 100000).toList, 0L)(_ + _).value)
  }
}

object WriterExample {

  import cats.data.Writer
  import cats.instances.vector._ // for Monoid

  val ex = Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859)

  import cats.instances.vector._ // for Monoid
  import cats.syntax.applicative._ // for pure

  type Logged[A] = Writer[Vector[String], A]

  123.pure[Logged]
  // res2: Logged[Int] = WriterT((Vector(),123))

  import cats.syntax.writer._ // for tell

  Vector("msg1", "msg2", "msg3").tell
  // res3: cats.data.Writer[scala.collection.immutable.Vector[String],Unit] = WriterT((Vector(msg1, msg2, msg3),()))

  import cats.syntax.writer._ // for writer

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  // a: cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] = WriterT((Vector(msg1, msg2, msg3),123))

  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
  // b: cats.data.Writer[scala.collection.immutable.Vector[String],Int] = WriterT((Vector(msg1, msg2, msg3),123))

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )

  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }

  val writer5 = writer1.reset

  val writer6 = writer1.swap

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorialW(n: Int): Logged[Int] = for {
    ans <- slowly(if (n == 0) 1.pure[Logged] else factorialW(n - 1).map(a => a * n))
    _ <- Vector(Thread.currentThread().toString, System.currentTimeMillis().toString, s"fact $n $ans").tell
  } yield ans

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  def main(args: Array[String]): Unit = {
    println(ex)

    println(a.value)
    println(a.written)

    val (log, result) = b.run

    println(writer1.run)
    println(writer2.run)
    println(writer3.run)
    println(writer4.run)
    println(writer5.run)
    println(writer6.run)

    Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), 5.seconds)

    println("**********************")

    Await.result(Future.sequence(Vector(
      Future(factorialW(3)),
      Future(factorialW(3))
    )), 5.seconds).map(_.run).foreach(println)

  }
}

object ReaderExample {

  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  // defined class Cat

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  catName.run(Cat("Garfield", "lasagne"))
  // res0: cats.Id[String] = Garfield

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")

  greetKitty.run(Cat("Heathcliff", "junk food"))
  // res1: cats.Id[String] = Hello Heathcliff

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne"))
  // res3: cats.Id[String] = Hello Garfield. Have a nice bowl of lasagne.

  greetAndFeed(Cat("Heathcliff", "junk food"))
  // res4: cats.Id[String] = Hello Heathcliff. Have a nice bowl of junk food.
}

object HackingOnReader {

  import cats._
  import cats.implicits._

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader { db =>
    db.usernames.get(userId)
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader { db =>
    db.passwords.get(username).contains(password)
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    ou <- findUsername(userId)
    result <- ou match {
      case None => false.pure[DbReader]
      case Some(userName) => checkPassword(userName, password)
    }
  } yield result

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  def main(args: Array[String]): Unit = {
    println(checkLogin(1, "zerocool").run(db))

    println(checkLogin(4, "davinci").run(db))
  }

}

object StateExample {

  import cats.data.State
  import cats._
  import cats.implicits._


  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // Get the state and the result:
  val (state, result) = a.run(10).value
  // state: Int = 10
  // result: String = The state is 10

  // Get the state, ignore the result:
  val state1 = a.runS(10).value
  // state: Int = 10

  // Get the result, ignore the state:
  val result1 = a.runA(10).value
  // result: String = The state is 10


  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }
  // step1: cats.data.State[Int,String] = cats.data.IndexedStateT@76122894

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }
  // step2: cats.data.State[Int,String] = cats.data.IndexedStateT@1eaaaa5d

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)
  // both: cats.data.IndexedStateT[cats.Eval,Int,Int,(String, String)] = cats.data.IndexedStateT@47a10835

  val (state2, result2) = both.run(20).value
  // state: Int = 42
  // result: (String, String) = (Result of step1: 21,Result of step2: 42)


  val getDemo = State.get[Int]
  // getDemo: cats.data.State[Int,Int] = cats.data.IndexedStateT@6ffe574a

  getDemo.run(10).value
  // res3: (Int, Int) = (10,10)

  val setDemo = State.set[Int](30)
  // setDemo: cats.data.State[Int,Unit] = cats.data.IndexedStateT@4168bec2

  setDemo.run(10).value
  // res4: (Int, Unit) = (30,())

  val pureDemo = State.pure[Int, String]("Result")
  // pureDemo: cats.data.State[Int,String] = cats.data.IndexedStateT@6812d576

  pureDemo.run(10).value
  // res5: (Int, String) = (10,Result)

  val inspectDemo = State.inspect[Int, String](_ + "!")
  // inspectDemo: cats.data.State[Int,String] = cats.data.IndexedStateT@37c08614

  inspectDemo.run(10).value
  // res6: (Int, String) = (10,10!)

  val modifyDemo = State.modify[Int](_ + 1)
  // modifyDemo: cats.data.State[Int,Unit] = cats.data.IndexedStateT@4242cae6

  modifyDemo.run(10).value
  // res7: (Int, Unit) = (11,())

  import State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(program.run(1).value)
    // state: Int = 3
    // result: (Int, Int, Int) = (1,2,3000)
  }

}

object PostOrderCalculator {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  type Numbers = (Int, Int)

  def popTwo(li: List[Int]): (Numbers, List[Int]) = li match {
    case l :: r :: tail => ((l, r), tail)
    case _ => throw new RuntimeException("Syntax error")
  }

  def isOperator(sym: String) = sym == "+" || sym == "-" || sym == "*" || sym == "/"

  def evalOperator(operator: String): CalcState[Int] = State { li =>
    val ((left, right), s) = popTwo(li)
    val value = if (operator == "+") {
      left + right
    } else if (operator == "-") {
      left - right
    } else if (operator == "*") {
      left * right
    } else if (operator == "/") {
      left / right
    } else {
      throw new RuntimeException("Syntax error")
    }
    (value :: s, value)
  }

  def evalNumeric(numeric: String): CalcState[Int] = State { li =>
    val value = numeric.toInt
    (value :: li, value)
  }

  def evalOne(sym: String): CalcState[Int] =
    if (isOperator(sym)) {
      evalOperator(sym)
    } else {
      evalNumeric(sym)
    }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0)) { case (s, sym) => s.flatMap(_ => evalOne(sym)) }

  val program2 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  def main(args: Array[String]): Unit = {
    println(evalOne("+").run(List(1, 2)).value)
    println(evalOne("-").run(List(1, 2)).value)
    println(evalOne("*").run(List(3, 2)).value)
    println(evalOne("/").run(List(4, 2)).value)
    println(evalOne("3").run(List(1, 2)).value)
    println(program.run(List()).value)
    println(evalAll(List("1", "2", "+", "3", "*")).run(List()).value)
    println(program2.runA(Nil).value)
    println(evalInput("1 2 + 3 4 + *"))
  }
}

object DefiningCustomMonads {

  import cats.Monad
  import scala.annotation.tailrec

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt flatMap fn

    def pure[A](opt: A): Option[A] =
      Some(opt)

    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }
}

object BranchingOutFurtherWithMonads {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    // not tail rec
    def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(func(a)) {
        case Left(value) =>
          tailRecM(value)(func)
        case Right(value) =>
          Leaf(value)
      }

    def tailRecM2[A, B](arg: A)
                       (func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)
          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(func(arg)), Nil).head
    }
  }

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

  for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

}
