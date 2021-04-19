package try_cats.scala_with_cats.chapter6

import cats.data.Validated

import scala.util.Try

object SemigroupalExample {

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

  Semigroupal[Option].product(Some(123), Some("abc"))

  Semigroupal[Option].product(None, Some("abc"))

  Semigroupal[Option].product(Some(123), None)

  import cats.instances.option._ // for Semigroupal

  Semigroupal.tuple3(Option(1), Option(2), Option(3))
  // res3: Option[(Int, Int, Int)] = Some((1,2,3))

  Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
  // res4: Option[(Int, Int, Int)] = None

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  // res5: Option[Int] = Some(6)

  Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
  // res6: Option[Int] = None
}

object ApplySyntaxObject {

  import cats.instances.option._ // for Semigroupal
  import cats.syntax.apply._ // for tupled and mapN

  val add: (Int, Int) => Int = (a, b) => a + b
  // add: (Int, Int) => Int = <function2>

  // (Option(1), Option(2), Option(3)).mapN(add)
  // <console>:27: error: type mismatch;
  //  found   : (Int, Int) => Int
  //  required: (Int, Int, Int) => ?
  //        (Option(1), Option(2), Option(3)).mapN(add)
  //                                               ^

  // (Option("cats"), Option(true)).mapN(add)
  // <console>:27: error: type mismatch;
  //  found   : (Int, Int) => Int
  //  required: (String, Boolean) => ?
  //        (Option("cats"), Option(true)).mapN(add)
  //

  def main(args: Array[String]): Unit = {
    println((Option(123), Option("abc")).tupled)

    println((None, Option("abc")).tupled)

    println((Option(123), Option("abc"), Option(true)).tupled)

    case class Cat(name: String, born: Int, color: String)

    println((Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply))
  }

}

object FunctorAndApplySyntax {

  import cats.Monoid
  import cats.instances.int._ // for Monoid
  import cats.instances.invariant._ // for Semigroupal
  import cats.instances.list._ // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.apply._ // for imapN
  import cats.Semigroupal
  import cats.instances.future._ // for Semigroupal
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.Semigroupal
  import cats.instances.list._ // for Semigroupal
  import cats.instances.either._ // for Semigroupal

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat =
    Cat.apply

  val catToTuple: Cat => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

  import cats.syntax.semigroup._ // for |+|

  val garfield = Cat("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

  type ErrorOr[A] = Either[Vector[String], A]

  def main(args: Array[String]): Unit = {
    println(garfield |+| heathcliff)

    val futurePair = Semigroupal[Future].
      product(Future("Hello"), Future(123))

    println(Await.result(futurePair, 1.second))
    // res1: (String, Int) = (Hello,123)

    val futureCat = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
      ).mapN(Cat.apply)

    println(Await.result(futureCat, 1.second))

    println(Semigroupal[List].product(List(1, 2), List(3, 4)))

    println(Semigroupal[ErrorOr].product(
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    ))
  }
}

object ProductOfMonads {

  import cats._
  import cats.implicits._

  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = for {
    a <- x
    b <- y
  } yield (a, b)
}

object ValidatedExample {

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type AllErrorsOr[A] = Validated[List[String], A]

  Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )
  // res1: AllErrorsOr[(Nothing, Nothing)] = Invalid(List(Error 1, Error 2))

  val v1 = Validated.Valid(123)
  // v: cats.data.Validated.Valid[Int] = Valid(123)

  val i2 = Validated.Invalid(List("Badness"))
  // i: cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))

  val v3 = Validated.valid[List[String], Int](123)
  // v: cats.data.Validated[List[String],Int] = Valid(123)

  val i4 = Validated.invalid[List[String], Int](List("Badness"))
  // i: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

  import cats.syntax.validated._ // for valid and invalid

  123.valid[List[String]]
  // res2: cats.data.Validated[List[String],Int] = Valid(123)

  List("Badness").invalid[Int]
  // res3: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError

  type ErrorsOr[A] = Validated[List[String], A]

  123.pure[ErrorsOr]
  // res5: ErrorsOr[Int] = Valid(123)

  List("Badness").raiseError[ErrorsOr, Int]
  // res6: ErrorsOr[Int] = Invalid(List(Badness))

  Validated.catchOnly[NumberFormatException]("foo".toInt)
  // res7: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

  Validated.catchNonFatal(sys.error("Badness"))
  // res8: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeException: Badness)

  Validated.fromTry(scala.util.Try("foo".toInt))
  // res9: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

  Validated.fromEither[String, Int](Left("Badness"))
  // res10: cats.data.Validated[String,Int] = Invalid(Badness)

  Validated.fromOption[String, Int](None, "Badness")
  // res11: cats.data.Validated[String,Int] = Invalid(Badness)

  import cats.instances.string._ // for Semigroup

  Semigroupal[AllErrorsOr]

  import cats.syntax.apply._ // for tupled

  (
    "Error 1".invalid[Int],
    "Error 2".invalid[Int]
    ).tupled
  // res14: cats.data.Validated[String,(Int, Int)] = Invalid(Error 1Error 2)

  import cats.instances.vector._ // for Semigroupal

  (
    Vector(404).invalid[Int],
    Vector(500).invalid[Int]
    ).tupled
  // res15: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, Int)] = Invalid(Vector(404, 500))

  import cats.data.NonEmptyVector

  (
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
    ).tupled
  // res16: cats.data.Validated[cats.data.NonEmptyVector[String],(Int, Int)] = Invalid(NonEmptyVector(Error 1, Error 2))

  123.valid.map(_ * 100)
  // res17: cats.data.Validated[Nothing,Int] = Valid(12300)

  "?".invalid.leftMap(_.toString)
  // res18: cats.data.Validated[String,Nothing] = Invalid(?)

  123.valid[String].bimap(_ + "!", _ * 100)
  // res19: cats.data.Validated[String,Int] = Valid(12300)

  "?".invalid[Int].bimap(_ + "!", _ * 100)
  // res20: cats.data.Validated[String,Int] = Invalid(?!)

  32.valid.andThen { a =>
    10.valid.map { b =>
      a + b
    }
  }
  // res21: cats.data.Validated[Nothing,Int] = Valid(42)

  import cats.syntax.either._ // for toValidated
  // import cats.syntax.either._

  "Badness".invalid[Int]
  // res22: cats.data.Validated[String,Int] = Invalid(Badness)

  "Badness".invalid[Int].toEither
  // res23: Either[String,Int] = Left(Badness)

  "Badness".invalid[Int].toEither.toValidated
  // res24: cats.data.Validated[String,Int] = Invalid(Badness)

  "fail".invalid[Int].getOrElse(0)
  // res26: Int = 0

  "fail".invalid[Int].fold(_ + "!!!", _.toString)
  // res27: String = fail!!!
}

object FormValidation {

  import cats._
  import cats.implicits._

  case class User(name: String, age: Int)

  type Valid[A] = Either[List[String], A]

  def readName(map: Map[String, String]): Valid[String] = for {
    name <- getName(map)
    result <- notEmpty("name", name)
  } yield result

  val getName: Map[String, String] => Valid[String] = getValue("name");

  def getValue(key: String)(map: Map[String, String]): Valid[String] =
    map.get(key).toRight(List(s"Could not find $key"))

  def notEmpty(attribute: String, s: String): Valid[String] =
    if (s != "") Right(s) else Left(List(s"$attribute must not be empty"))

  def readAge(map: Map[String, String]): Valid[Int] = for {
    age <- getAge(map)
    result <- nonNegative("age", age)
  } yield result

  val getAge = (map: Map[String, String]) => for {
    age <- getValue("age")(map);
    result <- parseInt(age)
  } yield result

  def parseInt(s: String): Valid[Int] = Try(s.toInt).toOption.toRight(List("Age must be an integer"))

  def nonNegative(attribute: String, i: Int): Valid[Int] =
    if (i > -1) Right(i) else Left(List(s"$attribute must not negative"))

  def readUser(data: Map[String, String]): Validated[List[String], User] =
    (readName(data).toValidated, readAge(data).toValidated).mapN(User.apply)

  def main(args: Array[String]): Unit = {
    println(readUser(Map("age" -> "20", "name" -> "Joe")))
    println(readUser(Map("age" -> "-1", "name" -> "")))
    println(readUser(Map("age" -> "-1", "name" -> "Joe")))
    println(readUser(Map("age" -> "-1")))
  }

}
