package cats.scala_with_cats.chapter6

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
