package scalaz_experiments.validation

import java.util.concurrent.Executors

import scala.concurrent.duration._
import scala.language.postfixOps
import scalaz._
import Scalaz._
import Validator._
import ValidatorApplicative._



import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, ExecutionContextExecutorService, Future}

object ValidateApplicative {

  case class Person(name: String, age: Int)

  def validateName(name: String): Validated[String] =
    if (name.length > 0 && name.length <= 21) Valid(name)
    else Invalid("Name validation failed")

  def validateAge(age: Int): Validated[Int] =
    if (age >= 0) Valid(age)
    else Invalid("Age failed the validation")

  def main(args: Array[String]): Unit = {
    functor()
    basicExamples()
    futureExample()
    validatorExample()
  }

  def functor(): Unit = {
    def addOne(a: Int) = a + 1

    val of = Functor[Option]
    val result: Option[Int] = of.map(Some(3))(addOne)
    assert(result == Some(4))

    List(1, 2, 3).map(addOne)
    Set(1, 2, 3).map(addOne)
    Some(1).map(addOne)

    val addOneLifted: Option[Int] => Option[Int] = of.lift(addOne)
    val result2: Option[Int] = addOneLifted(Some(3))
    assert(result2 == Some(4))
  }

  def basicExamples(): Unit = {
    val someOne = 1.some
    val someTwo = 2.some
    val someThree = 3.some

    def addOne(a: Int) = a + 1

    val result1 = someOne.map(addOne)
    assert(result1 == Some(2))

    val result2 = someTwo <*> (addOne _).some
    assert(result2 == Some(3))

    def addTwo(a: Int, b: Int): Int = a + b

    val result6 = someOne.map(a => someTwo.map(b => addTwo(a, b)))
    assert(result6 == Some(Some(3)))

    // if we're using Option as a monad we have access to flatMap...
    val result5 = for {
      a <- someOne
      b <- someTwo
    } yield addTwo(a, b)
    assert(result5 == Some(3))

    val result7 = ^(someOne, someTwo)(addTwo)
    assert(result7 == Some(3))

    val result8 = (someOne |@| someTwo) (addTwo)
    assert(result8 == Some(3))

    def addFour(a: Int, b: Int, c: Int, d: Int): Int = a + b + c + d

    val result9 = ^^^(someOne, someTwo, someOne, someTwo)(addFour)
    assert(result9 == Some(6))

    val result10 = (someOne |@| someTwo |@| someOne |@| someTwo) (addFour)
    assert(result10 == Some(6))

    val result11 = someTwo +++ someTwo +++ someTwo
    assert(result11 == Some(6))

    // as semigroup is a monoid without zero
    implicit val times: Semigroup[Int] = new Semigroup[Int] {
      override def append(f1: Int, f2: => Int): Int = f1 * f2
    }

    val ao = Applicative[Option]
    import ao._
    val result12 = plusA(someThree, someThree)(times)
    assert(result12 == Some(9))

    val result13 = someThree.+++(someTwo)(times) +++ someTwo
    assert(result13 == Some(8))

    assert(sequence(List(someOne, someTwo, someThree)) == Some(List(1, 2, 3)))
    assert(sequence(List(None, someTwo, someThree)) == None)

    def all(i: Int): Option[Int] = Some(i)

    assert(traverse(List(1, 2, 3))(all) == Some(List(1, 2, 3)))

    def onlyEven(i: Int): Option[Int] = if (i % 2 == 0) Some(i) else None

    assert(traverse(List(1, 2, 3))(onlyEven) == None)
    assert(traverse(List(2, 4))(onlyEven) == Some(List(2, 4)))

    assert(Some(IList(1, 1, 1)) == replicateM(3, Some(1)))

    def isEven(i: Int): Option[Boolean] = if (i % 2 == 0) Some(true) else None

    assert(filterM(List(1, 2, 3))(isEven) == None)
    assert(filterM(List(2, 4))(isEven) == Some(List(2, 4)))

    val la = Applicative[List]
    val lacao = la.compose(ao)

    val listOfOptions = List(someOne, someTwo, None)
    val result14 = lacao.map(listOfOptions)(a => a + 10)
    assert(result14 == List(Some(11), Some(12), None))

    val lapao = la.product(ao)

    val result15 = lapao.map((List(1, 2), Some(2)))(a => a + 10)
    assert(result15 == (List(11, 12), Some(12)))

    val result16 = apply2(someOne, someTwo)(addTwo)
    assert(result16 == Some(3))
  }

  def futureExample(): Unit = {


    implicit val ctx: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))
    try {
      def successFuture = Future {
        Thread.sleep(900)
        1
      }
      val failedFuture = Future.failed(new RuntimeException("oops"))

      val af = Applicative[Future]
      import af._

      val futureResult1 : Future[List[Int]] = sequence(List(successFuture, successFuture, successFuture))
      val result1 = Await.result(futureResult1, 1000 millis)
      assert(result1 == List(1, 1, 1))

      val futureResult2 : Future[List[Int]] =
        sequence(List(successFuture, successFuture, failedFuture.recoverWith{ case _ => Future.successful(2)}))
      val result2 = Await.result(futureResult2, 1000 millis)
      assert(result2 == List(1, 1, 2))

    } finally {
      ctx.shutdown()
    }
  }

  def validatorExample(): Unit = {
    val validator = Applicative[Validated]

    val person1 = validator.apply2(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    assert(person1 == Invalid(Vector("Age failed the validation", "Name validation failed")))

    val person2: Validated[Person] = ^(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    assert(person2 == Invalid(Vector("Age failed the validation", "Name validation failed")))

    def createPerson(name: String, age: Int): Person =
      Person(name, age)

    val person3: Validated[Person] = validator.lift2(createPerson)(
      validateName("NiklasNiklasNiklasNiklasNiklasNiklas"),
      validateAge(7)
    )
    assert(person3 == Invalid(Vector("Name validation failed")))

    val result: Validated[Person] = (validateName("Niklas") |@| validateAge(30)) { (name: String, age: Int) => Person(name, age) }
    assert(result == Valid(Person("Niklas", 30)))
  }

}
