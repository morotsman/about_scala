package scalaz_experiments.validation

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalacheck.Properties
import scalaz.Scalaz._
import scalaz._
import scalaz_experiments.validation.Validator.Validated
import ValidatorApplicative._

object ValidatorSpecification extends Properties("Validate") {

  val applicative: Applicative[Validated] = Applicative[Validated]
  val laws: applicative.ApplicativeLaw = applicative.applicativeLaw

  implicit val validatedApplicativeEquals: Equal[Validated[Int]] = new Equal[Validated[Int]] {
    override def equal(a1: Validated[Int], a2: Validated[Int]): Boolean = (a1, a2) match {
      case (Invalid(m1), Invalid(m2)) => m1 == m2
      case (Valid(i1), Valid(i2)) => i1 == i2
      case _ => false
    }
  }

  val intGenerator: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

  val stringGenerator: Gen[String] = Gen.alphaLowerStr

  val booleanGenerator: Gen[Boolean] = Gen.oneOf(List(true, false))

  val stringToIntGen: Gen[String => Int] = Gen.function1[String, Int](Gen.choose(Int.MinValue, Int.MaxValue))

  val intValidatorGenerator: Gen[Validated[Int]] = for {
    isValid <- booleanGenerator
    message <- stringGenerator
    v <- intGenerator
  } yield (if (isValid) Valid(v) else Invalid(message))

  property("identityAp Law") = forAll(intValidatorGenerator) { (fa: Validated[Int]) =>
    laws.identityAp(fa)
  }

  property("homomorphism Law") = forAll { (aTob: String => Int, a: String) =>
    laws.homomorphism(aTob, a)
  }

  val validatorStringToIntGenerator: Gen[(String, Validated[String => Int])] = for {
    isValid <- booleanGenerator
    a <- stringGenerator
    aTob <- stringToIntGen
  } yield (if (isValid) (a, Valid(aTob)) else (a, Invalid(a)))

  property("interchange Law") = forAll(validatorStringToIntGenerator) {
    case (a, faTob) => laws.interchange(faTob, a)
  }

  val generator: Gen[(Validated[String], String => Int)] = for {
    isValid <- booleanGenerator
    a <- stringGenerator
    aTob <- stringToIntGen
  } yield (if (isValid) (Valid(a), aTob) else (Invalid(a), aTob))

  property("mapLikeDerived Law") = forAll(generator) {
    case (fa, aTob) => laws.mapLikeDerived(aTob, fa)
  }

}
