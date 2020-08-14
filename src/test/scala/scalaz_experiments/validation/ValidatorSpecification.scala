package scalaz_experiments.validation

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalaz.Scalaz._
import scalaz._
import scalaz_experiments.validation.Validator.Validated
import ValidatorApplicative._

object ValidatorSpecification extends Properties("Validate") {

  private val applicative = Applicative[Validated]
  private val laws = applicative.applicativeLaw

  private implicit def validatedApplicativeEquals[A]: Equal[Validated[A]] = new Equal[Validated[A]] {
    override def equal(a1: Validated[A], a2: Validated[A]): Boolean = (a1, a2) match {
      case (Invalid(m1), Invalid(m2)) => m1 == m2
      case (Valid(a1), Valid(a2)) => a1 == a2
      case _ => false
    }
  }

  private def validatedGen[A](implicit a: Arbitrary[A]): Gen[Validated[A]] = for {
    isValid <- Arbitrary.arbitrary[Boolean]
    errors <- Gen.listOf[String](Gen.alphaLowerStr)
    a <- Arbitrary.arbitrary[A]
  } yield (if (isValid) Valid(a) else Invalid(errors.toVector))

  implicit def validatedArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Validated[A]] =
    Arbitrary(validatedGen)

  property("identityAp Law") = forAll {(fa: Validated[Int]) =>
    laws.identityAp(fa)
  }

  property("homomorphism Law") = forAll { (aTob: String => Int, a: String) =>
    laws.homomorphism(aTob, a)
  }

  property("interchange Law") = forAll { (a: String, faTob: Validated[String => Int]) =>
    laws.interchange(faTob, a)
  }

  property("mapLikeDerived Law") = forAll { (fa: Validated[String], aTob: String => Int) =>
    laws.mapLikeDerived(aTob, fa)
  }

}
