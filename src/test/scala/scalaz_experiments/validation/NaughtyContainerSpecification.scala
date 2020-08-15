package scalaz_experiments.validation

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import scalaz.Scalaz._
import scalaz._
import NaughtyContainerApplicative._
import scalaz_experiments.validation.CMaybe.MyMaybe

object NaughtyContainerSpecification extends Properties("NaughtyApplicative") {

  private val applicative = Applicative[MyMaybe]
  private val laws = applicative.applicativeLaw

  private implicit def validatedApplicativeEquals[A]: Equal[MyMaybe[A]] = new Equal[MyMaybe[A]] {
    override def equal(a1: MyMaybe[A], a2: MyMaybe[A]): Boolean = {
      a1 == a2
    }
  }

  private def maybeGen[A](implicit a: Arbitrary[A]): Gen[MyMaybe[A]] = for {
    isValid <- Arbitrary.arbitrary[Boolean]
    a <- Arbitrary.arbitrary[A]
  } yield if (isValid) CJust(0, a) else CNothing

  implicit def validatedArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[MyMaybe[A]] =
    Arbitrary(maybeGen)


  property("identityAp Law") = forAll { fa: MyMaybe[Int] =>
    laws.identityAp(fa)
  }

  property("homomorphism Law") = forAll { (aTob: String => Int, a: String) =>
    laws.homomorphism(aTob, a)
  }

  property("interchange Law") = forAll { (a: String, faTob: MyMaybe[String => Int]) =>
    laws.interchange(faTob, a)
  }


  property("composition Law") = forAll { (fbc: MyMaybe[Int => String], fab: MyMaybe[String => Int], fa: MyMaybe[String]) =>
    laws.composition(fbc, fab, fa)
  }

  property("mapLikeDerived Law") = forAll { (fa: MyMaybe[String], aTob: String => Int) =>
    laws.mapLikeDerived(aTob, fa)
  }


}
