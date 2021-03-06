package scalaz_experiments.validation

import scalaz.{Applicative, Bind, Monad}
import scalaz_experiments.validation.Validator.Validated

sealed trait Validator[+E, +T]

object Validator {
  type Validated[T] = Validator[String, T]
}

case class Valid[+T](t: T) extends Validator[Nothing, T]

case class Invalid[+E](es: Vector[E]) extends Validator[E, Nothing]

object Invalid {
  def apply[E](e: E): Invalid[E] = Invalid(Vector(e))
}

object ValidatorApplicative {
  implicit val validationApplicative: Applicative[Validated] = new Applicative[Validated] {
    override def point[A](a: => A): Validated[A] = Valid(a)

    override def ap[A, B](fa: => Validated[A])(f: => Validated[A => B]): Validated[B] = (fa, f) match {
      case (Valid(a), Valid(fab)) => Valid(fab(a))
      case (Valid(_), Invalid(es)) => Invalid(es)
      case (Invalid(es), Valid(_)) => Invalid(es)
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
    }
  }
}

object ValidatorMonad {
  implicit val validationMonad: Monad[Validated] = new Monad[Validated] {
    override def point[A](a: => A): Validated[A] = Valid(a)

    override def bind[A, B](fa: Validated[A])(f: A => Validated[B]): Validated[B] = fa match {
      case Valid(a) => f(a)
      case Invalid(i) => Invalid(i)
    }
  }

  implicit val validationBind: Bind[Validated] = new Bind[Validated] {
    override def bind[A, B](fa: Validated[A])(f: A => Validated[B]): Validated[B] = {
      val m = implicitly[Monad[Validated]]
      m.bind(fa)(f)
    }

    override def map[A, B](fa: Validated[A])(f: A => B): Validated[B] = {
      val m = implicitly[Monad[Validated]]
      m.map(fa)(f)
    }
  }
}




