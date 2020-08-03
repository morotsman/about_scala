package scalaz_experiments.validation

import scalaz.{Applicative, Monad}

sealed trait Validator[+E, +T]

object Validator {
  type Validated[T] = Validator[String, T]

  implicit val validationMonad: Monad[Validated] = new Monad[Validated] {
    override def point[A](a: => A): Validated[A] = Valid(a)

    override def bind[A, B](fa: Validated[A])(f: A => Validated[B]): Validated[B] = fa match {
      case Valid(a) => f(a)
      case Invalid(i) => Invalid(i)
    }
  }

  implicit val validationApplicative: Applicative[Validated] = new Applicative[Validated] {
    override def point[A](a: => A): Validated[A] = Valid(a)

    override def ap[A, B](fa: => Validated[A])(f: => Validated[A => B]): Validated[B] = (fa, f) match {
      case (Valid(a), Valid(fab)) => Valid(fab(a))
      case (Valid(a), Invalid(es)) => Invalid(es)
      case (Invalid(es), Valid(a)) => Invalid(es)
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
    }
  }
}

case class Valid[+T](t: T) extends Validator[Nothing, T]

case class Invalid[+E](es: Vector[E]) extends Validator[E, Nothing]

object Invalid {
  def apply[E](e: E): Invalid[E] = Invalid(Vector(e))
}


