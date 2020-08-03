package scalaz_experiments.validation

sealed trait Validator[+E, +T]

object Validator {
  type Validated[T] = Validator[String, T]
}

case class Valid[+T](t: T) extends Validator[Nothing, T]

case class Invalid[+E](es: Vector[E]) extends Validator[E, Nothing]

object Invalid {
  def apply[E](e: E): Invalid[E] = Invalid(Vector(e))
}


