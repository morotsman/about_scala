package scalaz_experiments.validation

import scalaz.Applicative

sealed trait CMaybe[+T]

object CMaybe {
  type MyMaybe[T] = CMaybe[T]
}

object CNothing extends CMaybe[Nothing]

case class CJust[+T](counter: Int, v: T) extends CMaybe[T]

object NaughtyContainerApplicative {
  implicit val cmaybeApplicative: Applicative[CMaybe] = new Applicative[CMaybe] {
    override def point[A](a: => A): CMaybe[A] = CJust(0, a)

    override def ap[A, B](fa: => CMaybe[A])(fab: => CMaybe[A => B]): CMaybe[B] = (fa, fab) match {
      case (CJust(i1, a), CJust(i2, fa)) => CJust((i1 max i2) + 1, fa(a))
      case (_, _) => CNothing
    }
  }
}
