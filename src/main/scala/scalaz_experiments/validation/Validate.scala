package scalaz_experiments.validation

import scalaz.Kleisli

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final case class ValidationException(message: String) extends Exception(message)

object Validate {

  type FieldValidator[T] = Kleisli[({type f[x] = Either[ValidationException, x]})#f, T, T]

  // alias for Kleisli
  def apply[T]: FieldValidator[T] =
    Kleisli(t => Right(t))

  def maxLengthString[T](max: Int): Validator[String] =
    Validator[String](
      v => v.length <= max,
      (t, v, f) => s"Length on $f was ${v.length} but it must be less or equal to $max for $t"
    )

  // TODO For some reason FT <: Any {def size: Int} doesn't work with String, revisit this when you know more!
  def minLengthString[T](min: Int): Validator[String] =
    Validator[String](
      v => v.length >= min,
      (t, v, f) => s"Length on $f was ${v.length} but it must be greater or equal to $min for $t"
    )

  final class MinLengthHelper[FT <: Any {def size: Int}] {
    def apply[T](min: Int): Validator[FT] =
      Validator[FT](
        v => v.size >= min,
        (t, v, f) => s"Length on $f was ${v.size} but it must be greater or equal to $min for $t"
      )
  }

  // "curry" the type parameters to enable type inference on second type parameter (the type of the object that we should validate)
  def minLength[FT <: Any {def size: Int}] = new MinLengthHelper[FT]

  final class MaxLengthHelper[FT <: Any {def size: Int}] {
    def apply[T](max: Int): Validator[FT] =
      Validator[FT](
        v => v.size <= max,
        (t, v, f) => s"Length on $f was ${v.size} but it must be less or equal to $max for $t"
      )
  }

  def maxLength[FT <: Any {def size: Int}] = new MaxLengthHelper[FT]

  def maxValue[T](max: Int): Validator[Integer] =
    Validator[Integer](
      v => v <= max,
      (t, v, f) => s"Value on $f was $v but it must be less or equal to $max for $t"
    )

  def minValue[T](min: Int): Validator[Integer] =
    Validator[Integer](
      value => value >= min,
      (t, v, f) => s"Value on $f was $v but it must be greater or equal to $min for $t"
    )

  final case class Validator[FT](op: FT => Boolean, errorMessage: (String, FT, String) => String) {
    def on[T](fieldName: String)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T], ctf: ClassTag[FT], ttf: TypeTag[FT]): FieldValidator[T] = {
      validateField(fieldName)
      Kleisli((t: T) => {
        for {
          value <- getValue(t, fieldName)
          result <- if (op(value)) Right(t) else Left(ValidationException(errorMessage(t.toString, value, fieldName)))
        } yield result
      })
    }

    def &&(v: Validator[FT])(errorMessage: (String, FT, String) => String): Validator[FT] =
      Validator(ft => this.op(ft) && v.op(ft), errorMessage)

    def ||(v: Validator[FT])(errorMessage: String): Validator[FT] =
      Validator(ft => this.op(ft) || v.op(ft), (_,_,_) => errorMessage)

    private def validateField[T](field: String)(implicit m: Manifest[T]): Unit = {
      val fieldExists = manifest[T].runtimeClass.getMethods.exists(m => m.getName == field)
      if (!fieldExists) throw new RuntimeException(s"Could not find a field called $field")
    }

    private def getValue[T](obj: T, field: String)(implicit ct: ClassTag[T], tt: TypeTag[T], ct1: ClassTag[FT], tt1: TypeTag[FT]): Either[ValidationException, FT] = {
      try {
        val symbol = typeOf[T].member(TermName(field)).asMethod
        val m = runtimeMirror(obj.getClass.getClassLoader)
        val im = m.reflect(obj)
        val value: FT = cast[FT](im.reflectMethod(symbol).apply())
        Right(value)
      } catch {
        case e: ClassCastException => Left(ValidationException(s"Wrong type on field $field:  $e"))
        case e: ScalaReflectionException => Left(ValidationException(s"Could not find a field called $field: $e"))
      }
    }

    // https://stackoverflow.com/questions/6686992/scala-asinstanceof-with-parameterized-types
    private def cast[A <: Any : Manifest](a: Any): A =
      manifest[A].runtimeClass.cast(a).asInstanceOf[A]
  }

}
