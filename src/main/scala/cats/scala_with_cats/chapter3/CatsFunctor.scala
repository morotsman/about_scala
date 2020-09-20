package cats.scala_with_cats.chapter3

object FunctionExample {

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  (func1 map func2) (1) // composition using map
  // res7: Double = 2.0

  (func1 andThen func2) (1) // composition using andThen
  // res8: Double = 2.0

  func2(func1(1)) // composition written out by hand
  // res9: Double = 2.0

  val func =
    ((x: Int) => x.toDouble).
      map(x => x + 1).
      map(x => x * 2).
      map(x => x + "!")

  def main(args: Array[String]): Unit = {
    println(func(123))
  }

  // res10: String = 248.0!
}

object TheFunctorTypeClass {

  import cats.Functor
  import cats.instances.list._ // for Functor
  import cats.instances.option._ // for Functor

  val list1 = List(1, 2, 3)
  // list1: List[Int] = List(1, 2, 3)

  val list2 = Functor[List].map(list1)(_ * 2)
  // list2: List[Int] = List(2, 4, 6)

  val option1 = Option(123)
  // option1: Option[Int] = Some(123)

  val option2 = Functor[Option].map(option1)(_.toString)
  // option2: Option[String] = Some(123)

  val func: Int => Int = (x: Int) => x + 1
  // func: Int => Int = <function1>

  val liftedFunc: Option[Int] => Option[Int] = Functor[Option].lift(func)
  // liftedFunc: Option[Int] => Option[Int] = cats.Functor$$Lambda$11698/1847181061@41c6929b

  liftedFunc(Option(1))
  // res0: Option[Int] = Some(2)

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => a + "!"
  val func4 = func1.map(func2).map(func3)

  func4(123)
  // res1: String = 248!

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  import cats.instances.option._ // for Functor
  import cats.instances.list._ // for Functor

  doMath(Option(20))
  // res3: Option[Int] = Some(22)

  doMath(List(1, 2, 3))
  // res4: List[Int] = List(3, 4, 5)

}

object ExampleFunctorOps {

  trait Functor[F[_]] {
    def myMap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_], A](src: F[A]) {
    def myMap[B](func: A => B)(implicit functor: Functor[F]): F[B] =
      functor.myMap(src)(func)
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def myMap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def myMap[A, B](value: Option[A])(func: A => B): Option[B] =
        value.map(func)
    }

  import scala.concurrent.{Future, ExecutionContext}

  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def myMap[A, B](value: Future[A])(func: A => B): Future[B] =
        value.map(func)
    }


  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor = new Functor[Tree] {
    override def myMap[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(myMap(left)(f), myMap(right)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }


  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).myMap(_ + 1))

    // Branch(Leaf(10), Leaf(20)).myMap(_ * 2) // will not compile

    Tree.leaf(100).myMap(_ * 2)
    // res10: wrapper.Tree[Int] = Leaf(200)

    Tree.branch(Tree.leaf(10), Tree.leaf(20)).myMap(_ * 2)
    // res11: wrapper.Tree[Int] = Branch(Leaf(20),Leaf(40))
  }

}

object ContraVariantFunctors {

  trait Printable[A] {
    self =>

    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String = {
          self.format(func(value))
        }
      }
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  format("hello")
  // res3: String = "hello"

  format(true)

  // res4: String = yes

  final case class Box[A](value: A)

  /*
    implicit def boxPrintable[A](implicit p: Printable[A]) =
      new Printable[Box[A]] {
        def format(box: Box[A]): String =
          p.format(box.value)
      }
  */

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](b => b.value)

  format(Box("hello world"))
  // res5: String = "hello world"

  format(Box(true))
  // res6: String = yes

  // format(Box(123)) will not compile
}

object InvariantFunctor {

  trait Codec[A] {
    self =>

    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  case class Box[A](value: A)

  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
    codec.imap[Box[A]](a => Box(a), ba => ba.value)

  def main(args: Array[String]): Unit = {
    println(encode(123.4))

    println(decode[Double]("123.4"))

    println(encode(Box(123.4)))

    println(decode[Box[Double]]("123.4"))
  }

}

object CatsContraAndInvariant {
  trait Contravariant[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  trait Invariant[F[_]] {
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  }
}

object Example {
  import cats.Contravariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]

  val showSymbol: Show[Symbol] = Contravariant[Show].
    contramap(showString)((sym: Symbol) => s"'${sym.name}")

  showSymbol.show('dave)

  import cats.syntax.contravariant._ // for contramap

  showString.contramap[Symbol](_.name).show('dave)

  import cats.Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.invariant._ // for imap
  import cats.syntax.semigroup._ // for |+|

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)

  Monoid[Symbol].empty
  // res5: Symbol = '

  'a |+| 'few |+| 'words
  // res6: Symbol = 'afewwords

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._     // for map

  val func1 = (x: Int)    => x.toDouble
  val func2 = (y: Double) => y * 2

  val func3 = func1.map(func2)



  val func3a: Int => Double =
    a => func2(func1(a))

  val func3b: Int => Double =
    func2.compose(func1)

  import cats.syntax.contravariant._ // for contramap
  // Hypothetical example. This won't actually compile:
  // val func3c: Int => Double =
  //    func2.contramap(func1)

  type <=[B, A] = A => B

  type F[A] = Double <= A

  val func2b: Double <= Double = func2

  val func3c = func2b.contramap(func1)
}
