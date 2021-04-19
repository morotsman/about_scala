package try_cats.scala_with_cats.chapter1

object Exercise1 {

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = (a: String) => a

    implicit val intPrintable = new Printable[Int] {
      override def format(a: Int): String = a.toString
    }
  }

  object Printable {
    def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

    def print[A](a: A)(implicit p: Printable[A]): Unit = System.out.println(p.format(a))
  }

  final case class Cat(name: String, age: Int, color: String)

  object CatInstances {

    import PrintableInstances._

    implicit val catPrintable = new Printable[Cat] {
      def format(cat: Cat) = {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)
        s"$name is a $age year-old $color cat."
      }
    }
  }

  object PrintableSyntax {

    implicit class PrintableOps[A](a: A) {
      def format(implicit printable: Printable[A]): String = Printable.format(a)

      def print(implicit printable: Printable[A]): Unit = Printable.print(a)
    }

  }

  def main(args: Array[String]): Unit = {
    import Printable._
    import PrintableInstances._
    import PrintableSyntax._
    import CatInstances._

    print("hepp")

    Cat("Gustav", 10, "Orange").print
  }

}
