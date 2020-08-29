package cats.scala_with_cats.chapter1

import cats._
import cats.implicits._

object Exercise2 {


  final case class Cat(name: String, age: Int, color: String)

  implicit val showCat: Show[Cat] =
    Show.show(c => {
      val name = c.name.show
      val age = c.age.show
      val color = c.color.show
      s"$name is a $age year-old $color cat."
    })


  def main(args: Array[String]): Unit = {
    println(Cat("Gustav", 10, "Orange").show)
  }

}
