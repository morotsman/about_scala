package try_cats.scala_with_cats.chapter1

import cats._
import cats.implicits._
import java.util.Date

object ShowExample {
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  /*
  implicit val dateShow: Show[Date] =
    new Show[Date] {
      def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }
   */

  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch.")

  def main(args: Array[String]): Unit = {
    val result: String = showInt.show(2)
    println(result)
    println(4.show)
  }
}
