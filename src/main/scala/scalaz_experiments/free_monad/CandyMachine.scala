package scalaz_experiments.free_monad

import scalaz._
import Scalaz._

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)



object CandyMachine  {
  def main(args: Array[String]): Unit = {
    println("test")
  }
}
