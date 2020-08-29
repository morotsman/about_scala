package scalaz_experiments.free_monad.candy3.pure

case class MachineState(id: Option[Long] = None, locked: Boolean, candies: Int, coins: Int)
