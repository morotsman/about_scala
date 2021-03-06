package scalaz_experiments.free_monad.candy.pure

object CandyRule {

  sealed trait Result

  case object NO_CANDIES_LEFT extends Result {
    override def toString: String = "No candies Left"
  }

  case object UNLOCKED extends Result {
    override def toString: String = "Unlocked, turn to get your candy"
  }

  case object ALREADY_UNLOCKED extends Result {
    override def toString: String = "Could not accept coin, turn to get a candy"
  }

  case object CANDY_DISPOSED extends Result {
    override def toString: String = "Here is your candy"
  }

  case object DISPOSE_A_COIN extends Result {
    override def toString: String = "You need to dispose a coin to get a candy"
  }

  def applyRule(input: Input)(machine: MachineState): (MachineState, Result) = input match {
    case Coin =>
      if (machine.candies == 0) {
        (machine, NO_CANDIES_LEFT)
      } else if (machine.locked) {
        val unlocked = false
        (MachineState(unlocked, machine.candies, machine.coins + 1), UNLOCKED)
      } else {
        (machine, ALREADY_UNLOCKED)
      }
    case Turn =>
      if (machine.candies == 0) {
        (machine, NO_CANDIES_LEFT)
      } else if (!machine.locked) {
        val locked = true
        (MachineState(locked, machine.candies - 1, machine.coins), CANDY_DISPOSED)
      } else {
        (machine, DISPOSE_A_COIN)
      }
  }
}
