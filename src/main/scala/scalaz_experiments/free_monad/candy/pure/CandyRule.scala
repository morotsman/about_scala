package scalaz_experiments.free_monad.candy.pure

object CandyRule {
  def applyRule(input: Input)(machine: MachineState): (MachineState, String) = input match {
    case Coin =>
      if (machine.candies == 0) {
        (machine, "No candies Left")
      } else if (machine.locked) {
        val unlocked = false
        (MachineState(unlocked, machine.candies, machine.coins + 1), "Unlocked, turn to get your candy")
      } else {
        (machine, "Could not accept coin, turn to get a candy")
      }
    case Turn =>
      if (machine.candies == 0) {
        (machine, "No candies Left")
      } else if (!machine.locked) {
        val locked = true
        (MachineState(locked, machine.candies - 1, machine.coins), "Here is your candy")
      } else {
        (machine, "You need to dispose a coin to get a candy")
      }
  }
}
