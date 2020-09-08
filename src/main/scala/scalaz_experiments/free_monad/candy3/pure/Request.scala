package scalaz_experiments.free_monad.candy3.pure

object Request {

  trait Request

  trait CliRequest extends Request

  case class HelpRequest() extends CliRequest

  case class QuitRequest() extends CliRequest

  trait MachineRequest extends Request

  case class CreateMachine(m: MachineState) extends MachineRequest

  case class GetMachineState(id: Long) extends MachineRequest

  case class InsertCoin(id: Long) extends MachineRequest

  case class Turn(id: Long) extends MachineRequest

}
