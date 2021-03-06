package scalaz_experiments.free_monad.candy3.pure

import org.scalatest.flatspec.AnyFlatSpec
import scalaz_experiments.free_monad.candy3.Types.InternalState
import scalaz_experiments.free_monad.candy3.interpreter.{StateIOInterpreter, StateMachineInterpreter}
import scalaz_experiments.free_monad.candy3.pure.Request._

class MachineProgramTest extends AnyFlatSpec {

  val interpreter = StateMachineInterpreter.stateMachineInterpreter() or StateIOInterpreter


  val locked = true
  val unlocked = false

  val initialMachine = MachineState(Some(0), locked, 20, 0)

  "A machineProgram" should "give back the state if asked" in {
    val state = InternalState[String](List(), List(), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.requestHandler(GetMachineState(0L)).value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._2
    val actualState = result._1.machine

    assert(actualOutput == Right(initialMachine))
    assert(actualState == Right(initialMachine))
  }

  "A machineProgram" should "accept a coin" in {
    val state = InternalState[String](List(), List(), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.requestHandler(InsertCoin(0L)).value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._2
    val actualState = result._1.machine

    val expectedMachine = initialMachine.copy(
      coins = initialMachine.coins + 1,
      locked = unlocked
    )

    assert(actualOutput == Right(expectedMachine))
    assert(actualState == Right(expectedMachine))
  }

  "A machineProgram" should "reject a turn if no coin has been disposed" in {
    val state = InternalState[String](List(), List(), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.requestHandler(Turn(0L)).value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._2
    val actualState = result._1.machine

    assert(actualOutput.toString == Left(new IllegalStateException("No coin has been disposed")).toString)
    assert(actualState == Right(initialMachine))
  }

  "A machineProgram" should "accept a turn if a coin has been disposed" in {
    val state = InternalState[String](List(), List(), Right(MachineState(Some(0), unlocked, 20, 1)): Either[Throwable, MachineState])

    val result = CliProgram.requestHandler(Turn(0L)).value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._2
    val actualState = result._1.machine

    val expectedState = Right(MachineState(Some(0), true, 19, 1))
    assert(actualOutput == expectedState)
    assert(actualState == expectedState)
  }

  "A machineProgram" should "be able to create a new machine" in {
    val state = InternalState[String](List(), List(), Right(MachineState(Some(0), unlocked, 20, 1)): Either[Throwable, MachineState])

    val result = CliProgram.requestHandler(CreateMachine(MachineState(Some(1), locked, 40, 1))).value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._2
    val actualState = result._1.machine

    val expectedState = Right(MachineState(Some(1), locked, 40, 1))
    assert(actualOutput == expectedState)
    assert(actualState == expectedState)
  }

}
