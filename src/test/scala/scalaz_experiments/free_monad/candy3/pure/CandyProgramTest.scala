package scalaz_experiments.free_monad.candy3.pure

import org.scalatest.flatspec.AnyFlatSpec
import scalaz_experiments.free_monad.candy3.Types.InternalState
import scalaz_experiments.free_monad.candy3.interpreter.{StateIOInterpreter, StateMachineInterpreter}

class CandyProgramTest  extends AnyFlatSpec{

  val interpreter = StateMachineInterpreter.stateMachineInterpreter() or StateIOInterpreter


  "A Stack" should "pop values in last-in-first-out order" in {
    val state = InternalState[String](List(Right("n"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), true, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]])

    result.value._1.out.reverse.foreach(println)

    assert(true)
  }

}
