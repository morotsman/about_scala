package scalaz_experiments.free_monad.candy

import cats.{Id, ~>}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }
import akka.actor.typed.scaladsl.Behaviors


import akka.actor.Actor


object CandyActor {
  case class UpdateState(f: MachineState => (MachineState, String))
  case class CurrentState()
}

class CandyActor(machine: MachineState) extends Actor {

  var currentMachine = machine

  override def receive: Receive = {
    case UpdateState(f) => {
      val (newMachine, output) = f(currentMachine)
      currentMachine = newMachine
      sender() ! output
    }
    case CurrentState => {
      sender() ! currentMachine
    }
    case _ => println("Handle error")
  }
}

/*
object HelloWorldMain {

  final case class SayHello(name: String)

  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      val greeter = context.spawn(HelloWorld(), "greeter")

      Behaviors.receiveMessage { message =>
        val replyTo = context.spawn(HelloWorldBot(max = 3), message.name)
        greeter ! HelloWorld.Greet(message.name, replyTo)
        Behaviors.same
      }
    }
}
 */

object ActorInpureMachineInterpreter extends (MachineOp ~> Future) {
  //val system: ActorSystem[HelloWorldMain.SayHello] =
  //  ActorSystem(HelloWorldMain(), "Candy")

  private[this] var machine = new MachineState(true, 10, 0)

  def apply[A](fa: MachineOp[A]) = fa match {
    case UpdateState(f) => Future.successful {
      val (newMachine, output) = f(machine)
      machine = newMachine
      output
    }
    case CurrentState() => Future.successful {
      machine
    }
  }
}

object ActorIOInterpreter extends (IOA ~> Future) {
  def apply[A](i: IOA[A]) = i match {
    case Read() => for {
      _ <- Future.successful(System.out.print("> "))
      i <- Future.successful(StdIn.readLine())
    } yield i
    case Write(msg) =>
      Future.successful(System.out.println(msg))
  }
}

