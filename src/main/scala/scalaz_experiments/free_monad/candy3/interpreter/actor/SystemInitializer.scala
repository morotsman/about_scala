package scalaz_experiments.free_monad.candy3.interpreter.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import scalaz_experiments.free_monad.candy3.interpreter.actor.MachineActor.MachineRequest

object SystemInitializer {

  case class Setup(replyTo: ActorRef[SystemContext])

  case class SystemContext(machineActor: ActorRef[MachineRequest])

  def setup(): Behavior[Setup] =
    Behaviors.setup { context =>
      val ref: ActorRef[MachineRequest] = context.spawn(MachineActor(), "machine")

      Behaviors.receiveMessage { message =>
        message.replyTo ! SystemContext(ref)
        Behaviors.same
      }
    }
}
