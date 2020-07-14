package generics.scala

trait Fruit {
  def color(): String
}

trait Eatable extends Fruit {
  def joice(): String
}

trait Toxic extends Fruit {
  def detoxify(): Eatable
}

case class Orange() extends Eatable() {
  override def joice(): String = "Orange joice"
  val color = "Orange"
}

class Apple(val color: String) extends Eatable() {
  override def joice(): String = "Apple joice"

  def removeAppleCores(): String = "Apple cores removed"
}

class ProcessedAckee(val color: String) extends Eatable() {
  override def joice(): String = "Ackee joice"
}

class Ackee extends Toxic {
  override def color(): String = "Red"

  override def detoxify(): ProcessedAckee = new ProcessedAckee("Orange");
}
