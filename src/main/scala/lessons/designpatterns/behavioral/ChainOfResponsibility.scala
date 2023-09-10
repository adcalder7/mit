package lessons.designpatterns.behavioral

/*
The purpose of the chain of responsibility design pattern is to decouple the sender
of a request from its receiver by giving multiple objects the chance to handle the request.
 */

// A pipeline basically. Each item handles its own discrete computations; decisions are made based on state
// Example: Chain Dispenser

case class Money(amount:Int)

trait Dispenser {
  def amount:Int
  def next:Option[Dispenser]

  def dispense(money:Money):Unit = {
    if (money.amount >= amount) {
      val coins = money.amount / amount
      val remaining = money.amount % amount

      println(s"Dispensed ${coins}:${amount} with remaining ${remaining}")

      if (remaining > 0 && next.isDefined) next.get.dispense(Money(remaining))
    } else if (next.isDefined) next.get.dispense(money)
  }
}

class Dispenser50(val next: Option[Dispenser]) extends Dispenser {
  override def amount: Int = 50
}

class Dispenser25(val next: Option[Dispenser]) extends Dispenser {
  override def amount: Int = 25
}

class Dispenser10(val next: Option[Dispenser]) extends Dispenser {
  override def amount: Int = 10
}

class Dispenser5(val next: Option[Dispenser]) extends Dispenser {
  override def amount: Int = 5
}

class ATM {
  private val dispenser:Dispenser = {
    val d1 = new Dispenser5(None)
    val d2 = new Dispenser10(Some(d1))
    val d3 = new Dispenser25(Some(d2))
    val d4 = new Dispenser50(Some(d3))
    d4
  }

  def requestMoney(money:Money):Unit = dispenser.dispense(money)
}

object ChainOfResponsibility extends App {
  new ATM().requestMoney(Money(160))
}
