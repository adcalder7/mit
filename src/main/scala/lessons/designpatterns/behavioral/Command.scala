package lessons.designpatterns.behavioral

// Encapsulate the information needed to perform an action at a later stage and pass this information
// to the object that will be running the actual code

/*
The command design pattern is useful for many things, some of which include supporting undo actions,
implementing parallel processing, or simply optimizing code by deferring, and possibly avoiding code execution.

Parts:

Command: We can think of this as the interface and its implementations that are being called by the invoker.

Receiver: This is the object that actually knows how commands are executed.
Think of this as an object that is being passed to the command and then used in the interface method.

Invoker: It invokes the commands by calling their interface method. As we mentioned earlier,
it might not even know what commands are being invoked.

Client: It more or less guides which commands are executed when by using the invoker.
 */

// Receiver
case class Robot(name:String) {
  def clean:Unit = println(s"${name} is cleaning")
  def wash:Unit = println(s"${name} is washing")
}

// Command
trait RobotCommand {
  def execute:Unit
}

case class RobotCommandClean(robot:Robot) extends RobotCommand {
  override def execute: Unit = robot.clean

  override def toString: String = s"${robot.name} cleaned"
}

case class RobotCommandWash(robot:Robot) extends RobotCommand {
  override def execute: Unit = robot.wash

  override def toString: String = s"${robot.name} washed"
}

// Controller
class RobotController {
  private var history:List[RobotCommand] = List[RobotCommand]()

  def runCommand(command:RobotCommand):Unit = {
    history = history :+ command
    command.execute
  }

  def showHistory:Unit = history.foreach(println)
}

// Client
object CommandClient extends App {
  val robot = Robot("Robot1")
  val controller = new RobotController()
  controller.runCommand(RobotCommandClean(robot))
  controller.runCommand(RobotCommandWash(robot))
  controller.showHistory
}
