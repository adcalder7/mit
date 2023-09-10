package lessons.designpatterns.behavioral

/*
The purpose of the mediator design pattern is to define an object that encapsulates
how a set of other objects interact with each other in order to promote loose
coupling and allow us to vary class interactions independently.

The mediator design pattern is good for keeping coupling between classes
loose in an application. It helps to achieve simplicity and maintainability,
while still allowing us to model complex interactions between objects in our applications.

It can be seen as the facade pattern but it is not because the facade pattern provides an interface
and the mediator pattern just provides a set of policies for object interactions

 */

case class Group(name:String)

// This is a mediator that mediates between Group and Student
trait School {
  val studentToGroup = Map[Student, List[Group]]()
  val groupToStudent = Map[String, List[Student]]()

  def addStudentToGroup(student:Student, group:Group)
  def removeStudentFromGroup(student:Student, group:Group)
}

object Mediator {

}
