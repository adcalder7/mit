package lessons.designpatterns.behavioral

/*
The purpose of the observer design pattern is to have an object (called subject)
that automatically notifies all of its observers of any state change by calling one of their methods.

Example: MVC
 */

trait Observer[T] {
  def handleUpdate(subject:T):Unit
}

trait Observable[T] {
  this: T =>
  private var observers = Set[Observer[T]]()

  def addObserver(observer: Observer[T]): Unit = {
    observers = observers + observer
  }

  def notifyObservers():Unit = {
    observers.foreach(_.handleUpdate(this))
  }
}

case class User(name:String) extends Observer[Post] {
  override def handleUpdate(subject: Post): Unit = {
    System.out.println(s"Hey, I'm ${name}. The post got some new comments: ${subject.comments}")
  }
}

case class Comment(user:User, text:String)

// A post can have replies
case class Post(user:User, text:String) extends Observable[Post] {
  var comments = List[Comment]()

  def addComment(comment:Comment):Unit = {
    comments = comments :+ comment
    notifyObservers()
  }
}

object ObserverTest extends App {

}
