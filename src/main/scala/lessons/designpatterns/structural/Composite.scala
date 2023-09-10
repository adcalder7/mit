package lessons.designpatterns.structural

// When group of objects are treated the

// File and Folder are the same because they both ARE FolderItems

trait FolderItem {
  def name:String
  def print(prefix:String):Unit
}

class File(fileName:String) extends FolderItem {
  override def name: String = fileName
  override def print(prefix:String): Unit = println(s"$prefix/$name")
}

class Folder(directoryName:String) extends FolderItem {
  override def name: String = directoryName
  private val children = collection.mutable.ListBuffer[FolderItem]()

  override def print(prefix:String): Unit = {
    children.foreach(_.print(s"$prefix/$directoryName"))
  }

  def add(child:FolderItem):Unit = {
    children += child
  }

  def remove():Unit = {
    if (children.nonEmpty) {
      children.remove(0)
    }
  }
}

object CompositeTest extends App {
  val rootFolder = new Folder("root")
  rootFolder.add(new File("log.txt"))
  rootFolder.add(new File("error.txt"))

  val usrFolder = new Folder("usr")
  rootFolder.add(usrFolder)
  usrFolder.add(new File("ledger.txt"))
  usrFolder.add(new File("audit.txt"))

  rootFolder.print("")
}