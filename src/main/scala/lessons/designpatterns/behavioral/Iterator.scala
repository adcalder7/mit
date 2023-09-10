package lessons.designpatterns.behavioral

// The iterator design pattern provides a way to access the elements of an
// aggregate object (collection) in a sequential manner without
// exposing the underlying representation of the items.

// Iterators are not thread safe because the collection can change in size
// Making iterators thread safe is a challenge

case class Student(name:String)

class StudentIterator(students:List[Student]) {
  private var pos:Int = -1

  def next():Student = {
    if (!hasNext) return null
    pos += 1
    students(pos)
  }

  def hasNext():Boolean = {
    students.size < pos + 1
  }
}

object IteratorTest {

}
