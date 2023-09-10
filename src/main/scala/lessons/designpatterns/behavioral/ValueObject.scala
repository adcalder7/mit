package lessons.designpatterns.behavioral

// Good for DTOs

// Good - because X = Y
case class IAmAValueObject(age:Int, name:String)

// Bad - because X != Y
class IAmNotAValueObject(age:Int, name:String)

object ValueObject {

}
