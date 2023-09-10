package lessons.designpatterns.behavioral

import scala.collection.mutable

// The interpreter design pattern is useful for specifying how to
// evaluate sentences in a language by representing it using classes
// and building syntax trees to evaluate the language expressions

// Interpreter has 3 components
// 1. AbstractExpression
// 2. TerminalExpression
// 3. NonTerminalExpression

// Abstract expression
trait Expression {
  def interpret():Int
}

// Terminal Expression
case class Number(n:Int) extends Expression {
  override def interpret(): Int = n
}

class Add(left:Expression, right:Expression) extends Expression {
  override def interpret(): Int = left.interpret() + right.interpret()
}

class Subtract(left:Expression, right:Expression) extends Expression {
  override def interpret(): Int = left.interpret() - right.interpret()
}

object InterpreterTest {
  val expressionExample = "+ 1 2 - 3"
  // Build parser
  // interpret
}
