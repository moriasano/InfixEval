/****************************************
 ** Infix Expression Calculator        **
 ** Mori Asano 11/19/16                **
 ** COSC 455, Programming Assignment 2 **
 ****************************************/

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable.Stack

trait constants {
  /** Definitions used across both objects **/
  val console = scala.io.StdIn  // For increased readability
  val numbers = for(i <- List.range(0, 10)) yield i.toString
  val symbols = List("+", "-", "*", "/", "^", ">", "<", "!", "(", ")")
  def gcd(x: Int,y: Int): Int = y match {
    /** Greatest Common Divisor **/

    case 0 => x.abs
    case _ => gcd(y, x % y)
  }
  def lcm(x: Int,y: Int): Int = {
    /** Least Common Multiple**/

    (x * y).abs / gcd(x, y)
  }
}

object InfixEval extends constants {
  /** Takes infix an arithmetic expression from the user,  **
   ** displays the S-Expression & numerical result         **/

  def main(args: Array[String]): Unit = {
    /** Wrapper to provide looping **/

    println(" ********** Infix Evaluation ********** ")
    go()
    println("Bye!")
  }
  def go(): Unit = {
    /** Main Control Flow **/

    val infixExpression = getInfix()
    val postfixExpression = RPN.ShuntingYard.convertFromInfix(infixExpression)
    // val sExpression = ...
    val numericValue = RPN.ShuntingYard.eval(postfixExpression)

    println(infixExpression)
    println(postfixExpression)
    println(numericValue)

    // Tail recursion for looping
    if(Array("yes", "y").contains(console.readLine("Again? (y/n)").toLowerCase)) { go() }
  }

  def getInfix(): String = {
    /** Prompt the user for a string w/ input validation **/

    val expression = console.readLine("Enter an expression: ")

    // Check that every character is in 'numbers' or 'symbols'
    if(!expression.forall(c => (numbers ::: symbols).contains(c.toString))) {
      println("Invalid input. Try again.")
      getInfix()
    }
    else { expression }
  }
}

// TODO: factorial is not working
object RPN extends constants {
  /** Grammar definition for expression in Reverse Polish Notation    **
   ** reference: blog.xebia.com/shuntingyard-algorithm-in-scala/      **/

  abstract class Expression { def rpnExpression: String }
  case class BinaryOperator(left: Expression, operation: String, right: Expression) extends Expression {
    def rpnExpression: String = left.rpnExpression + " " + right.rpnExpression + " " + operation
  }
  case class UnaryOperator(x: Expression, operation: String) extends Expression {
    def rpnExpression: String = x.rpnExpression + " " + operation
  }
  case class Number(value: String) extends Expression { def rpnExpression: String = value }

  object ShuntingYard extends StandardTokenParsers {
    /** Various methods for creation / conversion of RPN expression **/

    lexical.delimiters ++= InfixEval.symbols

    // Infix Expression Grammar
    def value: Parser[Expression] = numericLit ^^ { s => Number(s) }
    def parenthesis: Parser[Expression] = "(" ~> expression <~ ")"
    def term = value | parenthesis // | factorial
    // def factorial: Parser[Expression] = term ~ "!" ^^ { case x ~ _ => UnaryOperator(x, "!") } | term
    def power: Parser[Expression] = term ~ "^" ~ power ^^ { case left ~ _ ~ right => BinaryOperator(left, "^", right) } | term
    def GcfLcm = power * ("<" ^^^ { (left: Expression, right: Expression) => BinaryOperator(left, "<", right)} |
                          ">" ^^^ { (left: Expression, right: Expression) => BinaryOperator(left, ">", right)} )
    def MulDiv = GcfLcm* ("*" ^^^ { (left: Expression, right: Expression) => BinaryOperator(left, "*", right)} |
                          "/" ^^^ { (left: Expression, right: Expression) => BinaryOperator(left, "/", right)} )
    def AddSub = MulDiv* ("+" ^^^ { (left: Expression, right: Expression) => BinaryOperator(left, "+", right)} |
                          "-" ^^^ { (left: Expression, right: Expression) => BinaryOperator(left, "-", right)} )
    def expression = AddSub | term

    // Parse the input string
    def parse(s: String) = {
      val tokens = new lexical.Scanner(s)
      phrase(expression)(tokens)
    }

    def convertFromInfix(expressionString: String): String = expressionString match {
      case null => ""
      case ""   => ""
      case _    =>
        parse(expressionString) match {
          case Success(tree, _) => tree.rpnExpression
          case e: NoSuccess     => e.toString
        }
    }
    def eval(rpnExpression: String): Int = {
      /** Get the numeric result from the RPN expression **/

      val stack: Stack[Int] = new Stack[Int]
      for (token <- rpnExpression.split(" ")) {
        if (InfixEval.symbols.contains(token.toString)) token match {
          case "+" => stack.push(stack.pop + stack.pop)
          case "-" => val x = stack.pop; stack.push(stack.pop - x)
          case "*" => stack.push(stack.pop * stack.pop)
          case "/" => val x = stack.pop; stack.push(stack.pop / x)
          case "^" => val x = stack.pop; stack.push(math.pow(stack.pop, x).toInt)
          case ">" => stack.push(gcd(stack.pop, stack.pop))
          case "<" => stack.push(lcm(stack.pop, stack.pop))
          // case "!" =>
        }
        else stack.push(token.toInt)
      }
      stack.pop
    }
    def toPolish(rpnExpression: String): String = {
      /** Convert the RPN to Polish (Prefix notation) **/

      val stack: Stack[String] = new Stack[String]
      for (token <- rpnExpression.split(" ")) {

      }

    }
  }
}
