/****************************************
 ** Infix Expression Calculator        **
 ** Mori Asano 11/19/16                **
 ** COSC 455, Programming Assignment 2 **
 ****************************************/

object InfixEval {
  /** Takes infix an arithmetic expression from the user,  **
   ** displays the S-Expression & numerical result         **/

  // Constants
  val c = scala.io.StdIn  // For increased readability
  val numbers = for(i <- List.range(0, 10)) yield i.toString
  val symbols = List("+", "-", "*", "/", "^", ">", "<", "!", "(", ")")

  def main(args: Array[String]): Unit = {
    /** Wrapper to provide looping **/

    println(" ********** Infix Evaluation ********** ")
    go()
    println("Bye!")
  }
  def go(): Unit = {
    /** Main Control Flow **/

    val infixExpression = getInfix()
    val postfixExpression = InfixToRPN.ShuntingYard.convert(infixExpression)
    // val sExpression = ...
    val numericValue = rpnEval(postfixExpression)

    println(infixExpression)
    println(postfixExpression)
    println(numericValue)

    // Tail recursion for looping
    if(Array("yes", "y").contains(c.readLine("Again? (y/n)").toLowerCase)) { go() }
  }

  def rpnEval(rpnExpression: String): Int = {
    /** Get the numeric result from the RPN expression **/

    import scala.collection.mutable.Stack

    val stack: Stack[Int] = new Stack[Int]
    for (token <- rpnExpression.split(" ")) {
      if (symbols.contains(token.toString)) token match {
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

  def gcd(x: Int,y: Int): Int = y match {
    /** Greatest Common Divisor **/

    case 0 => x.abs
    case _ => gcd(y, x % y)
  }
  def lcm(x: Int,y: Int): Int = {
    /** Least Common Multiple**/

    (x * y).abs / gcd(x, y)
  }

  def getInfix(): String = {
    /** Prompt the user for a string w/ input validation **/

    val expression = c.readLine("Enter an expression: ")

    // Check that every char is in 'numbers' or 'symbols'
    if(!expression.forall(ch => (numbers ::: symbols).contains(ch.toString))) {
      println("Invalid input. Try again.")
      getInfix()
    }
    else { expression }
  }
}

// TODO: factorial is not working
object InfixToRPN {
  /** Conversion from infix to RPN w/ Shunting algorithm          **
   ** reference: blog.xebia.com/shuntingyard-algorithm-in-scala/  **/

  import scala.util.parsing.combinator.syntactical.StandardTokenParsers

  abstract class Expression { def rpnExpression: String }
  case class BinaryOperator(left: Expression, operation: String, right: Expression) extends Expression {
    def rpnExpression: String = left.rpnExpression + " " + right.rpnExpression + " " + operation
  }
  case class UnaryOperator(x: Expression, operation: String) extends Expression {
    def rpnExpression: String = x.rpnExpression + " " + operation
  }
  case class Number(value: String) extends Expression { def rpnExpression: String = value }

  object ShuntingYard extends StandardTokenParsers {
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

    def convert(expressionString: String): String = expressionString match {
      case null => ""
      case ""   => ""
      case _    =>
        parse(expressionString) match {
          case Success(tree, _) => tree.rpnExpression
          case e: NoSuccess     => e.toString
        }
    }
  }
}
