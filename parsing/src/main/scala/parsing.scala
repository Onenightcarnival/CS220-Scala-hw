import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
	def eval(e: Expr): Double = e match {
		case Num(e) => e
		case Add(e1, e2) => eval(e1) + eval(e2)
		case Sub(e1, e2) => eval(e1) - eval(e2)
		case Mul(e1, e2) => eval(e1) * eval(e2)
		case Div(e1, e2) => eval(e1) / eval(e2)
		case Exponent(e1, e2) => scala.math.pow(eval(e1), eval(e2))
	}
}

object ArithParser extends ArithParserLike {
// number: PackratParser[Double] is defined in ArithParserLike
	lazy val atom: PackratParser[Expr] = { 
		"(" ~ expr ~ ")" ^^ {case _ ~ e ~ _ => e} | 
		number ^^ {case e => Num(e)} 
	}
	lazy val exponent: PackratParser[Expr] = {
		exponent ~ "^" ~ atom ^^ {case e1 ~ "^" ~ e2 => Exponent(e1, e2)} | 
		atom
	}
	lazy val mul: PackratParser[Expr] = {
		mul ~ "*" ~ exponent ^^ {case e1 ~ "*" ~ e2 => Mul(e1,e2)} | 
		mul ~ "/" ~ exponent ^^ {case e1 ~ "/" ~ e2 => Div(e1, e2)} | 
		exponent
	}
	lazy val add: PackratParser[Expr] = {
		add ~ "+" ~ mul ^^ {case e1 ~ "+" ~ e2 => Add(e1, e2)} | 
		add ~ "-" ~ mul ^^ {case e1 ~ "-" ~ e2 => Sub(e1, e2)} | 
		mul
	}
	lazy val expr: PackratParser[Expr] = add
}

object ArithPrinter extends ArithPrinterLike {
	def print(e: Expr): String = e match {
	case Num(e) => e.toString
    case Add(e1,e2) => "(" + print(e1) + "+" + print(e2) + ")"
    case Sub(e1,e2) => "(" + print(e1) + "-" + print(e2) + ")"
    case Mul(e1,e2) => "(" + print(e1) + "*" + print(e2) + ")"
    case Div(e1,e2) => "(" + print(e1) + "/" + print(e2) + ")"
    case Exponent(e1,e2) => "(" + print(e1) + "^" + print(e2) + ")"
	}
}