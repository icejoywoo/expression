import scala.collection.mutable
import scala.util.Try

import com.twitter.util.Eval
import scala.util.parsing.combinator._


trait Expression {
  val env: mutable.Map[String, Double] = mutable.Map.empty

  def addVariable(name: String, value: Double) = env += name -> value
  def addVariables(kvs: (String, Double)*) = env ++= kvs

  def evaluate(expression: String): Try[Double]
}

class ArithmeticExpression extends JavaTokenParsers with Expression {
  def evaluate(expression: String): Try[Double] = {
    parseAll(expr, expression) match {
      case Success(matched,_) => scala.util.Success(matched.asInstanceOf[Double])
      case Failure(msg,_) => scala.util.Failure(new IllegalArgumentException("FAILURE: " + msg))
      case Error(msg,_) => scala.util.Failure(new IllegalArgumentException("ERROR: " + msg))
    }
  }

  private def expr: Parser[Double] = term~rep("+"~term | "-"~term) ^^ {
    case term~ops =>
      ops.foldLeft(term) {
        case (a, "+"~b) => a + b
        case (a, "-"~b) => a - b
      }
  }

  private def term: Parser[Double] = factor~rep("*"~factor | "/"~factor) ^^ {
    case factor~ops =>
      ops.foldLeft(factor) {
        case (a, "*"~b) => a * b
        case (a, "/"~b) => a / b
      }
  }

  private def factor: Parser[Double] = (
    floatingPointNumber                  ^^ { _.toDouble }
  | ("-" | "+")~floatingPointNumber      ^? ({
      case "-"~v => -v.toDouble
      case "+"~v => v.toDouble
    }, { i => s"unexpected error [$i]" })
  | ident                                ^? ({ case i if env.contains(i) => env(i) }, { i => s"$i is not found in context"})
  | ("-" | "+")~ident                    ^? ({
      case "-"~v if env.contains(v) => -env(v)
      case "+"~v if env.contains(v) => env(v)
    }, { case _~i => s"$i is not found in context" })
  | "("~expr~")"        ^? ({
      case "("~v~")" => v
    }, { i => s"unexpected error [$i]"})
  )
}

class EvalExpression extends Expression {
  private val cache: mutable.Map[String, Eval] = mutable.Map.empty

  private def getEval: Eval = {
    val initilization = env.map{ case (k, v) => s"val $k: Double = $v" }.mkString("\n")
    if (cache.contains(initilization)) {
      cache(initilization)
    } else {
      val e = new Eval
      e.compile(s"""object $$Env {
      |$initilization
      |}""".stripMargin)
      cache += initilization -> e
      e
    }
  }

  def evaluate(expression: String): Try[Double] = {
    val eval = getEval
    try {
      scala.util.Success(
        eval.inPlace[java.lang.Number](s"""import $$Env._
        |$expression""".stripMargin).doubleValue)
    } catch {
      case e: eval.CompilerException =>
        println(s"WARN: [failed to eval code] code[$expression] exception[${e.getMessage}]")
        scala.util.Failure(e)
    }    
  }
}

object TestArithmeticExpression extends ArithmeticExpression  {
  def main(args: Array[String]): Unit = {
    env += "a" -> 5
    env += "b" -> 6
    env += "c" -> 7

    assert(evaluate("2 * (3 + 7) * a + (b + c) * c").get == 191.0)
    assert(evaluate("2 * (3 + 7) * (a + (b + c) * c)").get == 1920.0)
    assert(evaluate("a + 4").get == 9.0)
    assert(evaluate("((a) + (4))").get == 9.0)
    assert(evaluate("a / 20 + 10 + a * b + c * 10").get == 110.25)
    assert(evaluate("+5").get == 5.0)
    assert(evaluate("-a").get == -5.0)

    evaluate("2 * (3 + 7)") match {
      case scala.util.Success(value) => println(value)
      case scala.util.Failure(e) => println("FAILURE: " + e.getMessage)
    }

    evaluate("2 * (3 + 7))") match {
      case scala.util.Success(value) => println(value)
      case scala.util.Failure(e) => println("FAILURE: " + e.getMessage)
    }

    assert(evaluate("2 * (3 + 7))").isFailure)
    assert(evaluate("(2 * (3 + 7)").isFailure)
  }
}

object TestArithmeticExpressionInNewClass {
  def main(args: Array[String]): Unit = {
    val a = new ArithmeticExpression
    a.env ++= Map(
      "a" -> 5,
      "b" -> 6,
      "c" -> 7
    )
    
    assert(a.evaluate("2 * (3 + 7) * a + (b + c) * c").get == 191.0)
    assert(a.evaluate("2 * (3 + 7) * (a + (b + c) * c)").get == 1920.0)
    assert(a.evaluate("a + 4").get == 9.0)
    assert(a.evaluate("((a) + (4))").get == 9.0)
    assert(a.evaluate("a / 20 + 10 + a * b + c * 10").get == 110.25)

    assert(a.evaluate("2 * (3 + 7))").isFailure)
    assert(a.evaluate("(2 * (3 + 7)").isFailure)
  }
}

object TestEvalExpression extends EvalExpression  {
  def main(args: Array[String]): Unit = {
    env += "a" -> 5
    env += "b" -> 6
    env += "c" -> 7

    assert(evaluate("2 * (3 + 7) * a + (b + c) * c").get == 191.0)
    assert(evaluate("2 * (3 + 7) * (a + (b + c) * c)").get == 1920.0)
    assert(evaluate("a + 4").get == 9.0)
    assert(evaluate("((a) + (4))").get == 9.0)
    assert(evaluate("a / 20 + 10 + a * b + c * 10").get == 110.25)
    assert(evaluate("+5").get == 5.0)
    assert(evaluate("-a").get == -5.0)

    evaluate("2 * (3 + 7)") match {
      case scala.util.Success(value) => println(value)
      case scala.util.Failure(e) => println("FAILURE: " + e.getMessage)
    }

    evaluate("2 * (3 + 7))") match {
      case scala.util.Success(value) => println(value)
      case scala.util.Failure(e) => println("FAILURE: " + e.getMessage)
    }

    assert(evaluate("2 * (3 + 7))").isFailure)
    assert(evaluate("(2 * (3 + 7)").isFailure)
  }
}