package calculator
package full

import scala.util.{Try, Success, Failure}

object FullEvaluator:
  /** Result of evaluation. */
  enum Result:
    case Ok(v: Double)
    case DivByZero
    case UndefinedVar(name: String)

    def get: Double = this match
      case Ok(v)              => v
      case DivByZero          => throw new RuntimeException("division by zero")
      case UndefinedVar(name) => throw new RuntimeException(s"undefined variable: $name")

  // Define your own context here
  enum MyContext:
    case EmptyContext
    case AddContext(name: String, value: Double, tail: MyContext)

  type Context = MyContext

  object Context:
    def empty: Context = MyContext.EmptyContext

    def cons(name: String, value: Double, tail: Context) = MyContext.AddContext(name, value, tail)

    def fromList(xs: List[(String, Double)]): Context =
      xs match
        case Nil           => empty
        case (n, v) :: rem => cons(n, v, fromList(rem))

class FullEvaluator(ctx: FullEvaluator.Context) extends Evaluator[FullExpr, FullEvaluator.Result]:
  import FullEvaluator.*
  import FullExpr.*
  import Result.*

  /** Evaluate an expression to its value. */
  def evaluate(e: FullExpr): Result =
    e match
      case Number(value) => Ok(value)

      case Add(e1, e2)   => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 + v2)
        case (UndefinedVar(_), _) => UndefinedVar("undefined variable")
        case (_, UndefinedVar(_)) => UndefinedVar("undefined variable")
        case _ => DivByZero

      case Minus(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 - v2)
        case (UndefinedVar(_), _) => UndefinedVar("undefined variable")
        case (_, UndefinedVar(_)) => UndefinedVar("undefined variable")
        case _ => DivByZero

      case Mul(e1, e2)   => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 * v2)
        case (UndefinedVar(_), _) => UndefinedVar("undefined variable")
        case (_, UndefinedVar(_)) => UndefinedVar("undefined variable")
        case _ => DivByZero

      case Div(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(_), Ok(0)) => DivByZero 
        case (Ok(v1), Ok(v2)) => Ok(v1 / v2)
        case (DivByZero, _) => DivByZero
        case (_, DivByZero) => DivByZero
        case (UndefinedVar(_), _) => UndefinedVar("undefined variable")
        case (_, UndefinedVar(_)) => UndefinedVar("undefined variable")

      case Neg(e) => evaluate(e) match
        case Ok(value) => Ok(-value)
        case UndefinedVar(_) => UndefinedVar("undefined variable")
        case _ => DivByZero

      case Var(name) => lookup(name) match
        case Ok(value) => Ok(value)
        case UndefinedVar(_) => UndefinedVar(name)
        case _ => DivByZero
      


  private def lookup(name: String): Result = ctx match
    case MyContext.EmptyContext => Result.UndefinedVar(name)
    case MyContext.AddContext(n, v, t) =>
      if (n == name) Result.Ok(v)
      else new FullEvaluator(t).lookup(name)
