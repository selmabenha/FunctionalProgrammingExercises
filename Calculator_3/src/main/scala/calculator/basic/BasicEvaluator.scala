package calculator
package basic

class BasicEvaluator extends Evaluator[BasicExpr, BasicEvaluator.Result]:
  import BasicExpr.*
  import BasicEvaluator.*
  import BasicEvaluator.Result.*

  /** Evaluate an expression to its value. */
  def evaluate(e: BasicExpr): Result =
    e match
      case Number(value) => Ok(value)  // Wrap the value in the Ok case
      case Add(e1, e2)   => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 + v2)
        case _ => DivByZero
      case Minus(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 - v2)
        case _ => DivByZero
      case Mul(e1, e2)   => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 * v2)
        case _ => DivByZero
      case Div(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(_), Ok(0)) => DivByZero 
        case (Ok(v1), Ok(v2)) => Ok(v1 / v2)
        case _ => DivByZero
      case Neg(e) => evaluate(e) match
        case Ok(value) => Ok(-value)
        case _ => DivByZero

object BasicEvaluator:
  enum Result:
    case Ok(v: Double)
    case DivByZero

    def get: Double = this match
      case Ok(v)     => v
      case DivByZero => throw new RuntimeException(s"division by zero")
