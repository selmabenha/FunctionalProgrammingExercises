package calculator
package full

/** Folds constant sub-expressions in values. */
object ConstFoldSimplifier extends Simplifier[FullExpr]:
  import FullExpr.*

    def simplify(e: FullExpr): FullExpr =
    e match
      case Number(value) => e

      case Add(e1, e2)   => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 + v2)
        case (v1, v2) => Add(v1, v2)

      case Minus(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 - v2)
        case (v1, v2) => Minus(v1, v2)

      case Mul(e1, e2)   => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 * v2)
        case (v1, v2) => Mul(v1, v2)

      case Div(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 / v2)
        case (v1, v2) => Div(v1, v2)

      case Neg(e) => simplify(e) match
        case Number(value) => Number(-value)
        case v => Neg(v)

      case Var(name) => Var(name)