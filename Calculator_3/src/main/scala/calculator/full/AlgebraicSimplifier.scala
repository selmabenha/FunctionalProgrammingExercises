package calculator
package full

/** Simplify expressions based on the listed algebraic rules
  * {{{
  * 1. 0 + e = e + 0 = e
  * 2. 0 - e = -e
  * 3. e - 0 = e
  * 4. 0 * e = e * 0 = 0
  * 5. 1 * e = e * 1 = e
  * 6. e / 1 = e
  * 7. e - e = 0
  * 8. -(-e) = e
  * }}}
  */
object AlgebraicSimplifier extends Simplifier[FullExpr]:
  import FullExpr.*

  def simplify(e: FullExpr): FullExpr =
    e match
      case Number(value) => e

      case Add(e1, e2) =>
        (e1, e2) match
          case (Number(0), _) => simplify(e2)
          case (_, Number(0)) => simplify(e1)
          case _ => Add(simplify(e1), simplify(e2))

      case Minus(e1, e2) =>
        (e1, e2) match
          case (_, Number(0)) => simplify(e1)
          case (Number(0), _) => Neg(simplify(e2))
          case _ if e1 == e2 => Number(0)
          case _ => Minus(simplify(e1), simplify(e2))

      case Mul(e1, e2) =>
        (e1, e2) match
          case (Number(0), _) => Number(0)
          case (_, Number(0)) => Number(0)
          case (Number(1), _) => simplify(e2)
          case (_, Number(1)) => simplify(e1)
          case _ => Mul(simplify(e1), simplify(e2))

      case Div(e1, e2) =>
        (e1, e2) match
          case (_, Number(1)) => simplify(e1)
          case _ => Div(simplify(e1), simplify(e2))

      case Neg(e) =>
        e match
          case Neg(inner) => simplify(inner)
          case _ => Neg(simplify(e))

      case Var(name) => Var(name)