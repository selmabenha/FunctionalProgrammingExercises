package calculator
package full

/** Fully (const folding and algebraic) simplify expressions */
object FullSimplifier extends Simplifier[FullExpr]:
  import FullExpr.*
  import ConstFoldSimplifier.* 
  import AlgebraicSimplifier.*
  
  def simplify(e: FullExpr): FullExpr = {
    def loop(expr: FullExpr): FullExpr = {
      val simplified = ConstFoldSimplifier.simplify(AlgebraicSimplifier.simplify(expr))
      if (simplified == expr) simplified
      else loop(simplified)
    }
    
    loop(e)
  }