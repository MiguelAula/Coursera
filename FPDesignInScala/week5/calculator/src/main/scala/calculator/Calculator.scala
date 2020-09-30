package calculator

import scala.annotation.tailrec

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map{ case (variable,_) => (variable,Signal(eval(Ref(variable),namedExpressions))) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalAux(expr: Expr, refAcc: Map[String, Signal[Expr]] = Map()): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) =>
          if (refAcc.contains(name)) {
            Double.NaN  //s'ha trobat una referència circular
          } else {
            val refExpr = getReferenceExpr(name,references)
            evalAux(refExpr,refAcc + (name -> Signal(refExpr)))
          }
        case Plus(a,b) => evalAux(a,refAcc) + evalAux(b,refAcc)
        case Minus(a,b) => evalAux(a,refAcc) - evalAux(b,refAcc)
        case Times(a,b) => evalAux(a,refAcc) * evalAux(b,refAcc)
        case Divide(a,b) => evalAux(a,refAcc) / evalAux(b,refAcc)
      }
    }
    evalAux(expr)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
