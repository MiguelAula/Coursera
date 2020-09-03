package week4

object CaseClass {
  def main(args: Array[String]): Unit = {
    val e1 = Sum(Prod(Number(2), Var("x")), Var("y"))
    println(show(e1))
    val e2 = Prod(Sum(Number(2), Var("x")), Var("y"))
    println(show(e2))
    val e3 = Prod(Sum(Number(2),Sum(Var("a"),Var("b"))),Prod(Var("c"),Sum(Var("d"),Var("e"))))
    println(show(e3))
  }

  class Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }
  }

  def show(e: Expr): String = {
    e match {
      case Number(n) => n.toString
      case Var(v) => v
      case Sum(e1, e2) => show(e1) + "+" + show(e2)
      case Prod(e1, e2) => {
        (e1 match {
          case Sum(x,y) => "(" + show(e1) + ")"
          case _ => show(e1)
        }) + "*" + (e2 match {
          case Sum(x,y) => "(" + show(e2) + ")"
          case _ => show(e2)
        })
      }
    }
  }

  case class Number(n: Int) extends Expr
  case class Var(v: String) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
}
