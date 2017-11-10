import week_4.{Expr, Number, Sum}

object exprs {

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => eval(e1) + " + " + eval(e2)
  }
  eval(Sum(Number(2), Number(3)))
  show(Sum(Number(2), Number(3)))
}