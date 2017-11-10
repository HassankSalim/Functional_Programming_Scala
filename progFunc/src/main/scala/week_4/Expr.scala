package week_4

/**
  * Created by hassan on 10/11/17.
  */
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
