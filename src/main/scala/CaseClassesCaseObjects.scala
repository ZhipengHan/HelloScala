/**
 * Created by zhhan on 10/12/2015.
 */

abstract class Expr{
  def eval: Int
  //def print
}

class Number(n: Int) extends Expr{
  def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr{
  def eval: Int = e1.eval + e2.eval
}

class Prod(e1: Expr, e2: Expr) extends Expr{
  def eval: Int = e1.eval * e2.eval
}

object CaseClassesCaseObjects {


}
