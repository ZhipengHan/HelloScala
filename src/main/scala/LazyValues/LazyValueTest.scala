package LazyValues

/**
 * Created by zhhan on 2015/10/28.
 */
case class Employee(id: Int, name: String, managerId: Int){
  lazy val manager: Employee = Db.get(managerId)
  lazy val team: List[Employee] = Db.team(id)
}

object Db{
  val table = Map(1 -> (1, "huaruki", -1),
                  2 -> (2, "Milan" , 1),
                  3 -> (3, "Jeffrey Eugenides", 1),
                  4 -> (4, "Mario Vargas Llosa", 1),
                  5 -> (5, "Julian Barnes", 2))
  def team(id: Int) ={
    for(rec <- table.values.toList; if rec._3 == id)
      yield recToEmployee(rec)
  }
  def get(id: Int) = recToEmployee(table(id))
  private def recToEmployee(rec: (Int, String, Int)) = {
    println("[db] fetching " + rec._1)
    Employee(rec._1, rec._2, rec._3)
  }
}

class Symbols(val compiler: Compiler){
  import compiler.types._

  val Add = new Symbol("+", FunType(List(IntType, IntType), IntType))
  val Sub = new Symbol("-", FunType(List(IntType, IntType), IntType))

  class Symbol(name: String, tpe: Type){
    override def toString = name + ": " + tpe
  }
}

class Types(val compiler: Compiler) {
  import compiler.symtab._
  abstract class Type
  case class FunType(args: List[Type], res: Type) extends Type
  case class NamedType(sym: Symbol) extends Type
  case object IntType extends Type
}

class Compiler {
  lazy val symtab = new Symbols(this)
  lazy val types = new Types(this)
}

// test
object LazyValueTest {
  def main (args: Array[String]) {
    println(Db.get(2).manager.name)
    println(Db.team(2))
  }
}
