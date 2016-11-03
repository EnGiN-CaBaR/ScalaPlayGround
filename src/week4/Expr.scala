package week4


/**
  * Created by engin on 27.10.2016.
  */

/**
  * Next Solutions Contain Classification Methods
*/
/**trait Expr {
  *def isNumber: Boolean
  *def isSum: Boolean
  *def numValue: Int
  *def leftOp: Expr
  *def rightOp: Expr
*}
 **
 *class Number(n: Int) extends Expr{
  *def isNumber: Boolean = true
  *def isSum: Boolean = false
  *def numValue: Int = n
  *def leftOp: Expr = throw new Error("Number.leftOp")
  *def rightOp: Expr = throw new Error("Number.rightOp")
*}
 **
 *class Sum(e1: Expr, e2: Expr) extends Expr{
  *def isNumber: Boolean = false
  *def isSum: Boolean = true
  *def numValue: Int = throw new Error("Sum.numValue")
  *def leftOp: Expr = e1
  *def rightOp: Expr = e2
*}
**/



/**
*trait Expr {
  *def numValue: Int
  *def leftOp: Expr
  *def rightOp: Expr
*}
 **
 *class Number(n: Int) extends Expr{
  *def numValue: Int = n
  *def leftOp: Expr = throw new Error("Number.leftOp")
  *def rightOp: Expr = throw new Error("Number.rightOp")
*}
 **
 *class Sum(e1: Expr, e2: Expr) extends Expr{
  *def numValue: Int = throw new Error("Sum.numValue")
  *def leftOp: Expr = e1
  *def rightOp: Expr = e2
*}
**/

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr


object ABC{
  /**def eval(e: Expr): Int = {
    *if(e.isNumber) e.numValue
    *else if(e.isSum) eval(e.leftOp) + eval(e.rightOp)
    *else throw new Error("Unknown expression " + e)
  *}**/

  // STAY AWAY CLASS CASTING :)

  /**def eval(e: Expr): Int =
    *if(e.isInstanceOf[Number])
      *e.asInstanceOf[Number].numValue
    *else if(e.isInstanceOf[Sum])
      *eval(e.asInstanceOf[Sum].leftOp) +
      *eval(e.asInstanceOf[Sum].rightOp)
    *else throw new Error("Unknown expression " + e)*/

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }



  def main(args: Array[String]) {

    val a = Sum(Number(1), Number(2))
    println(a)
  }
}