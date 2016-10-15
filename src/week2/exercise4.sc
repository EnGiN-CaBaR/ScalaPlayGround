class Rational(x: Int, y: Int){
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a % b)
  val num = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def + (that: Rational) = //add
    new Rational(num * that.denom + that.num * denom,
      denom * that.denom)

  def -(that: Rational) = this + -that //sub

  def * (that: Rational) = //add
    new Rational(num * that.num, denom * that.denom)


  def unary_- =
    new Rational(-num, denom)

  def < (that: Rational) = num * that.denom < that.num * denom //less
  def max(that: Rational) = if(<(that)) that else this

  override def toString = num + "/" + denom
}


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x - y - z
x max y

(x * x) + (y * y)