package week_3

/**
  * Created by hassan on 12/10/17.
  */
class Rational(x:Int, y: Int)
{
  require(y != 0, "Denominator must be nonzero")
  def this(x : Int) = this(x, 1)

  private def gcd(a:Int, b:Int): Int =
      if(b == 0) a
      else gcd(b, a % b)

  def numer = x
  def denom = y

  def + (secRational : Rational) =
      new Rational(numer * secRational.denom + denom * secRational.numer, denom * secRational.denom)

  def max(secRational : Rational) =
      if(this < secRational) secRational
      else this

  override def toString() =
  {
      val g = gcd(x, y)
      numer/g + "/" + denom/g
  }

  def < (secRational : Rational) = numer * secRational.denom < denom * secRational.numer

  def unary_- : Rational =  new Rational(-numer, denom)


  def - (y: Rational) = this + -y

  def mul(secRational : Rational) =
      new Rational(numer * secRational.numer, denom * secRational.denom)
}
