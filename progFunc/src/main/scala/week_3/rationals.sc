object rationals
{
    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)


    x.sub(y).sub(z)
    y.add(y)
    x.less(y)

    class Rational(x:Int, y: Int)
    {
        require(y != 0, "Denominator must be nonzero")

        def this(x : Int) = this(x, 1)

        private def gcd(a:Int, b:Int): Int =
            if(b == 0) a
            else gcd(b, a % b)

        def numer = x
        def denom = y

        def add(secRational : Rational) =
            new Rational(numer * secRational.denom + denom * secRational.numer, denom * secRational.denom)

        def max(secRational : Rational) =
            if(this.less(secRational)) secRational
            else this

        override def toString() =
            {
                val g = gcd(x, y)
                numer/g + "/" + denom/g
            }

        def less(secRational : Rational) = numer * secRational.denom < denom * secRational.numer

        def neg: Rational =  new Rational(-numer, denom)

        def sub(y: Rational) = add(y.neg)

        def mul(secRational : Rational) =
            new Rational(numer * secRational.numer, denom * secRational.denom)
    }
}

