object session
{

    def abs(x: Double): Double = if(x < 0) -x else x

    def isGoodEnough(guess: Double, x: Double): Boolean =
        abs(guess * guess - x) < 0.001

    def sqrtIter(guess: Double, x: Double): Double =
        if(isGoodEnough(guess, x)) guess
        else sqrtIter(improve(guess, x), x)

    def improve(guess: Double, x: Double): Double =
        ((x/guess) + guess) / 2

    def sqrt(x: Double): Double = sqrtIter(1.0, x)

    sqrt(2)
    sqrt(4)
}