import math.abs

object exercise4
{
    val tolerance = 0.001
    def isCloseEnough(x: Double, y: Double) =
        if (abs(x - y) / x < tolerance) true
        else false
    def fixedPoint(f: Double => Double)(guess: Double) =
    {
        def iterate(guess: Double): Double =
        {
            val next = f(guess)
            if (isCloseEnough(guess, next)) next
            else iterate(next)
        }
        iterate(guess)
    }
    fixedPoint(x => 1 + x / 2)(1.0)
    def avgDamp(f:Double => Double)(x: Double) = (x + f(x)) / 2
    def sqrt(x:Int) = fixedPoint(avgDamp(y => x/y))(1)
}