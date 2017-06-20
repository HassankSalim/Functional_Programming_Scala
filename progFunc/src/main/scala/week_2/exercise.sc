import scala.annotation.tailrec

object exercise
{
    def factorial(n: Int):Int =
    {
        @tailrec
        def loop(acc: Int, n: Int): Int =
            if (n == 0) acc
            else loop(acc*n, n-1)
        loop(1, n)
    }

    factorial(4)

    def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x*y, 1)(a, b)


    val id = (x:Int) => x
    def fact(x:Int): Int = product(id)(1, x)

    product(x => x*x)(3, 5)
    fact(5)

    def test(a:Int, b: Int) = product(id)(a, b)
    test(5, 6)

    def mapReduce(f: Int => Int, combine:(Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
        if(a > b) unit else combine(f(a), mapReduce(f, combine, unit)(a+1, b))



}