object exercise
{
    def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x*y, 1)(a, b)


    val id = (x:Int) => x
    def fact(x:Int): Int = product(id)(1, x)

    product(x => x*x)(3, 5)
    fact(5)

    def test = product(id)
    test(5, 6)

    def mapReduce(f: Int => Int, combine:(Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
        if(a > b) unit else combine(f(a), mapReduce(f, combine, unit)(a+1, b))

}