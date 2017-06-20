import scala.annotation.tailrec

object exercise2
{
    def sum(f:Int => Int)(a:Int, b:Int): Int =
    {
        @tailrec
        def loop(a:Int, acc:Int): Int =
            if(a > b) acc
            else loop(a+1, acc+f(a))
        loop(a, 0)
    }

    sum((x:Int) => x*x)(4, 6)
}