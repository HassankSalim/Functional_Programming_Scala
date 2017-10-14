import week_4._

object nth{
  def nth[T](n: Int, mList: List[T]): T =
    if(mList.isEmpty) throw  new IndexOutOfBoundsException("List out of index")
    else if(n == 0) mList.head
    else nth(n-1, mList.tail)

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)
  nth(1, list)
  nth(-1, list)
  nth(4, list)

}