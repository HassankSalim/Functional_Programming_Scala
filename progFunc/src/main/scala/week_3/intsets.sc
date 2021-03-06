object intsets{
    object Empty extends IntSet{ // object definition instead of class to make singleton class
        def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

        def contains(x: Int): Boolean = false

        override def toString() = "."

        def union(other: IntSet): IntSet = other
    }

    class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
        def incl(x: Int): IntSet =
            if (x < elem) new NonEmpty(elem, left incl x, right)
            else if(x > elem) new NonEmpty(elem, left, right incl x)
            else this

        def contains(x: Int): Boolean =
            if(x < elem) left contains x
            else if(x > elem) right contains x
            else true

        override def toString() = "{" + left + elem + right + "} test"

        def union(other: IntSet): IntSet =
            ((left union right) union other) incl elem
    }

    abstract class IntSet{
        def incl(x : Int): IntSet
        def contains(x: Int): Boolean
        def union(other: IntSet): IntSet
    }

    val t1 = new NonEmpty(3, Empty, Empty)
    val root = t1 incl 4



}
