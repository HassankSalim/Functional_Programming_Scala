package week_4

import java.util.NoSuchElementException

/**
  * Created by hassan on 14/10/17.
  */
trait List[T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

