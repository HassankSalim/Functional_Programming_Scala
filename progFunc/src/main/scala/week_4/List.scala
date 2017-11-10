package week_4

import java.util.NoSuchElementException

import sun.invoke.empty.Empty

/**
  * Created by hassan on 14/10/17.
  */
trait List[+T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List{
  // List(1, 2) = List.apply(1, 2)
  val x: List[String] = Nil
//  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, Nil))
//  def apply[T](x1: T, x2: T): List[T] = Nil
  def apply[T](x1: T): List[T] = new Cons[T](x1, Nil)
}

