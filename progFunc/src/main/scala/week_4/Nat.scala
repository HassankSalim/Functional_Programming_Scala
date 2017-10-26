package week_4

/**
  * Created by hassan on 21/10/17.
  */
abstract class Nat {
  def isZero : Boolean
  def successor: Nat = new Succ(this)
  def predecessor: Nat
  def + (that: Nat) : Nat
  def - (that: Nat) : Nat
}

object Zero extends Nat{
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(isZero) Zero else throw new Error("negative num")
}

class Succ(n: Nat) extends Nat{
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat =  new Succ(n + that)

  override def -(that: Nat): Nat = if(isZero) this else n - that.predecessor
}