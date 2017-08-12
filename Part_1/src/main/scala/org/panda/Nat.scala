package org.panda

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def - (that: Nat): Nat
  def + (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw RuntimeException

  override def -(that: Nat): Nat = if (that.isZero) this else throw RuntimeException

  override def +(that: Nat): Nat = that
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat =
    if (that.isZero) this
    else successor + that.predecessor

  override def -(that: Nat): Nat =
    if (that.isZero) this
    else predecessor - that.predecessor
}

