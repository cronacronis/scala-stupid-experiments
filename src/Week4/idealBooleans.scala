//package Week4
package idealized.scala

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: => Boolean): Boolean = ifThenElse(x, False)

  def ||(x: => Boolean): Boolean = ifThenElse(True, x)

  def not: Boolean = ifThenElse(False, True)

  def ==(x: => Boolean): Boolean = ifThenElse(x, x.not)

  def !=(x: => Boolean): Boolean = ifThenElse(x.not, x)

  def <(x: => Boolean): Boolean = ifThenElse(False, x)
}

object True extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = t
}

object False extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = e
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  lazy val successor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat

}

object Zero extends Nat {
  import scala.Exception
  def isZero = True
  def predecessor = throw new Error("No predecessor for zero")
  def +(that: Nat) = that
  def -(that: Nat) = if (that.isZero == True) this else throw new Error("Negative")
}
class Succ(n: Nat) extends Nat { // value generated from current, keeps n as a parent
  val isZero = False
  val predecessor = n

  def +(that: Nat): Nat = this.predecessor + that.successor

  def -(that: Nat): Nat = {
    if(that.predecessor.isZero == True) this else this.predecessor - that.predecessor
  }
} 
