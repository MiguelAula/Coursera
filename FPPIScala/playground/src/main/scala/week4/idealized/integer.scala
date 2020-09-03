package week4.idealized

object integer {
  def main(args: Array[String]): Unit = {
    println(Zero)
    val int2 = Zero.successor.successor
    val int3 = Zero.successor.successor.successor
    val int5 = int2 + int3
    println(int2)
    println(int3)
    println(int5)
    val int4 = int5 - Zero.successor
    println(int4)
  }
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }
  object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nothing = throw new Exception("Zero has no predecessor")
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat = that
    def - (that: Nat): Nat = {
      if (that.isZero) Zero
      else throw new Exception("Nat cannot be a negative number")
    }

    override def toString: String = ""
  }
  class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor: Nat = n
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat = new Succ(n + that)
    def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor

    override def toString: String = "." + n.toString
  }
}
