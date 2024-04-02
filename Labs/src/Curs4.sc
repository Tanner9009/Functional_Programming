trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat
def toInteger(x: Nat): Int = {
  x match {
    case Zero => 0
    case Succ(xp) => 1 + toInteger(xp)
  }
}

def fromInteger(i: Int): Nat = {
  if(i==0) Zero
  else Succ(fromInteger(i-1))
}

trait OONat {
  def isZero: Boolean
  def add(other: OONat): OONat
  def toInteger: Int
}

case object Zero extends OONat {
  override def isZero:Boolean = true
  override def add(other: OONat): OONat = other
  override def toInteger: Int = 0
}

case class Succ(x: OONat) extends OONat{
  override def isZero:Boolean = false
  override def add(other: OONat): OONat = Succ(x.add(other))
  override def toInteger: Int = 1 + x.toInteger
}