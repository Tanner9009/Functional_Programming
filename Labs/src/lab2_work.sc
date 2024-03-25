//ex1

def foldWith(b:Int)(op:(Int,Int) => Int)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int =
    if(crt > stop) b+acc
    else tail_fold(crt+1, op(crt, acc))
  tail_fold(start,0)
}

foldWith(0)(_ + _)(1,3)

//ex2
/*
def foldConditional(b:Int)(op: (Int, Int) => Int, p: Int => Boolean)(start: Int, stop : Int): Int ={

}*/

//ex3
def foldRight(b:Int)(op:(Int,Int) => Int)(start: Int, stop: Int): Int ={
  def tail_fold(crt: Int, acc: Int): Int =
    if(crt> stop) b
    else crt + tail_fold(crt+1, op(crt, acc))
  tail_fold(start,0)
}

foldRight(0)(_ + _)(1,3)

//ex4
def f: Int => Int = x => x*x
def foldMap(op:(Int,Int)=> Int, f: Int => Int)(start:Int, stop:Int): Int = {
  def aux(crt: Int, acc: Int): Int = {
    if (crt > stop) acc
    else aux(crt+1, op(f(crt), acc))

  }
  aux(start,0)
}

foldMap(_ * _, f)(3,6)

//ex5

def sumSquares(n: Int): Int = {
  foldMap(_ + _, f)(1,n)
}
sumSquares(3)

//ex6
/*
def hasDivisor(k: Int, start: Int, stop: Int): Boolean = {
  foldMap((x,y) => x%y, f)
}*/

//ex8

val l = (x:Int)
type Line2D = Int => Int
def translateOx(offset: Int)(l: Line2D): Line2D ={
  //l = l + offset
}
