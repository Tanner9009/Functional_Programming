//ex1

def foldWith(b:Int)(op:(Int,Int) => Int)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int =
    if(crt > stop) b+acc
    else tail_fold(crt+1, op(crt, acc))
  tail_fold(start,0)
}

foldWith(0)(_ + _)(1,3)

//ex2
def foldConditional(b:Int)(op: (Int, Int) => Int, p: Int => Boolean)(start: Int, stop : Int): Int ={
  def tail_fold(crt: Int, acc: Int):Int =
    if(crt >stop || crt > p) b+acc
    else tail_fold(crt+1, op(crt, acc))
  tail_fold(start,0)
}
foldConditional(0)(_ + _,(1,3)

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
  def aux(crt: Int, acc: Int
}*/

//ex8


type Line2D = Int => Int
val l1:Line2D = x => x+1
def translateOx(offset: Int)(l: Line2D): Line2D ={
  (y:Int) => y + offset
}
translateOx(2)(l1)(1)

//ex9
def translateOy(offset: Int)(l:Line2D): Line2D ={
  (y:Int) => l(y)+offset
}
translateOy(2)(l1)(1)

//ex10
val l2:Line2D = x => x+2
val l3:Line2D = x => x+3
def intersect(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean ={
  def aux(s1: Int, s2:Int): Boolean ={
    if(l1(s1) == l2(s1)) true
    else if (s1 > s2) false
    else aux(s1+1,s2)
  }
  aux(start,stop)
}
intersect(l2,l3)(1,3)

def larger(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
  def aux(s1: Int, s2: Int): Boolean ={
    if(l1(s1) < l2(s1)) false
    else if (s1 > s2) true
    else aux(s1+1,s2)
  }
  aux(start, stop)
}
larger(l2,l3)(1,3)