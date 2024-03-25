//ex1

def fact(n: Int): Int = {
  def aux_fact(i: Int, acc: Int): Int = {
    if( i == 0) acc
    else aux_fact(i-1, acc*i)
  }
  aux_fact(n, 1)
}

fact(5)
/*
def gcd(a: Int, b: Int): Int ={
  if(a>b) a%b
  else b%a
}

gcd(6,4)
*/


//ex3
def sumSquares(n: Int): Int={
  def sums(i: Int,sum: Int): Int = {
    if(i == n) sum+(i*i)
    else sums(i+1, sum+(i*i))
  }
  sums(1,0)
}

sumSquares(4)

//ex4
def sumNats(start: Int, stop: Int): Int= {
  if(start == stop) start
  else start + sumNats(start+1, stop)
}

sumNats(2,3)

def tailSumNats(start: Int, stop: Int): Int= {
  def sums(sta: Int, sto: Int, s: Int): Int = {
    if(sta == sto) s+sta
    else sums(sta+1, sto, s+sta)
  }
  sums(start, stop, 0)
}

tailSumNats(2,3)

//ex5

def substractRange(start: Int, stop: Int, x: Int): Int ={
  def substract(sta: Int, sto: Int, acc: Int): Int = {
    if(sta == sto) acc-sta
    else substract(sta+1, sto, acc-sta)
  }
  substract(start, stop, x)
}

substractRange(1,4,5)

//ex6
def substractRange2(start: Int, stop: Int, x: Int): Int = {
  if (start == stop) x
  else start-substractRange2(start+1,stop,x)
}

substractRange2(1,4,5)