val f: (Int, Int) => Int = (x,y) => x*y

f(2,3)

def sumwithf(start: Int, stop: Int, f: Int => Int): Int = {
  def aux(crt:Int, acc: Int): Int =
    if(crt>stop) acc
    else aux(crt+1, acc + f(crt))
    aux(start,0)
}