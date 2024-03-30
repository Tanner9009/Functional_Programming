trait IList
case object Void extends IList
case class Cons(x: Int, xs: IList) extends IList

//ex1
def isEmpty(l: IList): Boolean = {
  l match {
    case Cons(h,t) => false
    case Void => true
  }
}

//ex2
def size(l: IList): Int ={
  def aux(list: IList,i: Int): Int = {
    l match {
      case Void => i
      case Cons(i,t) => aux(t,i+1)
    }
  }
  aux(l,1)
}

//ex3
def contains(e: Int, l: IList): Boolean ={
  def loop(l:IList,e:Int, i: Int): Boolean ={
    l match{
      case Cons(e,t) => true
      case Cons(i,t) => loop(t,e,i+1)
      case Cons(_,Void) => false
    }
  }
  loop(l,e,1)
}

//ex4
def max(l: IList): Int={
  def loop(t:IList,i:Int,max:Int): Int ={
    t match{
      case max < Cons(h,t) => loop(t,i+1,max)
      case max == Cons(h,t) => h
      case Void => max
    }
  }
  loop(l,1,5)
}

//ex5
def take(n:Int)(l: IList):IList = {
  def loop(t:IList,i:Int):IList = {
    if(i>n) Cons(i,t)
    else Cons(i, loop(t,i+1))
  }
  loop(l,1)
}

//ex6
def drop(n: Int)(l: IList): IList = {
  def loop(t:IList, i:Int): IList = {
    if(i>n) Cons(n-2,Cons(n-1,Cons(n,Void)))
    else loop(t,i+1)
  }
  loop(l,1)
}

//ex7
def append(l1:IList, l2:IList): IList ={
  def loop1(t:IList): IList = {
    l1 match {
      case Cons(h,t) => Cons(h+1,t)
      case Void => Cons(h,t)
    }
  }
}