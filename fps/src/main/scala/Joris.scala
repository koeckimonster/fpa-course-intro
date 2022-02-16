package fps
package joris

import cats.instances.set
import scala.{List => sList}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
  def apply[A](as: A*):List[A]=
    as.foldRight[List[A]](Nil)((t,acc)=> Cons(t,acc))


def tail[A](ls: List[A]): List[A] = 
  ls match {
    case Nil => sys.error("Empty List")
    case Cons(_ , t) => t
  }
    
def set_head[A](h:A, ls: List[A]): List[A] =
  ls match {
    case Nil => sys.error("Empty List")
    case Cons(_,t) => Cons(h,t)
  }

def drop[A](h:Int, ls: List[A]): List[A] =
  if(h>0) then ls match {
                    case Nil => sys.error("Empty List")
                    
                    case Cons(_,t) => drop(h-1, t)
                  }
  else ls


def dropWhile[A](ls: List[A], f:A => Boolean): List[A] =
  ls match{
    case Nil => sys.error("Empty List")
    case Cons(h,t) => if(f(h)) then dropWhile(t,f)
                      else Cons(h,t)
  }

def init[A](ls:List[A]):List[A] =
  ls match{
    case Nil => sys.error("Empty List")
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
  

def isSorted[A](as:Array[A], ordered:(A,A)=> Boolean): Boolean =
  def loop(n: Int, n1: Int): Boolean =
    if (n >= as.length) true
    else if (ordered(as(n),as(n1))) loop(n+1,n1+1)
    else false
  loop(0,1)
        

object Main extends App:
  val list = List(1,2,3)
  println(init(list))