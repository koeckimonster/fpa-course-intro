package fps
package joris
package h4


sealed trait Option[+A]{
  def map[B](f: A=>B): Option[B] =
    this match{
      case None => None
      case Some(x) => Some(f(x))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match{
      case None => None
      case Some(x) => f(x)
    }

  def getOrElse[B >: A](default: => B): B =
    this match{
      case None => default
      case Some(x) => x
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match{
      case None => ob
      case Some(_) => this
    }
    
  def filter(f: A => Boolean): Option[A] =
    this match{
      case None => None
      case Some(x) if f(x) => Some(x)
      case _ => None
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def sequence[A](as: List[Option[A]]): Option[List[A]] =
  as match
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))


def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(ae => b.map(be => f(ae,be)))

def mean(xs: Seq[Double]): Option[Double] = 
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m=>mean(xs.map(x=> math.pow(x-m,2))))
  
def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
  age*numberOfSpeedingTickets

def parseInsuranceRateQuote(age: String, numOfTick: String) : Option[Double] =
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numOfTick.toInt)
  map2(optAge, optTickets)(insuranceRateQuote(_, _))

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None}  

sealed trait Either[+E, +A]:
  def map[B](f: A => B): Either[E, B] =
    this match
      case Left(e) => Left(e)
      case Right(b) => Right(f(b))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Left(e) => Left(e)
      case Right(b) => f(b)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(_) => b
      case Right(r) => Right(r)

  def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    this.flatMap(ae => b.map(b => f(ae,b)))

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
  es match
    case Nil => Right(Nil)
    case Left(h) :: t => Left(h)
    case Right(h) :: t => sequence(t) match
      case Left(e) => Left(e)
      case Right(t) => Right(h :: t)


def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]]=
  as match
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)



object hoofdstuk4 extends App:
  val lx: Seq[Double] = Seq(1,2,3,4)
  val x: Option[Int] = Some(2)
  val y: Option[Nothing] = None
  val t: List[Either[Error, Int]] = List(Right(1),Right(3),Right(1),Right(4))
  println(x.map(_+2))
  println(x.getOrElse(5))
  println(x.orElse(Some(3)))
  println(variance(lx))
  println(parseInsuranceRateQuote("2", "25"))
  println(sequence(t))


  