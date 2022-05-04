package fps
package joris
package h6

import javax.swing.InputMap

trait RNG:
  def nextInt: (Int, RNG)

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] =
  rng => (a,rng)

// def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//   rng => 
//     val (a, rng2) = s(rng)
//     (f(a),rng2)



// def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
//   rng => 
//     val (a, rng2) = ra(rng)
//     val (b, rng3) = rb(rng2)
//     (f(a,b),rng3)


def sequence[A](fs:List[Rand[A]]): Rand[List[A]] =
  rng => fs.foldRight(unit(Nil: List[A]))((elm,acc) => 
    map2(elm, acc)(_ :: _))(rng) 
    
// def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
//   rng => fs match 
//            case Nil => (Nil, rng)
//            case h :: t => 
//              val (a, rng1) = h(rng)
//              val (ls, rng2) = sequence(t)(rng1)
//              (a :: ls, rng2)


def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => val (a,rng1) = f(rng)
         g(a)(rng1)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(x => unit(f(x)))

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => map(rb)(b => f(a,b)))

def nonNegativeLessThan(max: Int): Rand[Int] =
  flatMap(nonNegativeInt)(i => rng => 
    val mod = i % max
    if (i + (max-1) - mod >= 0) (mod, rng) else nonNegativeLessThan(max)(rng))

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) = 
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

def nonNegativeInt(rng: RNG): (Int, RNG) =
  val (i, rng1) = rng.nextInt
  if (i != Int.MinValue) (math.abs(i), rng1)
  else (Int.MaxValue, rng1)

def double(rng: RNG): (Double, RNG) =
  val (i, rng1) = nonNegativeInt(rng)
  (i.toDouble / (Int.MaxValue.toDouble + 1), rng1)

def doubleMap: Rand[Double] =
  map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble +1))

def intDouble(rng: RNG): ((Int, Double), RNG) =
  val (i, rng1) = rng.nextInt
  val (d, rng2) = double(rng1)
  ((i,d), rng2)

def doubleInt(rng: RNG): ((Double, Int), RNG) =
  val (d, rng1) = double(rng)
  val (i, rng2) = rng1.nextInt
  ((d,i), rng2)

def double3(rng: RNG): ((Double, Double, Double), RNG) =
  val (d, rng1) = double(rng)
  val (d2, rng2) = double(rng1)
  val (d3, rng3) = double(rng2)
  ((d, d2, d3), rng3)

def ints(rng: RNG)(count: Int): (List[Int], RNG) =
  count match
    case 0 => (Nil, rng)
    case _ => 
      val (i, rng1)  = rng.nextInt
      val (ls, rng2) = ints(rng)(count-1)
      (i :: ls, rng2)

case class State[S, +A](run: S => (A,S)):
  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State(s => 
      val (a,s1) = run(s)
      g(a).run(s1))
  
  def map[B](f: A => B): State[S,B]=
    flatMap(x => State.unit(f(x)))

  def map2[B,C](rb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a => rb.map(b => f(a,b)))
  

object State: 
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  
  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight(unit(Nil: List[A]))((elm,acc) => elm.map2(acc)(_ :: _))
  

sealed trait Input
case object Coin extends Input
case object Turn extends Input 

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  State(machine => inputs match 
    case Nil        => ((machine.coins, machine.candies), machine)
    case Turn :: t if !machine.locked => 
      simulateMachine(t).run(machine.copy(locked = true, candies = machine.candies - 1))
    case Coin :: t if machine.locked & machine.candies > 0 => 
      simulateMachine(t).run(machine.copy(locked = false, coins = machine.coins + 1))
    case _ :: t => simulateMachine(t).run(machine)
  )

def simulateMachineFold(inputs: List[Input]): State[Machine, (Int, Int)] =
  State(machine => 
    val output = inputs.foldLeft(machine)((acc,input) => rules(input, acc))
    ((output.coins, output.candies), output))

    
def rules(i: Input, machine: Machine): Machine =
  i match 
    case Turn if !machine.locked => 
      (machine.copy(locked = true, candies = machine.candies - 1))
    case Coin if machine.locked & machine.candies > 0 => 
      (machine.copy(locked = false, coins = machine.coins + 1))
    case _ => machine


object hoofdstuk6 extends App:
  val (i, rng) = nonNegativeInt(SimpleRNG(42))
  val (d, rng2) = double(rng)
  val ((i2, d2), rng3) = intDouble(rng2)
  val (ls, rng4) = ints(rng3)(10)
  val (out, rng5) = sequence(List.fill(5)(double))(rng4)
  val (out2, rng6) = nonNegativeLessThan(6)(rng5)
  val (out3, rng7) = map(nonNegativeLessThan(6))(_*10)(rng6)
  val (out4, rng8) = map2(nonNegativeLessThan(1), nonNegativeLessThan(1))(_+_)(rng7)

  val (out5, machine) = simulateMachine(List(Coin, Turn, Turn, Coin, Turn, Coin, Turn)).run(Machine(true,2,10))
  print(out5," ", machine, "\n")
  println(doubleMap(rng4))
  print( s"$i, $d, ($i2, $d2) \n" )
