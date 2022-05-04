package fps
package joris
package h8

trait Prop:
  import Prop.*
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop =
    new Prop: 
      def check: Boolean = check && p.check

object Prop{
  type FailedCase   = String
  type SuccessCount = Int
}

import h6.*
case class Gen[A](sample: State[RNG, A])
object hoofdstuk8 extends App:
  ???

