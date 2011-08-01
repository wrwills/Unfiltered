package unfiltered.monad
import scalaz._
import Scalaz._

object Tester {
  case class Err(m:String)
  type Result[A] = Validation[Vector[Err],A]

  def inter(i:Int):Result[Int] = if (i<0) Err("no way bro").pure[Vector].fail else i.success

  def doIt(a:Int,b:Int) = (inter(a) |@| inter(b)){_ + _}
}
