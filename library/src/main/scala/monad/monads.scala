package unfiltered.monad
import scalaz._
import Scalaz._
import unfiltered.request._

sealed abstract class RequestMonad[T,E,R] {
  self => 
  import RequestLogger._
  import LogLevel._

  def apply(c:HttpRequest[T]):RequestLogger[E,R]

  def map[X](f: R => X): RequestMonad[T,E,X] = new RequestMonad[T,E,X] {
    def apply(a:HttpRequest[T]):RequestLogger[E,X] = self.apply(a) map f
  }

  def flatMap[X](f: R => RequestMonad[T,E,X]):RequestMonad[T,E,X] = new RequestMonad[T,E,X] {
    def apply(a:HttpRequest[T]):RequestLogger[E,X] = self.apply(a) flatMap { x => f(x)(a) }
  }

  def :+->(l:LogLevel):RequestMonad[T,E,R] = new RequestMonad[T,E,R] {
    def apply(c:HttpRequest[T]):RequestLogger[E,R] = {
      val r = self.apply(c)
      r.copy(log = r.log |+| l.pure[LOG])
    }
  }

  def orElse(r: => Validation[E,R]):RequestMonad[T,E,R] = new RequestMonad[T,E,R] {
    def apply(c:HttpRequest[T]):RequestLogger[E,R] = self.apply(c) orElse r
  }

  def orElseAndLog(r: => Validation[E,R])(implicit toLog: E => LogLevel):RequestMonad[T,E,R] = new RequestMonad[T,E,R] {
    def apply(c:HttpRequest[T]):RequestLogger[E,R] = self.apply(c).orElseAndLog(r)(toLog)
  }

}

object RequestMonad {
  implicit def toRequestMonad[T,E,R](x:RequestLogger[E,R]) = new RequestMonad[T,E,R] {
    def apply(c:HttpRequest[T]):RequestLogger[E,R] = x
  }

  def getParams[T,E]:RequestMonad[T,E,ParamOps] = new RequestMonad[T,E,ParamOps] {
    def apply(c:HttpRequest[T]):RequestLogger[E,ParamOps] = mkRequestLogger {
      val names = c.parameterNames
      (new ParamOps((Map.empty[String, Seq[String]] /: names) {
        (m, n) => m + (n -> c.parameterValues(n))
      })).success
    }
  }
}
