package unfiltered.monad
import scalaz._
import Scalaz._
import unfiltered.request._

sealed abstract class RequestMonad[T,R] {
  self => 
  import RequestLogger._
  import LogLevel._

  def apply(c:HttpRequest[T]):RequestLogger[R]

  def map[X](f: R => X): RequestMonad[T,X] = new RequestMonad[T,X] {
    def apply(a:HttpRequest[T]):RequestLogger[X] = self.apply(a) map f
  }

  def flatMap[X](f: R => RequestMonad[T,X]):RequestMonad[T,X] = new RequestMonad[T,X] {
    def apply(a:HttpRequest[T]):RequestLogger[X] = self.apply(a) flatMap { x => f(x)(a) }
  }

  def :+->(l:LogLevel):RequestMonad[T,R] = new RequestMonad[T,R] {
    def apply(c:HttpRequest[T]):RequestLogger[R] = {
      val r = self.apply(c)
      r.copy(log = r.log |+| l.pure[LOG])
    }
  }

  def ifMissing(r: => Validation[RequestError,R]):RequestMonad[T,R] = new RequestMonad[T,R] {
    def apply(c:HttpRequest[T]):RequestLogger[R] = self.apply(c) ifMissing r
  }

  def orElse(r: => Validation[RequestError,R]):RequestMonad[T,R] = new RequestMonad[T,R] {
    def apply(c:HttpRequest[T]):RequestLogger[R] = self.apply(c) orElse r
  }

  def orElseAndLog(r: => Validation[RequestError,R])(implicit toLog: RequestError => LogLevel):RequestMonad[T,R] = new RequestMonad[T,R] {
    def apply(c:HttpRequest[T]):RequestLogger[R] = self.apply(c).orElseAndLog(r)(toLog)
  }

}

object RequestMonad {
  implicit def toRequestMonad[T,R](x:RequestLogger[R]) = new RequestMonad[T,R] {
    def apply(c:HttpRequest[T]):RequestLogger[R] = x
  }

  def getParams[T]:RequestMonad[T,ParamOps] = new RequestMonad[T,ParamOps] {
    def apply(c:HttpRequest[T]):RequestLogger[ParamOps] = mkRequestLogger {
      val names = c.parameterNames
      (new ParamOps((Map.empty[String, Seq[String]] /: names) {
        (m, n) => m + (n -> c.parameterValues(n))
      })).success
    }
  }
}
