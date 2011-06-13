package unfiltered.monad
import scalaz._
import Scalaz._

trait LogLevel
case class Trace(message:String) extends LogLevel
case class Debug(message:String) extends LogLevel
case class Info(message:String) extends LogLevel
case class Warn(message:String, exception:Option[Throwable] = None) extends LogLevel
case class Error(message:String, exception:Option[Throwable] = None) extends LogLevel

object LogLevel {
  implicit val defaultException:Option[Throwable] = None

  class LogLevelW(message:String) {
    def trace:LogLevel = Trace(message)
    def debug:LogLevel = Debug(message)
    def info:LogLevel = Info(message)
    def warn(implicit exception:Option[Throwable]):LogLevel = Warn(message,exception)
    def err(implicit exception:Option[Throwable]):LogLevel = Error(message,exception)
  }

  implicit def string2LogLevel(message:String):LogLevelW = new LogLevelW(message)

  implicit def LogLevelShow: Show[LogLevel] = new Show[LogLevel] {
    def show(a: LogLevel) = a.toString.toList
  }

}

sealed case class RequestLogger[E, R](log: RequestLogger.LOG[LogLevel], over: Validation[E,R]) extends NewType[Writer[IndSeq[LogLevel], Validation[E,R]]] {
  import LogLevel._
  import RequestLogger._
  import FingerTree._

  val value = writer(log, over)

  def map[B](f:R => B): RequestLogger[E,B] = RequestLogger[E,B](RequestLogger.this.log,RequestLogger.this.over map f)

  def flatMap[B](f:R => RequestLogger[E,B]): RequestLogger[E,B] = {
    over.fold(failure = f1 => RequestLogger[E,B](RequestLogger.this.log,f1.fail),
              success = s => {
                val l = f(s)
                RequestLogger[E,B](RequestLogger.this.log |+| l.log,l.over)
              })
  }

  def foreach(f:R => Unit) =
    over.foreach(f)

  /**
   * Transform the log by the given function.
   */
  def withLog(k: LOG[LogLevel] => LOG[LogLevel]): RequestLogger[E,R] = RequestLogger[E,R](k(RequestLogger.this.log),RequestLogger.this.over)

  /**
   * Transform each log value by the given function.
   */
  def withEachLog(k: LogLevel => LogLevel): RequestLogger[E,R] =
    withLog(_ ∘ k)

  /**
   * Set the log to the given value, losing any previous value.
   */
  def setLog(l: LOG[LogLevel]): RequestLogger[E,R] =
    withLog(_ => l)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: LogLevel): RequestLogger[E,R] =
    withLog(_ |+| e.η[LOG])

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :->>(e:Validation[E,R] => LogLevel): RequestLogger[E,R] =
    :+->(e(over))

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LogLevel): RequestLogger[E,R] =
    withLog(e.η[LOG] |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-:(e:Validation[E,R] => LogLevel): RequestLogger[E,R] =
    <-+:(e(over))

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG[LogLevel]): RequestLogger[E,R] =
    withLog(_ |+| e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :+->>(e:Validation[E,R] => LOG[LogLevel]): RequestLogger[E,R] =
    withLog(_ |+| e(over))

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG[LogLevel]): RequestLogger[E,R] =
    withLog(e |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-+:(e:Validation[E,R] => LOG[LogLevel]): RequestLogger[E,R] =
    <-++:(e(over))

  /**
   * Set the log to be empty.
   */
  def resetLog: RequestLogger[E,R] =
    withLog(_ => ∅[LOG[LogLevel]])

  /**
   * Runs the given side-effect on the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def effectLog(k: LOG[LogLevel] => Unit): RequestLogger[E,R] = {
    k(log)
    this
  }

  /**
   * Runs the given side-effect on each element of the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def effectEachLog(k: LogLevel => Unit): RequestLogger[E,R] =
    effectLog(_ foreach k)

  /**
   * Runs the given side-effect on the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def flushLog(k: LOG[LogLevel] => Unit): RequestLogger[E,R] = {
    effectLog(k)
    resetLog
  }

  /**
   * Runs the given side-effect on each element of the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def flushEachLog(k: LogLevel => Unit): RequestLogger[E,R] = {
    effectLog(_ foreach k)
    resetLog
  }

  /**
   * Prints the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def printLog: RequestLogger[E,R] =
    effectLog(_.println)

  /**
   * Prints each element of the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def printEachLog: RequestLogger[E,R] =
    effectEachLog(_.println)

  /**
   * Prints the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def printFlushLog: RequestLogger[E,R] =
    flushLog(_.println)

  /**
   * Prints each element of the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def printFlushEachLog: RequestLogger[E,R] =
    flushEachLog(_.println)

  def orElse(r: => Validation[E,R]):RequestLogger[E,R] = 
    over.fold(failure = _ => RequestLogger[E,R](RequestLogger.this.log,r),
              success = _ => this)

  def orElseAndLog(r: => Validation[E,R])(implicit toLog: E => LogLevel):RequestLogger[E,R] = {
    over.fold(failure = f => RequestLogger[E,R](RequestLogger.this.log |+| toLog(f).pure[LOG],r),
              success = _ => this)
    
  }
}

object RequestLogger {
  type LOG[C] = IndSeq[C]
  import LogLevel._
  import FingerTree._

  implicit def RequestLoggerInjective[E] = Injective[({type λ[α]= RequestLogger[E,α]})#λ]

  implicit def RequestLoggerPure[E]: Pure[({type λ[α]= RequestLogger[E,α]})#λ] = new Pure[({type λ[α]=RequestLogger[E,α]})#λ] {
    def pure[R](a: => R) = RequestLogger[E,R](∅[LOG[LogLevel]],a.success)
  }

  implicit def RequestLoggerFunctor[E]: Functor[({type λ[α]=RequestLogger[E,α]})#λ] = new Functor[({type λ[α]= RequestLogger[E,α]})#λ] {
    def fmap[R, B](x: RequestLogger[E,R], f: R => B) =
      x map f
  }

  implicit def RequestLoggerApply[E]: Apply[({type λ[α]=RequestLogger[E,α]})#λ] = new Apply[({type λ[α]=RequestLogger[E,α]})#λ] {
    def apply[R, B](f: RequestLogger[E,R=>B], a: RequestLogger[E,R]): RequestLogger[E,B] = {
      val w1 = f.value
      val w2 = a.value
      RequestLogger[E,B](w1.written |+| w2.written,
                         w1.over.fold(failure = f1 => f1.fail,
                                      success = s => w2.over.fold(failure = f2 => f2.fail,
                                                                  success = s1 => s(s1).success)))
    }
  }

  implicit def RequestLoggerBind[E]: Bind[({type λ[α]=RequestLogger[E,α]})#λ] = new Bind[({type λ[α]=RequestLogger[E,α]})#λ] {
    def bind[R, B](a: RequestLogger[E,R], f:R => RequestLogger[E,B]) =
      a flatMap f
  }

  implicit def RequestLoggerEach[E]: Each[({type λ[α]=RequestLogger[E,α]})#λ] = new Each[({type λ[α]= RequestLogger[E,α]})#λ] {
    def each[R](x: RequestLogger[E,R], f:R => Unit) =
      x foreach f
  }

  /*  implicit def RequestLoggerIndex[E]: Index[({type λ[α]=RequestLogger[E,α]})#λ] = new Index[({type λ[α]=RequestLogger[E,α]})#λ] {
   def index[R](a: RequestLogger[E,R], n: Int) =
   if(n == 0) Some(a.over) else None
   }*/

  implicit def RequestLoggerFoldable[E]: Foldable[({type λ[α]=RequestLogger[E,α]})#λ] = new Foldable[({type λ[α]=RequestLogger[E,α]})#λ] {
    override def foldRight[R, B](t: RequestLogger[E,R], b: => B, f: (R, => B) => B) =
      t.over.fold(failure = _ => b,
                  success = s => f(s,b))
  }

  /*  implicit def RequestLoggerTraverse[E]: Traverse[({type λ[α]=RequestLogger[E,α]})#λ] = new Traverse[({type λ[α]=RequestLogger[E,α]})#λ] {
   def traverse[F[_] : Applicative, R, B](f: R => F[B], t: RequestLogger[E,R]) =
   t.over.fold(failure = _ => t <*> {_},
   success = s => f(s) ∘ (b => new RequestLogger[E,B] {
   val log = t.log
   val over = b.success
   }))
   }*/

  implicit def RequestLoggerShow[R : Show, E : Show]: Show[RequestLogger[E,R]] = new Show[RequestLogger[E,R]] {
    def show(a: RequestLogger[E,R]) =
      ("RequestLogger(" + a.log.shows + "," + a.over.shows + ")").toList
  }

  implicit def RequestLoggerEqual[ R: Equal, E:Equal]: Equal[RequestLogger[E,R]] = new Equal[RequestLogger[E,R]] {
    def equal(a1: RequestLogger[E,R], a2: RequestLogger[E,R]) =
      a1.over === a2.over
  }

  implicit def RequestLoggerOrder[ R: Order, E:Order]: Order[RequestLogger[E,R]] = new Order[RequestLogger[E,R]] {
    def order(a1: RequestLogger[E,R], a2: RequestLogger[E,R]) =
      a1.over ?|? a2.over
  }

  implicit def RequestLoggerZero[E,R:Zero]: Zero[RequestLogger[E,R]] = new Zero[RequestLogger[E,R]] {
    val zero = RequestLogger[E,R](∅[LOG[LogLevel]],∅[R].success)
  }
}

trait RequestLoggers {
  import RequestLogger._

  def mkRequestLogger[E,R](a:Validation[E,R]) = 
    RequestLogger[E,R](∅[LOG[LogLevel]],a)
}
