package unfiltered.monad
import scalaz._
import Scalaz._
import unfiltered.request._
import scala.util.control.Exception._

trait Conversion[T] {
  def to(in:String):T
  def toSeq(in:Seq[String]):Seq[T]
}

object DefaultConversions {
  implicit val stringConversions = new Conversion[String] {
    def to(in:String):String = in
    def toSeq(in:Seq[String]):Seq[String] = in
  }

  implicit val intConversions = new Conversion[Int] {
    def to(in:String):Int = in.toInt
    def toSeq(in:Seq[String]):Seq[Int] = in.map(_.toInt)
  }

  implicit val boolConversions = new Conversion[Boolean] {
    def to(in:String):Boolean = in.toBoolean
    def toSeq(in:Seq[String]):Seq[Boolean] = in.map(_.toBoolean)
  }
}

class ParamOps(val params:Map[String,Seq[String]]) {
  self => 
  import RequestLogger._
  import LogLevel._
  import DefaultConversions._

  def required[T](key:String)(implicit c:Conversion[T], m:Manifest[T]):RequestLogger[String,T] = mkRequestLogger {
    (params.get(key).map {
      x => allCatch.opt(c.to(x.head).success).getOrElse("Unable to convert %s->'%s' to %s".format(key,x.head,m.erasure.getName).fail)
    }) getOrElse ("No value for '%s'".format(key).fail)
  }

  def optional[T](key:String)(implicit c:Conversion[T], m:Manifest[T]):RequestLogger[String,Option[T]] = 
    mkRequestLogger { (allCatch.either(params
                                       .get(key)
                                       .map(x => c.to(x.head)))
                       .fold(fa = _ => "Unable to convert values of %s->'%s' to %s".format(key,params(key).head,m.erasure.getName).fail,
                             fb = s => s.success)) }

  def requiredSeq[T](key:String)(implicit c:Conversion[T], m:Manifest[T]):RequestLogger[String,Seq[T]] = mkRequestLogger {
    (params.get(key).map {
      x => allCatch.opt(c.toSeq(x).success).getOrElse("Unable to convert values of %s->'%s' to %s".format(key,x,m.erasure.getName).fail)
    }) getOrElse ("No value for '%s'".format(key).fail)
  }

  def optionalSeq[T](key:String)(implicit c:Conversion[T], m:Manifest[T]):RequestLogger[String,Option[Seq[T]]] = 
    mkRequestLogger { (allCatch.either(params
                                       .get(key)
                                       .map(x => c.toSeq(x)))
                       .fold(fa = _ => "Unable to convert values of %s->'%s' to %s".format(key,params(key),m.erasure.getName).fail,
                             fb = s => s.success)) }

}

