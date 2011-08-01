package unfiltered.request

import org.specs._

object MonadSpecJetty extends unfiltered.spec.jetty.Served with MonadSpec {
  def setup = { _.filter(unfiltered.filter.Planify(intent)) }
}
object MonadSpecNetty extends unfiltered.spec.netty.Served with MonadSpec {
  def setup = { p =>
    unfiltered.netty.Http(p).handler(unfiltered.netty.cycle.Planify(intent))
  }
}

trait MonadSpec extends unfiltered.spec.Hosted {
  import unfiltered.request._
  import unfiltered.request.{Path => UFPath}
  import unfiltered.response._
  import unfiltered.monad._
  import LogLevel._, RequestLogger._, RequestMonad._, DefaultConversions._, RequestError._, ParamOps._
  import java.util.{UUID => JUUID}
  import scala.util.control.Exception._
  import scalaz._
  import Scalaz._
  import dispatch._

  def intent[A,B]: unfiltered.Cycle.Intent[A,B] = {
    case request@GET(UFPath("/int")) => {
      val expected:RequestMonad[A,ResponseString] = for {
        params <- getParams
        number <- params.required[Int]("number")
      } yield ResponseString(number.toString)
      expected(request).over.fold(failure = f => BadRequest ~> ResponseString(f.toString),
                                  success = s => Ok ~> s)
    }
    case request@GET(UFPath("/ints")) => {
      val expected:RequestMonad[A,ResponseString] = for {
        params <- getParams
        number <- params.requiredSeq[Int]("number")
      } yield ResponseString(number.mkString("[",",","]"))
      expected(request).over.fold(failure = f => BadRequest ~> ResponseString(f.toString),
                                  success = s => Ok ~> s)
    }
    case request@GET(UFPath("/string")) => {
      val expected:RequestMonad[A,ResponseString] = for {
        params <- getParams
        string <- params.required[String]("string")
      } yield ResponseString(string)
      expected(request).over.fold(failure = f => BadRequest ~> ResponseString(f.toString),
                                  success = s => Ok ~> s)
    }
    case request@GET(UFPath("/applicative_ints")) => {
      val expected:RequestMonad[A,Int] = for {
        params <- getParams
        number <- ((params.required[Int]("num1") |@| params.required[Int]("num2")){_ + _})
      } yield number
      val result = expected(request)
      expected(request).over.fold(failure = f => BadRequest ~> ResponseString(f.toString),
                                  success = s => Ok ~> ResponseString(s.toString))
    }
  }

  "Monadic extractor" should {
    "match and return number" in {
      Http(host / "int" <<? Map("number" -> "8") as_str) must_=="8"
    }
    "match and return a list of numbers" in {
      Http(host / "ints?number=8" as_str) must_=="[8]"
      Http(host / "ints?number=8&number=9" as_str) must_=="[8,9]"
    }
    "fail if any conversion fails" in {
      Http.when(_ == 400)((host / "ints?number=8&number=nine") as_str) must_=="NonEmptyList(Invalid(Unable to convert values of parameter 'number'->'[8,nine]' to int,None))"
    }
    "fail on a non-number" in {
      Http.when(_ == 400)(host / "int" <<? Map("number" -> "8a") as_str) must_== "NonEmptyList(Invalid(Unable to convert values of parameter 'number'->'8a' to int,None))"
    }
    "match and return a string" in {
      Http(host / "string" <<? Map("string" -> "this is a test") as_str) must_=="this is a test"
    }
    "fail on a missing required argument" in {
      Http.when(_ == 400)(host / "string" as_str) must_=="NonEmptyList(Missing(No value for 'string'))"
    }
  }
  "Applicative Functor extractor" should {
    "match and return number" in {
      Http(host / "applicative_ints" <<? Map("num1" -> "8","num2" -> "8") as_str) must_=="16"
    }
    "Accumulate errors" in {
      Http.when(_ == 400)(host / "applicative_ints" as_str) must_=="NonEmptyList(Missing(No value for 'num1'), Missing(No value for 'num2'))"
      Http.when(_ == 400)(host / "applicative_ints" <<? Map("num1" -> "8") as_str) must_=="NonEmptyList(Missing(No value for 'num2'))"
      Http.when(_ == 400)(host / "applicative_ints" <<? Map("num2" -> "8") as_str) must_=="NonEmptyList(Missing(No value for 'num1'))"
    }
    "Properly fail on non-parseable inputs" in {
      Http.when(_ == 400)(host / "applicative_ints" <<? Map("num1" -> "8a","num2" -> "8b") as_str) must_=="NonEmptyList(Invalid(Unable to convert values of parameter 'num1'->'8a' to int,None), Invalid(Unable to convert values of parameter 'num2'->'8b' to int,None))"
      Http.when(_ == 400)(host / "applicative_ints" <<? Map("num1" -> "8","num2" -> "8b") as_str) must_=="NonEmptyList(Invalid(Unable to convert values of parameter 'num2'->'8b' to int,None))"
      Http.when(_ == 400)(host / "applicative_ints" <<? Map("num1" -> "8a","num2" -> "8") as_str) must_=="NonEmptyList(Invalid(Unable to convert values of parameter 'num1'->'8a' to int,None))"
    }
  }

}
