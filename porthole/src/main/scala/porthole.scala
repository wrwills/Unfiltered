package unfiltered.porthole

import unfiltered.request._
import unfiltered.response._

case class Uncrate[T](converter: Seq[String] => Option[T])
case class Read[T](converter: Option[String] => Option[T])

object Open {
  type Opener = Params.Map => Option[ResponseFunction[Any]]
  def apply[A,R](f: A => R, symA: Symbol)
    (implicit unA: Uncrate[A], write: Write[R])
    : Opener = map =>
      for {
        a <- unA.converter(map(symA.name))
      } yield write(f(a))

  def apply[A,B,R](f: (A,B) => R, symA: Symbol, symB: Symbol)
    (implicit unA: Uncrate[A], unB: Uncrate[B], write: Write[R])
    : Opener = map =>
      for {
        a <- unA.converter(map(symA.name))
        b <- unB.converter(map(symB.name))
      } yield write(f(a,b))

  def apply[A,B,C,R](f: (A,B,C) => R, symA: Symbol, symB: Symbol,
                     symC: Symbol)
    (implicit unA: Uncrate[A], unB: Uncrate[B], unC: Uncrate[C],
    write: Write[R])
    : Opener = map =>
      for {
        a <- unA.converter(map(symA.name))
        b <- unB.converter(map(symB.name))
        c <- unC.converter(map(symC.name))
      } yield write(f(a,b,c))
}

object Test {
  object Method extends Params.Extract("method", Params.first)
  val methods = Map(
    "test1" -> Open(Test.test1, 'a),
    "test2" -> Open(Test.test2, 'a),
    "test3" -> Open(Test.test3, 'a),
    "test4" -> Open(Test.test4, 'a),
    "test5" -> Open(Test.test5, 'a, 'b)
  )
  unfiltered.Cycle.Intent[Any,Any] {
    case Path("/api") & Params(params @ Method(name)) =>
      methods.get(name).flatMap { _(params) }.getOrElse(Pass)
  }

  def test1(a: Int) = "okay"
  def test2(a: String) = "okay"
  def test3(a: Seq[String]) = "okay"
  def test4(a: List[Int]) = "okay"
  def test5(a: Set[Int], b: Seq[String]) = "okay"
}
