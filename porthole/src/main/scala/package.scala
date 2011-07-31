package unfiltered

package object porthole {
  import unfiltered.request.Params
  import Params._
  import unfiltered.response.{ResponseFunction,ResponseString}

  private def reseq[A,C[_]](read: Read[A])(to: Seq[A] => C[A]) =
    Uncrate({ seq =>
      Some(to(seq.flatMap { item => read.converter(Some(item)) }))
    })
  implicit def seqC[T](implicit read: Read[T]) = reseq(read)(identity)
  implicit def listC[T](implicit read: Read[T]) = reseq(read){ _.toList }
  implicit def setC[T](implicit read: Read[T]) = reseq(read){ _.toSet }
  implicit def firstC[T](implicit read: Read[T]) =
    Uncrate(first ~> read.converter)
  implicit val intC = Read(int)
  implicit val longC = Read(long)
  implicit val floatC = Read(float)
  implicit val stringC = Read(identity)

  type Write[R] = R => ResponseFunction[Any]
  implicit val stringW = ResponseString
}
