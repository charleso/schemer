package com.bt.acp.codec

import hedgehog._
import hedgehog.runner._

object JsonSpec extends Properties {

  override def tests: List[Test] =
    List(
      example("codec example 1", JsonLaws.check(fooCodec, Foo(Bah("x", 3, X3(Foo(Bah("y", 4, X2(1)), 3, 4))), 1, 2)))
    , example("codec example 2", JsonLaws.check(fooCodec, Foo(Bah("x", 3, X2(1)), 1, 2)))
//    , property("codec laws", testJsonLaws)
    )

  case class Foo(s: Bah, i: Int, f: Int)
  case class Bah(s: String, i: Int, x: X1)
  sealed trait X1
  object X1 {

    def x2(i: Int): X1 =
      X2(i)
    def x2_(i: X1): Option[Int] =
      i match {
        case X2(j) =>
          Some(j)
        case _ =>
          None
      }
    def x3(i: Foo): X1 =
      X3(i)
    def x3_(i: X1): Option[Foo] =
      i match {
        case X3(j) =>
          Some(j)
        case _ =>
          None
      }
  }
  case class X2(i: Int) extends X1
  case class X3(s: Foo) extends X1

  val fooCodec: Codec[Foo] = obj(
      field("a", obj(
          field("a", string)
        , field("b", int)
        , field("c", or(
            obj(field("x", int))(X1.x2, X1.x2_)
          , obj(field("x", rec(fooCodec)))(X1.x3, X1.x3_)
          ).lift)
        )(Bah.apply, Bah.unapply).lift)
    , field("b", int)
    , field("c", int)
    )(Foo.apply, Foo.unapply).lift

  def testJsonLaws: Property =
    for {
      c <- CodecGens.genCodec.forAll
      r <- JsonLaws.json(c.codec, c.gen)
    } yield r
}

object JsonLaws {

  def json[A](c: Codec[A], gen: Gen[A]): Property = {
    for {
      a1 <- gen.forAll
    } yield check(c, a1)
  }

  def check[A](c: Codec[A], a1: A): Result = {
    val s = Json.toStringNoSpaces(c, a1)
    val a2 = Json.fromString(c, s)
    (a2 ==== Right(a1))
      .log(s)
  }
}
