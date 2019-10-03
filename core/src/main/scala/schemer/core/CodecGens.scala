package com.bt.acp.codec

import com.bt.acp.ueba.core.GenPlus

import hedgehog._

trait GenCodec {

  type A
  val codec: Codec[A]
  val gen: Gen[A]
}

object GenCodec {

  def apply[B](codec1: Codec[B], gen1: Gen[B]): GenCodec =
    new GenCodec {
      override type A = B
      override val codec: Codec[A] = codec1
      override val gen: Gen[A] = gen1
    }
}

trait GenObject {

  type A
  val codec: MapFormat[A, A]
  val gen: Gen[A]

  def lift: GenCodec  =
    GenCodec(codec.lift, gen)
}

object GenObject {

  def apply[B](codec1: MapFormat[B, B], gen1: Gen[B]): GenObject =
    new GenObject {
      override type A = B
      override val codec: MapFormat[A, A] = codec1
      override val gen: Gen[A] = gen1
    }
}

object CodecGens {

  def genCodec: Gen[GenCodec] = {
    val small = Gen.choice1(
      Gen.constant(Format.StringF).map(GenCodec.apply(_, GenPlus.genString))
    , Gen.constant(Format.IntF).map(GenCodec.apply(_, GenPlus.genInt))
    )
    def large = Gen.choice1(
      genCodec.map(c => GenCodec(Format.ListF(c.codec), c.gen.list(Range.linear(0, 10))))
    , genObject1.map(_.lift)
    , genObject2.map(_.lift)
    , genObject3.map(_.lift)
    )
    Gen.sized(s => if (s.value <= 1) small else Gen.choice1(small, large.small))
  }

  def genField(prefix: String): Gen[GenObject] =
    Gen.choice1(
      genFieldMandatory(prefix)
    , genFieldOptional(prefix)
    )

  def genFieldMandatory(prefix: String): Gen[GenObject] =
    for {
      s <- GenPlus.genString
      c <- genCodec
    } yield GenObject(field(prefix + s, c.codec), c.gen)

  def genFieldOptional(prefix: String): Gen[GenObject] =
    for {
      s <- GenPlus.genString
      c <- genCodec
    } yield GenObject(fieldOptional(prefix + s, c.codec), c.gen.option)

  def genObject1: Gen[GenObject] =
    genField("")

  def genObject2: Gen[GenObject] =
    for {
      c1 <- genField("1")
      c2 <- genField("2")
    } yield GenObject(
        obj(c1.codec, c2.codec)((x: c1.A, y: c2.A) => (x, y), (x: (c1.A, c2.A)) => Some(x))
      , c1.gen.flatMap(v1 => c2.gen.map(v1 -> _))
      )

  def genObject3: Gen[GenObject] =
    for {
      c1 <- genField("1")
      c2 <- genField("2")
      c3 <- genField("3")
    } yield GenObject(
        obj(c1.codec, c2.codec, c3.codec)((x: c1.A, y: c2.A, z: c3.A) => (x, y, z), (x: (c1.A, c2.A, c3.A)) => Some(x))
      , c1.gen.flatMap(v1 => c2.gen.flatMap(v2 => c3.gen.map(v3 => (v1, v2, v3))))
      )
}
