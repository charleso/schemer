package com.bt.acp.codec

sealed trait Format[A, B]

sealed trait MapFormat[A, B] {

  def lift: Format[A, B] =
    Format.ObjectF(this)
}

object Format {

  case object StringF extends Format[String, String]
  case object IntF extends Format[Int, Int]

  case class ListF[A](f: Format[A, A]) extends Format[List[A], List[A]] {

    def foreach(l: List[A])(g: (Format[A, A], Int, A) => Unit): Unit =
      l.zipWithIndex.foreach(a => g(f, a._2, a._1))
  }

  case class Map[A, B, C, D](a: Format[A, B], f: A => C, g: D => Option[B]) extends Format[C, D]

  case class ObjectF[A, B](f: MapFormat[A, B]) extends Format[A, B]

  case class Rec[A, B](a: () => Format[A, B]) extends Format[A, B]
}

object MapFormat {

  // TODO FIELD
  case class Pure[A, B](s: String, a: Format[A, B]) extends MapFormat[A, B] {

    def foreach[C](t: B)(fa: (Format[A, B], B) => C): C =
      fa(a, t)
  }

  case class FieldOptional[A, B](s: String, a: Format[A, B]) extends MapFormat[Option[A], Option[B]] {

    def foreach[C](t: B)(fa: (Format[A, B], B) => C): C =
      fa(a, t)
  }

  case class Ap[A, B, C, D](a: MapFormat[A, C], b: MapFormat[B, D]) extends MapFormat[(A, B), (C, D)] {

    def foreach(t: (A, B))(fa: (MapFormat[A, C], A) => Unit, fb: (MapFormat[B, D], B) => Unit): Unit = {
      fa(a, t._1)
      fb(b, t._2)
    }

    def foreach2(t: (C, D))(fa: (MapFormat[A, C], C) => Unit, fb: (MapFormat[B, D], D) => Unit): Unit = {
      fa(a, t._1)
      fb(b, t._2)
    }
  }

  case class Alt[A, B, C](a: MapFormat[A, C], b: MapFormat[B, C]) extends MapFormat[Either[A, B], C]

  case class Map[A, B, C, D](a: MapFormat[A, B], f: A => C, g: D => Option[B]) extends MapFormat[C, D]

  def ap[A1, B1, Z1, Z2](s1: String, f1: Format[A1, B1])(f: A1 => Z1, g: Z2 => Option[B1]): MapFormat[Z1, Z2] =
    Map(Pure(s1, f1), f, g)

  def ap[
      A1, A2
    , B1, B2
    , Z1, Z2
    ](s1: String, f1: Format[A1, B1]
    , s2: String, f2: Format[A2, B2]
    )(f: (A1, A2) => Z1
    , g: Z2 => Option[(B1, B2)]
    ): MapFormat[Z1, Z2] =
    Map(Ap(Pure(s1, f1), Pure(s2, f2)), f.tupled, g)

  def ap[
      A1, A2, A3
    , B1, B2, B3
    , Z1, Z2
    ](s1: String, f1: Format[A1, B1]
    , s2: String, f2: Format[A2, B2]
    , s3: String, f3: Format[A3, B3]
    )(f: (A1, A2, A3) => Z1
    , g: Z2 => Option[(B1, B2, B3)]
    ): MapFormat[Z1, Z2] =
    Map(
      Ap(Pure(s1, f1), Ap(Pure(s2, f2), Pure(s3, f3)))
    , (a: (A1, (A2, A3))) => a match { case (a1, (a2, a3)) => f(a1, a2, a3) }
    , b => g(b).map { case (b1, b2, b3) => (b1, (b2, b3)) }
    )

  def ap[
      A1, A2, A3, A4
    , B1, B2, B3, B4
    , Z1, Z2
    ](s1: String, f1: Format[A1, B1]
    , s2: String, f2: Format[A2, B2]
    , s3: String, f3: Format[A3, B3]
    , s4: String, f4: Format[A4, B4]
    )(f: (A1, A2, A3, A4) => Z1
    , g: Z2 => Option[(B1, B2, B3, B4)]
    ): MapFormat[Z1, Z2] =
    Map(
      Ap(Pure(s1, f1), Ap(Pure(s2, f2), Ap(Pure(s3, f3), Pure(s4, f4))))
    , (a: (A1, (A2, (A3, A4)))) => a match { case (a1, (a2, (a3, a4))) => f(a1, a2, a3, a4) }
    , b => g(b).map { case (b1, b2, b3, b4) => (b1, (b2, (b3, b4))) }
    )
}

trait CodecOps {

  import Format._

  def string: Format[String, String] =
    StringF

  def int: Format[Int, Int] =
    IntF

  def rec[A, B](f: => Format[A, B]): Format[A, B] =
    Rec(() => f)
}

trait ObjectOps {

  import MapFormat._

  def field[A, B](f: String, c: Format[A, B]): MapFormat[A, B] =
    Pure(f, c)

  def fieldOptional[A, B](f: String, c: Format[A, B]): MapFormat[Option[A], Option[B]] =
    FieldOptional(f, c)

  def obj[A1, B1, Z1, Z2](f1: MapFormat[A1, B1])(f: A1 => Z1, g: Z2 => Option[B1]): MapFormat[Z1, Z2] =
    Map(f1, f, g)

  def obj[
      A1, A2
    , B1, B2
    , Z1, Z2
    ](f1: MapFormat[A1, B1]
    , f2: MapFormat[A2, B2]
    )(f: (A1, A2) => Z1
    , g: Z2 => Option[(B1, B2)]
    ): MapFormat[Z1, Z2] =
    Map(Ap(f1, f2), f.tupled, g)

  def obj[
      A1, A2, A3
    , B1, B2, B3
    , Z1, Z2
    ](f1: MapFormat[A1, B1]
    , f2: MapFormat[A2, B2]
    , f3: MapFormat[A3, B3]
    )(f: (A1, A2, A3) => Z1
    , g: Z2 => Option[(B1, B2, B3)]
    ): MapFormat[Z1, Z2] =
    Map(
      Ap(f1, Ap(f2, f3))
    , (a: (A1, (A2, A3))) => a match { case (a1, (a2, a3)) => f(a1, a2, a3) }
    , b => g(b).map { case (b1, b2, b3) => (b1, (b2, b3)) }
    )

  def or[A, B](f1: MapFormat[A, B], f2: MapFormat[A, B]): MapFormat[A, B] =
    Map(Alt(f1, f2), (a: Either[A, A]) => a.merge, x => Some(x))
}
