package com.bt.acp.codec

import argonaut._

object Json {

  def toStringNoSpaces[A, B](fmt: Format[A, B], b: B): String = {
    val w = new java.io.StringWriter
    encode(fmt, b, w)
    w.toString
  }

  def fromString[A, B](f: Format[A, B], s: String): Either[String, A] =
    Parse.decodeEither(s)(decode(f))

  def encode[A, B](fmt: Format[A, B], a: B, w: java.io.Writer): Unit = {
    fmt match {
      case Format.StringF =>
        w.write('"')
        w.write(a.map(c => StringEscaping.escape(c)).mkString)
        w.write('"')
      case Format.IntF =>
        w.write(a.toString)
      case Format.Map(fa, _, f) =>
        f(a).foreach(b => encode(fa, b, w))
      case Format.Rec(f) =>
        encode(f(), a, w)
      case x@Format.ListF(_) =>
        w.write('[')
        x.foreach(a)((fa, i, b) => {
          if (i > 0) {
            w.write(',')
          }
          encode(fa, b, w)
        })
        w.write(']')
      case x@Format.ObjectF(_) =>
        w.write('{')
        encodeObject(x.f, a, w)
        w.write('}')
    }
  }

  def encodeObject[A, B](fmt: MapFormat[A, B], a: B, w: java.io.Writer): Unit = {
    fmt match {
      case x@MapFormat.Pure(s, _) =>
        w.write('"')
        w.write(s.map(c => StringEscaping.escape(c)).mkString)
        w.write('"')
        w.write(':')
        x.foreach(a)(encode(_, _, w))
      case x@MapFormat.FieldOptional(s, _) =>
        a match {
          case None =>
            // FIXME This should not write out the entire field instead of nulls
            w.write('"')
            w.write(s.map(c => StringEscaping.escape(c)).mkString)
            w.write('"')
            w.write(':')
            w.write("null")
          case Some(b) =>
            w.write('"')
            w.write(s.map(c => StringEscaping.escape(c)).mkString)
            w.write('"')
            w.write(':')
            x.foreach(b)(encode(_, _, w))
        }
      case x@MapFormat.Ap(_, _) =>
        x.foreach2(a)(
          (m, n) => {
            encodeObject(m, n, w)
            w.write(",")
          }
        , encodeObject(_, _, w)
        )
      case x@MapFormat.Alt(f, g) =>
        // TODO This doesn't seem right?!?
        encodeObject(f, a, w)
        encodeObject(g, a, w)
      case MapFormat.Map(fa, _, f) =>
        f(a).foreach(b => encodeObject(fa, b, w))
    }
  }

  def decode[A, B](fmt: Format[A, B]): DecodeJson[A] =
    DecodeJson[A](c =>
      fmt match {
        case Format.StringF =>
          c.as[String]
        case Format.IntF =>
          c.as[Int]
        case Format.Map(fa, f, _) =>
          decode(fa).decode(c).map(d => f(d))
        case Format.Rec(f) =>
          decode(f()).decode(c)
        case Format.ListF(f) =>
          DecodeJson.ListDecodeJson(decode(f)).decode(c)
        case x@Format.ObjectF(_) =>
          decodeObject(x.f).decode(c)
      }
    )

  def decodeObject[A, B](fmt: MapFormat[A, B]): DecodeJson[A] =
    DecodeJson[A](c =>
      fmt match {
        case MapFormat.Pure(s, f) =>
          decode(f).tryDecode(c --\ s)
        case MapFormat.FieldOptional(s, f) =>
          DecodeJson.OptionDecodeJson(decode(f)).tryDecode(c --\ s)
        case MapFormat.Ap(f, g) =>
          decodeObject(f).decode(c).flatMap(a =>
          decodeObject(g).decode(c).map(b =>
            (a, b)
          ))
        case MapFormat.Alt(f, g) =>
          decodeObject(f).decode(c).value match {
            case None =>
              decodeObject(g).decode(c).map(Right(_))
            case Some(x) =>
              DecodeResult.ok(Right(x))
          }
        case MapFormat.Map(fa, f, _) =>
          decodeObject(fa).decode(c).map(d => f(d))
      }
    )
}
