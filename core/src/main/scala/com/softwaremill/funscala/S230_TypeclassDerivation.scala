package com.softwaremill.funscala

import scala.compiletime.*
import scala.deriving.Mirror
import magnolia.*

object S230_TypeclassDerivation extends App:
  // 1.
  trait SafeShow[T]:
    def show(t: T): String

  object SafeShow extends Derivation[SafeShow]:
    // 2.
    override def join[T](ctx: CaseClass[Typeclass, T]): SafeShow[T] =
      val label = ctx.typeInfo.short
      new SafeShow[T]:
        def show(t: T): String = {
          val params = ctx.params.map { param =>
            val paramLabel = param.label
            val paramValue = if (isSensitive(param.label)) "***" else param.typeclass.show(param.deref(t))
            s"$paramLabel=$paramValue"
          }
          val paramsStr = params.mkString(",")
          s"$label($paramsStr)"
        }

    override def split[T](ctx: SealedTrait[SafeShow, T]): SafeShow[T] = (t: T) => t.toString

    // 3.
    private def isSensitive(n: String): Boolean = {
      val nn = n.toLowerCase
      nn.contains("token") || nn.contains("apikey") || nn.contains("password")
    }

    // 4.
    given safeShowAny[T]: SafeShow[T] = new SafeShow[T]:
      def show(t: T) = t.toString

  // 5.
  extension[T] (t: T) {
    def safeShow(using SafeShow[T]): String = summon[SafeShow[T]].show(t)
  }

  // 6.
  case class Test1(f1: String, f2: Int) derives SafeShow
  case class Test2(token: String, tx: Long) derives SafeShow
  case class Test3(x: Int, t2: Test2) derives SafeShow

  // 7. 
  println(Test1("x", 20).safeShow)
  println(Test2("xyz", 100L).safeShow)
  println(Test3(5, Test2("abc", 200L)).safeShow)
