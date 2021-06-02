package com.softwaremill.funscala

object S220_MacroTest extends App :

  import S210_Macro.timed

  def f(n: Int) = {
    Thread.sleep(500L * n)
    20
  }

  def test = {
    timed {
      println("Start2")
      Thread.sleep(1000L)
      println("End")
    }

    timed(f(1))
    timed(f(2))
  }

  println(test)