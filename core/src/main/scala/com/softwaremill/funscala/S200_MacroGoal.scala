package com.softwaremill.funscala

object S200_MacroGoal extends App:
  def f(n: Int) = {
    Thread.sleep(500L * n)
    20
  }

  val start = System.currentTimeMillis()
  println {
    try f(10)
    finally
      val end = System.currentTimeMillis()
      println(s"Evaluating f(10) took: ${end-start}ms")
  }
  