package com.twerkmeister.scalalda

import scala.annotation.tailrec
import scala.util.Random
object ProbabilityDistribution {
  def drawFrom[A](weightedValues : Seq[(A, Double)]): A = {
//    println(s"weightedValues: ${weightedValues.take(10).mkString("\n")}")
    @tailrec
    def loop(weightedValues: Seq[(A, Double)], lastValue: A, p: Double): A = {
      weightedValues match {
        case head +: tail =>
          val remainingP = p - head._2
          if (remainingP <= 0)
            head._1
          else
            loop(tail, head._1, remainingP)
        case _ =>
          println(s"returning lastvalue $lastValue, #values: ${weightedValues.size}")
          lastValue
      }
    }
    val p = Random.nextDouble()
    val reordered = weightedValues
    val value = reordered(0)._1
    loop(reordered, value, p)
  }
}
