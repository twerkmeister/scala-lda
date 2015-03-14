package com.twerkmeister.scalalda

import scala.annotation.tailrec
import scala.util.Random
import breeze.linalg.DenseVector

object ProbabilityDistribution {
  def drawFrom(probabilities : DenseVector[Double]): Int= {
//    println(s"weightedValues: ${weightedValues.take(10).mkString("\n")}")
    @tailrec
    def loop(index: Int , p: Double): Int = {
      if(index < probabilities.length){
        val remainingP = p - probabilities(index)
        if (remainingP <= 0) index
        else loop(index+1, remainingP)

      }
      else index - 1
    }
    val p = Random.nextDouble()
    loop(0, p)
  }
}
