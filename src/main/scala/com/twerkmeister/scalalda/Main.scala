package com.twerkmeister.scalalda

import java.io._
import scala.io.Source

object Main extends App {
  val folder = "/home/vegeboy/workspace/playground/articles"
  val articles = new File(folder).listFiles()
    .filter(f => f.getName().endsWith(".txt"))
    .take(1000)
    .map{docFile =>
      val s = Source.fromFile(docFile)
      val content = s.getLines().mkString("\n")
      s.close()
      content
    }


  val tm = new TopicModel
  val (theta, phi, vocab) = tm.run(articles, 0.01, 0.01, 100)
  println("done")
  tm.printTopics(phi, vocab, 8, 100)
  (0 until 100).foreach { i =>
    tm.printTopicProps(theta, i, 0.05)
  }
}
