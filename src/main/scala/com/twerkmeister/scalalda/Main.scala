package com.twerkmeister.scalalda

import java.io._
import scala.io.Source

object Main extends App {
  val folder = "/home/vegeboy/workspace/playground/articles"
  val articles = new File(folder).listFiles()
    .filter(f => f.getName().endsWith(".txt"))
    .take(200)
    .map{docFile => Source.fromFile(docFile).getLines().mkString("\n")}

  val tm = new TopicModel
  val (pDocTopic, pWordTopic) = tm.run(articles, 0.01, 0.01, 100)
  println("done")
  println(pDocTopic.mkString("\n"))
}
