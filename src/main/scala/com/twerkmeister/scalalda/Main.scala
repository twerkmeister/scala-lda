package com.twerkmeister.scalalda

import java.io._
import scala.io.Source
import org.joda.time.DateTime
import scala.collection.mutable.{Map => MutableMap}
import breeze.linalg._

object Main {
  val K = 75

  def main(args: Array[String]) = {
    val folder = "assets/articles"
    //those weeks are ok to take , i.e. this is a somewhat coherent group of weeks with enough data
    val okWeeks = Set("2014-44","2014-45","2014-46","2014-47","2014-48","2014-49","2014-50","2014-51","2014-52","2015-01","2015-02","2015-03","2015-04","2015-05","2015-06","2015-07","2015-08","2015-09","2015-10","2015-11")

    //read in all the files
    val articles = new File(folder).listFiles()
      .filter(f => f.getName().endsWith(".txt"))
//      .take(1000)
      .map { docFile =>
      val s = Source.fromFile(docFile)
      val content = s.getLines().mkString("\n")
      s.close()
      val date = new DateTime(docFile.getName.replace(".txt", "").toLong)
      val yearWeek = f"${date.getYear}-${date.getWeekOfWeekyear}%02d"
      val yearWeekDay = f"$yearWeek-${date.getDayOfWeek}%02d"
      (yearWeek, yearWeekDay, content)
    }.filter{
      case (week, _, _) => okWeeks.contains(week)
    }.map{
      case (_, yearWeekDay, content) => yearWeekDay -> content
    }

    /*At this point we have an array of articles that looks like this
    [("2014-50-03", "news article text"), ("2014-51-01", "news article text 2"), ...]
    */
    val stopWords = Source.fromFile(new File("assets/stopwords_de.txt")).getLines().toSet ++Set("the", "and", "com", "pic", "000")
    val tm = new TopicModel
    //run the topic model with the news article texts
    val (theta, phi, vocab) = tm.run(articles.map {
      _._2
    }, 1.0 / K, 0.01, K, stopWords)

    println("done")
    tm.printTopics(phi, vocab, 8, K)

    //article dates with article topic vectors
    val dateToDocTopics = MutableMap[String, List[DenseVector[Double]]]().withDefaultValue(Nil)
    articles.zipWithIndex.foreach{
      case ((dateString, _), docI) =>
        dateToDocTopics.update(dateString, theta(docI, ::).t +: dateToDocTopics(dateString))
    }
    //output article dates and their topic vectors as csv
    val out = new File("out.csv")
    val bw = new BufferedWriter(new FileWriter(out))
    for {(date, topicVectors) <- dateToDocTopics
         topicVector <- topicVectors}{
      val line = s"$date,${topicVector.toArray.map{d => "%.4f".format(d).toDouble}.mkString(",")}\n"
      bw.write(line)
    }
    bw.close()
  }
}
