package com.twerkmeister.scalalda

import scala.collection.immutable.SortedMap
import scala.util.matching.Regex
import scala.util.Random
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.breakOut
import probability_monad.Distribution

class TopicModel {
  val iterations = 100

  def tokenize(document: String): Array[String] = {
    val r = """\W"""
    document.split(r)
  }

  def filterWords(tokenizedDocuments: Array[Array[String]]): Array[Array[String]] = {
    val numDocs = tokenizedDocuments.size
    val minRatio = 0.005
    val maxRatio = 0.3
    val documentFrequency = MutableMap[String, Int]().withDefaultValue(0)
    val filteredOut = scala.collection.mutable.Set[String]()
    for {doc <- tokenizedDocuments
         words = doc.toSet
         word <- words
    } {
      documentFrequency(word) += 1
    }
    val documentRatio = documentFrequency.mapValues{count => count.toDouble / numDocs}
    val filteredTokenizedDocuments = tokenizedDocuments.map{tokens => tokens.filter{ token =>
      val passes = token.size > 2 && documentRatio(token) >= minRatio && documentRatio(token) <= maxRatio
      if(! passes) filteredOut.add(token)
      passes
    }}
    println("filtered out:\n===========")
    println(filteredOut.mkString(", "))
    filteredTokenizedDocuments
  }

  def initializeZ(tokenizedDocuments: Array[Array[String]], k: Int): Array[Array[Int]] = {
    tokenizedDocuments.map { doc =>
      doc.map { token =>
        Random.nextInt(k)
      }
    }
  }

  def initializeCounters(tokenizedDocuments: Array[Array[String]], z: Array[Array[Int]], K: Int) = {
    val nDocTopic: MutableMap[Int, MutableMap[Int, Int]] =
      (0 until tokenizedDocuments.size).map(i => i -> MutableMap[Int,Int]().withDefaultValue(0))(breakOut)
    val nTopicWord: MutableMap[Int, MutableMap[String,Int]] =
      (0 until K).map(i => i -> MutableMap[String,Int]().withDefaultValue(0))(breakOut)
    val nTopic = MutableMap[Int, Int]().withDefaultValue(0)
    val words = scala.collection.mutable.Set[String]()

    for {doc <- 0 until tokenizedDocuments.size
         word <- 0 until tokenizedDocuments(doc).size
    } {
      nDocTopic(doc)(z(doc)(word)) += 1
      nTopicWord(z(doc)(word))(tokenizedDocuments(doc)(word)) += 1
      nTopic(z(doc)(word)) += 1
      words.add(tokenizedDocuments(doc)(word))
    }
    (nDocTopic, nTopicWord, nTopic, words)
  }


  def run(documents: Array[String], alpha: Double, beta: Double, K: Int) = {
    val tokenizedDocuments = filterWords(documents.map(tokenize))
    val z = initializeZ(tokenizedDocuments, K)
    val (nDocTopic, nTopicWord, nTopic, words) = initializeCounters(tokenizedDocuments, z, K)
    val numWords = words.size
    println(s"ndocTopic: $nDocTopic")
    println(s"ntopic: $nTopic")

    for {i <- 0 until iterations} {
      println(i)
      for {
        doc <- 0 until tokenizedDocuments.size
        nDoc = nDocTopic(doc)
        word <- 0 until tokenizedDocuments(doc).size
      } {
        // lower counters
        val oldTopic = z(doc)(word)
        nDoc(oldTopic) -= 1
        nTopicWord(oldTopic)(tokenizedDocuments(doc)(word)) -= 1
        nTopic(oldTopic) -= 1

        val topicProbabilities = for {topic <- 0 until K} yield {
          val prob = (nDoc(topic) + alpha) * (nTopicWord(topic)(tokenizedDocuments(doc)(word)) + beta) / (nTopic(topic) + beta * numWords)
          topic -> prob
        }
        val topicProbabilitiesSum = topicProbabilities.map(_._2).sum
        val normalizedTopicProbabilites = topicProbabilities.map{p => (p._1, p._2 / topicProbabilitiesSum)}
//        println(normalizedTopicProbabilites.mkString("\n"))
        val newTopic = ProbabilityDistribution.drawFrom(normalizedTopicProbabilites)
//        val newTopic = Distribution.discrete(topicProbabilities: _*).sample(1).head
//        if(newTopic != z(doc)(word))
//          println(s"new Topic: $newTopic , oldTopic ${z(doc)(word)}")
        z(doc)(word) = newTopic
        nDoc(newTopic) += 1
        nTopicWord(newTopic)(tokenizedDocuments(doc)(word)) += 1
        nTopic(newTopic) += 1
      }
    }
    val pDocTopic: SortedMap[Int, MutableMap[Int, Double]] =
      (0 until tokenizedDocuments.size).map(i => i -> MutableMap[Int, Double]().withDefaultValue(0.0))(breakOut)
    val pTopicWord: SortedMap[Int, MutableMap[String,Double]] =
      (0 until K).map(i => i -> MutableMap[String,Double]().withDefaultValue(0.0))(breakOut)

    for {doc <- 0 until tokenizedDocuments.size
          topic <- 0 until K
    } {
      pDocTopic(doc)(topic) = (nDocTopic(doc)(topic) + alpha) / ((0 until K).map { k => nDocTopic(doc)(k) + alpha}.sum)
    }

    for {word <- words
          topic <- 0 until K
    } yield {
      pTopicWord(topic)(word) = (nTopicWord(topic)(word) + beta) / (nTopic(topic) + beta * numWords)
    }

    (pDocTopic, pTopicWord)

  }

  def inferTopics(doc: String, alpha: Double, beta: Double, K: Int, pWordTopic: MutableMap[(String, Int), Double]) = {
    val tokens = tokenize(doc)
    val topicCounts = MutableMap[Int, Double]().withDefaultValue(0.0)
    for {word <- tokens
         topic <- 0 until K
    } {
      topicCounts(topic) += pWordTopic(word, topic)
    }

    val topicProp: Map[Int, Double] =
      (for {topic <- 0 until K
      } yield {
        topic -> (topicCounts(topic) + alpha) / ((0 until K).map { k => topicCounts(k) + alpha}.sum)
      })(breakOut)

    topicProp
  }
}
