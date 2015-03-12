package com.twerkmeister.scalalda

import scala.util.matching.Regex
import scala.util.Random
import scala.collection.mutable.{Map => MutableMap}
import probability_monad.Distribution
import scala.collection.breakOut

class TopicModel {
  val iterations = 100

  def tokenize(document: String): Array[String] = {
    val r = """\W"""
    document.split(r)
  }

  def initializeZ(tokenizedDocuments: Array[Array[String]], k: Int): Array[Array[Int]] = {
    tokenizedDocuments.map { doc =>
      doc.map { token =>
        Random.nextInt(k)
      }
    }
  }

  def initializeCounters(tokenizedDocuments: Array[Array[String]], z: Array[Array[Int]]) = {
    val nDocTopic = MutableMap[(Int, Int), Int]().withDefaultValue(0)
    val nWordTopic = MutableMap[(String, Int), Int]().withDefaultValue(0)
    val nTopic = MutableMap[Int, Int]().withDefaultValue(0)
    val words = scala.collection.mutable.Set[String]()

    for {doc <- 0 until tokenizedDocuments.size
         word <- 0 until tokenizedDocuments(doc).size
    } {
      nDocTopic(doc, z(doc)(word)) += 1
      nWordTopic(tokenizedDocuments(doc)(word), z(doc)(word)) += 1
      nTopic(z(doc)(word)) += 1
      words.add(tokenizedDocuments(doc)(word))
    }
    (nDocTopic, nWordTopic, nTopic, words)
  }


  def run(documents: Array[String], alpha: Double, beta: Double, K: Int) = {
    val tokenizedDocuments = documents.map(tokenize)
    val z = initializeZ(tokenizedDocuments, K)
    val (nDocTopic, nWordTopic, nTopic, words) = initializeCounters(tokenizedDocuments, z)
    val numWords = words.size

    for {i <- 0 until iterations} {
      println(i)
      for {
        doc <- 0 until tokenizedDocuments.size
        word <- 0 until tokenizedDocuments(doc).size
      } {
        // lower counters
        nDocTopic(doc, z(doc)(word)) -= 1
        nWordTopic(tokenizedDocuments(doc)(word), z(doc)(word)) -= 1
        nTopic(z(doc)(word)) -= 1

        val topicProbabilities = for {topic <- 0 until K} yield {
          val prob = (nDocTopic(doc, topic) + alpha) * (nWordTopic(tokenizedDocuments(doc)(word), topic) + beta) / (nTopic(topic) + beta * numWords)
          topic -> prob
        }

        val distribution = Distribution.discrete(topicProbabilities: _*)
        val newTopic = distribution.sample(1).head
        z(doc)(word) = newTopic
        nDocTopic(doc, z(doc)(word)) += 1
        nWordTopic(tokenizedDocuments(doc)(word), z(doc)(word)) += 1
        nTopic(z(doc)(word)) += 1
      }
    }

    val pDocTopic: Map[(Int, Int), Double] =
      (for {doc <- 0 until tokenizedDocuments.size
            topic <- 0 until K
      } yield {
        (doc -> topic) -> (nDocTopic((doc, topic)) + alpha) / ((0 until K).map { k => nDocTopic((doc, k)) + alpha}.sum)
      })(breakOut)

    val pWordTopic: Map[(String, Int), Double] =
      (for {word <- words
            topic <- 0 until K
      } yield {
        (word -> topic) -> (nWordTopic(word, topic) + beta) / (nTopic(topic) + beta * numWords)
      })(breakOut)

    (pDocTopic, pWordTopic)

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
