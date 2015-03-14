package com.twerkmeister.scalalda

import scala.util.{Try, Random, Success, Failure}
import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.{Map => MutableMap, HashMap => MutableHashMap}

class TopicModel {
  val iterations = 100

  def tokenize(document: String): Array[String] = {
    val r = """\W"""
    document.split(r)
  }

  def buildVocab(tokenizedDocuments: Array[Array[String]]): (Array[Array[String]], MutableHashMap[String, Int]) = {
    val numDocs = tokenizedDocuments.size
    val minRatio = 0.005
    val maxRatio = 0.25

    val words = scala.collection.mutable.Set[String]()
    val documentFrequency = MutableMap[String, Int]().withDefaultValue(0)
    val filteredOut = scala.collection.mutable.SortedSet[String]()
    val vocab = MutableHashMap[String, Int]()

    for {doc <- tokenizedDocuments
         word <- doc.toSet[String]
    } {
      documentFrequency(word) += 1
      words += word
    }

    val documentRatio = documentFrequency.mapValues{count => count.toDouble / numDocs}

    words.foreach{ word =>
//      val passes = word.size > 2 && documentRatio(word) >= minRatio && documentRatio(word) <= maxRatio
      val passes = word.size > 2 && documentFrequency(word) > 1 && documentRatio(word) <= maxRatio
      if(passes) vocab(word) = vocab.size
      else filteredOut.add(word)
    }
    println(s"vocab size: ${vocab.size}")
    println("filtered out:\n===========")
    println(filteredOut.mkString(", "))


    (tokenizedDocuments.map{tokens => tokens.filter{ token => vocab.contains(token)}}, vocab)
  }

  def initializeZ(tokenizedDocuments: Array[Array[String]], K: Int): Array[Array[Int]] = {
    tokenizedDocuments.map { doc =>
      doc.map { token =>
        Random.nextInt(K)
      }
    }
  }

  def initializeCounters(tokenizedDocuments: Array[Array[String]], z: Array[Array[Int]], K: Int, vocab: MutableHashMap[String, Int]) = {
    val numDocs = tokenizedDocuments.size
    val vocabSize = vocab.size
    val theta = DenseMatrix.zeros[Double](numDocs, K)
    val phi = DenseMatrix.zeros[Double](K, vocabSize)
    val topicSums = DenseVector.zeros[Double](K)

    var docI = 0
    while(docI < numDocs){
      var wordI = 0
      val docSize = tokenizedDocuments(docI).size
      while(wordI < docSize){
        theta(docI, z(docI)(wordI)) += 1.0
        phi(z(docI)(wordI), vocab(tokenizedDocuments(docI)(wordI))) += 1.0
        topicSums(z(docI)(wordI)) += 1.0
        wordI += 1
      }
      docI += 1
    }
    (theta, phi, topicSums)
  }


  def run(documents: Array[String], alpha: Double, beta: Double, K: Int) = {
    val (tokenizedDocuments, vocab) = buildVocab(documents.map(tokenize))
    val z = initializeZ(tokenizedDocuments, K)
    val (theta, phi, topicSums) = initializeCounters(tokenizedDocuments, z, K, vocab)

    val vocabSize = vocab.size
    val numDocs = tokenizedDocuments.size

    //optimization
    theta += alpha
    phi += beta
    topicSums += beta * vocabSize


    var i = 0
    while(i < iterations) {
      println(s"iteration: $i")
      var docI = 0
      while(docI < numDocs) {
        var wordI = 0
        val docSize = tokenizedDocuments(docI).size
        while(wordI < docSize ){
          val vocabIndex = vocab(tokenizedDocuments(docI)(wordI))
          val oldTopic = z(docI)(wordI)

          //decrement counts of old topic assignment
          phi(oldTopic, vocabIndex) -= 1.0
          theta(docI, oldTopic) -= 1.0
          topicSums(oldTopic) -= 1.0

          val docTopicRow: DenseVector[Double] = theta(docI, ::).t
          val topicWordCol: DenseVector[Double] = phi(::, vocabIndex)
//          val params = (docTopicRow + alpha) :* (topicWordCol + beta) / (topicSums + vocabSize * beta)
          //adding alpha and beta optimized
          val params = (docTopicRow) :* (topicWordCol) / (topicSums)
          val normalizingConstant = sum(params)
          val normalizedParams = params / normalizingConstant

          val newTopic = ProbabilityDistribution.drawFrom(normalizedParams)
          z(docI)(wordI) = newTopic
          //increment counts to due to reassignment to new topic
          phi(newTopic, vocabIndex) += 1.0
          theta(docI, newTopic) += 1.0
          topicSums(newTopic) += 1.0

          wordI += 1
        }
        docI +=1
      }
      i += 1
    }

    //reversing optimization
    theta -= alpha
    phi -= beta
    topicSums -= beta * vocabSize

    //we turn the counts matrix into a probability matrix
    var docI = 0
    while (docI < numDocs){
      val countToProb: DenseVector[Double] = ((theta(docI, ::) + alpha) / (sum(theta(docI, ::).t) + K * alpha)).t
      theta(docI, ::) := countToProb.t
      docI += 1
    }

    var topicI = 0
    while (topicI < K){
      val countToProb: DenseVector[Double] = ((phi(topicI, ::) + alpha) / (sum(phi(topicI, ::).t) + K * alpha)).t
      phi(topicI, ::) := countToProb.t
      topicI += 1
    }

    (theta, phi, vocab)
  }

  def printTopics(phi: DenseMatrix[Double], vocab: MutableHashMap[String, Int], numWords: Int, K: Int) {

    val revVocab = vocab.map(_ swap)

    for (topic <- 0 until K) {
      //tie probability to column index, then sort by probabiltiy, take the top numWords, map column index to corresponding word
      println("Topic #" + topic + ":  " + phi(topic, ::).t.toArray.zipWithIndex.sortBy(-_._1).take(numWords).toList.map(x => revVocab(x._2)))
    }
  }

  def printTopicProps(theta: DenseMatrix[Double], docIndex: Int, probCutoff: Double) {
    println(theta(docIndex, ::).t.toArray.zipWithIndex.filter(x => x._1 > probCutoff).toList)

  }
}
