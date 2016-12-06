package day6

import scala.io.Source


class MessageRecovery {

  def getInputAsList(filename: String): List[String] = {
    val path = classOf[MessageRecovery].getResource(filename)
    Source.fromFile(path.getFile).getLines.toList
  }

  def getPassword(input: List[String]): String = {
    val transposed = input.map(s => s.toList).transpose
    transposed.map(l => l.groupBy(identity).mapValues(_.size).maxBy(_._2)._1).mkString
  }

  def computePasswordFromInput(filename: String): String = {
    val input: List[String] = getInputAsList(filename)
    getPassword(input)
  }

  def runTest() = {
    val pass = computePasswordFromInput("test")
    assert(pass == "easter")
  }
}

object MessageRecovery1 extends MessageRecovery {
  def main(args: Array[String]): Unit = {
    runTest()
    println(computePasswordFromInput("input"))
  }
}

object MessageRecovery2 extends MessageRecovery {

  override def getPassword(input: List[String]): String = {
    val transposed = input.map(s => s.toList).transpose
    transposed.map(l => l.groupBy(identity).mapValues(_.size).minBy(_._2)._1).mkString
  }

  override def runTest() = {
    val pass = computePasswordFromInput("test")
    assert(pass == "advent")
  }

  def main(args: Array[String]): Unit = {
    runTest()
    println(computePasswordFromInput("input"))
  }
}
