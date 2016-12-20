package day20


import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Firewall(filename: String, maxIP: Long) {

  val startMap = mutable.Map[Long, ListBuffer[List[Long]]]()
  val endMap = mutable.Map[Long, ListBuffer[List[Long]]]()
  var sortedStart = mutable.SortedSet[Long]()
  var sortedEnd = mutable.SortedSet[Long]()


  def populateMapsFromInput() = {
    val blacklists: List[List[Long]] = Source.fromFile(filename).getLines().toList.map(_.split("-").toList.map(_.toLong))
    blacklists.foreach(l => {
      var v = startMap.getOrElse(l.head, ListBuffer[List[Long]]())
      v += l
      startMap.put(l.head, v)

      v = endMap.getOrElse(l.tail.head, ListBuffer[List[Long]]())
      v += l
      endMap.put(l.tail.head, v)

    })
    sortedStart = mutable.SortedSet(blacklists.map(_.head): _*)
    sortedEnd = mutable.SortedSet(blacklists.map(_.tail.head): _*)
  }

  def findMinValidIP(): Long = {
    var i = 0L
    while (i <= maxIP) {
      if (startMap.contains(i)) {
        i = startMap(i).map(_.tail.head).max + 1
      } else {
        val candidateStarts = sortedStart range (0, i)
        if (candidateStarts.isEmpty) {
          return i
        }
        val maxEnd = candidateStarts.toList.map(startMap(_).map(_.tail.head).max).max
        if (i > maxEnd) {
          return i
        } else {
          i = maxEnd + 1
        }
      }
    }
    -1L
  }

  def countValidIPs(): Long = {
    var i = 0L
    var count = 0
    while (i <= maxIP) {
      if (startMap.contains(i)) {
        i = startMap(i).map(_.tail.head).max + 1
      } else {
        val candidateStarts = sortedStart range (0, i)
        if (candidateStarts.isEmpty) {
          i += 1
          count += 1
        }
        val maxEnd = candidateStarts.toList.map(startMap(_).map(_.tail.head).max).max
        if (i > maxEnd) {
          i += 1
          count += 1
        } else {
          i = maxEnd + 1
        }
      }
    }
    count
  }
}

object Firewall {

  def runTests(): Unit = {
    val firewall = new Firewall("/Users/tom/advent-of-code-scala/src/day20/test", 9L)
    firewall.populateMapsFromInput()
    assert(firewall.findMinValidIP() == 3L)
    assert(firewall.countValidIPs() == 2)

  }

  def main(args: Array[String]): Unit = {
    runTests()
    val firewall = new Firewall("/Users/tom/advent-of-code-scala/src/day20/input", 4294967295L)
    firewall.populateMapsFromInput()
    println(firewall.findMinValidIP())
    println(firewall.countValidIPs())
  }
}