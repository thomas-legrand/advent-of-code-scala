package day16


class day16(val puzzleInput: String, val diskLength: Int) {

  def fillDisk(): String = {
    var s = puzzleInput
    while (s.length < diskLength) {
      s = s + "0" + s.reverse.replaceAll("0", "2").replaceAll("1", "0").replaceAll("2", "1")
    }
    s.substring(0, diskLength)
  }

  def red(b1: Char, b2: Char): String = {
    if (b1.equals(b2)) "1" else "0"
  }

  def reduceBin(s: String): String = {
    assert(s.length % 2 == 0)
    s.grouped(2).toList.map(g => red(g.charAt(0), g.charAt(1))).mkString
  }

  def getChecksum(s: String): String = {
    println(s.length)
    if (s.length % 2 != 0) {
      s
    } else {
      getChecksum(reduceBin(s))
    }
  }
}

object day16 {

  def runTests(): Unit = {
    val d = new day16(puzzleInput = "10000", diskLength = 20)
    assert(d.getChecksum("110010110100") == "100")
    assert(d.getChecksum(d.fillDisk()) == "01100")
  }

  def main(args: Array[String]): Unit = {
    runTests()

    val puzzle_input: String = "01111001100111011"
    val length: Int = 272

    val d1 = new day16(puzzle_input, length)
    println(d1.getChecksum(d1.fillDisk()))

    val newLength: Int = 35651584

    val d2 = new day16(puzzle_input, newLength)
    println(d2.getChecksum(d2.fillDisk()))

  }
}