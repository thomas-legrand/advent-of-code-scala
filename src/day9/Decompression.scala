import scala.io.Source


object Decompression {

  def getInput(filename: String): String = {
    Source.fromFile(filename).getLines().toList.mkString
  }


  val pattern = """\w*\(([\dx]+)\)""".r

  def decompress(s: String): String = {
    val match_code = pattern findFirstMatchIn s
    if (match_code.isEmpty) {
      return s
    }
    val code = match_code map (_.group(1)) get
    val index_end_code: Int = match_code map (_.end(1)) get
    val index_start_code = match_code map (_.start(1)) get
    val which = code.split("x")(0).toInt
    val times = code.split("x")(1).toInt
    val charsToRepeat = s.substring(index_end_code + 1, index_end_code + which + 1)
    val repeatedChar = charsToRepeat * times
    val startS = s.substring(0, index_start_code -1)
    val endS = s.substring(index_end_code + which + 1)
    startS + repeatedChar + decompress(endS)
  }

  def decompress2(s: String): Long = {
    val match_code = pattern findFirstMatchIn s
    if (match_code.isEmpty) {
      return s.length
    }
    val code = match_code map (_.group(1)) get
    val index_end_code: Int = match_code map (_.end(1)) get
    val index_start_code = match_code map (_.start(1)) get
    val spl = code.split("x")
    val which = spl(0).toInt
    val times = spl(1).toInt
    val charsToRepeatLength = decompress2(s.substring(index_end_code + 1, index_end_code + which + 1))
    val repeatedCharLength = charsToRepeatLength * times
    val startS = index_start_code - 1
    val endS = s.substring(index_end_code + which + 1)
    startS + repeatedCharLength + decompress2(endS)
  }


  def runTests(): Unit = {
    /*
    ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6.
    A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7.
    (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
    A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11.
    (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of another marker, it is not treated any differently from the A that comes after it. It has a decompressed length of 6.
    X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed further.
    */
    assert(decompress("ADVENT") == "ADVENT")
    assert(decompress("A(1x5)BC") == "ABBBBBC")
    assert(decompress("(3x3)XYZ") == "XYZXYZXYZ")
    assert(decompress("A(2x2)BCD(2x2)EFG") == "ABCBCDEFEFG")
    assert(decompress("(6x1)(1x3)A") == "(1x3)A")
    assert(decompress("X(8x2)(3x3)ABCY") == "X(3x3)ABC(3x3)ABCY")

    /*
    (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
    X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from the (8x2) marker is then further decompressed, thus triggering the (3x3) marker twice for a total of six ABC sequences.
    (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
    (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
     */

    assert(decompress2("(3x3)XYZ") == 9)
    assert(decompress2("X(8x2)(3x3)ABCY") == "XABCABCABCABCABCABCY".length)
    assert(decompress2("(27x12)(20x12)(13x14)(7x10)(1x12)A") == 241920)
    assert(decompress2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") == 445)

  }

  def main(args: Array[String]): Unit = {
    runTests()
    val input = getInput("/Users/tom/advent-of-code-scala/src/day9/input")
    println(decompress(input).length)
    println(decompress2(input))
  }
}

