package day8


import scala.io.Source
import scala.reflect.ClassTag



class Display {

  val width = 50
  val height = 6
  var display = Array.fill(6, 50)(0)

  def updateArrayFromString(s :String) = {
    val spl = s.split(Array(' ', 'x'))
    for (r <- 0 until spl(1).toInt) {
      for (c <- 0 until spl(2).toInt) {
        display(c)(r) = 1
      }
    }
  }

  def rotateRow(row: Int, number: Int) = {
    this.display(row) = Display.sh(number, this.display(row))
  }

  def rotateColumn(column: Int, number: Int) = {
    val transposed = display.transpose
    transposed(column) = Display.sh(number, transposed(column))
    this.display = transposed.transpose
  }

  override def toString: String = {
    display.map(a => a.mkString(" ")).mkString("\n").concat("\n\n")
  }
}

object Display {

  def sh[T:ClassTag](num: Int, a : Array[T]): Array[T] = {
    val s: Int = num % a.length
    (a takeRight s) ++ (a dropRight s)
  }

  def getInput(filename: String): Iterator[String] = {
    Source.fromFile(filename).getLines()
  }


  def runTests(): Unit = {

    assert(sh(1, Array(1,2,3)).deep == Array(3, 1, 2).deep)
    assert(sh(3, Array(1,2,3)).deep == Array(1,2,3).deep)
    assert(sh(2, Array(1,2,3)).deep == Array(2,3, 1).deep)
    assert(sh(1, Array("a", "s", "e")).deep == Array("e", "a", "s").deep)
  }

  def main(args: Array[String]): Unit = {
    runTests()
    val d = new Display

    for (line <- getInput("/Users/tom/advent-of-code-scala/src/day8/input")) {
      if (line.startsWith("rect")) {
        d.updateArrayFromString(line)
      } else if (line.startsWith("rotate column")) {
        val spl = line.split("=")
        val instr = spl(1).split(" by ")
        d.rotateColumn(instr(0).toInt, instr(1).toInt)
      } else if (line.startsWith("rotate row")) {
        val spl = line.split("=")
        val instr = spl(1).split(" by ")
        d.rotateRow(instr(0).toInt, instr(1).toInt)
      }
    }
    
    print(d.display.map(a => a.sum).sum)
    print(d)
  }
}



