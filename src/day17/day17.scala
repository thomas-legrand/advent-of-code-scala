package day17

import java.security.MessageDigest

import scala.collection.mutable


class day17(puzzleInput: String) {

  val openChar: List[Char] = List('b', 'c', 'd', 'e', 'f')

  case class Position(x: Int, y: Int)
  def up(position: Position) = Position(position.x - 1, position.y)
  def down(position: Position) = Position(position.x + 1, position.y)
  def left(position: Position) = Position(position.x, position.y -1)
  def right(position: Position) = Position(position.x, position.y  + 1)

  case class State(position: Position, path: List[String])

  val dir = Map[String, Function[Position, Position]](
    "U" -> up,
    "D" -> down,
    "L" -> left,
    "R" -> right
  )

  val orders = List("U", "D", "L", "R")

  val order = Map[Int, String](0 -> "U", 1 -> "D", 2 -> "L", 3 -> "R")

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def possible(path: List[String]): List[String] = {
    val h: String = md5(puzzleInput + path.mkString).substring(0, 4)
    val mask = h.toStream.map(openChar.contains(_))
    orders.zip(mask).collect { case (v, true) => v }
  }

  def move(state: State, direction: String): State = {
    val m = dir.get(direction)
    if (m.nonEmpty) {
      val f = m.get
      val nextPosition = f(state.position)
      val nextPath: List[String] = state.path ::: List(direction)
      if (0 < nextPosition.x & nextPosition.x <= 4 & 0 < nextPosition.y & nextPosition.y <= 4) State(nextPosition, nextPath) else null
    } else null
  }

  val paths = new mutable.HashMap[Position, List[List[String]]]()

  def reset() = {
    paths.clear()
  }

  def shortestPath(): String = {
    val q = new mutable.Queue[State]()
    var minPathLength = 10000
    q += State(Position(1, 1), List())
    while (q.nonEmpty) {
      val current = q.dequeue
      if (current.path.length < minPathLength) {
        if (paths isDefinedAt current.position) paths(current.position) ++= List(current.path) else paths.put(current.position, List(current.path))

        if (current.position.equals(Position(4, 4)) & current.path.length < minPathLength) {
          minPathLength = current.path.length
        }
        q ++= possible(current.path).map(move(current, _)).filter(_!= null)
      }
    }
    paths(Position(4, 4)).sortWith((l1, l2) => l1.length < l2.length).head.mkString
  }

  def longestPath(): String = {
    val q = new mutable.Queue[State]()
    q += State(Position(1, 1), List())
    while (q.nonEmpty) {
      val current = q.dequeue

      if (paths isDefinedAt current.position) paths(current.position) ++= List(current.path) else paths.put(current.position, List(current.path))
      if (!current.position.equals(Position(4, 4))) {
        q ++= possible(current.path).map(move(current, _)).filter(_!= null)
      }
    }
    paths(Position(4, 4)).sortWith((l1, l2) => l1.length > l2.length).head.mkString
  }
}


object day17 {

  def runTests(): Unit = {
    val d = new day17("hijkl")
    assert(d.possible(List()).toSet == List("U", "D", "L").toSet)
    assert(d.possible(List("D")).toSet == List("U", "R", "L").toSet)
    assert(d.possible(List("DU")).toSet == List("R").toSet)
    assert(d.possible(List("DUR")).toSet == List().toSet)

    val d1 = new day17("ihgpwlah")
    assert(d1.shortestPath() == "DDRRRD")

    val d2 = new day17("kglvqrro")
    assert(d2.shortestPath() == "DDUDRLRRUDRD")

    val d3 = new day17("ulqzkmiv")
    assert(d3.shortestPath() == "DRURDRUDDLLDLUURRDULRLDUUDDDRR")

    assert(d1.longestPath().length == 370)
    assert(d2.longestPath().length == 492)
    assert(d3.longestPath().length == 830)

  }

  def main(args: Array[String]): Unit = {
    runTests()
    val d = new day17("mmsxrhfx")
    println(d.shortestPath())
    d.reset()
    println(d.longestPath().length)
  }
}