package day11





class Microchips {

  sealed trait ObjectType {
    def name: String
  }

  case object RTG extends ObjectType {
    val name = "RTG"
  }

  case object CHIP extends ObjectType {
    val name = "microchip"
  }

  val elevator = 1
  var state: Map[Int, Map[String, Set[ObjectType]]] = Map()

  def initialize() = {
    /*
    The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
    The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
    The third floor contains nothing relevant.
    The fourth floor contains nothing relevant.
    */
    state = Map(
      1 -> Map(
        "T" -> Set(CHIP, RTG),
        "Ru" -> Set(CHIP, RTG),
        "Co" -> Set(CHIP, RTG),
        "Po" -> Set(RTG),
        "Pr" -> Set(RTG)
      ),
      2 -> Map(
        "Po" -> Set(CHIP),
        "Pr" -> Set(CHIP)
      )
    )
  }

  def initializeTest() = {
    /*
    The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
    The second floor contains a hydrogen generator.
    The third floor contains a lithium generator.
    The fourth floor contains nothing relevant.
    */
    state = Map(
      1 -> Map(
        "H" -> Set(CHIP),
        "L" -> Set(CHIP)
      ),
      2 -> Map("H" -> Set(RTG)),
      3 -> Map("L" -> Set(RTG))
    )
  }

  // Make static (or the scala equivalent)!
  def is_possible(new_state: Map[Int, Map[String, Set[ObjectType]]]): Boolean = {
    for ((floor, objects) <- new_state) {
      // compute single chips
      val singleChips = objects.filter(g => g._2 == Set(CHIP)).keys
      val singleGen = objects.filter(g => g._2 == Set(RTG)).keys
      if (singleChips.nonEmpty && singleGen.nonEmpty) {
        return false
      }
    }
    true
  }

  def move(objects: Set[Map[String, ObjectType]], direction: Int): Boolean = {
    if (objects.size < 1 || objects.size > 2) {
      return false
    }
    if ((elevator == 1 && direction == -1) || (elevator == 4 && direction == 1)) {
      return false
    }
    for (o <- objects) {

    }
    true
  }

}class Microchips2 extends Microchips {
  override def initialize(): Unit = {
    state = Map(
      1 -> Map(
        "T" -> Set(CHIP, RTG),
        "Ru" -> Set(CHIP, RTG),
        "Co" -> Set(CHIP, RTG),
        "Po" -> Set(RTG),
        "Pr" -> Set(RTG)
      ),
      2 -> Map(
        "Po" -> Set(CHIP),
        "Pr" -> Set(CHIP)
      )
    )
    state = state.mapValues(m => {
      m.map(case (d, s) => s.map(e => (d, e))).toList
    })
  }
}


object Microchips {

  var m = new Microchips()

  def runTests(): Unit = {


    assert(m.is_possible(
      Map(
        1 -> Map(
          "H" -> Set(m.CHIP),
          "L" -> Set(m.CHIP)
        ),
        2 -> Map("H" -> Set(m.RTG)),
        3 -> Map("L" -> Set(m.RTG))
      )
    ))

    assert(m.is_possible(
      Map(
        1 -> Map(
          "H" -> Set(m.CHIP, m.RTG)
        ),
        2 -> Map("L" -> Set(m.CHIP)),
        3 -> Map("L" -> Set(m.RTG))
      )
    ))

    assert(!m.is_possible(
      Map(
        1 -> Map(
          "H" -> Set(m.CHIP),
          "L" -> Set(m.RTG)
        ),
        2 -> Map("L" -> Set(m.CHIP)),
        3 -> Map("H" -> Set(m.RTG))
      )
    ))

    m = new Microchips()


  }

  def main(args: Array[String]): Unit = {
    println(m.state)
    m.initializeTest()
    println(m.state)
    runTests()



  }

}
