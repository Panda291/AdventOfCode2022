import scala.io.Source

object Main extends App {
//  val filename = "src/resources/test_input.txt"
    val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList
  val lowerCaseOffset = 96 // a will become 1
  val upperCaseOffset = 38 // A will become 27

  val translatedPacks = input.map(_.map {
    case x: Char if x.isLower => x - lowerCaseOffset
    case x: Char if x.isUpper => x - upperCaseOffset
  })

  val rucksacks = translatedPacks.map(pack => {
    val length = pack.length
    pack.splitAt(length / 2)
  })

  val part1 = rucksacks.map(pack => {
    pack._1.intersect(pack._2)
      .head
  }).sum
  println(s"Part 1: $part1")

  var i = 0
  var badges: List[Int] = List()
  while (i < translatedPacks.length) {
    val badge = translatedPacks.apply(i)
      .intersect(translatedPacks.apply(i + 1))
      .intersect(translatedPacks.apply(i + 2))
      .head
    badges = badge :: badges
    i += 3
  }
  println(s"Part 2: ${badges.sum}")
}
