import scala.io.Source

object Main extends App {
//    val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList

  val pairs = input.map(_.split(',')
      .map(_.split('-'))
        .map(e => {
          Range.inclusive(e.head.toInt, e.last.toInt)
      }))

  val part1 = pairs.map(pair => {
    val intersection = pair.head.intersect(pair.last)
    intersection == pair.head || intersection == pair.last
  }).count(x => x)

  val part2 = pairs.map(pair => {
    pair.head.intersect(pair.last).nonEmpty
  }).count(x => x)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
