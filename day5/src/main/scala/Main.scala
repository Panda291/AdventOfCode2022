import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex

object Main extends App {
//  val filename = "src/resources/test_input.txt"
    val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList

  val (stack, procedures) = input.span(_ != "")

  val stackCount = stack.last.split("\\s+").tail.length
  val part1Stack = Array.fill(stackCount) {
    List[Char]()
  }


  stack.reverse.tail.foreach(line => {
    var idx = 0
    val boxes = line.drop(1).grouped(4).map(_.head).toList
    for (box <- boxes) {
      if (box != ' ') {
        part1Stack.update(idx, box :: part1Stack(idx))
      }
      idx += 1
    }
  })
  val part2Stack = part1Stack.clone()

  val re = """\d+""".r
  procedures.tail
    .map(re.findAllIn)
    .map(_.toList.map(_.toInt))
    .foreach(instruction => {
      val amount = instruction.head
      val from = instruction(1) - 1
      val to = instruction(2) - 1
      var movingBox: Char = '#'
      // part 1
      for (_ <- 0 until amount) {
        movingBox = part1Stack(from).head
        part1Stack.update(from, part1Stack(from).tail)
        part1Stack.update(to, movingBox :: part1Stack(to))
      }
      // part 2
      var movingBoxes: List[Char] = List()
      movingBoxes = part2Stack(from).take(amount)
      part2Stack.update(from, part2Stack(from).drop(amount))
      part2Stack.update(to, movingBoxes ::: part2Stack(to))
    })
  println(s"Part 1: ${part1Stack.toList.filter(_.nonEmpty).map(_.head).mkString}")
  println(s"Part 2: ${part2Stack.toList.filter(_.nonEmpty).map(_.head).mkString}")
}
