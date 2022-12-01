import scala.io.Source

object Main extends App {
//    val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
  var elves: List[Elf] = Elf(List()) :: List() // new list with an initial Elf inside

  input.foreach {
    case "" => elves = Elf(List()) :: elves // add new elf to front of list
    case snack => elves.head.addSnack(snack.toInt)
  }

  println("Part 1: ", elves.map(_.snackValue).max)
  println("Part 2: ", elves.map(_.snackValue).sortWith(_ > _).take(3).sum)
}

case class Elf(var snacks: List[Int]) {
  def addSnack(snack: Int): Unit = {
    snacks = snack :: snacks
  }

  def snackValue: Int = snacks.sum
}