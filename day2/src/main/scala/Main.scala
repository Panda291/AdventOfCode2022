import scala.io.Source

object Main extends App {
//  val filename = "src/resources/test_input.txt"
      val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines()
    .map(_
      .split(' ')
      .toList)
    .toList
  val part2 = input.map(pair => (pair.head.head, pair.last.head)) // chars
  val part1 = part2.map(pair => (toRPS(pair._1), toRPS(pair._2)))

  // part 1
  var score = 0
  part1.foreach {
    case (o, 'R') => score += 1 + win(o, 'R')
    case (o, 'P') => score += 2 + win(o, 'P')
    case (o, 'S') => score += 3 + win(o, 'S')
  }
  println(s"Part 1: $score")

  // part 2
  var score2 = 0
  part2.foreach {
    case (o, 'X') => score2 += 0 + losingMove(o)  // losing move
    case (o, 'Y') => score2 += 3 + drawingMove(o) // drawing move
    case (o, 'Z') => score2 += 6 + winningMove(o) // winning move
    case (o, m) => println(o, m)
  }
  println(s"Part 2: $score2")

  def win(o: Char, m: Char): Int = {
    if (o == m) {
      3
    } else if ((o == 'R' && m == 'P') || (o == 'P' && m == 'S') || (o == 'S' && m == 'R')) {
      6
    } else {
      0
    }
  }

  def toRPS(x: Char): Char = {
    x match {
      case 'A' => 'R'
      case 'X' => 'R'
      case 'B' => 'P'
      case 'Y' => 'P'
      case 'C' => 'S'
      case 'Z' => 'S'
    }
  }

  def losingMove(x: Char): Int = {
    x match {
      case 'A' => 3 // pick scissors
      case 'B' => 1 // pick rock
      case 'C' => 2 // pick paper
    }
  }
  def winningMove(x: Char): Int = {
    x match {
      case 'A' => 2 // pick paper
      case 'B' => 3 // pick scissors
      case 'C' => 1 // pick rock
    }
  }
  def drawingMove(x: Char): Int = {
    x match {
      case 'A' => 1 // pick rock
      case 'B' => 2 // pick paper
      case 'C' => 3 // pick scissors
    }
  }
}
