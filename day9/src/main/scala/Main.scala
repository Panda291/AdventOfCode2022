import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs


object Main extends App {
//        val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
//  val filename = "src/resources/second_test_input.txt"
  val input = Source.fromFile(filename).getLines().toList
    .map(_.split(' ')).map(line => (line.head.head, line(1).toInt))

  var H = (0, 0)
//  var T: ArrayBuffer[(Int, Int)] = ArrayBuffer.fill(1)(0,0) // part 1
  var T: ArrayBuffer[(Int, Int)] = ArrayBuffer.fill(9)(0,0) // part 2
  var TVisited: mutable.Set[(Int, Int)] = mutable.Set()
  TVisited.add(T.last)
  var ropeLength = 1

  input.foreach(instruction => {
    val direction = instruction._1
    val distance = instruction._2
    for (_ <- 0 until distance) {
      direction match {
        case 'R' => H = (H._1 + 1, H._2)
        case 'L' => H = (H._1 - 1, H._2)
        case 'U' => H = (H._1, H._2 - 1)
        case 'D' => H = (H._1, H._2 + 1)
      }

      T.update(0, follow(H._1, H._2, T(0)._1, T(0)._2))
      for (i <- 1 until T.length) {
        T.update(i, follow(T(i - 1)._1, T(i - 1)._2, T(i)._1, T(i)._2))
      }
      TVisited += T.last
    }
  })

  println(TVisited.size)
  val minH = TVisited.map(_._2).min
  val maxH = TVisited.map(_._2).max
  val minW = TVisited.map(_._1).min
  val maxW = TVisited.map(_._1).max

  for (x <- minH to maxH) {
    for (y <- minW to maxW) {
      if (TVisited.contains((y, x))) print('X')
      else print('.')
    }
    print('\n')
  }

  def follow(x1: Int, y1: Int, x2: Int, y2: Int): (Int, Int) = {
    val x_dist = x1 - x2
    val y_dist = y1 - y2

    if (abs(x_dist) == 2 && y_dist == 0) {
      if (x_dist > 0) (x2 + 1, y2)
      else (x2 - 1, y2)
    } else if (abs(y_dist) == 2 && x_dist == 0) {
      if (y_dist > 0) (x2, y2 + 1)
      else (x2, y2 - 1)
    } else if (abs(y_dist) == 2 && Set(1,2).contains(abs(x_dist)) || abs(x_dist) == 2 && Set(1,2).contains(abs(y_dist))) {
      var dx, dy = 0
      if (x_dist > 0) dx = 1
      else dx = -1
      if (y_dist > 0) dy = 1
      else dy = -1
      (x2 + dx, y2 + dy)
    } else {
      (x2, y2)
    }
//    TVisited += T
  }
}
