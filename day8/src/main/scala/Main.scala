import scala.io.Source

object Main extends App {
//      val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList
  val treeVisibilityMap = input.map(_.toList.map(_.toString.toInt))

  var visibleTrees = 0
  var highestScenicScore = 0

  for (i <- treeVisibilityMap.indices) {
    for (j <- treeVisibilityMap(i).indices) {
      val (left, _ :: right) = treeVisibilityMap(i).splitAt(j)
      val (top, _ :: bottom) = getColumn(j, treeVisibilityMap).splitAt(i)

      val visibleFromTop = top.isEmpty || top.max < treeVisibilityMap(i)(j)
      val visibleFromBottom = bottom.isEmpty || bottom.max < treeVisibilityMap(i)(j)
      val visibleFromLeft = left.isEmpty || left.max < treeVisibilityMap(i)(j)
      val visibleFromRight = right.isEmpty || right.max < treeVisibilityMap(i)(j)

      if (visibleFromTop || visibleFromBottom || visibleFromLeft || visibleFromRight) {
        visibleTrees += 1
      }

      if (i == 3 && j == 2) {
        1 + 1 // debugging line
      }
      val scenicScore = viewDistance(treeVisibilityMap(i)(j), top.reverse) *
                        viewDistance(treeVisibilityMap(i)(j), bottom) *
                        viewDistance(treeVisibilityMap(i)(j), left.reverse) *
                        viewDistance(treeVisibilityMap(i)(j), right)
      highestScenicScore = math.max(scenicScore, highestScenicScore)
    }
  }

  println(s"Part 1: $visibleTrees")
  println(s"Part 2: $highestScenicScore")

  def getColumn(j: Int, treeList: List[List[Int]]): List[Int] = {
    var column: List[Int] = List()
    for (i <- treeList.indices) {
      column = treeList(i)(j) :: column
    }
    column.reverse
  }

  def viewDistance(height: Int, viewDirection: List[Int]): Int = {
    var distance = 0
    var remainingView: List[Int] = viewDirection
    while (remainingView.nonEmpty && remainingView.head < height) {
      distance += 1
      remainingView = remainingView.tail
    }
    if (remainingView.nonEmpty && remainingView.head >= height) distance += 1
    distance
  }
}
