import scala.io.Source

object Main extends App {
//    val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList

  val tree: Tree = Tree("/", List(), List())
  var scopeStack: List[String] = List("/")

  input.drop(2).foreach(line => {
    val command = line.split(' ')
    if (command.head == "$") {
      if (command(1) == "cd" && command(2) == "..") scopeStack = scopeStack.tail
      else if (command(1) == "cd") scopeStack = command(2) :: scopeStack
      else if (command(1) == "ls") {} // no action needed
    } else if (command.head == "dir") tree.atStack(scopeStack.reverse.tail).addChild(command(1))
    else tree.atStack(scopeStack.reverse.tail).addFile(command(1), command(0).toInt)
  })

  println(s"Part 1: ${tree.toSizeList.map(_._2).filter(_ <= 100000).sum}")

  val rootSize = tree.size
  val neededSpace = 30000000
  val diskSpace = 70000000
  val maxUsedSpace = diskSpace - neededSpace
  println(s"Part 1: ${tree.toSizeList.map(_._2).sorted.find(rootSize - _ <= maxUsedSpace).get}")

  case class Tree(name: String, var children: List[Tree], var files: List[(String, Int)]) {
    def size: Int = {
      files.map(_._2).sum + children.map(_.size).sum
    }

    def atStack(stack: List[String]): Tree = {
      if (stack.isEmpty) this // last element left is self
      else {
        children.filter(_.name == stack.head)
          .head
          .atStack(stack.tail)
      }
    }

    def addChild(childName: String): Unit = {
      children = Tree(childName, List(), List()) :: children
    }

    def addFile(fileName: String, fileSize: Int): Unit = {
      files = (fileName, fileSize) :: files
    }

    def toSizeList: List[(String, Int)] = {
      (name, size) :: children.flatMap(_.toSizeList)
    }
  }
}
