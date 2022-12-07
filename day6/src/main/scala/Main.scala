import scala.io.Source

object Main extends App {
//  val filename = "src/resources/test_input.txt"
    val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList
  val dataStream = input.head

  println(s"Part 1: ${findPacket(dataStream, 4)}")
  println(s"Part 2: ${findPacket(dataStream, 14)}")


  def findPacket(stream: String, headerSize: Int, index: Int = 0): Int = {
    if (dataStream.substring(index, index + headerSize).toSet.size == headerSize) {
      index + headerSize
    } else if (!(stream.length <= index + headerSize)) {
      findPacket(stream, headerSize, index + 1)
    } else throw new Exception
  }
}
