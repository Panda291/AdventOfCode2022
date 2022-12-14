import scala.io.Source

object Main extends App {
//          val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList
    .map(_.split(' ').toList)

  var register: Int = 1
  var cycle: Int = 1
  var instruction: Int = 0
  var cooldown: Boolean = false
  var sum: Int = 0

  print('#')
  while (instruction < input.length) {
    if (cycle % 40 == 20) {
      sum += cycle * register
    }

    if (input(instruction).head == "noop") {
      cycle += 1
      instruction += 1
    } else if (input(instruction).head == "addx" && cooldown) { // i could omit checking for addx
      cooldown = false
      register += input(instruction)(1).toInt
      cycle += 1
      instruction += 1
    } else { // assume it will be addx
      cooldown = true
      cycle += 1
    }

    if ((register -1 to register + 1).contains((cycle - 1) % 40)) print('#') else print('.')
    if (cycle % 40 == 0) print('\n')
  }

  println(s"Part 1: $sum")

}
