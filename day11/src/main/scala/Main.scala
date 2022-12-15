import scala.io.Source

object Main extends App {
//            val filename = "src/resources/test_input.txt"
  val filename = "src/resources/input.txt"
  val input = Source.fromFile(filename).getLines().toList
    .sliding(6, 7).toList

 val monkeys = input.map(monkey => {
   val monkeyNumber = monkey(0).split(' ').last.filterNot(_ == ':').toString.toInt
   val items = monkey(1).split(' ').drop(4).map(_.filterNot(_ == ',').toInt).map(BigInt(_)).toList
   val operationParams = monkey(2).split(' ').drop(6).toList
   var operation: BigInt => BigInt = null
   try {
     val operationValue = operationParams.last.toInt
     operation = if (operationParams.head == "*") _ * operationValue else _ + operationValue
   } catch {
     case _: Exception => operation = if (operationParams.head == "*") _.pow(2) else _ * 2
   }
   val test = monkey(3).split(' ').last.toInt
   val trueMonkey = monkey(4).split(' ').last.toInt
   val falseMonkey = monkey(5).split(' ').last.toInt
   Monkey(items, operation, test, trueMonkey, falseMonkey)
 })

  val part2Monkeys: List[Monkey] = monkeys.map(_.copy())

  for (_ <- 1 to 20) {
    monkeys.foreach(monkey => {
      while (monkey.hasItems) {
        val (item, targetMonkey) = monkey.inspectItem()
        monkeys(targetMonkey).addItem(item)
      }
    })
  }

  println(s"Part 1: ${monkeys.sortBy(-_.inspections).take(2).map(_.inspections).product}")


  val totalModulo = part2Monkeys.map(_.test).product
  for (i <- 1 to 10000) {
    part2Monkeys.foreach(monkey => {
      while (monkey.hasItems) {
        val (item, targetMonkey) = monkey.worriedInspectItems(totalModulo)
        part2Monkeys(targetMonkey).addItem(item)
      }
    })
  }

  println(s"Part 2: ${part2Monkeys.sortBy(-_.inspections).take(2).map(_.inspections).product}")

  case class Monkey(var items: List[BigInt], operation: BigInt => BigInt, test: Int, trueMonkey: Int, falseMonkey: Int, var inspections: BigInt = 0) {
    def hasItems: Boolean = items.nonEmpty

    def inspectItem(): (BigInt, Int) = { // item, targetMonkey
      inspections += 1
      var current = items.head
      items = items.tail // first will be thrown soon
      current = operation(current)
      current = (current / 3)
      if (current % test == 0) (current, trueMonkey) else (current, falseMonkey)
    }

    def worriedInspectItems(totalModulo: Int): (BigInt, Int) = {
      inspections += 1
      var current = items.head
      items = items.tail // first will be thrown soon
      current = operation(current)
      current = current % totalModulo
      if (current % test == 0) (current, trueMonkey) else (current, falseMonkey)
    }

    def addItem(item: BigInt): Unit = {
      items = items ::: List(item)
    }
  }
}
