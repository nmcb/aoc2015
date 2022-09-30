import scala.io.*

object Day06 extends App:

  /** Modeling */

  case class Task(cmd: String, x0: Int, y0: Int, x1: Int, y1: Int):
    def affects(x: Int, y: Int): Boolean =
      x >= x0 && x <= x1 && y >= y0 && y <= y1

  object Task:
    def apply(cmd: String, p0x: String, p0y: String, p1x: String, p1y: String): Task =
      Task(cmd, p0x.toInt, p0y.toInt, p1x.toInt, p1y.toInt)

  sealed trait Lights[A]:
    val init: A
    val toggle: A => A
    val on: A => A
    val off: A => A

    private def exec(tasks: List[Task])(x: Int, y: Int): A =
      tasks
        .filter(_.affects(x, y))
        .map(_.cmd match
          case "toggle" => toggle
          case "on"     => on
          case "off"    => off)
        .foldLeft(init)((v,f) => f(v))

    def run(tasks: List[Task]): List[A] =
      List.tabulate(1000, 1000)(exec(tasks)).flatten

  val input: List[Task] =
    def parser(s: String): Task =
      s match
        case s"toggle $p0x,$p0y through $p1x,$p1y"   => Task("toggle", p0x, p0y, p1x, p1y)
        case s"turn on $p0x,$p0y through $p1x,$p1y"  => Task("on"    , p0x, p0y, p1x, p1y)
        case s"turn off $p0x,$p0y through $p1x,$p1y" => Task("off"   , p0x, p0y, p1x, p1y)

    Source
      .fromFile("src/main/resources/input06.txt")
      .getLines
      .map(parser)
      .toList


  /** Part 1 */

  object BooleanLights extends Lights[Boolean]:
    val init   = false
    val toggle = v => !v
    val on     = _ => true
    val off    = _ => false

  val start1: Long = System.currentTimeMillis
  val answer1: Int = BooleanLights.run(input).count(identity)

  println(s"Answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  object IntLights extends Lights[Int]:
    val init   = 0
    val toggle = v => v + 2
    val on     = v => v + 1
    val off    = v => if v <= 0 then 0 else v - 1

  val start2: Long = System.currentTimeMillis
  val answer2: Int = IntLights.run(input).sum

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


  /** Solution assertions */

  assert(answer1 ==   569999, s"Wrong answer part 1: $answer1")
  assert(answer2 == 17836115, s"Wrong answer part 2: $answer2")
