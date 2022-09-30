import scala.io.*

object Day06 extends App:


  /** Input */

  case class Task(cmd: String, rec: Rec)

  val input: List[Task] =
    def parser(s: String): Task =
      s match
        case s"toggle $p0x,$p0y through $p1x,$p1y"   => Task("toggle", Rec(p0x, p0y, p1x, p1y))
        case s"turn on $p0x,$p0y through $p1x,$p1y"  => Task("on"    , Rec(p0x, p0y, p1x, p1y))
        case s"turn off $p0x,$p0y through $p1x,$p1y" => Task("off"   , Rec(p0x, p0y, p1x, p1y))

    Source
      .fromFile("src/main/resources/input06.txt")
      .getLines
      .map(parser)
      .toList


  /** Modeling */

  case class Rec(x0: Int, y0: Int, x1: Int, y1: Int):
    def contains(x: Int, y: Int): Boolean =
      x >= x0 && x <= x1 && y >= y0 && y <= y1

  object Rec:
    def apply(p0x: String, p0y: String, p1x: String, p1y: String): Rec =
      Rec(p0x.toInt, p0y.toInt, p1x.toInt, p1y.toInt)

  case class Light[A](value: A):
    def exec(cmd: Command[A]): Light[A] =
      Light(cmd.op(value))

  case class Command[A](op: A => A)

  sealed trait Commands[A]:
    val init: Light[A]
    val toggle: Command[A]
    val on: Command[A]
    val off: Command[A]

    private def exec(tasks: List[Task])(x: Int, y: Int): A =
      tasks
        .filter(_.rec.contains(x, y))
        .map(_.cmd match
          case "toggle" => toggle
          case "on"     => on
          case "off"    => off)
        .foldLeft(init)(_ exec _)
        .value

    def exec(tasks: List[Task]): List[A] =
      List.tabulate(1000, 1000)(exec(tasks)).flatten


  /** Part 1 */

  object BooleanLights extends Commands[Boolean]:
    val init   = Light(false)
    val toggle = Command(v => !v)
    val on     = Command(_ => true)
    val off    = Command(_ => false)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = BooleanLights.exec(input).map(on => if on then 1 else 0).sum

  println(s"Answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  object IntLights extends Commands[Int]:
    val init   = Light(0)
    val toggle = Command(v => v + 2)
    val on     = Command(v => v + 1)
    val off    = Command(v => if v <= 0 then 0 else v - 1)

  val start2: Long = System.currentTimeMillis
  val answer2: Int = IntLights.exec(input).sum

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


  /** Solution assertions */

  assert(answer1 ==   569999, s"Wrong answer part 1: $answer1")
  assert(answer2 == 17836115, s"Wrong answer part 2: $answer2")
