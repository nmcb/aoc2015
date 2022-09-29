import scala.io.*

object Day06 extends App:

  val start1: Long =
    System.currentTimeMillis

  case class Pos(x: Int, y: Int):
    assert(x >= 0 && x <= 999)
    assert(y >= 0 && y <= 999)

  case class Rec(p0: Pos, p1: Pos):
    assert(p0.x <= p1.x)
    assert(p0.y <= p1.y)

    def contains(p: Pos): Boolean =
      p.x >= p0.x && p.x <= p1.x && p.y >= p0.y && p.y <= p1.y

  case class Task(command: String, rec: Rec)

  case class Sol[A](lights: Map[Pos,A]):

    def run(toggle: A => A, on: A => A, off: A => A)(task: Task): Sol[A] =
      task match
        case Task("toggle", rec) => copy(lights = lights.map((p,l) => if rec.contains(p) then (p, toggle(l)) else (p, l)))
        case Task("on",     rec) => copy(lights = lights.map((p,l) => if rec.contains(p) then (p,     on(l)) else (p, l)))
        case Task("off",    rec) => copy(lights = lights.map((p,l) => if rec.contains(p) then (p,    off(l)) else (p, l)))
        case _ => sys.error("boom!")

  object Sol:
    def empty[A](init: => A): Sol[A] =
      Sol(List.tabulate(1000, 1000)((x,y) => Pos(x,y) -> init).flatten.toMap)

  val tasks: List[Task] =
    Source
      .fromFile("src/resources/input06.txt")
      .getLines
      .toList
      .map {
        case s"toggle $p0x,$p0y through $p1x,$p1y" => Task("toggle", Rec(Pos(p0x.toInt, p0y.toInt), Pos(p1x.toInt, p1y.toInt)))
        case s"turn on $p0x,$p0y through $p1x,$p1y" => Task("on", Rec(Pos(p0x.toInt, p0y.toInt), Pos(p1x.toInt, p1y.toInt)))
        case s"turn off $p0x,$p0y through $p1x,$p1y" => Task("off", Rec(Pos(p0x.toInt, p0y.toInt), Pos(p1x.toInt, p1y.toInt)))
      }

  val answer1: Int =
    var c = 0
    tasks.foldLeft(Sol.empty(false))((sol,task) => {
      println(c)
      c = c + 1
      sol.run(l => !l, _ => true, _ => false)(task)
    }).lights.filter(_._2).size

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    var c = 0
    tasks.foldLeft(Sol.empty(0))((sol,task) => {
      println(c)
      c = c + 1
      sol.run(l => l + 2, l => l + 1, l => if (l - 1) >= 0 then l - 1 else 0)(task)
    }).lights.values.sum

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
