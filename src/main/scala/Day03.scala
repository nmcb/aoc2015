import scala.io._

object Day03 extends App:

  val start1: Long =
    System.currentTimeMillis

  val commands: List[Char] =
    Source
      .fromFile("src/resources/input03.txt")
      .mkString
      .trim
      .toList

  case class Loc(x: Int, y: Int)

  case class Area(cur: Loc = Loc(0, 0), houses: Map[Loc,Int] = Map(Loc(0, 0) -> 1)):

    private def moveTo(l: Loc): Area =
      houses.get(l) match
        case Some(c) => copy(cur = l, houses = houses.updated(l, c + 1))
        case None    => copy(cur = l, houses = houses + (l -> 1))

    def next(c: Char): Area =
      c match
        case '>' => { val loc = cur.copy(x = cur.x + 1) ; moveTo(loc) }
        case '<' => { val loc = cur.copy(x = cur.x - 1) ; moveTo(loc) }
        case '^' => { val loc = cur.copy(y = cur.y + 1) ; moveTo(loc) }
        case 'v' => { val loc = cur.copy(y = cur.y - 1) ; moveTo(loc) }
      
  val answer1: Int =
    commands
      .foldLeft(Area())((a,c) => a.next(c))
      .houses
      .values
      .size

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val (a1, a2) =
    commands
      .zipWithIndex
      .foldLeft((Area(),Area())){ case (a,(c,i)) => if (i % 2 != 0) (a._1.next(c), a._2) else (a._1, a._2.next(c)) }

  val answer2: Int =
    (a1.houses ++ a2.houses).values.size
  
  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
