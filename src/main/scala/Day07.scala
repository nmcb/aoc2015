import scala.io.*
import scala.annotation.*

object Day07 extends App:

  /** Modeling */

  sealed abstract class Node(val args: List[String], val ret: String):
    def run(wires: Map[String, Int]): Option[Int]

  case class Op2(op: Int => Int => Int, override val args: List[String], override val ret: String) extends Node(args, ret):
    def run(wires: Map[String, Int]): Option[Int] =
      val List(lhs, rhs) = args.map(wires.get)
      for {
        v1 <- lhs
        v2 <- rhs
      } yield op(v1)(v2)

  case class Op1(op: Int => Int, override val args: List[String], override val ret: String) extends Node(args, ret):
    def run(wires: Map[String, Int]): Option[Int] =
      args.map(wires.get).headOption.flatten.map(op)

  case class Val(value: Int, override val ret: String) extends Node(List.empty, ret):
    def run(wires: Map[String, Int]): Option[Int] =
      Some(value)

  object Solver:
    def run(nodes: Seq[Node], wire: String, b: Option[Int] = None): Int =
      @tailrec def loop(todo: Seq[Node], wires: Map[String, Int] = Map.empty): Int =
        wires.get(wire) match
          case Some(v) => v
          case None => todo match
            case Seq(n, ns*) => n.run(wires) match
              case Some(v) => loop(ns, wires.updated(n.ret, v))
              case None => loop(ns :+ n, wires)
            case _ => sys.error("not found")

      val ns = b.map(v => Val(v, "b") +: nodes.filterNot(_.ret == "b")).getOrElse(nodes)
      loop(ns)

  val input: IndexedSeq[Node] =
    def parser(s: String): Node =
      s match
        case s"$lhs AND $rhs -> $ret" if lhs.toIntOption.isDefined
          => Op1(rv => lhs.toInt & rv, List(rhs), ret)
        case s"$lhs AND $rhs -> $ret"
          => Op2(lv => rv => lv & rv, List(lhs, rhs), ret)
        case s"$lhs OR $rhs -> $ret"
          => Op2(lv => rv => lv | rv, List(lhs, rhs), ret)
        case s"$lhs RSHIFT $rhs -> $ret" if rhs.toIntOption.isDefined
          => Op1(lv => lv >> rhs.toInt, List(lhs), ret)
        case s"$lhs RSHIFT $rhs -> $ret"
          => Op2(lv => rv => lv >> rv, List(lhs, rhs), ret)
        case s"$lhs LSHIFT $rhs -> $ret" if rhs.toIntOption.isDefined
          => Op1(lv => lv << rhs.toInt, List(lhs), ret)
        case s"$lhs LSHIFT $rhs -> $ret"
          => Op2(lv => rv => lv << rv, List(lhs, rhs), ret)
        case s"NOT $rhs -> $ret"
          => Op1(rv => ~rv & 0x0000FFFF, List(rhs), ret)
        case s"$arg -> $ret" if arg.toIntOption.isDefined
          => Val(arg.toInt, ret)
        case s"$rhs -> $ret"
          => Op1(identity, List(rhs), ret)

    Source
      .fromFile("src/main/resources/input07.txt")
      .getLines
      .map(parser)
      .toIndexedSeq


  /** Part 1 */

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Solver.run(input, "a")

  println(s"Answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Solver.run(input, "a", Some(answer1))

  println(s"Answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


  /** Solution assertions */

  assert(answer1 ==   956, s"Wrong answer part 1: $answer1")
  assert(answer2 == 40149, s"Wrong answer part 2: $answer2")
