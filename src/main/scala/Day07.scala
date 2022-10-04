import scala.io.*
import scala.annotation.*

object Day07 extends App:

  /** Modeling */

  sealed trait Expr:
    def args: List[String]
    def ret: String
    def run(wires: Map[String, Int]): Option[Int]

  case class Op2(op: Int => Int => Int, args: List[String], ret: String) extends Expr:
    def run(wires: Map[String, Int]): Option[Int] =
      val List(lhs, rhs) = args.map(wires.get)
      for {
        v1 <- lhs
        v2 <- rhs
      } yield op(v1)(v2)

  case class Op1(op: Int => Int, args: List[String], ret: String) extends Expr:
    def run(wires: Map[String, Int]): Option[Int] =
      args.map(wires.get).headOption.flatten.map(op)

  case class Val(value: Int, ret: String) extends Expr:
    def args: List[String] =
      List.empty
    def run(wires: Map[String, Int]): Option[Int] =
      Some(value)

  object Solver:
    def solve(nodes: Seq[Expr], wire: String, setValueB: Option[Int] = None): Int =
      @tailrec def fold(todo: Seq[Expr], wires: Map[String, Int] = Map.empty): Int =
        wires.get(wire) match
          case Some(v) => v
          case None => todo match
            case Seq(expr, exprs*) => expr.run(wires) match
              case Some(v) => fold(exprs, wires.updated(expr.ret, v))
              case None => fold(exprs :+ expr, wires)
            case _ => sys.error(s"undefined wire=$wire")

      val puzzleInput: Seq[Expr] =
        setValueB
          .map(v => Val(v, "b") +: nodes.filterNot(_.ret == "b"))
          .getOrElse(nodes)

      fold(puzzleInput)

  val input: IndexedSeq[Expr] =
    def parser(s: String): Expr =
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
  val answer1: Int = Solver.solve(input, "a")

  println(s"Answer day 7 part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Solver.solve(input, "a", setValueB = Some(answer1))

  println(s"Answer day 7 part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
