import scala.annotation.*
import scala.io.*

object Day10 extends App:

  val start1: Long =
    System.currentTimeMillis

  val day: String =
    this.getClass.getName.drop(3).init

  val input: String =
    "1113222113"


  @tailrec def solve(n: Int, str: String): String =

    println(s"n=$n, length=${str.length} [${System.currentTimeMillis - start1}ms]")

    val res: StringBuilder = StringBuilder()
    var acc: StringBuilder = StringBuilder()

    @tailrec def build(todo: String, current: Char): String =

      def encode(seq: String): String =
        s"${seq.length}${seq.head.toString}"

      if todo.nonEmpty then
        if todo.head == current then
          acc.addOne(current)
          build(todo.tail, current)
        else
          res ++= encode(acc.result)
          acc   = StringBuilder()
          build(todo, todo.head)
      else
        (res ++= encode(acc.result)).result

    if n == 0 then str else solve(n - 1, build(str, str.head))


  /** Part 1 */

  val output40: String = solve(40, input)
  val answer1: Int     = output40.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(10, output40).length
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
