import scala.annotation.*
import scala.io.*

object Day10 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val input: String =
    "1113222113"


  @tailrec def solve(n: Int, str: String): String =

    println(s"n=$n, length=${str.length} [${System.currentTimeMillis - start2}ms]")

    @tailrec def loop(todo: String, current: Char, acc: String, res: StringBuilder): String =
      if todo.nonEmpty then
        if todo.head == current then
          loop(todo.tail, current, todo.head.toString + acc, res)
        else
          loop(todo, todo.head, "", res ++= encode(acc))
      else
        (res ++= encode(acc)).toString()

    def encode(seq: String): String =
      s"${seq.length}${seq.head.toString}"

    if n == 0 then str
    else solve(n - 1, loop(str, str.head, "", StringBuilder()))


  /** Part 1 */

  val start1: Long = System.currentTimeMillis
  val output40: String = solve(40, input)
  val answer1: Int     = output40.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1 / 1000}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(10, output40).length
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
