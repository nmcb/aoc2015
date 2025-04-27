import scala.annotation.*
import scala.io.*

object Day10 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val input: String =
    "1113222113"

  def lookAndSay(s: String): String =
    val result = new StringBuilder
    var index  = 0
    while index < s.length do
      val char   = s(index)
      val length = 1 + s.segmentLength(_ == char, index + 1)
      result.append(length).append(char)
      index += length
    result.toString

  def solve(s: String, times: Int): Int =
    Iterator.iterate(s)(lookAndSay).drop(times).next.length

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve(input, 40)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(input, 50)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
