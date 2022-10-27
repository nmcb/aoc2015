import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val commands: List[Char] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .toList

  val answer1: Int =
    commands
      .foldLeft(0)((f,c) => if (c == '(') f + 1 else if (c == ')') f - 1 else sys.error(s"unknow command: '$c'"))
  
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    commands
      .scanLeft(0)((f,c) => if (c == '(') f + 1 else if (c == ')') f - 1 else sys.error(s"unknow command: '$c'"))
      .zipWithIndex
      .find((f,i) => f == -1)
      .getOrElse(sys.error("not found"))
      ._2
  
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
