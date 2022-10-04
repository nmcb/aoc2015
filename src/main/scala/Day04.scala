import scala.io._

object Day04 extends App:

  val start1: Long =
    System.currentTimeMillis

  val secret: String =
    "yzbqklnj"

  import java.security.MessageDigest

  val MD5: MessageDigest =
    MessageDigest.getInstance("MD5")

  def hashMD5(s: String): String =
    def convertBytesToHex(bytes: Array[Byte]): String =
      val sb = new StringBuilder
      for (b <- bytes)(sb.append(String.format("%02x", Byte.box(b))))
      sb.toString
    convertBytesToHex(MD5.digest(s.getBytes))
  
  val answer1: Int =
    def loop(i: Int = 1): Int =
      if (hashMD5(secret + i.toString).startsWith("00000")) i else loop(i + 1)
    loop()

  println(s"Answer day 4 part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    def loop(l: Long = 1): Long =
      if (hashMD5(secret + l.toString).startsWith("000000")) l else loop(l + 1)
    loop()
  
  println(s"Answer day 4 part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
