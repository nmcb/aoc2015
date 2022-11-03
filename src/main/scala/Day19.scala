object Day19 extends App:

  import scala.io.*

  val day: String =
    this.getClass.getName.drop(3).init

  /** Modeling */

  import Day12.*
  import P.*

  object Molecules:

    private val upper: P[Char] =
      satisfy(_.isUpper)

    private val lower: P[Char] =
      satisfy(_.isLower)

    private val molecule: P[String] =
      for { c1 <- upper ; c2 <- lower.zeroOrMore } yield (c1 :: c2).mkString

    val parser: P[List[String]] =
      molecule.oneOrMore


  /** Input */

  val molecules: List[String] =
    Molecules
      .parser
      .run("HOH")
//      .run("CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCa" +
//           "CaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMg" +
//           "ArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTi" +
//           "BPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFAr" +
//           "CaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMg" +
//           "ArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRn" +
//           "BPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgAr" +
//           "SiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCa" +
//           "PTiBPTiBCaSiThSiRnMgArCaF")

  val replacements: Map[String,List[List[String]]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$molecule => $replacement" => molecule -> Molecules.parser.run(replacement) }
      .toList
      .groupMap(_._1)(_._2)

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1 =
    666

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    669

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
