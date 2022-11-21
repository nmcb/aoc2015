import scala.annotation.*

object Day19 extends App:

  import scala.io.*

  val day: String =
    this.getClass.getName.drop(3).init

  /** Modeling */

  type Mol = String

  import Day12.*
  import P.*

  object Molecules:

    val E = "E"

    private val upper: P[Char] =
      satisfy(_.isUpper)

    private val lower: P[Char] =
      satisfy(_.isLower)

    private val molecule: P[Mol] =
      for { c1 <- upper ; c2 <- lower.zeroOrMore } yield (c1 +: c2).mkString

    val parser: P[Seq[Mol]] =
      molecule.oneOrMore


  /** Input */

  val molecules: Seq[Mol] =
    Molecules
      .parser
      .run("CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCa" +
           "CaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMg" +
           "ArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTi" +
           "BPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFAr" +
           "CaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMg" +
           "ArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRn" +
           "BPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgAr" +
           "SiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCa" +
           "PTiBPTiBCaSiThSiRnMgArCaF")

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val replacements: Map[Mol,Seq[Seq[Mol]]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$molecule => $replacement" => molecule -> Molecules.parser.run(replacement) }
      .toSeq
      .groupMap(_._1)(_._2)

  @tailrec
  def calibrate(todo: Seq[Mol], done: Seq[Mol] = Seq.empty, found: Set[Seq[Mol]] = Set.empty): Set[Seq[Mol]] =
    todo match
      case m :: ms if replacements.contains(m) =>
        calibrate(ms, done :+ m, found ++ replacements(m).map(rs => done ++ rs ++ ms))
      case m :: ms =>
        calibrate(ms, done :+ m, found)
      case _ =>
        found

  val answer1 =
    calibrate(molecules).size

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  assert(answer1 == 535)

  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val generators: Map[Mol,Mol] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$molecule => $replacement" => replacement -> molecule }
      .toMap

  var found: Map[Mol, Seq[Mol]] =
    Map.empty

  @tailrec
  def replace(replacement: (Mol,Mol), seq: Mol, cur: Int = 0, acc: Set[Mol] = Set.empty): Set[Mol] =
    val (from, to) = replacement
    seq.indexOfSlice(from, cur) match {
      case -1 => acc
      case  i => replace(replacement, seq, i + 1, acc + (seq.slice(0, i) + to + seq.substring(i + from.length)))
    }

  def predecessors(seq: Mol): Set[Mol] =
    generators.map(replace(_, seq)).foldLeft(Set.empty[Mol])(_ ++ _)

  def findGenerationDepth(seq: Mol): Int =
    def go(todo: Mol, count: Int = 0): Option[Int] =
      def nonEmptyTraces: PartialFunction[Mol,Option[Int]] =
        s => go(s, count + 1) match
          case c : Some[Int] => c
      todo match
          case ""            => None
          case Molecules.E   => println(s"found depth=$count") ; Some(count)
          case _             => predecessors(todo).collect(nonEmptyTraces).head
    go(seq).head


  lazy val answer2: Int = findGenerationDepth(molecules.mkString("")) // doesn't terminate - performance ???
  //  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
