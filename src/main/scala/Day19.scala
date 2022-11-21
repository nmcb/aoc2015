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


  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val generators: Map[Seq[Mol],Mol] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$molecule => $replacement" => Molecules.parser.run(replacement) -> molecule }
      .toMap

  def replace(from: (Seq[Mol],Mol), seq: Seq[Mol], cur: Int = 0, acc: Set[Seq[Mol]] = Set.empty): Set[Seq[Mol]] =
    seq.indexOfSlice(from._1, cur) match {
      case -1 => acc
      case  i => replace(from, seq, i + 1, acc + (seq.slice(0, i) :+ from._2 :++ seq.slice(i + from._1.length, seq.length)))
    }

  def predecessors(seq: Seq[Mol]): Set[Seq[Mol]] =
    generators.map(replace(_, seq)).foldLeft(Set.empty[Seq[Mol]])(_ ++ _)

  def find(seq: Seq[Mol]): Int =
    def go(count: Int, todo: Seq[Mol]): Option[Int] =
      def nonEmptyTraces: PartialFunction[Seq[Mol],Option[Int]] =
        s => go(count + 1, s) match
          case c : Some[Int] => c
      todo match
          case Seq()            => None
          case Seq(Molecules.E) => println(s"found count=$count") ; Some(count)
          case _                => predecessors(todo).collect(nonEmptyTraces).head
    go(0, seq).head


  val answer2: Int =
    find(molecules)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
