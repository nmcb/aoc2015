import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [3ms]") {
    assertResult( 138)(actual = Day01.answer1) // 1ms
    assertResult(1771)(actual = Day01.answer2) // 2ms
  }
  test("Day02 [5ms]") {
    assertResult(1606483)(actual = Day02.answer1) // 3ms
    assertResult(3842356)(actual = Day02.answer2) // 2ms
  }
  test("Day03 [34ms]") {
    assertResult(2572)(actual = Day03.answer1) // 24ms
    assertResult(2631)(actual = Day03.answer2) // 10ms
  }
  ignore("Day04 [229s]") {
    assertResult( 282749)(actual = Day04.answer1)   // 6s
    assertResult(9962624)(actual = Day04.answer2) // 223s
  }
  test("Day05 [21ms]") {
    assertResult(258)(actual = Day05.answer1) // 12ms
    assertResult( 53)(actual = Day05.answer2) //  9ms
  }
  test("Day06 [4s]") {
    assertResult(  569999)(actual = Day06.answer1) // 2043ms
    assertResult(17836115)(actual = Day06.answer2) // 1953ms
  }
  test("Day07 [41ms]") {
    assertResult(   956)(actual = Day07.answer1) // 21ms
    assertResult( 40149)(actual = Day07.answer2) // 20ms
  }
  test("Day08 [5ms]") {
    assertResult(1333)(actual = Day08.answer1) // 2ms
    assertResult(2046)(actual = Day08.answer2) // 3ms
  }
  test("Day09 [204ms]") {
    assertResult(207)(actual = Day09.answer1) // 202ms
    assertResult(804)(actual = Day09.answer2) //   2ms
  }
  ignore("Day10 [812s]") {
    assertResult( 252594)(actual = Day10.answer1) //  12s
    assertResult(3579328)(actual = Day10.answer2) // 800s
  }
  test("Day11 [2338ms]") {
    assertResult("hxbxxyzz")(actual = Day11.answer1) //  145ms
    assertResult("hxcaabcc")(actual = Day11.answer2) // 2193ms
  }
  test("Day12 [12ms]") {
    assertResult(111754)(actual = Day12.answer1) // 10ms
    assertResult( 65402)(actual = Day12.answer2) //  2ms
  }
  test("Day13 [1724ms]") {
    assertResult(733)(actual = Day13.answer1) //  450ms
    assertResult(725)(actual = Day13.answer2) // 1274ms
  }
  test("Day14 [20ms]") {
    assertResult(2660)(actual = Day14.answer1) //  14ms
    assertResult(1256)(actual = Day14.answer2) //   6ms
  }
  test("Day15 [313ms]") {
    assertResult(13882464)(actual = Day15.answer1) //  210ms
    assertResult(11171160)(actual = Day15.answer2) //  103ms
  }
  test("Day16 [2ms]") {
    assertResult( 40)(actual = Day16.answer1) //  1ms
    assertResult(241)(actual = Day16.answer2) //  1ms
  }
  test("Day17 [1013ms]") {
    assertResult( 4372)(actual = Day17.answer1) // 1012ms
    assertResult(    4)(actual = Day17.answer2) //    1ms
  }
  test("Day18 [5133ms]") {
    assertResult( 814)(actual = Day18.answer1) // 2505ms
    assertResult( 924)(actual = Day18.answer2) // 2628ms
  }
  ignore("Day19 [???ms]") {
    assertResult( 666)(actual = Day19.answer1) // ???ms
    assertResult( 666)(actual = Day19.answer2) // ???ms
  }
  test("Day20 [3463ms]") {
    assertResult( 831600)(actual = Day20.answer1) // 1523ms
    assertResult( 884520)(actual = Day20.answer2) // 1940ms
  }
  test("Day21 [5ms]") {
    assertResult( 111)(actual = Day21.answer1) // 4ms
    assertResult( 188)(actual = Day21.answer2) // 1ms
  }
