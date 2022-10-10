import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [3ms]") {
    assertResult( 138)(actual = Day01.answer1) // 1ms
    assertResult(1771)(actual = Day01.answer2) // 2ms
  }
  test("Day02") {
    assertResult(1606483)(actual = Day02.answer1)
    assertResult(3842356)(actual = Day02.answer2)
  }
  test("Day03") {
    assertResult(2572)(actual = Day03.answer1)
    assertResult(2631)(actual = Day03.answer2)
  }
  ignore("Day04 [229s]") {
    assertResult( 282749)(actual = Day04.answer1)   // 6s
    assertResult(9962624)(actual = Day04.answer2) // 223s
  }
  test("Day05") {
    assertResult(258)(actual = Day05.answer1)
    assertResult( 53)(actual = Day05.answer2)
  }
  test("Day06") {
    assertResult(  569999)(actual = Day06.answer1)
    assertResult(17836115)(actual = Day06.answer2)
  }
  test("Day07") {
    assertResult(   956)(actual = Day07.answer1)
    assertResult( 40149)(actual = Day07.answer2)
  }
  test("Day08") {
    assertResult(1333)(actual = Day08.answer1)
    assertResult(2046)(actual = Day08.answer2)
  }
  test("Day09") {
    assertResult(207)(actual = Day09.answer1)
    assertResult(804)(actual = Day09.answer2)
  }
  ignore("Day10 [812s]") {
    assertResult( 252594)(actual = Day10.answer1) //  12s
    assertResult(3579328)(actual = Day10.answer2) // 800s
  }
  test("Day11") {
    assertResult("hxbxxyzz")(actual = Day11.answer1) //  145ms
    assertResult("hxcaabcc")(actual = Day11.answer2) // 2193ms
  }
