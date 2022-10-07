import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult( 138)(actual = Day01.answer1)
    assertResult(1771)(actual = Day01.answer2)
  }
  test("Day02") {
    assertResult(1606483)(actual = Day02.answer1)
    assertResult(3842356)(actual = Day02.answer2)
  }
  test("Day03") {
    assertResult(2572)(actual = Day03.answer1)
    assertResult(2631)(actual = Day03.answer2)
  }
  ignore("Day04") {
    assertResult( 282749)(actual = Day04.answer1) // takes 6s
    assertResult(9962624)(actual = Day04.answer2) // takes 223s
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
  ignore("Day10") {
    assertResult(252594)(actual = Day10.answer1) // takes 12s
    assertResult(3579328)(actual = Day09.answer2)
  }
