import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [3ms]"):
    assertResult( 138)(actual = Day01.answer1)
    assertResult(1771)(actual = Day01.answer2)

  test("Day02 [5ms]"):
    assertResult(1606483)(actual = Day02.answer1)
    assertResult(3842356)(actual = Day02.answer2)

  test("Day03 [34ms]"):
    assertResult(2572)(actual = Day03.answer1)
    assertResult(2631)(actual = Day03.answer2)

  test("Day04 [229s]"):
    assertResult( 282749)(actual = Day04.answer1)
    assertResult(9962624)(actual = Day04.answer2)

  test("Day05 [21ms]"):
    assertResult(258)(actual = Day05.answer1)
    assertResult( 53)(actual = Day05.answer2)

  test("Day06 [4s]"):
    assertResult(  569999)(actual = Day06.answer1)
    assertResult(17836115)(actual = Day06.answer2)

  test("Day07 [41ms]"):
    assertResult(   956)(actual = Day07.answer1)
    assertResult( 40149)(actual = Day07.answer2)

  test("Day08 [5ms]"):
    assertResult(1333)(actual = Day08.answer1)
    assertResult(2046)(actual = Day08.answer2)

  test("Day09 [204ms]"):
    assertResult(207)(actual = Day09.answer1)
    assertResult(804)(actual = Day09.answer2)

  ignore("Day10 [812s]"):
    assertResult( 252594)(actual = Day10.answer1)
    assertResult(3579328)(actual = Day10.answer2)

  test("Day11 [2338ms]"):
    assertResult("hxbxxyzz")(actual = Day11.answer1)
    assertResult("hxcaabcc")(actual = Day11.answer2)

  test("Day12 [12ms]"):
    assertResult(111754)(actual = Day12.answer1)
    assertResult( 65402)(actual = Day12.answer2)

  test("Day13 [1724ms]"):
    assertResult(733)(actual = Day13.answer1)
    assertResult(725)(actual = Day13.answer2)

  test("Day14 [20ms]"):
    assertResult(2660)(actual = Day14.answer1)
    assertResult(1256)(actual = Day14.answer2)

  test("Day15 [313ms]"):
    assertResult(13882464)(actual = Day15.answer1)
    assertResult(11171160)(actual = Day15.answer2)

  test("Day16 [2ms]"):
    assertResult( 40)(actual = Day16.answer1)
    assertResult(241)(actual = Day16.answer2)

  test("Day17 [1013ms]"):
    assertResult( 4372)(actual = Day17.answer1)
    assertResult(    4)(actual = Day17.answer2)

  test("Day18 [5133ms]"):
    assertResult( 814)(actual = Day18.answer1)
    assertResult( 924)(actual = Day18.answer2)

  ignore("Day19 [???ms]"):
    assertResult( 666)(actual = Day19.answer1)
    assertResult( 666)(actual = Day19.answer2)

  test("Day20 [3463ms]"):
    assertResult( 831600)(actual = Day20.answer1)
    assertResult( 884520)(actual = Day20.answer2)
  test("Day21 [5ms]"):
    assertResult( 111)(actual = Day21.answer1)
    assertResult( 188)(actual = Day21.answer2)
