package anagrams

import cs214.MultiSets.*
import scala.concurrent.duration.*

class MultiSetTest extends munit.FunSuite:
  override val munitTimeout = 10.seconds

  // from
  test("from[Int]: initial order of elements does not affect the result (3pts)"):
    val set1 = IntMultiSet.from(Seq(1, 1, 2, 2, 3))
    val set2 = IntMultiSet.from(Seq(1, 2, 3, 2, 1))
    assertEquals(set1, set2)

  test("from[Char]: initial order of elements does not affect the result (3pts)"):
    val set1 = CharMultiSet.from("aabbc")
    val set2 = CharMultiSet.from("abcab")
    assertEquals(set1, set2)

  // subsets
  def testSubsets[A](message: String, points: Int, result: MultiSet[A], expected: Seq[MultiSet[A]]) =
    test(f"subsets: $message (${points}pts)"):
      assertEquals(result.subsets.toSet, expected.toSet)

  testSubsets("Nil", 3, DoubleMultiSet.from(Nil), Seq(DoubleMultiSet.from(Nil)))

  testSubsets(
    "(1, 2, 3)",
    3,
    IntMultiSet.from(Seq(1, 2, 3)),
    Seq(
      IntMultiSet.from(Nil),
      IntMultiSet.from(Seq(1)),
      IntMultiSet.from(Seq(2)),
      IntMultiSet.from(Seq(3)),
      IntMultiSet.from(Seq(1, 2)),
      IntMultiSet.from(Seq(1, 3)),
      IntMultiSet.from(Seq(2, 3)),
      IntMultiSet.from(Seq(1, 2, 3))
    )
  )

  testSubsets(
    "aaa",
    3,
    CharMultiSet.from("aaa"),
    Seq(CharMultiSet.from(""), CharMultiSet.from("a"), CharMultiSet.from("aa"), CharMultiSet.from("aaa"))
  )

  testSubsets(
    "aabb",
    3,
    CharMultiSet.from("aabb"),
    Seq(
      CharMultiSet.from(""),
      CharMultiSet.from("a"),
      CharMultiSet.from("aa"),
      CharMultiSet.from("b"),
      CharMultiSet.from("bb"),
      CharMultiSet.from("ab"),
      CharMultiSet.from("abb"),
      CharMultiSet.from("aab"),
      CharMultiSet.from("aabb")
    )
  )

  testSubsets(
    "(500*a, 500*b)",
    3,
    CharMultiSet.from("a" * 500 + "b" * 500),
    for
      i <- 0 to 500
      j <- 0 to 500
    yield CharMultiSet.from("a" * i + "b" * j)
  )

  // subtract
  test("subtract: Clément - \"\" is Clément (3pts)"):
    val s1 = CharMultiSet.from("Clément")
    val s2 = CharMultiSet.from("")
    val expected = CharMultiSet.from("Clément")
    assertEquals(s1.subtract(s2), expected)

  test("subtract: Martin - Martin is \"\" (6pts)"):
    val s1 = CharMultiSet.from("Martin")
    val s2 = CharMultiSet.from("Martin")
    val expected = CharMultiSet.from("")
    assertEquals(s1.subtract(s2), expected)

  test("subtract: Vik - Viktor is \"\" (6pts)"):
    val s1 = CharMultiSet.from("Vik")
    val s2 = CharMultiSet.from("Viktor")
    val expected = CharMultiSet.from("")
    assertEquals(s1.subtract(s2), expected)

  test("subtract: (List(2), List(2, 3), List(2), List(3)) - (List(2), List(3)) is (List(2), List(2, 3)) (6pts)"):
    val s1 = ListIntMultiSet.from(Seq(List(2), List(2, 3), List(2), List(3)))
    val s2 = ListIntMultiSet.from(Seq(List(2), List(3)))
    val expected = ListIntMultiSet.from(Seq(List(2), List(2, 3)))
    assertEquals(s1.subtract(s2), expected)

  test("subtract: (100 * 1, 200 * 2, 300 * 3) - (90 * 1, 180 * 2, 270 * 3) is (10 * 1, 20 * 2, 30 * 3) (6pts)"):
    val s1 = IntMultiSet.from(Seq.fill(100)(1) ++ Seq.fill(200)(2) ++ Seq.fill(300)(3))
    val s2 = IntMultiSet.from(Seq.fill(90)(1) ++ Seq.fill(180)(2) ++ Seq.fill(270)(3))
    val expected = IntMultiSet.from(Seq.fill(10)(1) ++ Seq.fill(20)(2) ++ Seq.fill(30)(3))
    assertEquals(s1.subtract(s2), expected)
