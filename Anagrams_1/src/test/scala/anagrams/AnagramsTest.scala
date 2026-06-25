package anagrams

import cs214.*
import cs214.MultiSets.*
import scala.concurrent.duration.*

object TreeTestLogger:
  extension (str: String)
    def bold = s"${Console.BOLD}$str${Console.RESET}"
    def red = s"${Console.RED}$str${Console.RESET}"

  def logWrongTree(input: String, res: AnagramsTree, expected: AnagramsTree): String =
    f"$input\n" +
      f"=> Obtained:\n".bold + f"$res\n" + f"${res.show}\n" +
      f"=> Expected:\n".bold + f"$expected\n" + f"${expected.show}"

class AnagramsTest extends munit.FunSuite:
  import TreeTestLogger.*
  override val munitTimeout = 10.seconds

  // sentenceOccurrences
  val sentenceOccurrencesTestCases = Seq(
    "",
    "abcd",
    "ABCD",
    "Égée",
    "**   cats & dogs   **",
    "Clément Pit-Claudel"
  )

  def testSentenceOccurrences(input: String, points: Int) =
    test(f"sentenceOccurrences: $input (${points}pts)"):
      assertEquals(sentenceOccurrences(List(input)), CharMultiSet.from(normalizeString(input)))

  for
    input <- sentenceOccurrencesTestCases
  yield testSentenceOccurrences(input, 2)

  // createDictionary
  val enDebian = loadWordlist("en-debian")

  test("createDictionary: Set.empty (2pts)"):
    assertEquals(createDictionary(Set.empty), Map.empty)

  test("createDictionary: normalization 3pts)"):
    assertEquals(
      createDictionary(Set("à", "noël", "Léon", "a")),
      Map(
        CharMultiSet.from("a") -> Set("à", "a"),
        CharMultiSet.from("Léon") -> Set("Léon", "noël")
      )
    )

  test("createDictionary: duplicates (3pts)"):
    assertEquals(
      createDictionary(Set("eat", "tea", "wasp", "swap")),
      Map(
        CharMultiSet.from("eat") -> Set("eat", "tea"),
        CharMultiSet.from("swap") -> Set("swap", "wasp")
      )
    )

  test("createDictionary en-debian: get eat (3pts)"):
    assertEquals(
      createDictionary(enDebian).get(CharMultiSet.from("eat")),
      Some(Set("ate", "eat", "tea"))
    )

  test("createDictionary en-debian: get grab (3pts)"):
    assertEquals(
      createDictionary(enDebian).get(CharMultiSet.from("grab")),
      Some(Set("grab", "garb", "brag"))
    )

  // anagrams
  for
    dictName <- TestData.dicts
    dict = createDictionary(loadWordlist(dictName))
    (sentence, expected) <- TestData.testData(dictName)
  yield test(f"sentenceAnagrams: \"${sentence.mkString(" ")}\" using dict \"$dictName\" (5pts)"):
    val input = sentence.mkString(" ")
    val res = anagrams(dict, sentenceOccurrences(sentence))

    def sortAttribute(branch: Branch) = branch.headWords.mkString.sorted
    assert(
      res.sortBy(sortAttribute) == expected.sortBy(sortAttribute),
      logWrongTree(input, res, expected)
    ) // assert and not assertEquals because the diff message is confusing and unreadable
