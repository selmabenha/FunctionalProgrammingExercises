package anagrams

import cs214.*
import java.text.Normalizer
import java.util.Locale

type Word = String
type Sentence = List[Word]

/** A set of words that are all anagrams of each other
  *
  * For example `{eat, tea, ate}` are all in the same equivalence class
  */
type EquivalenceClass = Set[Word]

/** An `OccurrenceList` is a multiset of characters: pairs of characters and
  * positive integers indicating how many times the character appears. All
  * characters in the occurrence list are lowercase.
  *
  * For example, the word "eat" has the following character occurrence list:
  * {{{
  * {('a', 1), ('e', 1), ('t', 1)}
  * }}}
  * Incidentally, so do the words "ate" and "tea".
  */
type OccurrenceList = MultiSet[Char]

/** A `Dictionary` is a mapping from occurrence lists to corresponding words.
  *
  * For example, if the original word list contains the entries `ate`, `eat`,
  * `tea`, then the resulting `Dictionary` will contain an entry:
  * {{{
  * {('a', 1), ('e', 1), ('t', 1)} -> Set("ate", "eat", "tea")
  * }}}
  */
type Dictionary = Map[OccurrenceList, Set[Word]]

/** Removes diacritics and all non-alphabetic characters from `s`. */
def normalizeString(str: String): String =
  Normalizer
    .normalize(str, Normalizer.Form.NFD)
    .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    .replaceAll("[^a-zA-Z]+", "")
    .toLowerCase(Locale.ROOT)
    .ensuring(_.forall(c => 'a' <= c && c <= 'z'))


/** Converts a sentence `s` into its character occurrence list.
  *
  * Remember to normalize the sentence (`normalizeString`)
  */
def sentenceOccurrences(s: Sentence): OccurrenceList = {
  normalizeString(s.mkString("")).groupBy(identity).view.mapValues(_.length).toList
}

/** Constructs a `Map` from occurrence lists to a sequence of all the words that
  * have that occurrence count. This map makes it easy to obtain all the
  * anagrams of a word given its occurrence list.
  */
def createDictionary(wordlist: Set[String]): Dictionary = {
  wordlist.groupBy(word => sentenceOccurrences(List(word))).view.mapValues(words => words.toSet).toMap
}

/** Returns a tree of all anagram sentences of the given occurrence list using
  * the given dictionary.
  *
  * For example, on input "banana" with occurrences List((a, 3), (b, 1), (n, 2))
  * you could get the following tree:
  * {{{
  * .
  * ├── a
  * │   ├── an
  * │   │   └── ban
  * │   └── ban
  * │       └── an
  * ├── an
  * │   ├── a
  * │   │   └── ban
  * │   └── ban
  * │       └── a
  * ├── ban
  * │   ├── a
  * │   │   └── an
  * │   └── an
  * │       └── a
  * └── banana
  * }}}
  * Which corresponds to this Scala entity:
  * {{{
  * List(
  *   Branch(Set("a"),List(
  *     Branch(Set("an"),List(
  *       Branch(Set("ban"),Nil))),
  *     Branch(Set("ban"),List(
  *       Branch(Set("an"),Nil))))),
  *
  *   Branch(Set("an"),List(
  *     Branch(Set("a"),List(
  *       Branch(Set("ban"),Nil))),
  *     Branch(Set("ban"),List(
  *       Branch(Set("a"),Nil))))),
  *
  *   Branch(Set("ban"),List(
  *     Branch(Set("a"),List(
  *       Branch(Set("an"),Nil))),
  *     Branch(Set("an"),List(
  *       Branch(Set("a"),Nil))))),
  *
  *   Branch(Set("banana"), Nil),
  * )
  * }}}
  *
  * The different sentences do not have to be output in the order shown above:
  * any order is fine as long as all the anagrams are there. Every word returned
  * has to exist in the dictionary.
  *
  * Note: If the words of the sentence are in the dictionary, then the sentence
  * is an anagram of itself, and it must appear in the result.
  */

def anagrams(dict: Dictionary, occurrences: OccurrenceList): AnagramsTree = {
  if (occurrences.isEmpty) {
    List(Branch(Set.empty[Word], Nil)) // Base case for empty occurrences
  } else {
    val branches = for {
      subset <- occurrences.subsets // Iterate over all subsets
      if dict.contains(subset) // Check if the subset exists in the dictionary
      words = dict(subset) // Retrieve words for the subset
      restAnagrams = anagrams(dict, occurrences.subtract(subset)) // Recur for the remaining occurrences
      if restAnagrams.nonEmpty || subset.isEmpty // Only add non-empty branches or if the subset is empty
    } yield {
      // Combine words from the subset and the rest of the anagrams in the branch
      Branch(words.toSet, restAnagrams)
    }
    
    // Remove duplicates and group similar structures
    branches.toList.distinct // Convert to List and ensure uniqueness
  }
}






