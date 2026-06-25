package cs214

import anagrams.*

/** An `AnagramsTree` is a compact tree representation of a collection of
  * anagrams
  */
type AnagramsTree = List[Branch]

/** A branch represents a (subpart) of a sentence of anagrams
  *
  * For example, from the input `"below"`, three different branches (anagrams
  * sentences) can be created:
  * {{{
  * List(
  *   Branch(Set("be"), List( // 0
  *     Branch(Set("low"), Nil))), // 0.0
  *   Branch(Set("low"), List( // 1
  *     Branch(Set("be"), Nil))), // 1.0
  *   Branch(Set("below"), Nil) // 2
  * )
  * }}}
  * Which can be represented as such:
  * {{{
  * .
  * ├─ Branch 0 ─ be
  * │             └─ Branch 0.0 ─ low
  * ├─ Branch 1 ─ low
  * │             └─ Branch 1.0 ─ be
  * └─ Branch 2 ─ below
  * }}}
  * @param headWords
  *   Value held by this branch, that is words belonging to the same equivalence
  *   class
  * @param suffixes
  *   A sequence of branches representing the possible suffixes of the current
  *   anagrams sentence
  */
case class Branch(val headWords: EquivalenceClass, val suffixes: AnagramsTree):
  override def equals(other: Any): Boolean = other match
    case Branch(headWords, suffixes) => this.headWords == headWords && this.suffixes.toSet == suffixes.toSet
    case _                           => false
  override def hashCode(): Int = headWords.hashCode() + suffixes.toSet.hashCode()

extension (tree: AnagramsTree)
  /** Returns a human-readable representation of a tree, inspired from the
    * `tree` UNIX command.
    *
    * For example, it represents the input tree for "sadder":
    * {{{
    * List(
    *   Branch(Set(ad), List(
    *     Branch(Set(reds),Nil))),
    *   Branch(Set(red),List(
    *     Branch(Set(ads, sad),Nil))),
    *   Branch(Set(ads, sad),List(
    *     Branch(Set(red),Nil))),
    *   Branch(Set(reds),List(
    *     Branch(Set(ad),Nil))),
    *   Branch(Set(sadder),Nil))
    * }}}
    * as follows:
    * {{{
    * .
    * ├── ad
    * │   └── reds
    * ├── red
    * │   └── {ads, sad}
    * ├── {ads, sad}
    * │   └── red
    * ├── reds
    * │   └── ad
    * └── sadder
    * }}}
    */
  def show: String =
    def showHead(headWords: Set[String]): String =
      if headWords.size == 1 then
        headWords.mkString
      else
        headWords.mkString("{", ", ", "}")

    def showBranch(
        branch: Branch,
        paddings: Seq[String],
        isLast: Boolean
    ): Seq[String] =
      val padding =
        if paddings.isEmpty then "" else paddings.init.mkString + (if isLast then "└── " else "├── ")
      val head = showHead(branch.headWords)
      val firstSuffixes = branch.suffixes
        .take(branch.suffixes.length - 1)
        .flatMap(showBranch(_, paddings :+ "│   ", false))
      val lastSuffix = branch.suffixes
        .lastOption
        .map(showBranch(_, paddings :+ "    ", true))
        .getOrElse(Nil)
      (padding + head) +: (firstSuffixes ++ lastSuffix)

    showBranch(Branch(Set("."), tree), Nil, false).mkString("\n")
