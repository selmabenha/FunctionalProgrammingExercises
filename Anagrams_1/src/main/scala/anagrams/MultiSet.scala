package anagrams

/** A multiset (also known as a bag) is a generalization of a set that allows
  * for multiple occurrences of the same element. Elements of the set should be
  * represented using a unique _canonical_ representation.
  *
  * Two multisets containing the same occurrences of the same elements should be
  * equal using default scala equality:
  * {{{
  * // Given that both m1 and m2 contain elements {a, a, b, b, b}
  * m1 == m2 // true
  * }}}
  */
type MultiSet[+A] = List[(A, Int)]

extension [A](set: MultiSet[A])
  /** Returns the list of all subsets of a given MultiSet
    *
    * This always includes the multiset itself, i.e. `set.subsets.contains(set)
    * // true` and the empty multiset.
    *
    * For example, the subsets of `{a, b, c}` should be `({}, {a}, {b}, {c}, {a,
    * b}, {a, c}, {b, c}, {a, b, c})`
    *
    * Note that the order in which subsets are returned does not matter.
    * However, the elements in each subset must remain canonical.
    */
  def subsets: List[MultiSet[A]] = set match {
    case Nil => List(Nil)
    case (elem, freq)::tail => for {
      subset <- tail.subsets
      count <- 0 to freq
    } yield if (count == 0) subset else (elem,count)::subset
  }

  /** Subtracts multiset `other` from this multiset
    *
    * For example, `{1, 2, 2, 2, 3, 4, 4} - {1, 2, 4}` should be `{2, 2, 3, 4}`
    *
    * Remember that `MultiSet[+A]` is a generic type, hence your solution can't
    * use any kind of ordering.
    *
    * Note: the resulting set must be a valid multiset: it must be canonical and
    * have no zero entries.
    */
  def subtract(other: MultiSet[A]): MultiSet[A] = {
    val otherMap: Map[A, Int] = other.toMap
    for {
      (elem, freq) <- set
      subFreq = freq - otherMap.getOrElse(elem, 0)
      if subFreq > 0
    } yield (elem, subFreq)
  }
    

object MultiSet:

  /** Creates a MultiSet from a given sequence, using the given comparison
    * function.
    *
    * @param lt
    *   the comparison function which tests whether its first argument precedes
    *   its second argument in the desired ordering.
    * @see
    *   https://dotty.epfl.ch/api/scala/collection/SeqOps.html#sortWith
    */
  def from[A](seq: Seq[A])(lt: (A, A) => Boolean): MultiSet[A] = {
    seq.sorted(Ordering.fromLessThan(lt)).groupBy(identity).view.mapValues(_.size).toList.map{ case (elem, freq) => (elem, freq)}
  }
