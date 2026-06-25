package cs214

import anagrams.*

object MultiSets:
  object CharMultiSet:
    def from(str: String): MultiSet[Char] =
      MultiSet.from(normalizeString(str))(_ < _)
  object IntMultiSet:
    def from(seq: Seq[Int]): MultiSet[Int] =
      MultiSet.from(seq)(_ < _)
  object DoubleMultiSet:
    def from(seq: Seq[Double]): MultiSet[Double] =
      MultiSet.from(seq)(_ < _)
  object ListIntMultiSet:
    def from(seq: Seq[List[Int]]): MultiSet[List[Int]] =
      MultiSet.from(seq)(_.sum < _.sum) // Arbitrary comparison function
