package recursion

def length(l: IntList): Int =
  if l.isEmpty then 0 else length(l.tail) + 1

def allPositiveOrZero(l: IntList): Boolean =
  l.isEmpty || (l.head >= 0 && allPositiveOrZero(l.tail))

def countPositive(l: IntList): Int =
  if l.isEmpty then 0 else countPositive(l.tail) + (if (l.head > 0) then 1 else 0)

def sum(l: IntList): Int =
  if l.isEmpty then 0 else sum(l.tail) + l.head

def product(l: IntList): Int =
  if l.isEmpty then 1 else product(l.tail) * l.head

def anyOdd(l: IntList): Boolean =
  if l.isEmpty then false else (anyOdd(l.tail) || !(l.head % 2 == 0))

def decrement(l: IntList): IntList =
  if l.isEmpty then l else IntCons(l.head - 1, decrement(l.tail))

def collectEven(l: IntList): IntList =
  if l.isEmpty then l else if (l.head % 2 == 0) then IntCons(l.head, collectEven(l.tail)) else IntCons(l.tail.head, collectEven(l.tail.tail))

def min(l: IntList): Int =
  if l.isEmpty then throw new IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty || l.head < min(l.tail) then l.head
  else min(l.tail)

def increment(l: IntList): IntList =
  if l.isEmpty then l else IntCons(l.head + 1, increment(l.tail))

def subtract(l: IntList): Int =
  if l.isEmpty then throw new IllegalArgumentException("Empty list!")
  else l.head - (if l.tail.isEmpty then 0 else subtract(l.tail))

def removeOdd(l: IntList): IntList =
  collectEven(l)

def countEven(l: IntList): Int =
  if l.isEmpty then 0 else countEven(l.tail) + (if (l.head%2 == 0) then 1 else 0)

/** `countEven` using `collectEven` and `length` */
def countEven2(l: IntList): Int =
  length(collectEven(l))

def multiplyBy2(l: IntList): IntList =
  collectEven(l)

def anyNegative(l: IntList): Boolean =
  ???

def allEven(l: IntList): Boolean =
  ???

def multiplyOdd(l: IntList): Int =
  ???

def horner(x: Int, l: IntList): Int =
  ???

def capAtZero(l: IntList): IntList =
  ???

def removeZeroes(l: IntList): IntList =
  ???

def reverseAppend(l1: IntList, l2: IntList): IntList =
  ???

def reverse(l: IntList): IntList =
  ???

def takeWhilePositive(l: IntList): IntList =
  ???

def append(l1: IntList, l2: IntList): IntList =
  ???

def collectMultiples(d: Int, l: IntList): IntList =
  ???

def last(l: IntList): Int =
  ???

def init(l: IntList): IntList =
  ???

def contains(l: IntList, n: Int): Boolean =
  ???

def isSubset(l: IntList, L: IntList): Boolean =
  ???

def intersection(l: IntList, L: IntList): IntList =
  ???

def difference(l: IntList, L: IntList): IntList =
  ???

def minMax(l: IntList): (Int, Int) =
  ???

val Add = -1
val Multiply = -2

def polishEval(l: IntList): (Int, IntList) =
  ???
