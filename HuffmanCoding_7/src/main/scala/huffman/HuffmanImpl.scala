package huffman

import java.nio.channels.NonReadableChannelException

trait HuffmanImpl[T] extends HuffmanBase[T]:

  // Part 1: Basics

  def weight(tree: CodeTree[T]): Int = tree match
    case Leaf(_, w) => w
    case Fork(_, _, _, w) => w

  def symbols(tree: CodeTree[T]): List[T] = tree match
    case Leaf(s, _) => List(s)
    case Fork(_, _, s, _) => s

  def makeCodeTree(left: CodeTree[T], right: CodeTree[T]): CodeTree[T] =
    Fork(left, right, symbols(left) ++ symbols(right), weight(left) + weight(right))

  // Part 2: Constructing Huffman trees

  def symbolFreqs(symbols: List[T]): List[(T, Int)] =
    symbols.foldLeft(Map.empty[T, Int]) { (freqMap, symbol) =>
      freqMap.updated(symbol, freqMap.getOrElse(symbol, 0) + 1)
    }.toList

  def makeOrderedLeafList(freqs: List[(T, Int)]): List[Leaf[T]] =
    freqs.map { case (symbol, weight) => Leaf(symbol, weight) }
      .sortBy(_.weight)

  def isSingleton(trees: List[CodeTree[T]]): Boolean =
    trees.length == 1

  def combine(trees: List[CodeTree[T]]): List[CodeTree[T]] = trees match {
    case left :: right :: rest => 
      val newFork = makeCodeTree(left, right)
      (newFork :: rest).sortBy(weight) 
    case _ => trees
  }

  def until(
      isDone: List[CodeTree[T]] => Boolean,
      merge: List[CodeTree[T]] => List[CodeTree[T]]
  )(trees: List[CodeTree[T]]): List[CodeTree[T]] = {
    if (isDone(trees)) trees
    else until(isDone, merge)(merge(trees))
  }
    

  def createCodeTree(symbols: List[T]): CodeTree[T] =
    until(isSingleton, combine)(makeOrderedLeafList(symbolFreqs(symbols))).head

  // Part 3: Decoding
  // Reminder: type Bit = Int

  def decodeOne(tree: CodeTree[T], bits: List[Bit]): Option[(T, List[Bit])] = tree match {
    case Leaf(sym, _) => Some(sym, bits)
    case Fork(left, right, _, _) => bits match{
      case 0 :: rest => decodeOne(left, rest)
      case 1 :: rest => decodeOne(right, rest)
      case _ => None
    }
  }
    

  def decode(tree: CodeTree[T], bits: List[Bit]): List[T] = {
    if (tree == null || bits.isEmpty) List()
    else {
      def decodeAll(remainingBits: List[Bit]): List[T] = {
        decodeOne(tree, remainingBits) match {
          case Some((sym, remaining)) => sym :: decodeAll(remaining)
          case None => Nil
        }
      }
      decodeAll(bits)
    }
  }
  
  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree[T])(text: List[T]): List[Bit] = {
    def lookup(tree: CodeTree[T])(symbol: T): List[Bit] = tree match {
      case Leaf(sym, _) if sym == symbol => List() 
      case Fork(left, right, _, _) =>
        if (symbols(left).contains(symbol)) 
          0 :: lookup(left)(symbol) 
        else 
          1 :: lookup(right)(symbol) 
    }
    text.flatMap(lookup(tree)) 
  }


  // Part 4b: Encoding using code table

  // Reminder: type CodeTable = List[(T, List[Bit])]

  def codeBits(table: CodeTable)(symbol: T): List[Bit] =
    table.find { case (sym, _) => sym == symbol } match {
      case Some((_, bits)) => bits
      case None => List() //
    }
  def convert(tree: CodeTree[T]): CodeTable = {
    def buildTable(tree: CodeTree[T], prefix: List[Bit]): CodeTable = tree match {
      case Leaf(symbol, _) => List((symbol, prefix)) 
      case Fork(left, right, _, _) =>
        val leftTable = buildTable(left, prefix :+ 0) 
        val rightTable = buildTable(right, prefix :+ 1) 
        mergeCodeTables(leftTable, rightTable) 
    }
    buildTable(tree, List()) 
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    a ++ b

  def quickEncode(tree: CodeTree[T])(text: List[T]): List[Bit] = {
    val table = convert(tree)
    text.flatMap(symbol => codeBits(table)(symbol))
  }
