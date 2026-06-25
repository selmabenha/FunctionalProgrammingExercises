package cs214

import anagrams.*
import scala.io.Source
import scala.io.Codec

case class CLI(
    words: List[String],
    wordlist: String
)

val CLI_DEFAULT = CLI(
  words = List(),
  wordlist = "en-ngsl"
)

def parseOpts(args: Seq[String]): CLI =
  def loop(args: Seq[String], cli: CLI): CLI =
    args match
      case Seq() => cli
      case Seq("--wordlist", name, rest*) =>
        loop(rest, cli.copy(wordlist = name))
      case Seq(word, rest*) =>
        if word.startsWith("-") then
          throw new IllegalArgumentException(f"Unrecognized argument $word.")
        loop(rest, cli.copy(words = word :: cli.words))
  val cli = loop(args, CLI_DEFAULT)
  if cli.words.length == 0 then
    throw new IllegalArgumentException(f"Need at least one word.")
  cli.copy(words = cli.words.reverse)

def loadWordlist(name: String): Set[String] =
  val fname = f"$name.txt"
  val wordstream =
    Option(this.getClass.getResourceAsStream(f"/anagrams/$fname"))
      .getOrElse(sys.error(f"File not found: $fname"))
  try Source.fromInputStream(wordstream)(Codec.UTF8).getLines().toSet
  finally wordstream.close()

@main def main(args: String*) =
  try
    val cli = parseOpts(args)

    val dict = createDictionary(loadWordlist(cli.wordlist))
    val sentence = cli.words.map(normalizeString)
    val tree = anagrams(dict, sentenceOccurrences(sentence))
    println(tree.show)
  catch
    case e: IllegalArgumentException =>
      System.err.println(e.getMessage())
      System.err.println("Usage: run [--one-by-one] [--wordlist name] [--method name] word [word…]")
    case e: Exception =>
      System.err.println(e.getMessage())
