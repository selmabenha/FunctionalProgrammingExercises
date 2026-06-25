package cs214

import anagrams.*

object TestData:
  class UnknownDictionaryException(name: String) extends Exception("Unknown dictionary " + name)

  type TestData = (Sentence, AnagramsTree) // (input, expected output)

  val dicts = List(
    "en-debian",
    "en-ngsl"
  )

  def testData(dictName: String): List[TestData] = dictName match
    case "en-ngsl" => List(
        List("Linux", "rulez") -> Nil,
        List("Lukas", "Rytz") -> Nil,
        List("Yell", "Xerxes") -> Nil,
        List("I", "love", "you") -> List(
          Branch(
            Set("i"),
            List(Branch(Set("love"), List(Branch(Set("you"), Nil))), Branch(Set("you"), List(Branch(Set("love"), Nil))))
          ),
          Branch(
            Set("love"),
            List(Branch(Set("i"), List(Branch(Set("you"), Nil))), Branch(Set("you"), List(Branch(Set("i"), Nil))))
          ),
          Branch(
            Set("you"),
            List(Branch(Set("i"), List(Branch(Set("love"), Nil))), Branch(Set("love"), List(Branch(Set("i"), Nil))))
          )
        ),
        List("Heather") -> List(
          Branch(
            Set("a"),
            List(Branch(Set("her"), List(Branch(Set("the"), Nil))), Branch(Set("the"), List(Branch(Set("her"), Nil))))
          ),
          Branch(
            Set("he"),
            List(
              Branch(Set("he"), List(Branch(Set("art", "rat"), Nil))),
              Branch(Set("her"), List(Branch(Set("at"), Nil))),
              Branch(Set("at"), List(Branch(Set("her"), Nil))),
              Branch(Set("art", "rat"), List(Branch(Set("he"), Nil))),
              Branch(Set("earth", "heart"), Nil)
            )
          ),
          Branch(
            Set("her"),
            List(
              Branch(Set("a"), List(Branch(Set("the"), Nil))),
              Branch(Set("he"), List(Branch(Set("at"), Nil))),
              Branch(Set("at"), List(Branch(Set("he"), Nil))),
              Branch(Set("the"), List(Branch(Set("a"), Nil))),
              Branch(Set("heat", "hate"), Nil)
            )
          ),
          Branch(Set("hear"), List(Branch(Set("the"), Nil))),
          Branch(Set("here"), List(Branch(Set("hat"), Nil))),
          Branch(
            Set("at"),
            List(Branch(Set("he"), List(Branch(Set("her"), Nil))), Branch(Set("her"), List(Branch(Set("he"), Nil))))
          ),
          Branch(Set("hat"), List(Branch(Set("here"), Nil))),
          Branch(
            Set("the"),
            List(
              Branch(Set("a"), List(Branch(Set("her"), Nil))),
              Branch(Set("her"), List(Branch(Set("a"), Nil))),
              Branch(Set("hear"), Nil)
            )
          ),
          Branch(Set("heat", "hate"), List(Branch(Set("her"), Nil))),
          Branch(Set("art", "rat"), List(Branch(Set("he"), List(Branch(Set("he"), Nil))))),
          Branch(Set("earth", "heart"), List(Branch(Set("he"), Nil)))
        )
      )
    case "en-debian" => List(
        List("Linux", "rulez") -> List(
          Branch(
            Set("nil", "Lin"),
            List(Branch(Set("Rex"), List(Branch(Set("Zulu"), Nil))), Branch(Set("Zulu"), List(Branch(Set("Rex"), Nil))))
          ),
          Branch(
            Set("null"),
            List(Branch(Set("Rex"), List(Branch(Set("Uzi"), Nil))), Branch(Set("Uzi"), List(Branch(Set("Rex"), Nil))))
          ),
          Branch(
            Set("Rex"),
            List(
              Branch(Set("nil", "Lin"), List(Branch(Set("Zulu"), Nil))),
              Branch(Set("null"), List(Branch(Set("Uzi"), Nil))),
              Branch(Set("Uzi"), List(Branch(Set("null"), Nil))),
              Branch(Set("Zulu"), List(Branch(Set("nil", "Lin"), Nil)))
            )
          ),
          Branch(Set("Linux"), List(Branch(Set("rulez"), Nil))),
          Branch(
            Set("Uzi"),
            List(Branch(Set("null"), List(Branch(Set("Rex"), Nil))), Branch(Set("Rex"), List(Branch(Set("null"), Nil))))
          ),
          Branch(Set("rulez"), List(Branch(Set("Linux"), Nil))),
          Branch(
            Set("Zulu"),
            List(
              Branch(Set("nil", "Lin"), List(Branch(Set("Rex"), Nil))),
              Branch(Set("Rex"), List(Branch(Set("nil", "Lin"), Nil)))
            )
          )
        ),
        List("Lukas", "Rytz") -> List(
          Branch(
            Set("Ku"),
            List(Branch(Set("try"), List(Branch(Set("Salz"), Nil))), Branch(Set("Salz"), List(Branch(Set("try"), Nil))))
          ),
          Branch(
            Set("try"),
            List(Branch(Set("Ku"), List(Branch(Set("Salz"), Nil))), Branch(Set("Salz"), List(Branch(Set("Ku"), Nil))))
          ),
          Branch(Set("surly"), List(Branch(Set("Katz"), Nil))),
          Branch(
            Set("Salz"),
            List(Branch(Set("Ku"), List(Branch(Set("try"), Nil))), Branch(Set("try"), List(Branch(Set("Ku"), Nil))))
          ),
          Branch(Set("Katz"), List(Branch(Set("surly"), Nil)))
        ),
        List("Yell", "Xerxes") -> List(
          Branch(
            Set("Rex"),
            List(
              Branch(Set("sex"), List(Branch(Set("yell", "Lyle"), Nil))),
              Branch(Set("yell", "Lyle"), List(Branch(Set("sex"), Nil)))
            )
          ),
          Branch(
            Set("sex"),
            List(
              Branch(Set("Rex"), List(Branch(Set("yell", "Lyle"), Nil))),
              Branch(Set("yell", "Lyle"), List(Branch(Set("Rex"), Nil)))
            )
          ),
          Branch(Set("Xerxes"), List(Branch(Set("yell", "Lyle"), Nil))),
          Branch(
            Set("yell", "Lyle"),
            List(
              Branch(Set("Rex"), List(Branch(Set("sex"), Nil))),
              Branch(Set("sex"), List(Branch(Set("Rex"), Nil))),
              Branch(Set("Xerxes"), Nil)
            )
          )
        ),
        List("I", "love", "you") -> List(
          Branch(
            Set("I"),
            List(Branch(Set("love"), List(Branch(Set("you"), Nil))), Branch(Set("you"), List(Branch(Set("love"), Nil))))
          ),
          Branch(
            Set("Io"),
            List(Branch(Set("Lev"), List(Branch(Set("you"), Nil))), Branch(Set("you"), List(Branch(Set("Lev"), Nil))))
          ),
          Branch(
            Set("Lev"),
            List(Branch(Set("Io"), List(Branch(Set("you"), Nil))), Branch(Set("you"), List(Branch(Set("Io"), Nil))))
          ),
          Branch(
            Set("love"),
            List(Branch(Set("I"), List(Branch(Set("you"), Nil))), Branch(Set("you"), List(Branch(Set("I"), Nil))))
          ),
          Branch(Set("olive"), List(Branch(Set("you"), Nil))),
          Branch(
            Set("you"),
            List(
              Branch(Set("I"), List(Branch(Set("love"), Nil))),
              Branch(Set("Io"), List(Branch(Set("Lev"), Nil))),
              Branch(Set("Lev"), List(Branch(Set("Io"), Nil))),
              Branch(Set("love"), List(Branch(Set("I"), Nil))),
              Branch(Set("olive"), Nil)
            )
          )
        ),
        List("Heather") -> List(
          Branch(
            Set("a"),
            List(Branch(Set("her"), List(Branch(Set("the"), Nil))), Branch(Set("the"), List(Branch(Set("her"), Nil))))
          ),
          Branch(
            Set("ah", "ha"),
            List(
              Branch(Set("re"), List(Branch(Set("the"), Nil))),
              Branch(Set("her"), List(Branch(Set("et"), Nil))),
              Branch(Set("et"), List(Branch(Set("her"), Nil))),
              Branch(Set("the"), List(Branch(Set("re"), Nil))),
              Branch(Set("three", "there", "ether"), Nil)
            )
          ),
          Branch(
            Set("he"),
            List(
              Branch(Set("he"), List(Branch(Set("art", "tar", "rat"), Nil))),
              Branch(Set("re"), List(Branch(Set("hat"), Nil))),
              Branch(Set("her"), List(Branch(Set("at"), Nil))),
              Branch(Set("at"), List(Branch(Set("her"), Nil))),
              Branch(Set("hat"), List(Branch(Set("re"), Nil))),
              Branch(Set("art", "tar", "rat"), List(Branch(Set("he"), Nil))),
              Branch(Set("heart", "earth", "hater"), Nil)
            )
          ),
          Branch(
            Set("re"),
            List(
              Branch(Set("ah", "ha"), List(Branch(Set("the"), Nil))),
              Branch(Set("he"), List(Branch(Set("hat"), Nil))),
              Branch(Set("hat"), List(Branch(Set("he"), Nil))),
              Branch(Set("the"), List(Branch(Set("ah", "ha"), Nil))),
              Branch(Set("heath"), Nil)
            )
          ),
          Branch(
            Set("her"),
            List(
              Branch(Set("a"), List(Branch(Set("the"), Nil))),
              Branch(Set("ah", "ha"), List(Branch(Set("et"), Nil))),
              Branch(Set("he"), List(Branch(Set("at"), Nil))),
              Branch(Set("at"), List(Branch(Set("he"), Nil))),
              Branch(Set("et"), List(Branch(Set("ah", "ha"), Nil))),
              Branch(Set("the"), List(Branch(Set("a"), Nil))),
              Branch(Set("hate", "heat", "Thea"), Nil)
            )
          ),
          Branch(Set("hare", "Rhea", "hear", "Hera"), List(Branch(Set("the"), Nil))),
          Branch(Set("here"), List(Branch(Set("hat"), Nil))),
          Branch(
            Set("at"),
            List(Branch(Set("he"), List(Branch(Set("her"), Nil))), Branch(Set("her"), List(Branch(Set("he"), Nil))))
          ),
          Branch(
            Set("et"),
            List(
              Branch(Set("ah", "ha"), List(Branch(Set("her"), Nil))),
              Branch(Set("her"), List(Branch(Set("ah", "ha"), Nil)))
            )
          ),
          Branch(
            Set("hat"),
            List(
              Branch(Set("he"), List(Branch(Set("re"), Nil))),
              Branch(Set("re"), List(Branch(Set("he"), Nil))),
              Branch(Set("here"), Nil)
            )
          ),
          Branch(
            Set("the"),
            List(
              Branch(Set("a"), List(Branch(Set("her"), Nil))),
              Branch(Set("ah", "ha"), List(Branch(Set("re"), Nil))),
              Branch(Set("re"), List(Branch(Set("ah", "ha"), Nil))),
              Branch(Set("her"), List(Branch(Set("a"), Nil))),
              Branch(Set("hare", "Rhea", "hear", "Hera"), Nil)
            )
          ),
          Branch(Set("hate", "heat", "Thea"), List(Branch(Set("her"), Nil))),
          Branch(Set("heath"), List(Branch(Set("re"), Nil))),
          Branch(Set("art", "tar", "rat"), List(Branch(Set("he"), List(Branch(Set("he"), Nil))))),
          Branch(Set("heart", "earth", "hater"), List(Branch(Set("he"), Nil))),
          Branch(Set("three", "there", "ether"), List(Branch(Set("ah", "ha"), Nil))),
          Branch(Set("heather"), Nil)
        )
      )
    case _ => throw UnknownDictionaryException(dictName)

