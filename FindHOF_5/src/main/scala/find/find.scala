package find

def findAndPrint(entry: cs214.Entry, f: cs214.Entry => Boolean): Boolean = 
  val thisFound =
    f(entry)
      && { println(entry.path()); true }

  val childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findAndPrint(entry.firstChild(), f)

  val nextSiblingsFound =
    entry.hasNextSibling()
      && findAndPrint(entry.nextSibling(), f)

  thisFound || childrenFound || nextSiblingsFound

def findAllAndPrint(entry: cs214.Entry): Boolean = findAndPrint(entry, e => true)

def findByNameAndPrint(entry: cs214.Entry, name: String): Boolean = findAndPrint(entry, e => e.name() == name)

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean = findAndPrint(entry, e => !e.isDirectory() && e.size() == size)

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean = findAndPrint(entry, e => !e.isDirectory() && e.size() >= minSize)

def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  findAndPrint(entry, e => 
    if e.isDirectory() then
      !e.hasChildren()
    else
      e.size() == 0
      )

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  val thisFound =
    entry.name() == name
      && { println(entry.path()); true }

  def childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findFirstByNameAndPrint(entry.firstChild(), name)

  def nextSiblingsFound =
    entry.hasNextSibling()
      && findFirstByNameAndPrint(entry.nextSibling(), name)

  thisFound || childrenFound || nextSiblingsFound
