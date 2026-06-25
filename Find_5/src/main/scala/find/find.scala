package find

def findAllAndPrint(entry: cs214.Entry): Boolean =
  println(entry.path())
  if entry.isDirectory() && entry.hasChildren() then findAllAndPrint(entry.firstChild())
  if entry.hasNextSibling() then findAllAndPrint(entry.nextSibling())
  true

def findByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  val foundName =
    if (entry.name() == name) then
      println(entry.path())
      true
    else false

  val foundNameinChildren = 
    if entry.isDirectory() && entry.hasChildren() then findByNameAndPrint(entry.firstChild(), name) else false
  
  val foundNameinSibling =
    if entry.hasNextSibling() then findByNameAndPrint(entry.nextSibling(), name) else false

  foundName || foundNameinChildren || foundNameinSibling

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean =
  val foundSize =
    if !(entry.isDirectory()) && (entry.size() == size) then
      println(entry.path())
      true
    else false

  val foundSizeinChildren = 
    if entry.isDirectory() && entry.hasChildren() then findBySizeEqAndPrint(entry.firstChild(), size) else false
  
  val foundSizeinSibling =
    if entry.hasNextSibling() then findBySizeEqAndPrint(entry.nextSibling(), size) else false

  foundSize || foundSizeinChildren || foundSizeinSibling

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean =
  val foundSize =
    if !(entry.isDirectory()) && (entry.size() >= minSize) then
      println(entry.path())
      true
    else false

  val foundSizeinChildren = 
    if entry.isDirectory() && entry.hasChildren() then findBySizeGeAndPrint(entry.firstChild(), minSize) else false
  
  val foundSizeinSibling =
    if entry.hasNextSibling() then findBySizeGeAndPrint(entry.nextSibling(), minSize) else false

  foundSize || foundSizeinChildren || foundSizeinSibling

def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  val foundEmpty =
    if (entry.isDirectory() && !(entry.hasChildren())) || (!(entry.isDirectory()) && entry.size() == 0) then
      println(entry.path())
      true
    else false

  val foundEmptyinChildren = 
    if entry.isDirectory() && entry.hasChildren() then findEmptyAndPrint(entry.firstChild()) else false
  
  val foundEmptyinSibling =
    if entry.hasNextSibling() then findEmptyAndPrint(entry.nextSibling()) else false

  foundEmpty || foundEmptyinChildren || foundEmptyinSibling

///////////////////////////////
// The following is optional //
///////////////////////////////

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  if (entry.name() == name) then
    println(entry.path())
    true
  else (entry.isDirectory() && entry.hasChildren() && findFirstByNameAndPrint(entry.firstChild(), name)) || (entry.hasNextSibling() && findFirstByNameAndPrint(entry.nextSibling(), name))

