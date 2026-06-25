package find.cs214

sealed trait MockEntry(pth: String, next: Option[MockEntry]) extends Entry:
  def path(): String = pth
  def name(): String = pth.split("/").last
  def hasNextSibling(): Boolean = this.next.nonEmpty
  def nextSibling(): MockEntry = this.next.getOrElse(throw NoNextSiblingException())

case class MockFile(pth: String, next: Option[MockEntry], bytes: Int) extends MockEntry(pth, next):
  def firstChild(): Entry = throw NotADirectoryException()
  def hasChildren(): Boolean = throw NotADirectoryException()
  def isDirectory(): Boolean = false
  def size(): Long = bytes

case class MockDirectory(pth: String, next: Option[MockEntry], first: Option[MockEntry]) extends MockEntry(pth, next):
  def firstChild(): Entry = this.first.getOrElse(throw NoChildrenException())
  def hasChildren(): Boolean = this.first.nonEmpty
  def isDirectory(): Boolean = true
  def size(): Long = throw NotAFileException()
