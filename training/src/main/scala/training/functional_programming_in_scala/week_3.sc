abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = other

  override def toString: String = "."
}

class NonEmpty(root: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < root) left contains  x
    else if (x > root) right contains x
    else true
  }

  def include(x: Int): IntSet = {
    if (x < root) new NonEmpty(root, left include x, right)
    else if (x > root) new NonEmpty(root, left, right include x)
    else this
  }

  def union(other: IntSet): IntSet = {
    left union right union other include root
  }

  override def toString: String = "{" + left + root + right + "}"
}

val t1 = new NonEmpty(42, Empty, Empty)
val t2 = t1 include 7
val t3 = Empty include 13 include 17 include 23
val t4 = t2 union t3
