package BinaryTrees

/*A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.

  We shall use the following classes to represent binary trees. (Also available in tree1.scala.) An End is equivalent to
  an empty tree. A Branch has a value, and two descendant trees. The toString functions are relatively arbitrary, but they
  yield a more compact output than Scala's default. Putting a plus in front of the T makes the class covariant; it will
  be able to hold subtypes of whatever type it's created for. (This is important so that End can be a singleton object;
  as a singleton, it must have a specific type, so we give it type Nothing, which is a subtype of every other type.)
*/

sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
case object End extends Tree[Nothing] {
  override def toString = "."
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
/*
The example tree on the right is given by

Node('a',
  Node('b', Node('d'), Node('e')),
  Node('c', End, Node('f', Node('g'), End)))

A tree with only a root node would be Node('a') and an empty tree would be End.

Throughout this section, we will be adding methods to the classes above, mostly to Tree.
*/