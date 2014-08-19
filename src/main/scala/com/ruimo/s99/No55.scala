package com.ruimo.s99

import scala.collection.immutable

object No55 {
  sealed abstract class Tree[+T] {
    def weight: Int
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override val weight: Int = left.weight + right.weight + 1
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
    override val weight: Int = 0
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  def cBalanced[T](n: Int, value: T): Seq[Tree[T]] = 
    if (n < 1) throw new IllegalArgumentException("n (=" + n + ") should > 0")
    else if (n == 1) List(Node(value))
    else cBalanced(n - 1, value).foldLeft(immutable.HashSet[Tree[T]]()) {
      (sum, e) => sum ++ addOneNode(e, value)
    }.toList

  def addOneNode[T](tree: Tree[T], value: T): Seq[Tree[T]] = tree match {
    case End => List(Node(value))
    case Node(v, lhs, rhs) =>
      if (lhs.weight < rhs.weight)
        addOneNode(lhs, value).map { l => Node(value, l, rhs) }
      else if(lhs.weight > rhs.weight)
        addOneNode(rhs, value).map { r => Node(value, lhs, r) }
      else
        addOneNode(lhs, value).map { l => Node(value, l, rhs) } ++
        addOneNode(rhs, value).map { r => Node(value, lhs, r) }
  }

  def main(args: Array[String]) {
    (1 to 4) foreach { i =>
      println("n = " + i + ":" + cBalanced(i, "x"))
    }
  }
}
