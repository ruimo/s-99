package com.ruimo.s99

import scala.collection.immutable

object No50 {
  sealed trait Tree extends {
    def weight: Int
  }
  case class Leaf(value: String, weight: Int) extends Tree
  case class Node(zero: Tree, one: Tree) extends Tree {
    val weight: Int = zero.weight + one.weight
  }
  implicit val treeOrdering = new Ordering[Tree] {
    def compare(t1: Tree, t2: Tree) = t1.weight compare t2.weight
  }

  def huffman(seq: Seq[(String, Int)]): Seq[(String, String)] =
    if (seq.size == 0) List()
    else if (seq.size == 1) seq.map {t => (t._1, "1")}
    else huffman(
      seq.map {t => Leaf(t._1, t._2)}.foldLeft(immutable.SortedSet.empty) {(sum, t) => sum + t}
    )

  def huffman(set: immutable.SortedSet[Tree]): Seq[(String, String)] = createCode(createTree(set))

  def createCode(tree: Tree): Seq[(String, String)] = {
    def createCode(tree: Tree, code: String, result: List[(String, String)]): List[(String, String)] = tree match {
      case Leaf(value, weight) => (value, code) :: result
      case Node(zero, one) => {
        val zeroResult = createCode(zero, code + "0", result)
        createCode(one, code + "1", zeroResult)
      }
    }

    createCode(tree, "", List())
  }

  def createTree(set: immutable.SortedSet[Tree]): Tree = {
    if (set.size == 1) set.head
    else {
      val (firstTwo, rest) = set.splitAt(2)
      createTree(rest + Node(firstTwo.head, firstTwo.last))
    }
  }

  def main(args: Array[String]) {
    println(huffman(List()))
    println(huffman(List(("a", 10))))
    println(huffman(List(("a", 10), ("b", 5))))
    println(huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))))
  }
}
