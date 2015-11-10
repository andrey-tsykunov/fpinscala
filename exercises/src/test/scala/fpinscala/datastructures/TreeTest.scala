package fpinscala.datastructures

import org.scalatest.{Matchers, FunSuite}

import fpinscala.datastructures.Tree._

class TreeTest extends FunSuite with Matchers {

  test("testSize") {
    Tree.size(Leaf("a")) should be (1)
    Tree.size(Branch(Leaf("a"), Leaf("b"))) should be (3)

    Tree.sizeViaFold(Leaf("a")) should be (1)
    Tree.sizeViaFold(Branch(Leaf("a"), Leaf("b"))) should be (3)
  }

  test("max") {
    Tree.max(Branch(Leaf(2), Leaf(3))) should be (3)
    Tree.max(Leaf(5)) should be (5)

    Tree.maxViaFold(Branch(Leaf(2), Leaf(3))) should be (3)
    Tree.maxViaFold(Leaf(5)) should be (5)
  }

  test("depth") {
    Tree.depth(Leaf(5)) should be (1)
    Tree.depth(Branch(Leaf(2), Leaf(3))) should be (2)
    Tree.depth(Branch(Leaf(2), Branch(Leaf(4), Leaf(3)))) should be (3)

    Tree.depthViaFold(Leaf(5)) should be (1)
    Tree.depthViaFold(Branch(Leaf(2), Leaf(3))) should be (2)
    Tree.depthViaFold(Branch(Leaf(2), Branch(Leaf(4), Leaf(3)))) should be (3)
  }

  test("map") {
    val input = Branch(Leaf(2), Branch(Leaf(4), Leaf(3)))
    Tree.map(input)(_ + 1) should be (Branch(Leaf(3), Branch(Leaf(5), Leaf(4))))

    Tree.mapViaFold(input)(_ + 1) should be (Branch(Leaf(3), Branch(Leaf(5), Leaf(4))))
  }

}
