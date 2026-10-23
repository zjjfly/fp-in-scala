package com.github.zjjfly.fp
package ch3

import scala.util.{Failure, Random, Success}

/**
  * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

case object EmptyTree extends Tree[Nothing]

object Tree {

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    println(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println(maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(5))))
    println(depth(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(5))))
    println(
      map(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(5)))(_ + 1)
    )
    val t:util.Try[String] = util.Try {
      val i = Random.nextInt()
      if(i % 2 == 0 ) "xx" else throw new RuntimeException("odd number")
    }
  }
}
