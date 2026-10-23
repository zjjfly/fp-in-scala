package com.github.zjjfly.fp
package ch2

import scala.annotation.tailrec

/**
  * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
  */
object Exercise1 {

  private def fib(i: Int): Int = {
    @tailrec
    def loop(n: Int, current: Int, next: Int): Int =
      if (n <= 0) current
      else loop(n - 1, next, current + next)
    loop(i, 0, 1)
  }

  private def isSorted[A](s: Seq[A], f: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = n match {
      case n if n >= (s.size - 1) => true
      case _                      => if (f(s(n), s(n + 1))) loop(n + 1) else false
    }
    if (s.isEmpty) true else loop(0)
  }

  private def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  private def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  private def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))

    println(isSorted(Array(1, 2, 3), (i1: Int, i2: Int) => i1 <= i2))
    println(isSorted(Array(3, 2, 1), (i1: Int, i2: Int) => i1 >= i2))
    println(isSorted(Array(1), (i1: Int, i2: Int) => i1 <= i2))
    println(isSorted(Array(1), (i1: Int, i2: Int) => i1 >= i2))
    println(isSorted(Array.empty[Int], (i1: Int, i2: Int) => i1 >= i2))
    println(isSorted(Array(1, 3, 2), (i1: Int, i2: Int) => i1 <= i2))
    println(isSorted(Array(1, 3, 2), (i1: Int, i2: Int) => i1 >= i2))

    val curried: Int => Int => Boolean = curry((i1: Int, i2: Int) => i1 >= i2)
    println(curried(2)(1))

    val uncurried: (Int, Int) => Boolean = uncurry(
      (i1: Int) => (i2: Int) => i1 >= i2
    )
    println(uncurried(2, 1))

    val composed: String => Int =
      compose((i: Int) => i + 1, (s: String) => s.length)
    println(composed("abc"))

  }

}
