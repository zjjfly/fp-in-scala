package com.github.zjjfly.fp
package ch2

import scala.annotation.tailrec

/**
  * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
  */
object MyModule {

  def abs(n: Int): Int = if (n < 0) -1 * n else n

  private def formatAbs(x: Int) = {
    s"The absolute value of $x is ${abs(x)}"
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)
    go(n, 1)
  }

  private def formatFactorial(x: Int) = {
    s"The factorial of $x is ${factorial(x)}"
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    s"The $name of $n is ${f(n)}"
  }

  def findFirst[A](as: Seq[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    val i = findFirst("123ABC", (c: Char) => c == 'A')
    println(s"i = ${i}")
  }

}
