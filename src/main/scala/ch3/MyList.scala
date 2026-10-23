package com.github.zjjfly.fp
package ch3

import scala.annotation.tailrec

/**
  * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
  */
sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil       => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil       => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case MyNil      => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](a: A, l: MyList[A]): MyList[A] = l match {
    case MyNil      => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(a, t)
  }

  @tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) l
    else
      l match {
        case MyNil      => MyNil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  @tailrec
  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _                  => l
    }
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil          => sys.error("init of empty list")
    case Cons(_, MyNil) => MyNil
    case Cons(h, t)     => Cons(h, init(t))
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil      => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: MyList[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: MyList[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](l: MyList[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil      => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(l: MyList[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: MyList[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: MyList[A]): MyList[A] =
    foldLeft(l, MyNil: MyList[A])((h, acc) => Cons(acc, h))

  def foldRightViaFoldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft1[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: MyList[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def append[A](a: MyList[A], b: MyList[A]): MyList[A] =
    foldRight(a, b)(Cons(_, _))

  def concat[A](l: MyList[MyList[A]]): MyList[A] =
    foldRight(l, MyNil: MyList[A])(append)

  def map[A, B](l: MyList[A])(f: A => B): MyList[B] =
    foldRightViaFoldLeft(l, MyNil: MyList[B])((x, acc) => Cons(f(x), acc))

  //内部使用一个可变集合实现，对于外部是不可见的
  def map2[A, B](l: MyList[A])(f: A => B): MyList[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    @tailrec
    def go(l: MyList[A]): Unit = l match {
      case MyNil      => ()
      case Cons(h, t) => buf += f(h); go(t)
    }

    go(l)
    MyList(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def filter[A](l: MyList[A])(p: A => Boolean): MyList[A] =
    foldRightViaFoldLeft(l, MyNil: MyList[A])(
      (x, acc) => if (p(x)) Cons(x, acc) else acc
    )

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldRightViaFoldLeft(as, MyNil: MyList[B])(
      (x, acc) => MyList.append(f(x), acc)
    )

  def filterViaFlatMap[A](l: MyList[A])(p: A => Boolean): MyList[A] =
    flatMap(l)(a => if (p(a)) MyList(a) else MyNil)

  def zipWith[A, B, C](l: MyList[A], r: MyList[B])(f: (A, B) => C): MyList[C] =
    (l, r) match {
      case (_, MyNil)                   => MyNil
      case (MyNil, _)                   => MyNil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  @tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean =
    (sup, sub) match {
      case (_, MyNil) => true
      case (MyNil, _) => false
      case (Cons(h1, t1), (Cons(h2, t2))) =>
        if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
    }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))

}
