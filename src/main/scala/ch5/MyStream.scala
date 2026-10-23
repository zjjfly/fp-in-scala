package com.github.zjjfly.fp
package ch5

import scala.annotation.tailrec

/**
 * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
 */
sealed trait MyStream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): MyStream[A] = this match {
    case Cons(h, t) if n > 1 => MyStream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => MyStream.cons(h(), MyStream.empty)
    case _ => MyStream.empty
  }

  @tailrec
  final def drop(n: Int): MyStream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case Cons(h, t) if p(h()) => MyStream.cons(h(), t().takeWhile(p))
    case _ => MyStream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = !exists(a => !p(a))

  def takeWhileByFoldRight(p: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A])(
      (a, b) => if (p(a)) MyStream.cons(a, b) else MyStream.empty
    )

  def headOptionByFoldRight(): Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): MyStream[B] =
    foldRight(MyStream.empty[B])((a, b) => MyStream.cons(f(a), b))

  def filter(p: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A])((a, b) => if (p(a)) MyStream.cons(a, b) else b)

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(MyStream.empty[B])((a, b) => f(a).append(b))

  def append[B >: A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((a, b) => MyStream.cons(a, b))
}

case object Empty extends MyStream[Nothing]

case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {

  //下面这种方法被称为智能构造器，习惯上会和类构造器名称相同但首字母小写，可以提供类构造器不具备的一些能力
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    //下面这种写法很常见，可以避免对非严格求值参数的重复求值
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](a: A*): MyStream[A] =
    if (a.isEmpty) empty else cons(a.head, apply(a.tail: _*))

  def constant[A](a: A): MyStream[A] = {
    lazy val stream: MyStream[A] = cons(a, stream)
    stream
  }

  def from(n: Int): MyStream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
    f(z) match {
      case None => MyStream.empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def fib(): MyStream[Int] = {
    unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }
  }

  def main(args: Array[String]): Unit = {
    //使用Cons类构造器会产生重复求值的问题，所以需要使用智能构造器
    val stream1: MyStream[Int] = Cons(() => {
      println("evaluated head");
      1
    }, () => Cons(() => 2, () => Cons(() => 3, () => empty)))
    stream1.headOption
    //evaluated head
    stream1.headOption
    //evaluated head

    val stream2 = MyStream(1, 2, 3)
    println("steam2.toList = " + stream2.toList)
    println("stream2.take(1) = " + stream2.take(1).toList)
    println("stream2.take(2) = " + stream2.take(2).toList)
    println("stream2.take(3) = " + stream2.take(3).toList)
    println("stream2.drop(1) = " + stream2.drop(1).toList)
    println("stream2.drop(2) = " + stream2.drop(2).toList)
    println("stream2.drop(3) = " + stream2.drop(3).toList)
    println("stream2.takeWhile(_ > 2) = " + stream2.takeWhile(_ > 2).toList)
    println("stream2.takeWhile(_ < 2) = " + stream2.takeWhile(_ < 2).toList)
    println("stream2.takeWhile(_ <= 2) = " + stream2.takeWhile(_ <= 2).toList)
    println("stream2.takeWhile(_ <= 3) = " + stream2.takeWhile(_ <= 3).toList)
    println("stream2.exists(_ >= 3) = " + stream2.exists(_ >= 3))
    println("stream2.exists(_ > 3) = " + stream2.exists(_ > 3))
    println("stream2.forAll(_ >= 0) = " + stream2.forAll(_ >= 0))
    println("stream2.forAll(_ >= 2) = " + stream2.forAll(_ >= 2))
    println(
      "stream2.takeWhileByFoldRight(_ > 2) = " + stream2
        .takeWhileByFoldRight(_ > 2)
        .toList
    )
    println(
      "stream2.takeWhileByFoldRight(_ < 2) = " + stream2
        .takeWhileByFoldRight(_ < 2)
        .toList
    )
    println(
      "stream2.takeWhileByFoldRight(_ <= 2) = " + stream2
        .takeWhileByFoldRight(_ <= 2)
        .toList
    )
    println(
      "stream2.takeWhileByFoldRight(_ <= 3) = " + stream2
        .takeWhileByFoldRight(_ <= 3)
        .toList
    )
    println(
      "stream2.headOptionByFoldRight() = " + stream2.headOptionByFoldRight()
    )

    println("stream2.map(_ + 1) = " + stream2.map(_ + 1).toList)
    println("stream2.filter(_ >= 2) = " + stream2.filter(_ >= 2).toList)
    println(
      "stream2.append(MyStream(4,5,6)) = " + stream2
        .append(MyStream(4, 5, 6))
        .toList
    )
    val stream3 = MyStream("a,b,c", "d,e,f")
    println(
      "stream3.flatMap(s=>MyStream(s.split(\",\"):_*)) = " + stream3
        .flatMap(s => MyStream(s.split(","): _*))
        .toList
    )

    // infinite stream
    val stream4 = constant(1)
    println(stream4.take(4).toList)
    println(stream4.map(_ + 1).take(4).toList)

    val stream5 = from(1)
    println(stream5.take(3).toList)

    val stream6 = unfold(1)(i => Some((i, i + 1)))
    println(stream6.take(3).toList)

    val fibStream = fib()
    println(fibStream.take(10).toList)
  }
}
