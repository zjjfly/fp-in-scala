package com.github.zjjfly.fp
package ch4

/**
 * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
 */
sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](default: => MyOption[B]): MyOption[B] = this match {
    case Some(_) => this
    case None => default
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](o: A) extends MyOption[A]

case object None extends MyOption[Nothing]

object MyOption {
  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    (age / 10) + (numberOfSpeedingTickets / 100)

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String
                             ): MyOption[Double] = {
    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(
    f: (A, B) => C
  ): MyOption[C] = a flatMap (aa => b.map(bb => f(aa, bb)))

  def Try[A](a: => A): MyOption[A] =
    try Some(a)
    catch {
      case _: Exception => None
    }

  def sequence[A](l: List[MyOption[A]]): MyOption[List[A]] = l match {
    case x :: xs => x.flatMap((xx: A) => sequence(xs).map(xx :: _))
    case Nil => Some(Nil)
  }

  def traverse[A, B](l: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    l match {
      case Nil => Some(Nil)
      case x :: xs =>
        traverse(xs)(f).flatMap((xx: List[B]) => f(x).map(yy => yy :: xx))
    }

  def sequenceViaTraverse[A](l: List[MyOption[A]]): MyOption[List[A]] =
    traverse(l)(x => x)

  def map2ViaFor[A, B, C](a: MyOption[A],
                          b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    //for其实是语法糖，其中的binding会转换成flatMap调用，最后一个binding和yield会组成map调用
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def main(args: Array[String]): Unit = {
    println(mean(Seq(1.0, 2.0)))
    println(mean(Seq()))
    println(sequence(List(Some(1), Some(2))))
    println(sequence(List(Some(1), None)))
    println(sequence(List(None, Some(2))))
    println(traverse(List("2", "3"))(a => Try(a.toInt)))
    println(sequenceViaTraverse(List(Some(1), Some(2))))
    println(sequenceViaTraverse(List(Some(1), None)))
    println(sequenceViaTraverse(List(None, Some(2))))
    println(map2ViaFor(Some(1), Some(2))((i, j) => i + j))
  }
}
