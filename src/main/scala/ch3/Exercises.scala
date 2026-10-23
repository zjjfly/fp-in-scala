package com.github.zjjfly.fp
package ch3

/**
  * @author Zi JunJie <a href="https://github.com/zjjfly"/>github</a>
  */
object Exercises {

  def main(args: Array[String]): Unit = {
    val x = MyList(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case MyNil                                 => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + MyList.sum(t)
      case _                                     => 10
    }
    println(s"x = ${x}")
    println(MyList.dropWhile(MyList(1, 2, 3, 4))(_ < 3))
    println(MyList.foldRight(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
    println(MyList.length(MyList(1, 2, 3, 4, 5)))
    println(MyList.reverse(MyList(1, 2, 3, 4, 5)))
    println(MyList.concat(MyList(MyList(1, 2, 3), MyList(4, 5, 6))))
    println(MyList.map(MyList(1, 2, 3))(_ + 1))
    println(MyList.map2(MyList(1.0, 2.0, 3.0))(_.toString))
    println(MyList.filter(MyList(1, 2, 3, 4, 5))(_ > 2))
    println(MyList.flatMap(MyList("Hello", ",", "World"))(MyList(_: _*)))
    println(MyList.filterViaFlatMap(MyList(1, 2, 3, 4, 5))(_ > 2))
    println(MyList.zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_ + _))
    println(MyList.hasSubsequence(MyList(1, 2, 3), MyList(1)))

  }

}
