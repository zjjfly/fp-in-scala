package com.github.zjjfly

import org.specs2.mutable._

/**
  * @author Zi JunJie[https://github.com/zjjfly]
  * @date 2022/6/13
  */
class HelloWorldSpec extends Specification {
  "The 'Hello world' string" should {
    "contain 11 characters" in {
      "Hello world" must haveSize(11)
    }
    "start with 'Hello'" in {
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      "Hello world" must endWith("world")
    }
  }
}
