package com.mlekena.core

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest._
import org.scalatest.Matchers._

class CalculateImplSpec extends ScalaTestWithActorTestKit with WordSpecLike with FlatSpec {
  "Lisp like expression" should "parse to MathMember list" in {
    val lispLikeExpression = "+ 2 3"
    val expectedMathMemberList = List(Plus(), Number(2), Number(3))
    val actualMathMemberList = CalcParser.mmParse(lispLikeExpression)
    assert(actualMathMemberList == actualMathMemberList)
  }
}
