package com.mlekena.core

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest._
import org.scalatest.Matchers._

class CalculateImplSpec extends FlatSpec  {
  "Lisp like expression" should "parse to MathMember list" in {
    val lispLikeExpression = "+ 2 3"
    val expectedMathMemberList = List(Plus(), Number(2), Number(3))
    val actualMathMemberList = CalcParser.mmParse(lispLikeExpression)
    actualMathMemberList shouldEqual expectedMathMemberList
  }
  "Lisp like expression" should "parse to MathMember list from larger numbers" in {
    val lispLikeExpression = "- 224356 386795"
    val expectedMathMemberList = List(Minus(), Number(224356), Number(386795))
    val actualMathMemberList = CalcParser.mmParse(lispLikeExpression)
    actualMathMemberList shouldEqual expectedMathMemberList
  }
  "Lisp like expression" should "parse to MathMember of complex expressions" in {
    val lispLikeExpression = "- + 386795 99 23"
    val expectedMathMemberList = List(Minus(), Plus(), Number(386795), Number(99), Number(23))
    val actualMathMemberList = CalcParser.mmParse(lispLikeExpression)
    actualMathMemberList shouldEqual expectedMathMemberList
  }
}
