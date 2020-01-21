package com.mlekena.core

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import com.mlekena.core.Adder.FailedMathException
import fastparse._, NoWhitespace._
import org.scalatest._
import org.scalatest.Matchers._

class CalculateImplSpec extends FlatSpec {
  "parser components" should "return correct parse results num" in {
    parse("1", CalcParser.number(_)) shouldEqual Parsed.Success(Num(1), 1)
  }

  "parser parens component" should "return correct parse results num" in {
    parse("(1)", CalcParser.parens(_)) shouldEqual Parsed.Success(Num(1), 3)
  }

  "parser parens component" should "return correct parse results num with add" in {
    parse("(1 + 1)", CalcParser.parens(_)) shouldEqual Parsed.Success(
      mnPlus(Num(1), Num(1)),
      5)
  }

  "math expressions" should "parse to MathNodes" in {
    val math = "1 + 1"
    val res = mnPlus(Num(1), Num(1))
    CalcParser.mmFParse(math) shouldEqual Some(res)
  }

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
    val expectedMathMemberList =
      List(Minus(), Plus(), Number(386795), Number(99), Number(23))
    val actualMathMemberList = CalcParser.mmParse(lispLikeExpression)
    actualMathMemberList shouldEqual expectedMathMemberList
  }

  "Adder operation" should "calculate sum over operands" in {
    val calc = List(Plus(), Number(1), Number(1))
    val result = Number(2)
    Adder.performAdd(calc).head shouldEqual result

  }
  "Adder operation" should "calculate sum over operands with no Plus" in {
    val calc = List(Number(1), Number(1))
    val result = Number(2)
    Adder.performAdd(calc).head shouldEqual result

  }
  "Adder operation" should "calculate sum over operands with multiple additions" in {
    val calc = List(Plus(), Plus(), Number(1), Number(1), Number(1))
    val result = Number(3)
    Adder.performAdd(calc).head shouldEqual result

  }
  "Adder operation" should "calculate should fail with not enough operands" in {
    val calc = List(Plus(), Number(1))
    assertThrows[FailedMathException] {
      Adder.performAdd(calc)
    }
  }
  "Adder operation" should "returns rest of calculation on minus" in {
    val calc = List(Minus(), Plus(), Number(1), Number(1), Number(1))
    val result = List(Minus(), Number(2), Number(1))
    Adder.performAdd(calc).head shouldEqual result

  }
}
