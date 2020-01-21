package com.mlekena.core

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import fastparse._, NoWhitespace._

trait MathMember
final case class Number(value: Int) extends MathMember
final case class Plus() extends MathMember
final case class Minus() extends MathMember

trait MathNode
final case class Num(value: Int) extends MathNode
final case class mnPlus(lhs: MathNode, rhs: MathNode) extends MathNode
final case class mnMinus(lhs: MathNode, rhs: MathNode) extends MathNode

object MathNodeOp {
  def compute[O](lhs: MathMember, rhs: MathMember): MathNode = ???
}

class Main {}

object CalcParser {
  val aPlus = "+"
  val aMinus = "-"
  val digitR = "^([0-9])*\\d".r

  /**
    * This function is a trivial parser that reads lisp like math expressions of the form
    * Op Num Num
    * @param in
    * @return
    */
  def mmParse(in: String): List[MathMember] = {
    val charList = in.split(" ")
    def parsing(list: List[String]): List[MathMember] = {
      list match {
        case Nil                              => List[MathMember]()
        case plus :: rest if plus == aPlus    => Plus() :: parsing(rest)
        case minus :: rest if minus == aMinus => Minus() :: parsing(rest)
        case chr :: rest => {
          digitR findFirstIn (chr) match {
            case Some(str) if str == chr => Number(str.toInt) :: parsing(rest)
            case None                    => Nil
          }
        }
      }
    }
    parsing(charList.toList)
  }
  def eval(tree: (MathNode, Seq[(String, MathNode)])): MathNode = {
    println(s"${tree._1} ::: ${tree._2.mkString}")
    Num(999)
  }

  def number[_: P] = P(CharIn("0-9").rep(1).!.map(v => Num(v.toInt)))
  def parens[_: P] = P("(" ~/ addSub.! ~ ")").!
  def factor[_: P] = P(number | parens)

//    def divMul[_: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  def addSub[_: P]: P[MathNode] =
    P(factor ~ (CharIn("+\\-").! ~/ factor).rep).map(eval)
  def expr[_: P]: P[MathNode] = P(addSub ~ End)

  def mmFParse(in: String): Option[MathNode] = {

    parse(in, expr(_)) match {
      case Parsed.Success(node, _)               => Some(node)
      case Parsed.Failure(exp, failedidx, extra) => None
    }
  }

}

object CalculatorMain {
  final case class MathToCompute(computation: List[MathMember])

  def apply(): Behavior[MathToCompute] = Behaviors.setup { context =>
    val anAdder = context.spawn(Adder(), "Adder")
    val aSubtractor = context.spawn(Subtractor(), "Subtractor")
    val calcResolver = context.spawn(CalculationResolver(), "CalcResolver")
    Behaviors.receiveMessage { message =>
      message.computation match {
        case Nil => {
          context.log.debug("No Computation passed.")
          Behaviors.stopped
        }
        case Plus() :: rest =>
          anAdder ! Adder.Calculate(rest, calcResolver); Behaviors.same
        case Minus() :: rest =>
          aSubtractor ! Subtractor.Calculate(rest, calcResolver); Behaviors.same
        case Number(v) :: Nil => {
          context.log.debug(v.toString); Behaviors.stopped
        }
        case _ :: _ => {
          context.log.debug("FAILED CALCULATION ENTRY!"); Behaviors.stopped
        }
      }
    }
  }
}
object CalculationResolver {
  final case class CompleteCalculation(result: Int)

  def apply(): Behavior[CompleteCalculation] = Behaviors.receive {
    (context, res) =>
      {
        context.log.debug(res.result.toString)
        Behaviors.same
      }
  }
}

object Adder {
  final case class Calculate(
      lmm: List[MathMember],
      resolver: ActorRef[CalculationResolver.CompleteCalculation])
  final case class FailedMathException(msg: String) extends Exception

  def apply(): Behavior[Calculate] = Behaviors.receive { (context, comp) =>
    context.log.debug(comp.lmm.mkString)

    Behaviors.same
  }

  def performAdd(mm: List[MathMember]): List[MathMember] = {
    mm match {
      case Minus() :: Plus() :: rest => Minus() :: performAdd(rest)
      case Minus() :: rest           => Minus() :: rest
      case Number(lhs) :: Number(rhs) :: rest =>
        Number(lhs + rhs) :: performAdd(rest)
      case Plus() :: Number(_) :: Nil =>
        throw FailedMathException("Not enough Operands")
      case Plus() :: Number(v) :: rest => performAdd(Number(v) :: rest)
      case Plus() :: Plus() :: rest    => performAdd(Plus() :: performAdd(rest))
      case Number(v) :: Nil            => List(Number(v))
      case Nil                         => List[MathMember]()
      case e @ _                       => throw FailedMathException(s"Malformed Math Expression ${e}")
    }
  }
}

object Subtractor {
  final case class Calculate(
      lmm: List[MathMember],
      resolver: ActorRef[CalculationResolver.CompleteCalculation])

  def apply(): Behavior[Calculate] = ???
}
