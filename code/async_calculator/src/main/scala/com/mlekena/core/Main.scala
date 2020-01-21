package com.mlekena.core

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.Int
import scala.annotation.tailrec

trait MathMember
final case class Number(value: Int) extends MathMember
final case class Plus() extends MathMember
final case class Minus() extends MathMember

class Main {

}

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
        case Nil => List[MathMember]()
        case plus :: rest if plus == aPlus => Plus() :: parsing(rest)
        case minus :: rest if minus == aMinus => Minus() :: parsing(rest)
        case chr :: rest => {
          digitR findFirstIn(chr) match {
            case Some(str) if str == chr => Number(str.toInt) :: parsing(rest)
            case None => Nil
          }
        }
      }
    }
    parsing(charList.toList)
  }

}


object CalculatorMain {
  final case class MathToCompute(computation: List[MathMember])

  def apply(): Behavior[MathToCompute] = Behaviors.setup {
    context =>
      val anAdder = context.spawn(Adder(), "Adder")
      val aSubtractor = context.spawn(Subtractor(), "Subtractor")
      val calcResolver = context.spawn(CalculationResolver(), "CalcResolver")
      Behaviors.receiveMessage{message =>
        message.computation match {
          case Nil => {
            context.log.debug("No Computation passed.")
            Behaviors.stopped
          }
          case Plus() :: rest => anAdder ! Adder.Calculate(rest, calcResolver); Behaviors.same
          case Minus() :: rest => aSubtractor ! Subtractor.Calculate(rest, calcResolver); Behaviors.same
          case Number(v) :: Nil => {context.log.debug(v.toString); Behaviors.stopped}
          case _ :: _ => {context.log.debug("FAILED CALCULATION ENTRY!"); Behaviors.stopped}
        }
      }
  }
}
object CalculationResolver {
  final case class CompleteCalculation(result: Int)

  def apply(): Behavior[CompleteCalculation] = Behaviors.receive {(context, res) => {
    context.log.debug(res.result.toString)
    Behaviors.same
  }}
}


object Adder {
  final case class Calculate(lmm: List[MathMember], resolver: ActorRef[CalculationResolver.CompleteCalculation])

  def apply(): Behavior[Calculate] = Behaviors.receive { (context, comp) =>
    context.log.debug(comp.lmm.mkString)
    Behaviors.same
  }
}

object Subtractor {
  final case class Calculate(lmm: List[MathMember], resolver: ActorRef[CalculationResolver.CompleteCalculation])

  def apply(): Behavior[Calculate] = ???
}
