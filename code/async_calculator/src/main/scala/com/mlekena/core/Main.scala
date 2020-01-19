package com.mlekena.core

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

trait MathMember
final case class Number(value: Int) extends MathMember
final case class Plus() extends MathMember
final case class Minus() extends MathMember

class Main {

}

object CalcParser {
  def mmParse(in: String): List[MathMember] = {
    List[MathMember]()
  }

}


object CalculatorMain {
  final case class MathToCompute(computation: List[MathMember])

  def apply(): Behavior[MathToCompute] = Behaviors.setup {
    context =>
      val anAdder = context.spawn(Adder(), "Adder")
      val aSubtractor = context.spawn(Subtractor(), "Subtractor")

      Behaviors.receiveMessage{message =>
        message.computation match {
          case Nil => {
            context.log.debug("No Computation passed.")
            Behaviors.stopped
          }
          case Plus() :: rest => anAdder ! Adder.Calculate(rest); Behaviors.same
          case Minus() :: rest => aSubtractor ! Subtractor.Calculate(rest); Behaviors.same
          case Number(v) :: Nil => {context.log.debug(v.toString); Behaviors.stopped}
          case _ :: _ => {context.log.debug("FAILED CALCULATION ENTRY!"); Behaviors.stopped}
        }
      }
  }
}

object Adder {
  final case class Calculate(lmm: List[MathMember])

  def apply(): Behavior[Calculate] = Behaviors.receive { (context, comp) =>
    context.log.debug(comp.lmm.mkString)
    Behaviors.same
  }
}

object Subtractor {
  final case class Calculate(lmm: List[MathMember])

  def apply(): Behavior[Calculate] = ???
}
