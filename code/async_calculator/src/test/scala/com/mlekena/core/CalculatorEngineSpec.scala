package com.mlekena.core

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike
import com.mlekena.core.Adder.Calculate
import com.mlekena.core.CalculationResolver.CompleteCalculation

class CalculatorEngineSpec extends ScalaTestWithActorTestKit with WordSpecLike {
  "An Adder" must {
    "reply with the sum of 1 + 1 " in {
      val calculateProbe = createTestProbe[CompleteCalculation]()
      val adderUnderTest = spawn(Adder())
//      adderUnderTest ! Calculate(List(Plus(), Number(1), Number(1)), calculateProbe.ref)
//      calculateProbe.expectMessage(CompleteCalculation(2))
    }
  }
}
