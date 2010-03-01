package test.scala.de.downgra.statemachine

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

import de.downgra.statemachine._

class StatemachineSpec extends WordSpec with ShouldMatchers {

  "A Statemachine" when {

    "empty" should {
      val machine = new Statemachine {}

      "be not running" in {
        assert(machine.isRunning === false)
      }

      "immediately returns from start" in {
        machine.start()
      }

    }

    "it contains a transition from STOP to STOP" should {
      val machine = new Statemachine {
        define from STOP to STOP
      }

      "immediately returns from start" in {
        machine.start()
      }
    }

    "it contains three simple transitions with a last StopState" should {
      val machine = new Statemachine {
        val A, B, C = State
        override val START = Some(A)

        var path = ""

        define entry A as { path += "eA" }
        define exit  A as { path += "xA" }
        define entry B as { path += "eB" }
        define exit  B as { path += "xB" }
        define entry C as { path += "eC" }
        define exit  C as { path += "xC" }

        define from A to B
        define from B to C
        define from C to STOP
      }

      "go through all 3 transitions" in {
        machine.start()
        assert(machine.path === "eAxAeBxBeCxC")
      }
    }

    "it have a transition with a condition" should {
      val machine = new Statemachine {
        val A, B = State
        override val START = Some(A)

        var x = 0
        var y = 0

        define entry A as { x += 1 }
        define entry B as { y += 10 }

        define from A to B
        define from B as { if(x >= 10) STOP else A }
      }

      "should " in {
        machine.start()
        assert(machine.x === 10)
        assert(machine.y === 100)
      }
    }

  }
}

// vim: set ts=2 sw=2 et:
