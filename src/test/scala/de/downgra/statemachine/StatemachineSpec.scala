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
        machine start
      }

    }

    "it contains a transition from STOP to STOP" should {
      val machine = new Statemachine {
        define from STOP as { STOP }
      }

      "immediately returns from start" in {
        machine start
      }
    }

    // "it contains three simple transitions with a last StopState" should {
      // val machine = new Statemachine {
        // var path = ""
        // from (A to B        ) { path += "AB" }
        // from (B to C        ) { path += "BC" }
        // from (C to StopState) { path += "C;" }
      // }

      // "go through all 3 transitions" in {
        // machine start A
        // assert(machine.path === "ABBCC;")
      // }
    // }

    // "it have a transition with a condition" should {
      // val machine = new Statemachine {
        // var x = 0

        // from (A to B) { x += 1 }
        // from (B to A) {}

        // from (A to StopState) {} when(x > 10)

      // }
    // }

  }
}

// vim: set ts=2 sw=2 et:
