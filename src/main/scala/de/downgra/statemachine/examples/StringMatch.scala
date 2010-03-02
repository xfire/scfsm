package de.downgra.statemachine.examples

import de.downgra.statemachine.Statemachine

object FooBarMatcher {

  private class FooBarMatcher(val input: String) extends Statemachine {
    val I, F, O1, O2, B, A, R = State
    startState is I

    var found = false   // marks a found value

    val chars = input.iterator // get iterator over the input string
    def nextChar = if(chars.hasNext) Some(chars.next.toUpper) else None

    def forceNext(ch: Char, state: Statemachine) = nextChar match {
      case Some(c) if(c == ch) => state
      case _ => STOP
    }

    define exit R as { if(!chars.hasNext) found = true }

    transition from I  as { forceNext('F', F) }
    transition from F  as { forceNext('O', O1) }
    transition from O1 as { forceNext('O', O2) }
    transition from O2 as { forceNext('B', B) }
    transition from B  as { forceNext('A', A) }
    transition from A  as { forceNext('R', R) }
    transition from R  to STOP
  }

  def apply(input: String): Boolean = {
    val m = new FooBarMatcher(input)
    m.start()
    return m.found
  }
}

object StringMatch {
  def main(args: Array[String]) = {
    if(args.length > 0) {
      FooBarMatcher(args(0)) match {
        case true  => println(args(0) + " matched.")
        case false => println(args(0) + " do not match!")
      }
    } else
      println("Syntax: StringMatch <some string>")
  }
}

// vim: set ts=2 sw=2 et:
