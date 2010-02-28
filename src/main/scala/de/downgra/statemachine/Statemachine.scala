package de.downgra.statemachine

abstract class Statemachine {

  protected val START: Option[Statemachine] = None
  object STOP extends Statemachine

  protected def State: Statemachine = new Statemachine {
    override def transition = {}
  }

  protected def transition: Unit = start
  private var running = false

  private var entryActions: Map[Statemachine, Function0[Unit]] = Map.empty
  private var exitActions: Map[Statemachine, Function0[Unit]] = Map.empty
  private var conditions: Map[Statemachine, Function0[Statemachine]] = Map.empty

  abstract class MappingSetter[T] {
    def as(fn: => T)
  }

  val define = this

  def onEntry(state: Statemachine) = new MappingSetter[Unit] {
    def as(fn: => Unit) = entryActions += (state -> fn _)
  }

  def onExit(state: Statemachine) = new MappingSetter[Unit] {
    def as(fn: => Unit) = exitActions += (state -> fn _)
  }
  def from(state: Statemachine) = new MappingSetter[Statemachine] {
    def as(fn: => Statemachine) = conditions += (state -> fn _)
  }

  def isRunning = running

  def start: Unit = start(START)
  def start(state: Option[Statemachine]): Unit = start(state.getOrElse(STOP))
  def start(state: Statemachine): Unit = {
    // TODO: throw exception
    if(running) return
    var next = state
    running = true
    while(next != STOP) {
      next.transition
      entryActions.get(next) match {
        case Some(action) => action()
        case None =>
      }
      conditions.get(next) match {
        case Some(cond) => 
          exitActions.get(next) match {
            case Some(action) => action()
            case None =>
          }
          next = cond()
        case None => next = STOP
      }
    }
    running = false
  }
}

object Test {
  def main(args: Array[String]) {

    val s = new Statemachine {
      object Context {
        private var _counter = 0

        def counter = _counter
        def next = _counter = _counter + 1
      }

      val A, B, C = State

      define onEntry A as { println("entering A"); Context next }
      define onEntry B as { println("entering B"); Context next }
      define onEntry C as { println("entering C"); Context next }

      define onExit A as { println("exiting A") }
      define onExit B as { println("exiting B") }
      define onExit C as { println("exiting C") }


      define from A as { B }
      define from B as { C }
      define from C as { if (Context.counter > 10) STOP else A }
    }

    println(">>> s")
    s start s.A


    val s2 = new Statemachine {
      val A, B = State

      define onEntry A as {
        new Statemachine {
          val X, Y = State
          override val START = Some(X)

          define onEntry X as { println("  entering X") }
          define onEntry Y as { println("  entering Y") }

          define from X as { Y }
          define from Y as { STOP }
        } start
      }

      define from A as { B }
      define from B as { STOP }
    }

    println(">>> s2")
    s2 start s2.A

    val s3 = new Statemachine {
      val A = State
      val B = new Statemachine {
        val X, Y = State
        override val START = Some(X)

        define onEntry X as { println("  entering X") }
        define onEntry Y as { println("  entering Y") }

        define from X as { Y }
        define from Y as { STOP }
      }

      define onEntry A as { println("entering A") }
      define onEntry B as { println("entering B") }

      define from A as { B }
      define from B as { STOP }
    }

    println(">>> s3")
    s3 start s3.A
  }
}

