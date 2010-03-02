package de.downgra.statemachine

abstract class Statemachine {

  object STOP extends Statemachine {
    override final def onTransition = {}
  }

  private var startingState: Option[Statemachine] = None
  private var running = false
  private var entryActions: Map[Statemachine, Function0[Unit]] = Map.empty
  private var exitActions: Map[Statemachine, Function0[Unit]] = Map.empty
  private var conditions: Map[Statemachine, Function0[Statemachine]] = Map.empty

  protected def onTransition: Unit = start

  protected def State: Statemachine = new Statemachine {
    override final def onTransition = {}
  }

  // some building sugar
  protected val define: this.type = this
  protected val transition: this.type = this

  protected trait FunctionSetter[T] {
    def as(fn: => T)
  }

  protected trait StateSetter {
    def to(toState: Statemachine)
  }

  protected def entry(state: Statemachine) = new FunctionSetter[Unit] {
    def as(fn: => Unit) = entryActions += (state -> fn _)
  }

  protected def exit(state: Statemachine) = new FunctionSetter[Unit] {
    def as(fn: => Unit) = exitActions += (state -> fn _)
  }

  protected def from(state: Statemachine) = new FunctionSetter[Statemachine] with StateSetter {
    def as(fn: => Statemachine) = conditions += (state -> fn _)
    def to(toState: Statemachine) = conditions += (state -> { () => toState })
  }

  protected def startState() = new {
    def is(state: Statemachine) = startingState = Option(state)
  }

  // public interface
  def isRunning = running

  def start(): Unit = start(startingState)
  def start(state: Option[Statemachine]): Unit = start(state.getOrElse(STOP))
  def start(state: Statemachine): Unit = {
    // TODO: throw exception
    if(running) return
    var next = state
    running = true
    while(next != STOP) {
      next.onTransition
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
