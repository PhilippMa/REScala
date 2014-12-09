package rescala.graph

import rescala.turns.Turn
import rescala.turns.instances.{AbstractTurn, Pessimistic, Simple}

import scala.util.DynamicVariable

object Engines {

  implicit def default: Engine = pessimistic

  implicit val pessimistic: Engine = new Impl(new Pessimistic())
  implicit val synchronized: Engine = new Impl(new Simple()) {
    override def startNew[T](f: Turn => T): T = synchronized(super.startNew(f))
  }
  implicit val unmanaged: Engine = new Impl(new Simple())

  val currentTurn: DynamicVariable[Option[AbstractTurn]] = new DynamicVariable[Option[AbstractTurn]](None)

  class Impl(makeTurn: => AbstractTurn) extends Engine {

    override def startKeep[T](f: (Turn) => T): T = currentTurn.value match {
      case None => startNew(f)
      case Some(turn) => f(turn)
    }

    /** goes through the whole turn lifecycle
      * - create a new turn and put it on the stack
      * - run the admission phase
      *   - this is user defined and sets source values, needs investigation of reactive creation, and read then act stuff
      * - run the locking phase
      *   - to give the turn a chance to do something before the propagation starts when it is known which reactives will change
      * - run the propagation phase
      *   - calculate the actual new value of the reactive graph
      * - run the commit phase
      *   - do cleanups on the reactives, make values permanent and so on, the turn is still valid during this phase
      * - run the observer phase
      *   - this may have side effects as the turn is guaranteed to be finished (no rollbacks). this should still keep locks to run things in order.
      * - run the release phase
      *   - this must is aways run, even in the case that something above fails. it should do cleanup and free any locks to avoid starvation.
      * - run the party! phase
      *   - not yet implemented
      * */
    override def startNew[T](admissionPhase: Turn => T): T = {
      implicit class sequentialLeftResult(result: T) {def ~<(sideEffects_! : Unit): T = result }
      val turn = makeTurn
      try {
        currentTurn.withValue(Some(turn)) {
          admissionPhase(turn) ~< {
            turn.lockingPhase()
            turn.propagationPhase()
            turn.commitPhase()
          }
        } ~< turn.observerPhase()
      }
      finally {
        turn.realeasePhase()
      }
    }

  }

}