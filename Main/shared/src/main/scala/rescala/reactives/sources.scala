package rescala.reactives

import rescala.core._


abstract class Source[T, S <: Struct](initialState: S#State[Pulse[T], S], name: REName) extends Base[T, S](initialState, name) {
  final def admit(value: T)(implicit ticket: AdmissionTicket[S]): Unit = admitPulse(Pulse.Value(value))
  final def admitPulse(value: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit = ticket.recordChange(new InitialChange[S] {
    override val r: Source[T, S] = Source.this
    override def v(before: Pulse[T]): ReevaluationResult[Pulse[T], S] = ReevaluationResult.Static(if (before == value) Pulse.NoChange else value, Set.empty)
  })

  final override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[Value, S] = ReevaluationResult.Static(Pulse.NoChange, indeps)
}

/**
  * Source events that can be imperatively updated
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class Evt[T, S <: Struct] private[rescala] (initialState: S#State[Pulse[T], S], name: REName) extends Source[T, S](initialState, name) with Event[T, S] {
  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S]): Unit = fire(value)
  def fire()(implicit fac: Engine[S], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S]): Unit = fac.transaction(this) {admit(value)(_)}
  override def disconnect()(implicit engine: Engine[S]): Unit = ()
}

/**
  * Companion object that allows external users to create new source events.
  */
object Evt {
  def apply[T, S <: Struct]()(implicit ticket: CreationTicket[S]): Evt[T, S] = ticket { t =>
    t.create[Pulse[T], Evt[T, S]](Set.empty, ValuePersistency.Event)(new Evt[T, S](_, ticket.rename))
  }
}

/**
  * Source signals that can be imperatively updated
  *
  * @param initialState of the signal
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
final class Var[A, S <: Struct] private[rescala] (initialState: S#State[Pulse[A], S], name: REName) extends Source[A, S](initialState, name) with Signal[A, S] {
  def update(value: A)(implicit fac: Engine[S]): Unit = set(value)
  def set(value: A)(implicit fac: Engine[S]): Unit = fac.transaction(this) {admit(value)(_)}

  def transform(f: A => A)(implicit fac: Engine[S]): Unit = fac.transaction(this) { t =>
    // minor TODO should f be evaluated only during reevaluation, given t.selfBefore(this) as parameter?
    admit(f(t.now(this)))(t)
  }

  def setEmpty()(implicit fac: Engine[S]): Unit = fac.transaction(this)(t => admitPulse(Pulse.empty)(t))

  override def disconnect()(implicit engine: Engine[S]): Unit = ()
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T: ReSerializable, S <: Struct](initval: T)(implicit ticket: CreationTicket[S]): Var[T, S] = fromChange(Pulse.Value(initval))
  def empty[T: ReSerializable, S <: Struct]()(implicit ticket: CreationTicket[S]): Var[T, S] = fromChange(Pulse.empty)
  private[this] def fromChange[T: ReSerializable, S <: Struct](change: Pulse.Change[T])(implicit ticket: CreationTicket[S]): Var[T, S] = ticket { t =>
    t.create[Pulse[T], Var[T, S]](Set.empty, ValuePersistency.InitializedSignal(change))(new Var[T, S](_, ticket.rename))
  }
}

