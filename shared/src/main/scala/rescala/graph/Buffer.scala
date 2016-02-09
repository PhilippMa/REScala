package rescala.graph

import rescala.graph.Buffer.CommitStrategy
import rescala.turns.Turn

import scala.language.{higherKinds, implicitConversions}

trait Committable {
  def commit(implicit turn: Turn[_]): Unit
  def release(implicit turn: Turn[_]): Unit
}

object Buffer {
  type CommitStrategy[A] = (A, A) => A
  def commitAsIs[A](base: A, cur: A): A = cur
  def transactionLocal[A](base: A, cur: A) = base
  def keepPulse[P](base: Pulse[P], cur: Pulse[P]) = cur.keep
}

trait Spores {
  type TBuffer[A] <: Buffer[A]
  type TLock
  type Bud[P] <: TraitBud[P]
  def bud[P](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true): Bud[P]
  trait TraitBud[P] {
    def buffer[A, S <: Spores](default: A, commitStrategy: CommitStrategy[A]): TBuffer[A]
    def lock(): TLock
    val pulses: TBuffer[Pulse[P]]
  }
}

object SimpleSpores extends Spores {
  override type TBuffer[A] = SimpleBuffer[A]
  override type TLock = Unit
  override type Bud[P] = SimpleBud[P]

  def bud[P](initialValue: Pulse[P] = Pulse.none, transient: Boolean = true): Bud[P] = new SimpleBud[P](new SimpleBuffer[Pulse[P]](initialValue, if (transient) Buffer.transactionLocal else Buffer.keepPulse))

  class SimpleBud[P](override val pulses: SimpleBuffer[Pulse[P]]) extends TraitBud[P] {
    override def buffer[A, S <: Spores](default: A, commitStrategy: CommitStrategy[A]): SimpleBuffer[A] = new SimpleBuffer[A](default, commitStrategy)
    override def lock(): Unit = Unit
  }
}

trait Buffer[A] {
  def transform(f: (A) => A)(implicit turn: Turn[_]): A
  def set(value: A)(implicit turn: Turn[_]): Unit
  def base(implicit turn: Turn[_]): A
  def get(implicit turn: Turn[_]): A
}

final class SimpleBuffer[A](initialValue: A, initialStrategy: (A, A) => A) extends Buffer[A] with Committable {

  var current: A = initialValue
  private var update: Option[A] = None
  private var owner: Turn[_] = null
  private val commitStrategy: (A, A) => A = initialStrategy

  override def transform(f: (A) => A)(implicit turn: Turn[_]): A = synchronized {
    val value = f(get)
    set(value)
    value
  }

  override def set(value: A)(implicit turn: Turn[_]): Unit = synchronized {
    assert(owner == null || owner == turn, s"buffer owned by $owner written by $turn")
    update = Some(value)
    owner = turn
    turn.schedule(this)
  }

  override def base(implicit turn: Turn[_]): A = synchronized(current)

  override def get(implicit turn: Turn[_]): A = synchronized {if (turn eq owner) update.getOrElse(current) else current}

  override def release(implicit turn: Turn[_]): Unit = synchronized {
    update = None
    owner = null
  }

  override def commit(implicit turn: Turn[_]): Unit = synchronized {
    current = commitStrategy(current, get)
    release(turn)
  }
}
