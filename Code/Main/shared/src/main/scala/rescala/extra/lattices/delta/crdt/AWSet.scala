package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta._

object AWSetCRDT {
  type State[E, C] = Causal[DotMap[E, DotSet], C]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): DeltaCRDT[State[E, C]] =
    DeltaCRDT.empty[State[E, C]](antiEntropy)

  def elements[E, C: CContext]: DeltaQuery[State[E, C], Set[E]] = {
    case Causal(dm, _) => dm.keySet
  }

  def add[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (replicaID, Causal(dm, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)
      val v = dm.getOrElse(e, DotSet.empty)

      Causal(
        DotMap[E, DotSet].empty.updated(e, Set(nextDot)),
        CContext[C].fromSet(v + nextDot)
      )
  }

  def remove[E, C: CContext](e: E): DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      val v = dm.getOrElse(e, DotSet.empty)

      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(v)
      )
  }

  def clear[E, C: CContext]: DeltaMutator[State[E, C]] = {
    case (_, Causal(dm, _)) =>
      Causal(
        DotMap[E, DotSet].empty,
        CContext[C].fromSet(DotMap[E, DotSet].dots(dm))
      )
  }
}

class AWSet[E, C: CContext](crdt: DeltaCRDT[AWSetCRDT.State[E, C]]) {
  def elements: Set[E] = crdt.query(AWSetCRDT.elements)

  def add(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.add(e)))

  def remove(e: E): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.remove(e)))

  def clear(): AWSet[E, C] = new AWSet(crdt.mutate(AWSetCRDT.clear))

  def state: AWSetCRDT.State[E, C] = crdt.state

  def processReceivedDeltas(): AWSet[E, C] = new AWSet(crdt.processReceivedDeltas())
}

object AWSet {
  type State[E, C] = AWSetCRDT.State[E, C]
  type Embedded[E] = DotMap[E, DotSet]

  def apply[E, C: CContext](antiEntropy: AntiEntropy[State[E, C]]): AWSet[E, C] =
    new AWSet(AWSetCRDT[E, C](antiEntropy))

  implicit def AWSetStateCodec[E: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[E, Set[Dot]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
