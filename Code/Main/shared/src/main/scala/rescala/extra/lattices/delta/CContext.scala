package rescala.extra.lattices.delta

import cats.collections._
import cats.implicits._
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

trait CContext[A] {
  def contains(cc: A, d: Dot): Boolean

  def fromSet(dots: Set[Dot]): A

  def toSet(cc: A): Set[Dot]

  def union[B: CContext](left: A, right: B): A

  def max(cc: A, replicaID: String): Option[Dot]

  def nextDot(cc: A, replicaID: String): Dot = max(cc, replicaID) match {
    case Some(dot) => dot.next
    case None => Dot(replicaID, 0)
  }

  def empty: A

  def convert[B: CContext](cc: A): B = CContext[B].fromSet(toSet(cc))
}

object CContext {
  def apply[A](implicit cc: CContext[A]): CContext[A] = cc

  type SetCContext = Set[Dot]
  implicit def SetCContext: CContext[Set[Dot]] = new CContext[Set[Dot]] {
    override def contains(cc: Set[Dot], d: Dot): Boolean = cc.contains(d)

    override def fromSet(dots: Set[Dot]): Set[Dot] = dots

    override def toSet(cc: Set[Dot]): Set[Dot] = cc

    override def union[B: CContext](left: Set[Dot], right: B): Set[Dot] = left union CContext[B].toSet(right)

    override def max(cc: Set[Dot], replicaID: String): Option[Dot] =
      cc.filter(_.replicaID == replicaID).maxByOption(_.counter)

    override def empty: Set[Dot] = Set.empty[Dot]
  }

  type DietMapCContext = Map[String, Diet[Int]]
  implicit def DietMapCContext: CContext[Map[String, Diet[Int]]] = new CContext[Map[String, Diet[Int]]] {
    override def contains(cc: Map[String, Diet[Int]], d: Dot): Boolean = d match {
      case Dot(replicaID, counter) =>
        cc.getOrElse(replicaID, Diet.empty[Int]).contains(counter)
    }

    private def addDot(cc: Map[String, Diet[Int]], dot: Dot): Map[String, Diet[Int]] = dot match {
      case Dot(replicaID, counter) =>
        val oldDiet = cc.getOrElse(replicaID, Diet.empty[Int])
        cc + (replicaID -> (oldDiet + counter))
    }

    override def fromSet(dots: Set[Dot]): Map[String, Diet[Int]] = dots.foldLeft(Map[String, Diet[Int]]())(addDot)

    override def toSet(cc: Map[String, Diet[Int]]): Set[Dot] =
      for (replicaID <- cc.keySet;
           counter <- cc.getOrElse(replicaID, Diet.empty[Int]).toList)
        yield Dot(replicaID, counter)

    override def union[B: CContext](left: Map[String, Diet[Int]], right: B): Map[String, Diet[Int]] =
      CContext[B].toSet(right).foldLeft(left)(addDot)

    override def max(cc: Map[String, Diet[Int]], replicaID: String): Option[Dot] =
      cc.getOrElse(replicaID, Diet.empty[Int]).max.map(Dot(replicaID, _))

    override def empty: Map[String, Diet[Int]] = Map.empty[String, Diet[Int]]
  }

  implicit val DietCodec: JsonValueCodec[Diet[Int]] = new JsonValueCodec[Diet[Int]] {
    override def decodeValue(in: JsonReader, default: Diet[Int]): Diet[Int] = {
      var result = Diet.empty[Int]

      in.isNextToken('"')

      while (!in.isNextToken('"')) {
        in.rollbackToken()

        in.nextToken() match {
          case 'R' =>
            while (!in.isNextToken('(')) {}
            val lower = in.readInt()
            in.isNextToken(',')
            val upper = in.readInt()
            result = result + cats.collections.Range(lower, upper)
            in.isNextToken(')')
          case _ =>
        }
      }

      result
    }

    override def encodeValue(x: Diet[Int], out: JsonWriter): Unit = {
      out.writeVal(x.toString)
    }

    override def nullValue: Diet[Int] = null
  }

  implicit val DietMapCContextCodec: JsonValueCodec[DietMapCContext] = JsonCodecMaker.make
}
