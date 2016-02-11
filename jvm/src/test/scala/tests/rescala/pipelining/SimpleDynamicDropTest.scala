package tests.rescala.pipelining

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Signals
import rescala.Var
import rescala.graph.Buffer
import rescala.pipelining.PipelineEngine
import rescala.pipelining.PipeliningTurn
import tests.rescala.pipelining.PipelineTestUtils._
import rescala.turns.Ticket
import rescala.turns.Turn
import rescala.pipelining.Pipeline

class SimpleDynamicDropTest extends AssertionsForJUnit with MockitoSugar {

  implicit val engine = new PipelineEngine
  val timeToAllowOtherTurnToCreateFrames = 100

  val source1 = Var(0)
  val source2 = Var(2)
  var numEvaluated = 0;
  val dynDep = Signals.dynamic()(implicit t => {
    println(s"BEGIN evaluate $t")
    numEvaluated += 1;
    Thread.sleep(timeToAllowOtherTurnToCreateFrames)
    val source1Val = source1(t)
    println(s"Read val $source1Val")
    val newValue = if (source1Val % 2 == 0) {
      source2(t)
    } else {
      0
    }
    println(s"END evaluate $t with $newValue")
    newValue
  })(Ticket(Right(engine)))

  val dynDepTracker = new ValueTracker(dynDep)

  dynDepTracker.reset()
  numEvaluated = 0

  @Test
  def serialDropTest() = {
    println("=====")
    assert(dynDep.now == 2)
    source1.set(1)
    assert(dynDep.now == 0)
    assert(numEvaluated == 1)
    numEvaluated = 0
    PipelineTestUtils.readLatestValue {implicit turn => 
      assert(source2.outgoing.get == Set())
      assert(source1.outgoing.get == Set(dynDep))
      assert(dynDep.incoming.get == Set(source1))
    }
    source2.set(93)
    assert(dynDep.now == 0)
    assert(numEvaluated == 0)
    assert(dynDepTracker.values == List(0))
  }

  @Test
  def parallelAddAndRemove() = {
    var removeBeforeAdd = false
    var addBeforeRemove = false

    // Hope that thread scheduling is not deterministic enough to cover both cases with not too many tries
    while (!removeBeforeAdd || !addBeforeRemove) {

      source1.set(0)
      source2.set(2)

      PipelineTestUtils.readLatestValue { implicit turn =>
        assert(source1.outgoing.get == Set(dynDep))
        assert(source2.outgoing.get == Set(dynDep))
        assert(dynDep.incoming.get == Set(source1, source2))
      }

      numEvaluated = 0
      dynDepTracker.reset()

      println()
      println()

      val threadRemoveDep = createThread(source1.set(1))
      val threadAddDep = createThread(source1.set(2))

      if (addBeforeRemove) {
        threadRemoveDep.start()
        threadAddDep.start()
      } else {
        threadAddDep.start()
        threadRemoveDep.start()
      }

      threadRemoveDep.join()
      threadAddDep.join()

      PipelineTestUtils.readLatestValue { implicit turn =>
        dynDep.now match {
          case 0 =>
            println("==> Add before remove")
            assert(dynDep.incoming.get == Set(source1))
            assert(dynDep.now == 0)
            assert(source1.outgoing.get == Set(dynDep))
            assert(source2.outgoing.get == Set())
            assert(numEvaluated == 2)
            assert(dynDepTracker.values == List(0)) // Only one change because the change 2 -> 2 is not observed
            addBeforeRemove = true
          case 2 =>
            println("==> Remove before add")
            assert(dynDep.now == 2)
            assert(dynDep.incoming.get == Set(source1, source2))
            assert(source1.outgoing.get == Set(dynDep))
            assert(source2.outgoing.get == Set(dynDep))
            removeBeforeAdd = true
            assert(numEvaluated == 2)
            assert(dynDepTracker.values == List(0, 2))
          case _ => fail("Invalid result value")
        }
      }

    }
  }

  @Test //(timeout = 10000)
  def parallelRemoveAndUpdateFromRemovedDep() = {
    var removeBeforeUpdateSuspicious = false
    var updateBeforeRemove = false

    // Again trust the nondeterminicness of execution
    while (!removeBeforeUpdateSuspicious || !updateBeforeRemove) {

      println
      println

      val removeDepThread = createThread(source1.set(1))
      val updateDepThread = createThread(source2.set(100))

      numEvaluated = 0
      dynDepTracker.reset()

      // Lets the the scheduler are bit to decide which thread starts creating frames
      if (updateBeforeRemove) {
        removeDepThread.start()
        updateDepThread.start()
      } else {
        updateDepThread.start()
        removeDepThread.start()
      }

      removeDepThread.join()
      updateDepThread.join()

      implicit val dummyTurn = engine.makeTurn
      engine.addTurn(dummyTurn)

      assert(Pipeline.pipelineFor(dynDep).getPipelineFrames().isEmpty)

      // in scheduling case, there should no dependency to source2
      assert(dynDep.incoming.get == Set(source1))
      assert(source2.outgoing.get == Set())
      assert(source1.outgoing.get == Set(dynDep))

      engine.turnCompleted(dummyTurn)

      // But scheduling defines seen values
      numEvaluated match {
        case 1 =>

          //    case 0 =>
          //     println("==> Remove before Update no suspicious frame")
          // I dont force to get this result because it is equal to sequential execution
          // and hard to get scheduled in parallel
          //   case 1 =>
          println("==> Remove before Update suspicious frame")
          removeBeforeUpdateSuspicious = true
          //   case x =>
          //    fail(s"Illegal number of suspicious and not evaluated frames $x")
          // }
          assert(dynDepTracker.values == List(0))
        case 2 =>
          println("==> Update before remove")
          updateBeforeRemove = true
          assert(dynDepTracker.values == List(100, 0))
        case _ =>
          fail("Invalid number of evaluations")
      }

      // Reset graph
      source1.set(0)
      source2.set(2)
    }

  }

}