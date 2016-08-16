package tests.rescala


//These 3 are for JUnitRunner

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.Signals





class SignalsAndVarsTestSuite extends RETests {




  allEngines("handlerIsCalledWhenChangeOccurs"){ engine => import engine._

    var test = 0
    val v1 = Var(1)
    val v2 = Var(2)

    val s1 = Signals.lift(v1, v2) { _ + _ }
    s1.changed += { (_) => test += 1 }

    assert(s1.now == 3)
    assert(test == 0)

    v2.set(3)
    assert(s1.now == 4)
    assert(test == 1)

    v2.set(3)
    assert(s1.now == 4)
    assert(test == 1)

  }


}
