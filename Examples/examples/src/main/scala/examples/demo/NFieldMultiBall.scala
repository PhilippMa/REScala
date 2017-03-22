package examples.demo

import examples.demo.LFullyModularBall.BouncingBall
import examples.demo.MPlayingFieldBall.PlayingField
import examples.demo.ui._
import rescala._

/**
  * Because we implemented the collision events inside a method, with each
  * derived from parameters to the method, this method implements a blueprint
  * pattern of several Signal, that can be instantiated multiple times for
  * different input parameters. We exploit this here by wrapping our ball
  * instantiation code into a closure and simply executing it twice,
  * resulting in two bally bouncing around the PlayingField.
  */
object NFieldMultiBall extends Main {
  val shapes = Var[List[Shape]](List.empty)
  val panel = new ShapesPanel(shapes)

  val fieldWidth = panel.width.map(_ - 25)
  val fieldHeight = panel.height.map(_ - 25)

  val playingField = new PlayingField(fieldWidth, fieldHeight)
  shapes.transform(playingField.shape :: _)

  def makeBall(initVx: Double, initVy: Double) = {
    val bouncingBall = new BouncingBall(initVx, initVy, Var(50), panel.Mouse.middleButton.pressed)
    shapes.transform(bouncingBall.shape :: _)

    val fieldCollisions = playingField.colliders(bouncingBall.shape)
    bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
    bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)
  }
  makeBall(200d, 150d)
  makeBall(-200d, 100d)
}