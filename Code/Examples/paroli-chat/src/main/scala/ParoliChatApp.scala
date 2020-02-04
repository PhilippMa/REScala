
import akka.cluster.Cluster
import rescala.extra.distributables._
import com.typesafe.config.ConfigFactory
import akka.actor._
import akka.actor.Props
import rescala.extra.distributables.PVertexList
import rescala.default._

import scala.tools.jline

class DistributionEngine extends Actor {
  override def receive: Receive = {
    case any => println(s"dummy implementation received $any")
  }
}

object DistributionEngine {
    def props(hostName: String) = Props(new DistributionEngine())
}

/**
  * Created by julian on 26.07.17.
  */
object ParoliChatApp {
  val console = new jline.console.ConsoleReader()

  def main(args: Array[String]): Unit = if (args.length >= 1) args(0) match {
    case "Alice" => startup("Alice", "2550")
    case "Bob" => startup("Bob", "2551")
    case "Charlie" => startup("Charlie", "2552")
  }
  else {
    val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + 2553).
      withFallback(ConfigFactory.load())
    val system = ActorSystem("ClusterSystem", config)
    val joinAddress = Cluster(system).selfAddress
    Cluster(system).join(joinAddress)

    /*val logActor =*/ system.actorOf(Props[MemberListener], "memberListener")
  }

  def startup(name: String, port: String): Unit = {
    val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
      withFallback(ConfigFactory.load())

    // Create an Akka system
    val system = ActorSystem("ClusterSystem", config)
    val engine: ActorRef = system.actorOf(DistributionEngine.props(name), name)

    run(name, engine)
  }

  def run(name: String, e: ActorRef): Unit = {
    //implicit val engine = e
    val history: PVertexList[String] = PVertexList(List())
    //history.publish("ChatHistory")

    // redraw interface every time the history changes:
    history.crdtSignal.observe { value => {
      val safeLine = console.getCursorBuffer().copy()
      drawInterface(name, value.iterator.toList)
      console.resetPromptLine(console.getPrompt(), safeLine.toString, safeLine.cursor)
    }
    }

    history.append(s"System: Hello $name!")

    while (true) {
      // print input prompt
      val msg: String = console.readLine(s"[$name]: ")

      // add messages to the end of the history
      history.append(s"[$name] $msg")
    }

    def drawInterface(name: String, log: List[String]): Unit = {
      // clear screen and make some room between current interface and older interfaces
      (0 to 50).foreach(_ => println())
      console.clearScreen()

      val terminalWidth = jline.TerminalFactory.get.getWidth
      val terminalHeight = jline.TerminalFactory.get.getHeight
      val header = (1 to terminalWidth).map(_ => "_").mkString
      val footer = (1 to terminalWidth).map(_ => "_").mkString

      var outputLines = 3

      // print header
      println(header)

      // print history
      log.foreach {
        s: String =>
          // slice output string into substrings of the same length as the terminal
          if (s.length > terminalWidth) {
            s.grouped(terminalWidth).foreach(slice => {
              println(slice)
              outputLines += 1
            })
          }
          else {
            println(s)
            outputLines += 1
          }
      }

      // print empty lines if the terminal window is bigger than the current history to place footer at the bottom of the screen
      if (terminalHeight > log.length) (1 to terminalHeight - outputLines).foreach(_ => println())
      println(footer)
    }
  }

}