import java.net._
import java.io._
import scala.io._

object SimpleServer {
  // Simple server
  def main(args: Array[String]) {
    val server = new ServerSocket(8888)
    var a = 1
    while (a == 1) {
      val s = server.accept()
      val in = new BufferedSource(s.getInputStream()).getLines()
      println("Received " + in.next())

      //val out = new PrintStream(s.getOutputStream())
      //out.println(in.next())
      //out.flush()
      a = 2
      s.close()
    }

    val s = new Socket(InetAddress.getByName("localhost"), 8889)
    //lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())
    out.println("acc2")
    out.flush()
    println("Send: acc2")
    s.close()
  }
}
