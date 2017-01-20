import java.net._
import java.io._
import scala.io._

object SimpleClient {
  // Simple client
  def main(args: Array[String]) {
    var s = new Socket(InetAddress.getByName("localhost"), 8888)
    //lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())
    out.println("acc1")
    out.flush()
    println("Send: acc1")
    s.close()
    
    val server = new ServerSocket(8889)
    var a = 1
    while (a == 1) {
      s = server.accept()
      val in = new BufferedSource(s.getInputStream()).getLines()
      println(in.next())
      s.close()
      a = 2
    }

  }
}
