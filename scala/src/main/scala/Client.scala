import scala.util.Marshal
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.Charset
import scala.util.parsing.input.{ StreamReader, Reader }
import java.nio.channels.{ServerSocketChannel, SocketChannel, Selector, SelectionKey}
import java.net._
import java.io._
import java.nio.channels.{Selector}
import java.util.HashSet
import scala.collection.JavaConversions
import Connect._
import N2Sprelude._
import General._
import Config._
import Location._

object Client {

  def mk_bcast(n: Int) : NVal = {
    val header = NVPair(NVStr("bcast"),Ax)
    val body = NVPair(NVInt(n), NVPair(NVStr("body" + n), Ax))
    val msg = NVPair(header, body)
    return msg
  }

  def main(args: Array[String]) {
    val conf_file: String = args(0)
    val loc: String = args(1)

    println(" + parsing config file: " + conf_file)
    val input  = StreamReader(new InputStreamReader(new FileInputStream(conf_file)))
    val output = parseAll(Config.getTerms(), input)
    val conf   = output.get
    println(" - done")

    val (groupname,ip,port,ext) = conf.get_info_from_loc(loc)
    val (lt,gt) = conf.get_lt_and_gt(loc)

    if (ext) {
      if (lt.length == 0) {
        println("found information of external location " + loc + ": [groupname:" + groupname + ",ip:" + ip + ",port:" + port + "]")

        val selector = Selector.open
        var sockets : List[(Location,SocketChannel)] = Nil

        println("connecting...")
        gt.foreach {
          destLoc => {
            println("connecting to " + destLoc)
            sockets ::= (destLoc,connectToLoc(loc,port,destLoc,selector))
          }
        }

        val keys = selector.keys
        var n : Int = 0
        var ok = true

        while (ok) {
          n += 1
          println("sending?")
          val l = readLine()

          if (l == "y") {
            val msg = mk_bcast(n)
            var waitingOnKeys = new HashSet(keys)
            val nowMillis = System.currentTimeMillis

            println("sending...")

            sockets.foreach {
              p => p match {
                case (destLoc,socket) => {
                  println("sending message to " + destLoc + ": " + msg)
                  writeObjTimer(loc,socket,msg)
                }
              }
            }

            while(!(waitingOnKeys.isEmpty)) {
              println("waiting for responses...")
              selector.select
              println("received something...")

              val jKeys = selector.selectedKeys
              val keys = JavaConversions.asScalaSet(jKeys).toList
              selector.selectedKeys.clear
              if (!(keys.isEmpty)) {
                keys.foreach {
	          key => {
                    waitingOnKeys.remove(key)
	            val sock: SocketChannel = key.attachment.asInstanceOf[SocketChannel]
                    try {
	              val msg: NVal = readObjTimer(loc,sock)
                      val time = System.currentTimeMillis - nowMillis
	              println("received message (@" + time + "ms): " + msg)
                    } catch {
                      case (e: ReadException) => {
                        println("closing socket that appears to have died: " + sock)
                        sock.close()
                      }
                    }
	          }
                }
              } else { println("error: selector was activated but no available key") }
            }
            println("received all answers")
          } else { ok = false }
        }

        println("stopped sending")
      } else { throw new Exception("found lower machines for location " + loc) }
    } else { throw new Exception("loction " + loc + " is not an external location") }
  }

}
