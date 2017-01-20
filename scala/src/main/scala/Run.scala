import scala.util.parsing.input.{ StreamReader, Reader }
import java.io._
import java.nio.channels.{Selector}
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import General._
import Config._
import Connect._
import N2Sprelude._
import DelayedMessage._

object Run {

  val TIMER = true

  def runProcessOnMessage(loc: String, process: Any, msg: NVal) : (Any,List[DelayedMessage]) = {
 	process match {
 	  case Inl(p) => {
 	    printEmlLocDbg(loc,"EX","process still running")
 	    nuprl_apply(p,msg) match {
 	      case Pair(newp, msgs_out) => { return (newp,mk_delayed_messages(msgs_out)) }
 	      case (newp, msgs_out) => { return (newp,mk_delayed_messages(msgs_out)) }
              case _ => throw new Exception ("process returned unexpected result")
 	    }
          }
 	  case Inr(_) => {
            println("process has halted")
            return (process,Nil)
          }
 	  case (x:Any) => throw new Exception("not a process: " + x)
 	} // end of process
  }

  def runProcessOnMessageTimer(loc: String, process: Any, msg: NVal)
  : (Any,List[DelayedMessage],Long,Long) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      val (newproc,msgs) = runProcessOnMessage(loc,process,msg)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"EX","[runProcess] time: " + tmicro + "microseconds (" + tmilli + "ms)")
      return (newproc,msgs,tmilli,tmicro)
    } else {
      val (newproc,msgs) = runProcessOnMessage(loc,process,msg)
      return (newproc,msgs,0,0)
    }
  }

  class Executor(
    received: BlockingQueue[NVal],
    tosend: BlockingQueue[DelayedMessage],
    selector: Selector,
    process: Any,
    loc: String
  ) extends Thread {

    var proc:Any = process

    def loop() : Unit = {
      while (true) {
        printEmlLoc(loc,"EX","waiting...")
        val msg:NVal = received.take
        printEmlLoc(loc,"EX","received")
        printEmlLocDbg(loc,"EX","applying message " + msg)
        val (newprocess:Any,msgs_out:List[DelayedMessage],_,_) =
          runProcessOnMessageTimer(loc,proc,msg)
	var wkup: Boolean = false
        msgs_out.foreach {
          m => {
	    if (m.hereAndNow(loc)) {
	      printEmlLocDbg(loc,"EX","adding local message to input queue")
              received.add(m.msg)
	    } else {
	      printEmlLocDbg(loc,"EX","adding message to output queue")
	      wkup = true
              tosend.add(m)
	    }
          }
        }
        if (wkup) { selector.wakeup() }
        proc = newprocess
      }
    }

    override def run() = { loop() }
  }

  def runprocess(conf_file: String, loc: String, process: Any) {
    println(" + parsing config file: " + conf_file)
    val input  = StreamReader(new InputStreamReader(new FileInputStream(conf_file)))
    val output = parseAll(Config.getTerms(), input)
    val conf   = output.get
    println(" - done")

    val received : BlockingQueue[NVal] = new LinkedBlockingQueue[NVal]
    val tosend : BlockingQueue[DelayedMessage] = new LinkedBlockingQueue[DelayedMessage]

    val (groupname,ip,port,ext) = conf.get_info_from_loc(loc)
    val (lt,gt) = conf.get_lt_and_gt(loc)
    val selector = Selector.open
    val mailbox = new Mailbox(loc,ip,port,lt,gt,received,tosend,selector)
    val executor = new Executor(received,tosend,selector,process,loc)
    printEmlLoc(loc,"starting mailbox")
    mailbox.start
    printEmlLoc(loc,"starting executor")
    executor.start
  }

// TODO: run mailbox, start an executor

  def main(args: Array[String]) { ()  }

}
