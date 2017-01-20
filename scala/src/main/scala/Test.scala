import collection.mutable.HashMap
import process_rep1._
import process_rep2._
import process_ldr1._
import process_ldr2._
import process_acc1._
import process_acc2._
import process_acc3._
import process_loc1._
import process_loc2._
import process_loc3._
import process_loc4._
import Run._
import General._
import N2Sprelude._
import DelayedMessage._

object Test {

  def drop_pings(msg: Any) : Boolean = {
    msg match {
      case (hdr: Any, body: Any) =>
        hdr match {
          case ("aneris_pax_v1",p) =>
            p match {
              case ("ping",Axiom()) => true
              case ("pong",Axiom()) => true
              case ("react",Axiom()) => true
              case _ => false
            }
          case _ => false
        }
      case _ => throw new Exception("message does not have the right format")
    }
  }

  def runOnMessages(system: HashMap[String,Any], msgs: List[DelayedMessage])
      : List[(String,Long,Long)] = {
    msgs match {
      case Nil => {
        println("no more messages to send")
        Nil
      }
      case dmsg :: dmsgs => {
        println("handling: " + dmsg)
        val (delay,loc,msg) = dmsg.destruct
        if (system.contains(loc)) {
          if (drop_pings(msg)) {
            println("dropping ping or react message")
            runOnMessages(system,dmsgs)
          } else {
            val proc = system(loc)
	    val (newproc,newmsgs,t1,t2) = runProcessOnMessageTimer(loc,proc,msg)
            //println("appending new messages: " + newmsgs)
            (loc,t1,t2) :: runOnMessages(system += loc -> newproc,dmsgs ++ newmsgs)
          }
        } else {
          println("location " + loc + " is not part of the system, dropping message")
          runOnMessages(system,dmsgs)
        }
      }
    }
  }

  def runInstances(instance: Int, m: Int, system: HashMap[String,Any]) : Unit = {
    if (instance <= m) {
      val msg=mkBcastMsg(instance)
      val dmsg=new DelayedMessage((0,("rep1",msg)))

      val times = runOnMessages(system,dmsg :: Nil)
      println("--------------------------------------------")
      println("times: " + times)
      println("--------------------------------------------")
      runInstances(instance + 1, m, system)
    }
  }

  def main(args : Array[String]) = {
    var system = new HashMap[String,Any]()
    system += "rep1" -> processrep1
    system += "rep2" -> processrep2
    system += "ldr1" -> processldr1
    system += "ldr2" -> processldr2
    system += "acc1" -> processacc1
    system += "acc2" -> processacc2
    system += "acc3" -> processacc3
    system += "loc1" -> processloc1
    system += "loc2" -> processloc2
    system += "loc3" -> processloc3
    system += "loc4" -> processloc4

    runInstances(1,10,system)
  }

}
