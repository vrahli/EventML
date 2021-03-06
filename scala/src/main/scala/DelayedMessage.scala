import N2Sprelude._

object DelayedMessage {

  /** Delayed messages are what are generated by processes */
  class DelayedMessage(a: Any) {
    private def info:(Int,String,NVal) = destructDelayedMessage(a)
    def delay: Int = info._1
    def recipient: String = info._2
    def msg: NVal = info._3

    override def equals(o: Any) : Boolean = o match {
      case that: DelayedMessage => {
	that.delay.equals(delay) &&
	that.recipient.equals(recipient) &&
	that.msg.equals(msg)
      }
      case _ => false
    }

    def eqDmsg(dm: DelayedMessage) : Boolean = {
      dm.delay.equals(delay) &&
      dm.msg.equals(msg)
    }

    private def recipient2string(id:Any) : String = {
      id match {
        case (x: String) => x
        case NVStr(x) => x
      }
    }

    private def delay2int(delay:Any) : Int = {
      delay match {
        case (x: Int) => x
        case NVInt(x) => x
      }
    }

    private def destructDirectedMessage(m:Any) : (String,NVal) = {
      m match {
	case NVPair(id,msg) => (recipient2string(id),cast2NVal(msg))
	case (id,msg) => (recipient2string(id),cast2NVal(msg))
	case _ => throw new Exception("get_msg_of_message:wrong format")
      }
    }

    private def destructDelayedMessage(m:Any) : (Int,String,NVal) = {
      m match {
        case NVPair(delay,rest) => {
          destructDirectedMessage(rest) match {
            case (id,msg) => (delay2int(delay),id,msg)
          }
        }
        case (delay,rest) => {
          destructDirectedMessage(rest) match {
            case (id,msg) => (delay2int(delay),id,msg)
          }
        }
        case _ => throw new Exception("get_msg_of_message:wrong format")
      }
    }

    def hereAndNow(loc:String) : Boolean = { loc == recipient && delay == 0 }

    def destruct() : (Int,String,Any) = info

    override def toString() : String = recipient + "(" + delay + "):" + msg
  }

  def mk_delayed_messages(lst : Any) : List[DelayedMessage] = {
    lst match {
      case NVPair(msg,msgs) => new DelayedMessage(msg) :: mk_delayed_messages(msgs)
      case (msg,msgs) => new DelayedMessage(msg) :: mk_delayed_messages(msgs)
      case (x : Axiom) => Nil
      case _ => throw new Exception("list of messages is not a list")
    }
  }

  def main(args: Array[String]) { () }

}
