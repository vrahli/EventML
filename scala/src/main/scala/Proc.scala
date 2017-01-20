object Proc {

  /* type of messages.
   */
  class Message[M](val hdr : List[String], val content : M) {}

  class Location(val host : String, val port : Int) {}

  class DirectedMessage[M](val loc : Location, m : Message[M]) {}

  /* A process should have a react function.
   * A process takes a message as an input and returns a list of values.
   */
  abstract class Proc[M,T]() {
    val running : Bool = true
    def output(m : Message[M]) : List[T] = List()
    def react(m : Message[M]) : List[T] = {
      if running { output(m) } else { List() }
  }

  /* State machines.
   */
  case class State[M,S,I](init : S, update : I => S => S, input : Proc[M,I]) extends Proc[M,S]() {
    var state : List[S] = List(init)

    override def output(m : Message[M]) : List[S] = {
      val l = input.react(m)
      if (running) {
        if (!(null l)) {
          state = state.flatMap((s) => l.map((i) => update(i)(s)))
        }
        return state
      }
    }
  }

  /* Runs two processes in parallel.
   */
  case class Parallel[M,T](p1 : Proc[M,T], p2 : Proc[M,T]) extends Proc[M,T] {
    override def output(m : Message[M]) : List[T] = {
      if (!p1.running && !p2.running) { running = false }
      return (p1.react(m) ++ p2.react(m))
    }
  }

  /* Checks that the header of the message is hdr, and if it is
   * extracts the content from the message
   */
  case class Base[M](hdr : List[String]) extends Proc[M,M] {
    override def output(m : Message[M]) : List[M] = {
      return (if (m.hdr == hdr) { List(m.content) } else { List() })
    }
  }

  /** Process that simply outputs its argument
    */
  case class Output[M,T](v : T) extends Proc[M,T] {
    override def output(m : Message[M]) : List[T] = {
      if (running) {
        running = false
        return (v :: Nil)
      }
    }
  }

  case class At[M,T](p : Proc[M,T], loc : Location, slf : Location) extends Proc[M,T] {
    // starts a server socket, receives messages, calls react
    // check that [loc] is [L], and run [p] only if they're equal,
    // otherwise do nothing.
    override def output(m : Message[M]) : List[T] = {
      if (loc == slf) {
        p.react(m)
      }
    }
  }

  case class Compose[M,T,U](f : T => U, p : Proc[M,T]) extends Proc[M,U] {
    override def output(m : Message[M]) : List[U] = {
      p.react(m).map(f)
    }
  }

  case class Once[M,T](p : Proc[M,T]) extends Proc[M,T] {
    override def output(m : Message[M]) : List[U] = {
      if (running) {
        val l = p.react(m)
        if (!(null l)) {
          running = false
          return l
        }
      }
    }
  }

    /* Some other kinds of state machines that keep track of the previous state.
     */
    case class Memory[M,S,I](init : S, update : I => S => S, input : Proc[M,I]) extends Proc[M,S]() {
      var cstate : List[S] = List(init)
      var pstate : List[S] = List(init)

      override def output(m : Message[M]) : List[S] = {
        val l = input.react(m)
        if (running) {
          if (!(null l)) {
            pstate = cstate
            cstate = cstate.flatMap((s) => l.map((i) => update(i)(s)))
          }
          return pstate
        }
      }
    }

  case class Until[M,T,U](p1 : Proc[M,T], p2 : Proc[M,U]) extends Proc[M,T] {
    override def output(m : Message[M]) : List[U] = {
      if (running) {
        val l = p2.react(m)
        if (!(null l)) {
          running = false
          return p1.react(m)
        }
      }
    }
  }

    case class Delegation[M,T,U](p : Proc[M,T], q : T -> Proc[M,U]) extends Proc[M,U]{
      var state = List[Proc[M,U]]()

      override def output(m : Message[M]) : List[U] = {
        // run every process in state on m
        val l = p.react(m)
        // for every t in l, start a new process q(t) and run that process on m
        // and put it in the state
      }
    }

  def main(args: Array[String]) {
    // One of these arguments is the location of the process [L]
    val locServer = new Location("localhost",8889)
    val locClient = new Location("localhost",8888)

    val Input : Base[Int] = Base("int" :: Nil)
    val Server : State[Int,Int,Int] = State(0, (i) => (s) => i + s, Input)

    def makeIntMsg (v : Int) = {new Message("int" :: Nil,2)}

    val Client : Output[Int,List[DirectedMessage[Int]]] =
      Output(new DirectedMessage(locServer,makeIntMsg(2)) :: Nil)

    val ServerSend : Compose[Int,Int,List[DirectedMessage[Int]]] =
      Compose((i) => Nil,Server)

    val ServerAt : At[Int,List[DirectedMessage[Int]]] = At(ServerSend,locServer)
    val ClientAt : At[Int,List[DirectedMessage[Int]]] = At(Client,locClient)

    // [System] awaits for a message and produces a list of directed message.
    // Once we've produces that list by applying [System] to the message,
    // actually send the messages to the locations.
    //
    // if System runs at [locClient] and receives a message, its going
    // to produce [DirectMessage(locServer,2)].  Then we have to
    // actually send [2] to [locSerevr].
    //
    // Run(System)
    //  - await for a message [m]
    //  - run System.react(m)
    //  - get the outputs
    //  - if DirectMessage(l,v) is one of these outputs send v to l.
    val System = Parallel(ClientAt,ServerAt)

    println("Testing server")

    val out = Server.react(new Message("int" :: Nil,17))

    println(out.toString())
  }

}
