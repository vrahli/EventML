import java.io.{ByteArrayOutputStream,ObjectOutputStream,ByteArrayInputStream,ObjectInputStream}
import java.net.{InetSocketAddress,ConnectException}
import java.nio.channels.{ServerSocketChannel,SocketChannel,Selector,SelectionKey}
import java.nio.{ByteBuffer,CharBuffer}
import java.nio.charset.Charset
import collection.mutable.HashMap
import java.util.concurrent.BlockingQueue
import scala.collection.JavaConversions
import Config._
import General._
import Location._
import DelayedMessage._
import N2Sprelude._
import scala.pickling._
import binary._
//import akka.actor._
//import akka.util.{ ByteString, ByteStringBuilder }

object Connect {

  private val BYTE_COUNT_INT = 4
  private val ENCODING_CHARSET = "UTF-8"
  private val SIZE_BUFFER: Int = 1024
  private val TIMER = true
  private var BUFFER: ByteBuffer = ByteBuffer.allocate(SIZE_BUFFER)


  def time[R](block: => R) : R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def encoder = Charset.forName(ENCODING_CHARSET).newEncoder
  def decoder = Charset.forName(ENCODING_CHARSET).newDecoder

  def serializeObject(loc: String, obj: NVal, buffer: ByteBuffer) : Boolean = {
    // Move byteOutputStream outside and reset it everytime here.
    lazy val BYTE_OUTPUT_STREAM: ByteArrayOutputStream = new ByteArrayOutputStream()
    //BYTE_OUTPUT_STREAM.reset()
    lazy val objOutputStream: ObjectOutputStream = new ObjectOutputStream(BYTE_OUTPUT_STREAM)

    objOutputStream.writeObject(obj)
    val objAsByteArray: Array[Byte] = BYTE_OUTPUT_STREAM.toByteArray
    objOutputStream.close

    if ((objAsByteArray.length + BYTE_COUNT_INT) > buffer.capacity) { return false }

    // First write the number of bytes the object takes in serialized form.
    buffer.clear
    //buffer.putInt(objAsByteArray.length)
    buffer.put(objAsByteArray)

    buffer.flip
    return true
  }

  def deserializeObject(loc: String, obj: ByteBuffer) : NVal = {
    var objAsByteArray: Array[Byte] = new Array[Byte](obj.limit)
    obj.get(objAsByteArray)

    lazy val byteInputStream: ByteArrayInputStream = new ByteArrayInputStream(objAsByteArray)
    lazy val objInputStream: ObjectInputStream = new ObjectInputStream(byteInputStream)
    val result: NVal = objInputStream.readObject.asInstanceOf[NVal]
    objInputStream.close

    return result
  }

  def deserializeObject1(loc: String, buf: ByteBuffer) : NVal = {
    var objAsByteArray: Array[Byte] = new Array[Byte](buf.limit)
    buf.get(objAsByteArray)
    return objAsByteArray.unpickle[NVal]
  }

  def testSerializer(loc: String, obj: NVal, buf: ByteBuffer) = {
    printEmlLoc(loc,"MB","testing deserialization of serialized object " + obj)
    val obj2 = deserializeObject(loc,buf.duplicate)
    printEmlLoc(loc,"MB","serialized " + obj + " got " + obj2)
  }

  // serialize obj into buffer
  def serializeObject1(loc: String, obj: NVal, buffer: ByteBuffer) : Boolean = {
    val objAsByteArray: Array[Byte] = obj.pickle.value
    val len = objAsByteArray.length
    printEmlLoc(loc,"MB","serializing(" + len + ")")
    if ((len + BYTE_COUNT_INT) > buffer.capacity) { return false }
    buffer.clear
    buffer.put(objAsByteArray)
    buffer.flip
    testSerializer(loc,obj,buffer)
    return true
  }

  /** loc tries to connect to locTo.  If it can't, it sleeps for 1s and
    * retry */
  def connect(loc: String, locTo: String, ipTo : String, portTo: Int, selector: Selector, iteration: Int)
      : SocketChannel = {
    try {
      val socket : SocketChannel = SocketChannel.open()
      socket.connect(new InetSocketAddress(ipTo, portTo))
      socket.configureBlocking(false)
      socket.socket().setTcpNoDelay(true)
      val key : SelectionKey = socket.register(selector, SelectionKey.OP_READ)
      key.attach(socket)
      return socket
    } catch {
      case (e : ConnectException) =>
        {
          printEmlLoc(loc,"MB","could not connect to " + locTo + "(iteration: " + iteration + ")")
          printEmlLoc(loc,"MB","will retry in 1 second")
          Thread.sleep(1000)
          connect(loc,locTo,ipTo,portTo,selector,iteration+1)
        }
    }
  }

  def readString1(loc: String, socket: SocketChannel) : String = {
    val n: Int = socket.read(BUFFER)
    printEmlLocDbg(loc,"MB", "read(" + n + ")")
    val s: String = ((BUFFER.flip()).asInstanceOf[ByteBuffer]).asCharBuffer.toString()
    BUFFER.clear()
    return s
  }

  def readString2(loc: String, socket: SocketChannel) : String = {
    val n: Int = socket.read(BUFFER)
    printEmlLocDbg(loc,"MB", "read(" + n + ")")
    val stringBytes = new Array[Byte](n)
    BUFFER.flip
    BUFFER.get(stringBytes)
    val s: String = new String(stringBytes, ENCODING_CHARSET)
    BUFFER.clear()
    return s
  }

  case class ReadException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

  def readString(loc: String, socket: SocketChannel) : String = {
    val n: Int = socket.read(BUFFER)
    if (n < 0) {
      throw new ReadException()
    } else {
      printEmlLocDbg(loc,"MB", "read(" + n + ")")
      BUFFER.flip
      val s: String = decoder.decode(BUFFER).toString()
      BUFFER.clear()
      return s
    }
  }

  def readObj(loc: String, socket: SocketChannel) : NVal = {
    val n: Int = socket.read(BUFFER)
    if (n < 0) {
      throw new ReadException()
    } else {
      printEmlLocDbg(loc,"MB", "read(" + n + ")")
      BUFFER.flip
      val obj: NVal = deserializeObject(loc,BUFFER)
      BUFFER.clear()
      return obj
    }
  }

  def readObjTimer(loc:String,socket:SocketChannel) : NVal = {
    if (TIMER) {
      val nowNano = System.nanoTime
      val o = readObj(loc,socket)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[readObj] time: " + tmicro + "microseconds (" + tmilli + "ms)")
      return o
    } else { readObj(loc,socket) }
  }

  def writeString1(socket: SocketChannel, s:String) = {
    socket.write(ByteBuffer.wrap(s.getBytes(ENCODING_CHARSET)))
  }

  def writeString(socket: SocketChannel, s:String) = {
    socket.write(encoder.encode(CharBuffer.wrap(s)))
  }

  def serializeObj(loc:String,obj:NVal) = {
    val b: Boolean = serializeObject(loc,obj,BUFFER)
    if (!b) { throw new Exception("Exceeded buffer capacity while serializing") }
  }

  def serializeObjTimer(loc:String,obj:NVal) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      serializeObj(loc,obj)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[serializeObj] time: " + tmicro + "microseconds (" + tmilli + "ms)")
    } else { serializeObj(loc,obj) }
  }

  def write2Socket(loc:String,socket:SocketChannel) = {
    socket.write(BUFFER)
    BUFFER.clear()
  }

  def write2SocketTimer(loc:String,socket:SocketChannel) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      write2Socket(loc,socket)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[write2Socket] time: " + tmicro + "microseconds (" + tmilli + "ms)")
    } else { write2Socket(loc,socket) }
  }

  def writeObj(loc: String, socket: SocketChannel, obj: NVal) = {
    serializeObjTimer(loc,obj)
    write2SocketTimer(loc,socket)
  }

  def writeObjTimer(loc: String, socket: SocketChannel, obj: NVal) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      writeObj(loc,socket,obj)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[writeObj] time: " + tmicro + "microseconds (" + tmilli + "ms)")
    } else { writeObj(loc,socket,obj) }
  }

  def writeDelayedObj(loc: String, socket: SocketChannel, obj: NVal) = {
    var buf: ByteBuffer = ByteBuffer.allocate(1024)
    val b: Boolean = serializeObject(loc,obj,buf)
    socket.write(buf)
    buf.clear()
  }

  def connectToLoc(loc: String, port: Int, location: Location, selector: Selector)
      : SocketChannel = {
        val locTo = location.id
        printEmlLoc(loc,"MB","connecting to " + locTo)
        // create socket
        val socket = connect(loc,locTo,location.ip,location.port,selector,0)
	printEmlLoc(loc,"MB","connected to " + socket)
        // WRITING
	writeString(socket,loc)
	printEmlLoc(loc,"MB","sent name: " + loc)
	/*	  val out = new PrintStream(socket.socket.getOutputStream)
         // send location name, we used to send the port number
	 out.println(loc)
	 printEmlLoc(loc,"MB","sent " + loc)
         out.flush
	 out.close*/
        return socket
      }

  /** Connect to higher machines */
  def connectTo(loc: String, port: Int, gt: List[Location], selector: Selector)
      : HashMap[String,SocketChannel] = {
    gt match {
      case Nil =>
        {
          printEmlLoc(loc,"MB","no more machines to connect to")
          return new HashMap[String,SocketChannel]()
        }
      case location :: locs =>
        {
          printEmlLoc(loc,"MB","still " + gt.length.toString + " machines to connect to")
	  val socket: SocketChannel = connectToLoc(loc,port,location,selector)
          return connectTo(loc,port,locs,selector) += (location.id) -> socket
        }
    }
  }

  def createServerSocket(loc: String, ip: String, port: Int, selector: Selector)
      : (ServerSocketChannel, SelectionKey) = {
    val server = ServerSocketChannel.open()
    server.configureBlocking(false)

    // Bind the server socket to the specified address and port
    val isa = new InetSocketAddress(ip, port)
    server.socket().bind(isa)

    // Register the server socket channel, indicating an interest in
    // accepting new connections
    val key = server.register(selector, SelectionKey.OP_ACCEPT)
    key.attach(server)

    return (server, key)
  }

  def readName (loc:String, socket: SocketChannel) : String = {
    var name: String = ""
    while (name == "") { name = readString(loc,socket) }
    printEmlLoc(loc,"MB","received name: -" + name + "-")
    return name
  }

  def serverHandler(
    loc      : String,
    server   : ServerSocketChannel,
    selector : Selector,
    sockets  : HashMap[String,SocketChannel]
  ) = {
    val socket : SocketChannel = server.accept
    printEmlLoc(loc,"MB","received connection request on " + socket)
    socket.configureBlocking(false)
    socket.socket().setTcpNoDelay(true)
    val newkey : SelectionKey = socket.register(selector, SelectionKey.OP_READ)
    newkey.attach(socket)
    // READING
    val name: String = readName(loc,socket)
    sockets += name -> socket
  }

  def add2received(loc:String,received:BlockingQueue[NVal],msg:NVal) = received.add(msg)

  def add2receivedTimer(loc:String,received:BlockingQueue[NVal],msg:NVal) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      add2received(loc,received,msg)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[add2received] time: " + tmicro + "microseconds (" + tmilli + "ms)")
    } else { add2received(loc,received,msg) }
  }

  def socketHandler(
    loc      : String,
    key      : SelectionKey,
    received : BlockingQueue[NVal]
  ) = {
    val nowNano = System.nanoTime
    printEmlLocDbg(loc,"MB","[socketHander-1] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    printEmlLocDbg(loc,"MB","received message?")
    printEmlLocDbg(loc,"MB","[socketHander-2] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    val socket: SocketChannel = key.attachment.asInstanceOf[SocketChannel]
    printEmlLocDbg(loc,"MB","[socketHander-3] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    printEmlLocDbg(loc,"MB","readable socket: " + socket)
    try {
      // READING
      val msg: NVal = readObjTimer(loc,socket)
      printEmlLocDbg(loc,"MB","[socketHander-4] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
      printEmlLocDbg(loc,"MB","received message: " + msg)
      add2receivedTimer(loc,received,msg)
      printEmlLocDbg(loc,"MB","[socketHander-5] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    } catch {
      case (e: ReadException) =>
        {
          printEmlLoc(loc,"MB","closing socket that appears to have died: " + socket)
          socket.close()
        }
    }
  }

  def socketHandlerTimer(
    loc      : String,
    key      : SelectionKey,
    received : BlockingQueue[NVal]
  ) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      socketHandler(loc,key,received)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[socketHander] time: " + tmicro + "microseconds (" + tmilli + "ms)")
    } else { socketHandler(loc,key,received) }
  }

  class SendDelayedMessageSelf(loc:String,received:BlockingQueue[NVal],msg:NVal,delay:Int)
      extends Thread {
    override def run() = {
      // delay is in milliseconds
      Thread.sleep(delay)
      received.add(msg)
    }
  }

  class SendDelayedMessage(loc:String,sock:SocketChannel,msg:NVal,delay:Int)
      extends Thread {
    override def run() = {
      // delay is in milliseconds
      Thread.sleep(delay)
      writeDelayedObj(loc,sock,msg)
    }
  }

  def senderHandler(
    loc      : String,
    received : BlockingQueue[NVal],
    tosend   : BlockingQueue[DelayedMessage],
    sockets  : HashMap[String,SocketChannel]
  ) = {
    val nowNano = System.nanoTime
    printEmlLocDbg(loc,"MB","[senderHander-1] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    printEmlLocDbg(loc,"MB","message to send?")
    printEmlLocDbg(loc,"MB","[senderHander-2] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    var dmsgs: List[DelayedMessage] = Nil
    printEmlLocDbg(loc,"MB","[senderHander-3] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    // We get the messages from to send
    var b = true
    try {
      while (b) {
        var dmsg: DelayedMessage = tosend.poll
        if (dmsg == null) { b = false }
        else {
          printEmlLocDbg(loc,"MB","got message from output queue")
          /*	if (dmsgs.exists((m) => m.msg.equals(dmsg.msg))) {
           printEmlLocDbg(loc,"MB","already got such a message")
           }*/
	  dmsgs ::= dmsg
        }
      }
    } catch { case (e: NullPointerException) => () }
    printEmlLocDbg(loc,"MB","[senderHander-4] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
    // We send the messages through the sockets
    while (!(dmsgs.isEmpty)) {
      val dmsg = dmsgs.head
      val rest = dmsgs.tail
      dmsgs = rest
      val (delay:Int,id:String,msg:NVal) = dmsg.destruct
      if (loc == id) {
        printEmlLocDbg(loc,"MB","sending message to itself")
        if (delay == 0) { received.add(msg) /* this should never happen */ }
        else { (new SendDelayedMessageSelf(loc,received,msg,delay)).start }
      } else {
        if (delay == 0) {
          // The messages in dmsgsEq have same content/delay as dmsg while the ones in dmsgsNeq don't
          val (dmsgsEq,dmsgsNeq) = rest.partition(m => m.eqDmsg(dmsg))
          dmsgs = dmsgsNeq
          // We serialize the msg into BUFFER
          serializeObjTimer(loc,msg)
          // We get the list of destinations
          val dests = id :: dmsgsEq.map(m => m.recipient)
          val nb = dests.length
          // We send the messages
          dests.foreach {
            dest => {
	      printEmlLocDbg(loc,"MB","[senderHander-5-1] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
              printEmlLocDbg(loc,"MB","sending message(" + nb + ") to " + dest + ": " + msg)
              write2SocketTimer(loc,sockets(dest))
	      printEmlLocDbg(loc,"MB","[senderHander-5-2] time: " + (System.nanoTime - nowNano) / 1000 + "microseconds")
            }
          }
        } else { (new SendDelayedMessage(loc,sockets(id),msg,delay)).start }
      }
    }
  }

  def senderHandlerTimer(
    loc      : String,
    received : BlockingQueue[NVal],
    tosend   : BlockingQueue[DelayedMessage],
    sockets  : HashMap[String,SocketChannel]
  ) = {
    if (TIMER) {
      val nowNano = System.nanoTime
      senderHandler(loc,received,tosend,sockets)
      val tmicro = (System.nanoTime - nowNano) / 1000
      val tmilli = tmicro / 1000
      printEmlLocDbg(loc,"MB","[senderHandler] time: " + tmicro + "microseconds (" + tmilli + "ms)")
    } else { senderHandler(loc,received,tosend,sockets) }
  }

  class Mailbox(
    loc      : String,
    ip       : String,
    port     : Int,
    lt       : List[Location],
    gt       : List[Location],
    received : BlockingQueue[NVal],
    tosend   : BlockingQueue[DelayedMessage],
    selector : Selector
  ) extends Thread {

    def loop(
      server    : ServerSocketChannel,
      serverkey : SelectionKey,
      sockets   : HashMap[String,SocketChannel]
    ) = {
      while (true) {
	printEmlLoc(loc,"MB","waiting...")
        selector.select
	printEmlLoc(loc,"MB","woke up")
        val jKeys = selector.selectedKeys
	val keys = JavaConversions.asScalaSet(jKeys).toList
	selector.selectedKeys.clear

        // if keys has next then we have to deal with a registered socket
        // otherwise it's tosend
        if (!(keys.isEmpty)) {
          keys.foreach {key =>
            if (key.equals(serverkey)) {
              // SERVER
              // the key corresponds to the server, therefore we have
              // to accept a new connection request
	      if (!(key.isAcceptable)) { throw new Exception("key is server, but not acceptable") }
              serverHandler(loc,server,selector,sockets)
            } else {
              // RECEIVING
              // the key does not correspond to the server, therefore
              // it corresponds to a socket that we have to read from
	      if (!(key.isReadable)) { throw new Exception("key is not server, but not readable") }
              socketHandlerTimer(loc,key,received)
            }
          }
        } else {
          // SENDING
          // Otherwise select returned because something was added onto tosend
          senderHandlerTimer(loc,received,tosend,sockets)
        }
      }
    }

    override def run() = {
      printEmlLoc(loc,"MB","creating server socket")
      // server is registered as ACCEPT
      val (server, serverkey) = createServerSocket(loc,ip,port,selector)

      printEmlLoc(loc,"MB","connecting to " + gt.length.toString + " higher machines")
      // sockets are registered as READ
      var sockets_gt = connectTo(loc,port,gt,selector)

      loop(server,serverkey,sockets_gt)
    }

  }

  def main(args: Array[String]) {}

}

