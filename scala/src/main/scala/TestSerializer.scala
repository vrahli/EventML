import scala.pickling._
import binary._

object TestSerializer {

  case class Pair(val x: Any, val y: Any)

  abstract class NVal

  case class NVPair(val x: NVal, val y: NVal) extends NVal
  case class NVInl(val value: NVal) extends NVal
  case class NVInr(val value: NVal) extends NVal
  case class NVInt(val i: Int) extends NVal
  case class NVStr(val s: String) extends NVal
  case class Axiom() extends NVal

  def main(args : Array[String]) = {
    // TEST 1
    val obj1: Any = NVPair(NVPair(NVStr("aneris_pax_v1"),NVPair(NVStr("p1b"),Axiom())),NVPair(NVStr("acc1"),NVPair(NVInl(NVPair(NVInt(0),NVStr("ldr2"))),NVPair(NVInl(NVPair(NVInt(0),NVStr("ldr2"))),Axiom()))))
    val s: Array[Byte] = obj1.pickle.value
    println("serialized " + obj1)
    val obj2: Any = s.unpickle[Any]
    println("deserialized " + obj2) // does not fail but gives the wrong answer

    // TEST 1
    val objA1: Any = Pair(Pair(1,2),1)
    val sA: Array[Byte] = objA1.pickle.value
    println("serialized " + objA1)
    val objA2: Any = sA.unpickle[Any]
    println("deserialized " + objA2) // does not fail but gives the wrong answer

    // TEST 2
    val objB1: Any = Pair(1,Pair(1,2))
    val sB: Array[Byte] = objB1.pickle.value
    println("serialized " + objB1)
    val objB2: Any = sB.unpickle[Any]
    println("deserialized " + objB2) // fails
  }

}
