import scala.util.parsing.input.{ StreamReader, Reader }
import java.io._
import N2Sclass._
import N2Sparser._
import N2Sprelude._
import N2Sstrings._
import Config._


object N2S {

  def mk_lib_obj(output: String) = (new File(output)).getName + "_lib"
  def mk_lib_file(output: String) = output + "_lib.scala"

  def mk_gen_obj(output: String) = (new File(output)).getName + "_gen"
  def mk_gen_file(output: String) = output + "_gen.scala"

  def mk_inst_obj(output: String) = (new File(output)).getName + "_inst"
  def mk_inst_file(output: String) = output + "_inst.scala"

  def mk_out_obj(output: String, id: String) = (new File(output)).getName + "_" + id
  def mk_out_file(output: String, id: String) = output + "_" + id + ".scala"

  val procgen  = "process_generator"
  val procinst = "process_instance"
  val procname = "process"

  def generate_lib(output: String) = {
    val lib_obj = mk_lib_obj(output)
    val lib_file = mk_lib_file(output)

    println(" + exporting dependencies to " + lib_file)
    val writer = new PrintWriter(new File(lib_file))

    // header
    writer.write("import N2Sprelude._\n")
    writer.write("\n")

    //object
    writer.write("object " + lib_obj + " {\n\n")

    // dependencies
    functions.foreach { kv => writer.write(kv._2 + "\n") }
    writer.write("\n")

    // closing object
    writer.write("}\n")

    writer.close()

    println(" - done")
  }

  def generate_proc_gen(output: String, proc_gen: String) = {
    val lib_obj  = mk_lib_obj(output)
    val gen_obj  = mk_gen_obj(output)
    val gen_file = mk_gen_file(output)

    println(" + exporting process generator to " + gen_file)
    val writer = new PrintWriter(new File(gen_file))

    // header
    writer.write("import N2Sprelude._\n")
    writer.write("import " + lib_obj + "._\n")
    writer.write("\n")

    //object
    writer.write("object " + gen_obj + " {\n\n")

    // process generator
    writer.write("def " + procgen + " = {\n" + proc_gen + "\n}\n\n")

    // closing object
    writer.write("}\n")

    writer.close()

    println(" - done")
  }

  def generate_proc_inst(output: String, params: List[Parameter]) = {
    val gen_obj   = mk_gen_obj(output)
    val inst_obj  = mk_inst_obj(output)
    val inst_file = mk_inst_file(output)

    println(" + exporting process instance to " + inst_file)
    val writer = new PrintWriter(new File(inst_file))

    // header
    writer.write("import N2Sprelude._\n")
    writer.write("import " + gen_obj + "._\n")
    writer.write("\n")

    //object
    writer.write("object " + inst_obj + " {\n\n")

    // process instance
    writer.write("def " + procinst + " = {\n\t")
    var instance = procgen
    params.foreach { p => instance = mk_nuprl_apply(instance, p.str) }
    writer.write(instance + "\n}\n\n")

    // closing object
    writer.write("}\n")

    writer.close()

    println(" - done")
  }

  def generate_one_file(id: String, output: String) = {
    val inst_obj = mk_inst_obj(output)
    val out_obj  = mk_out_obj(output,id)
    val out_file = mk_out_file(output,id)

    val procstr = procname + id

    println(" + exporting process to " + out_file)
    val writer = new PrintWriter(new File(out_file))

    // header
    writer.write("import Run._\n")
    writer.write("import N2Sprelude._\n")
    writer.write("import " + inst_obj + "._\n")
    writer.write("\n")

    //object
    writer.write("object " + out_obj + " {\n\n")

    // process instance
    writer.write("def " + procstr + " = {\n\t")
    writer.write(mk_nuprl_apply(procinst, "\"" + id + "\"") + "\n}\n\n")

    // main
    writer.write("// 1st argument is configuration file\n")
    writer.write("def main(args: Array[String]) { Run.runprocess(args(0),\"" + id + "\"," + procstr + ") }\n\n")

    // closing object
    writer.write("}\n")

    writer.close()

    println(" - done")
  }

  def generate_files(
    in_file   : String,
    alldefs   : String,
    conf_file : String,
    output    : String) = {

    println("++++ TO SCALA ++++")

    val curTime = System.currentTimeMillis

    // we parse the alldefs file, it generates the library
    println(" + parsing library file: " + alldefs)
    val library = StreamReader(new InputStreamReader(new FileInputStream(alldefs)))
    val libout  = N2Sparser.parseAll(N2Sparser.getTerms(),library)
    println(" - done")

    // we parse the input file, i.e., the EML spec.
    println(" + parsing input file: " + in_file)
    val input   = StreamReader(new InputStreamReader((new FileInputStream(in_file))))
    val classes = N2Sparser.parseAll(N2Sparser.getTerms(),input).get
    println(" - done")

    // We now parse the configuration file
    println(" + parsing config file: " + conf_file)
    val confin = StreamReader(new InputStreamReader(new FileInputStream(conf_file)))
    val conf   = Config.parseAll(Config.getTerms(),confin).get
    val params = conf.params.list
    println(" - done")

    // Converting EML processes to Scala
    println(" + converting to scala")
    val proc_gen : String = classes(0).toScala(new N2Sparam())
    // when calling toScala above, wee also generate 'function', which
    // contains the dependencies of our program
    println(" - done")

    // generate all the files
    generate_lib(output)
    generate_proc_gen(output,proc_gen)
    generate_proc_inst(output,params)
    conf.get_internal_locs().foreach {loc => generate_one_file(loc.id,output)}

    val endTime = System.currentTimeMillis
    println("time: " + (endTime - curTime) + "ms")
    println("List of opids missing from library:")
    for (i <- 0 to N2Sclass.missing_opid.length -1) {
      println(N2Sclass.missing_opid(i))
    }
    println("---- DONE ----")

  }

/*
  def run[InputType, OutputType](code: InputType => OutputType) =
    (input: InputType) => code(input)
*/

  def main(args: Array[String]) {
    val output  = args(0)
    val conf    = args(1)
    val in_file = args(2)
    val alldefs = args(3)
    generate_files(in_file,alldefs,conf,output)
  }

}
