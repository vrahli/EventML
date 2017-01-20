module type INTRANSIT_MESSAGE =
  sig
    type message
    val apply2msg : (NuprlTerms.nuprl_term -> NuprlTerms.nuprl_term)
      -> message
	-> message
    val mk_message   : int -> string -> NuprlTerms.nuprl_term -> message
    val dest_message : message -> (int * string * NuprlTerms.nuprl_term)
    val message2term : message -> NuprlTerms.nuprl_term
    val term2message : NuprlTerms.nuprl_term -> message
  end

module Intransit_Message : INTRANSIT_MESSAGE =
  struct

    module NT = NuprlTerms

    type message =
	{delay : int;
	 id    : string;
	 msg   : NT.nuprl_term}

    let mk_message delay id msg = {delay = delay; id = id; msg = msg}

    let dest_message {delay;id;msg} = (delay,id,msg)

    let get_delay (msg : message) = msg.delay
    let get_id    (msg : message) = msg.id
    let get_msg   (msg : message) = msg.msg

    let apply2msg f {delay;id;msg} = mk_message delay id (f msg)

    let message2term {delay;id;msg} =
      let nterm = NT.mk_nuprl_small_natural_number_term delay in
      let iterm = NT.mk_mkid_term id in
      NT.mk_pair_term nterm (NT.mk_pair_term iterm msg)

    let term2message msg =
      let (delay,rest) = NT.dest_pair 5 msg in
      let (id,m) = NT.dest_pair 5 rest in
      mk_message (NT.dest_small_integer delay) (NT.dest_id id) m

end

module C  = ConfigParser
module NT = NuprlTerms
module PN = ParserNuprlAscii
module EV = Evaluators
module IM = Intransit_Message


(* ------ ARGUMENTS ------ *)

type args =
    {input   : string;
     output  : string;
     lib     : string;
     time    : int;
     sub     : bool;
     sanity  : bool;
     nuprl   : bool;
     obid    : string;
     ascii   : bool;
     tcheck  : bool;
     parse   : bool;
     split   : bool;
     prt     : bool;
     eval    : string option;
     alldef  : string;
     test    : int option;
     session : bool;
     simul   : bool;
     step    : int option;
     mono    : bool;
     host    : string;
     port    : int;
     conf    : string;
     client  : bool;
     send    : bool;
     other   : bool;
     all     : bool;
     ev      : string;
     id      : string;
     extra   : string;
     gc      : bool}

let default_el_output = "/tmp/eventml-output.el"
let default_output    = ""
let default_lib       = ""
let default_input     = "test.esh"
let default_time      = 1 (* 1sec by default *)
let default_sub       = false
let default_sanity    = false
let default_nuprl     = false
let default_obid      = ""
let default_ascii     = false
let default_tcheck    = false
let default_parse     = false
let default_split     = false
let default_prt       = false
let default_eval      = None
let default_alldef    = ""
let default_test      = None
let default_session   = false
let default_simul     = false
let default_step      = None
let default_mono      = false
let default_host      = "127.0.0.0"
let default_port      = 14567
let default_conf      = ""
let default_client    = false
let default_send      = false
let default_other     = false
let default_all       = false
let default_ev        = "ev2b" (* ev1 *)
let default_id        = ""
let default_extra     = ""
let default_gc        = false

let initArgs =
  {input   = default_input;
   output  = default_output;
   lib     = default_lib;
   time    = default_time;
   sub     = default_sub;
   sanity  = default_sanity;
   nuprl   = default_nuprl;
   obid    = default_obid;
   ascii   = default_ascii;
   tcheck  = default_tcheck;
   parse   = default_parse;
   split   = default_split;
   prt     = default_prt;
   eval    = default_eval;
   alldef  = default_alldef;
   test    = default_test;
   session = default_session;
   simul   = default_simul;
   step    = default_step;
   mono    = default_mono;
   host    = default_host;
   port    = default_port;
   conf    = default_conf;
   client  = default_client;
   send    = default_send;
   other   = default_other;
   all     = default_all;
   ev      = default_ev;
   id      = default_id;
   extra   = default_extra;
   gc      = default_gc}

let mk_args
    input   output lib    time   sub     sanity
    nuprl   obid   ascii  tcheck parse   split
    prt     eval   alldef test   session simul
    step    mono   host   port   conf    client
    send    other  all    ev     id      extra
    gc =
  {input   = input;
   output  = output;
   lib     = lib;
   time    = time;
   sub     = sub;
   sanity  = sanity;
   nuprl   = nuprl;
   obid    = obid;
   ascii   = ascii;
   tcheck  = tcheck;
   parse   = parse;
   split   = split;
   prt     = prt;
   eval    = eval;
   alldef  = alldef;
   test    = test;
   session = session;
   simul   = simul;
   step    = step;
   mono    = mono;
   host    = host;
   port    = port;
   conf    = conf;
   client  = client;
   send    = send;
   other   = other;
   all     = all;
   ev      = ev;
   id      = id;
   extra   = extra;
   gc      = gc}

let mk_args_ref
    input   output lib    time   sub     sanity
    nuprl   obid   ascii  tcheck parse   split
    prt     eval   alldef test   session simul
    step    mono   host   port   conf    client
    send    other  all    ev     id      extra
    gc =
  mk_args
    (!input)  (!output) (!lib)    (!time)   (!sub)     (!sanity)
    (!nuprl)  (!obid)   (!ascii)  (!tcheck) (!parse)   (!split)
    (!prt)    (!eval)   (!alldef) (!test)   (!session) (!simul)
    (!step)   (!mono)   (!host)   (!port)   (!conf)    (!client)
    (!send)   (!other)  (!all)    (!ev)     (!id)      (!extra)
    (!gc)

let getElOutput outop =
  match outop with
    Some output -> output
  | None -> default_el_output

let updInput   {input = _; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} input   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updOutput  {input; output = _; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} output  = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updLib     {input; output; lib = _; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} lib     = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updTime    {input; output; lib; time = _; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} time    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updSub     {input; output; lib; time; sub = _; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} sub     = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updSanity  {input; output; lib; time; sub; sanity = _; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} sanity  = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updNuprl   {input; output; lib; time; sub; sanity; nuprl = _; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} nuprl   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updObid    {input; output; lib; time; sub; sanity; nuprl; obid = _; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} obid    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updAscii   {input; output; lib; time; sub; sanity; nuprl; obid; ascii = _; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} ascii   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updTcheck  {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck = _; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} tcheck  = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updParse   {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse = _; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} parse   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updSplit   {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split = _; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} split   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updPrint   {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt = _; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} prt     = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updEval    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval = _; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} eval    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updAlldef  {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef = _; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} alldef  = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updTest    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test = _; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} test    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updSession {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session = _; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} session = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updSimul   {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul = _; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} simul   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updStep    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step = _; mono; host; port; conf; client; send; other; all; ev; id; extra; gc} step    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updMono    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono = _; host; port; conf; client; send; other; all; ev; id; extra; gc} mono    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updHost    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host = _; port; conf; client; send; other; all; ev; id; extra; gc} host    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updPort    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port = _; conf; client; send; other; all; ev; id; extra; gc} port    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updConf    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf = _; client; send; other; all; ev; id; extra; gc} conf    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updClient  {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client = _; send; other; all; ev; id; extra; gc} client  = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updSend    {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send = _; other; all; ev; id; extra; gc} send    = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updOther   {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other = _; all; ev; id; extra; gc} other   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updAll     {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all = _; ev; id; extra; gc} all     = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updEv      {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev = _; id; extra; gc} ev      = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updId      {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id = _; extra; gc} id      = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updExtra   {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra = _; gc} extra   = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}
let updGc      {input; output; lib; time; sub; sanity; nuprl; obid; ascii; tcheck; parse; split; prt; eval; alldef; test; session; simul; step; mono; host; port; conf; client; send; other; all; ev; id; extra; gc = _} gc      = {input = input; output = output; lib = lib; time = time; sub = sub; sanity = sanity; nuprl = nuprl; obid = obid; ascii = ascii; tcheck = tcheck; parse = parse; split = split; prt = prt; eval = eval; alldef = alldef; test = test; session = session; simul = simul; step = step; mono = mono; host = host; port = port; conf = conf; client = client; send = send; other = other; all = all; ev = ev; id = id; extra = extra; gc = gc}

type arg =
    I       of string        (* input     *)
  | O       of string        (* ouput     *)
  | L       of string        (* library   *)
  | T       of int           (* timelimit *)
  | OBID    of string        (* object_id to use when generating the nuprl code *)
  | EVAL    of string        (* term to evaluate *)
  | DEF     of string        (* the alldef file  *)
  | TEST    of int
  | STEP    of int           (* to step through a protocl, by specifying (int) the message to send next *)
  | HOST    of string        (* host name *)
  | PORT    of int           (* port number *)
  | CONF    of string        (* run an EML program in a distributed environment using a conf file *)
  | EV      of string        (* picks one of our Nuprl evaluator *)
  | ID      of string        (* machine identifier *)
  | EXTRA   of string        (* extra arguments *)
  | SIMUL                    (* simulate an EML program with a configuration file *)
  | CLIENT                   (* start a dummy client *)
  | SEND                     (* send the initial messages in transit *)
  | OTHER                    (* run a `forward` machine *)
  | ALL                      (* simulates all the machines *)
  | SESSION
  | TONUPRL
  | SUBTYPING
  | SANITY
  | FROMASCII
  | TYPECHECK
  | PARSE
  | SPLIT
  | PRINT
  | MONO
  | GC

let format args =
  let rec gen lst r =
    match lst with
      [] -> r
    | SUBTYPING      :: list -> gen list (updSub     r true)
    | SANITY         :: list -> gen list (updSanity  r true)
    | FROMASCII      :: list -> gen list (updAscii   r true)
    | TYPECHECK      :: list -> gen list (updTcheck  r true)
    | PARSE          :: list -> gen list (updParse   r true)
    | SPLIT          :: list -> gen list (updSplit   r true)
    | PRINT          :: list -> gen list (updPrint   r true)
    | TONUPRL        :: list -> gen list (updNuprl   r true)
    | SESSION        :: list -> gen list (updSession r true)
    | MONO           :: list -> gen list (updMono    r true)
    | SEND           :: list -> gen list (updSend    r true)
    | OTHER          :: list -> gen list (updOther   r true)
    | CLIENT         :: list -> gen list (updClient  r true)
    | ALL            :: list -> gen list (updAll     r true)
    | GC             :: list -> gen list (updGc      r true)
    | SIMUL          :: list -> gen list (updSimul   r true)
    | (EXTRA   str)  :: list -> gen list (updExtra   r str)
    | (HOST    host) :: list -> gen list (updHost    r host)
    | (PORT    port) :: list -> gen list (updPort    r port)
    | (OBID    obid) :: list -> gen list (updObid    r obid)
    | (T       time) :: list -> gen list (updTime    r time)
    | (I       file) :: list -> gen list (updInput   r file)
    | (EV      ev)   :: list -> gen list (updEv      r ev)
    | (ID      id)   :: list -> gen list (updId      r id)
    | (CONF    file) :: list -> gen list (updConf    r file)
    | (STEP    n)    :: list -> gen list (updStep    r (Some n))
    | (TEST    n)    :: list -> gen list (updTest    r (Some n))
    | (EVAL    str)  :: list -> gen list (updEval    r (Some str))
    | (O       file) :: list -> gen list (updOutput  r file)
    | (L       file) :: list -> gen list (updLib     r file)
    | (DEF     file) :: list -> gen list (updAlldef  r file)
  in gen args initArgs


(* ------ A FEW GLOBAL VARIABLES ------ *)

let program : (string list * NT.nuprl_term) option ref = ref None

let default_alldefs = "/usr/fdl/lib/alldefs"

let loaded     = ref false
let configured = ref false

let components : (string * NT.nuprl_term) list ref = ref []
let intransit : IM.message list ref = ref []

let process : NT.nuprl_term option ref = ref None

let reset_all () =
  (program    := None;
   components := [];
   intransit  := [];
   process    := None;
   ())

let ip_server = "127.0.0.1"
let port_server = 8987

let a_sock_ref = ref ([] : Unix.file_descr list)
let p_sock_ref = ref ([] : Unix.file_descr list)

let add_a_sock_ref sock = a_sock_ref := sock :: !a_sock_ref
let add_p_sock_ref sock = p_sock_ref := sock :: !p_sock_ref


(* ------ EXCEPTIONS ------ *)

exception RCV_INT of Unix.file_descr
exception RCV_SYS of Unix.file_descr
exception SEL_SYS of string

exception DONE


(* ------ A FEW USEFUL FUNCTIONS ------ *)

let print_eml str = print_endline ("[" ^ str ^ "]")
let print_eml_id id str = print_eml (id ^ ":" ^ str)
let print_eml2 str = print_endline str

let rec implode chars =
  match chars with
    [] -> ""
  | char :: chars -> String.make 1 char ^ implode chars


(* ------ TYPES ------ *)

type extra =
    EX_INT  of int
  | EX_BOOL of bool
  | EX_DUMP
  | EX_HASKELL
  | EX_NSOCK
  | EX_NONE
  | EX_CBVA
  | EX_FOLD
  | EX_NEWPROG
  | EX_OLDPROG

type sock_info =
    {si_id   : string option;
     si_host : string option;
     si_port : int option;
     si_sock : Unix.file_descr}

type ident =
    Loc of string
  | Mac of string * int


(* ------ TIMER ------ *)

let start_timer () = Unix.time ()

let get_time timer =
  let time = Unix.time () in time -. timer

let string_of_time timer =
  let time = Unix.time () in string_of_float ((float 1000) *. (time -. timer))


(* ------ IDENTS ------ *)

let ident_to_string ident =
  match ident with
    Mac (host, port) -> "(" ^ host ^ "," ^ string_of_int port ^ ")"
  | Loc id -> id


(* ------ EXTRA ------ *)

let split_extras str = 
  Str.split
    (Str.regexp "[ ,:;]+")
    str

let getExtras str =
  List.map
    (fun elt ->
      try EX_INT (int_of_string elt)
      with _ ->
	match elt with
	  "true"    -> EX_BOOL true
	| "false"   -> EX_BOOL false
	| "T"       -> EX_BOOL true
	| "F"       -> EX_BOOL false
	| "newsock" -> EX_NSOCK
	| "fold"    -> EX_FOLD
	| "newprog" -> EX_NEWPROG
	| "oldprog" -> EX_OLDPROG
	| _         -> EX_NONE)
    (split_extras str)

let print_extras extra =
  List.iter
    (fun x -> print_eml ("extra: " ^ x))
    (split_extras extra)

let get_int_extra str =
  try
    Some (List.find
	    (fun ex -> match ex with (EX_INT n) -> true  | _ -> false)
	    (getExtras str))
  with _ -> None

let is_nsock_extra   str = List.exists (fun ex -> match ex with EX_NSOCK   -> true  | _ -> false) (getExtras str)
let is_fold_extra    str = List.exists (fun ex -> match ex with EX_FOLD    -> true  | _ -> false) (getExtras str)
let is_newprog_extra str = List.exists (fun ex -> match ex with EX_NEWPROG -> true  | _ -> false) (getExtras str)
let is_oldprog_extra str = List.exists (fun ex -> match ex with EX_OLDPROG -> true  | _ -> false) (getExtras str)

let newprog_extra str = is_newprog_extra str || not (is_oldprog_extra str)


(* ------ TO STRING + DESTRUCTORS ------ *)

let addr_to_string sock_addr =
  match sock_addr with
    Unix.ADDR_UNIX str -> "(" ^ str ^ ")"
  | Unix.ADDR_INET (inet_addr, port) ->
      "(" ^ Unix.string_of_inet_addr inet_addr ^ "," ^ string_of_int port ^ ")"

let dest_inet_sockaddr sock_addr =
  match sock_addr with
    Unix.ADDR_UNIX str -> failwith "dest_inet_sockaddr"
  | Unix.ADDR_INET (inet_addr, port) -> (inet_addr, port)

let rec print_message_list n lst =
  match lst with
    [] -> ()
  | msg :: msgs ->
      (print_string (string_of_int n ^ ": " ^ NT.nuprlTerm2eml msg ^ "\n--\n");
       print_message_list (n + 1) msgs)

let unfold_intransit_message lib = IM.apply2msg (NT.unfold_all lib)

let unfold_intransit_messages lib = List.map (unfold_intransit_message lib)

let print_messages_intransit () =
  (print_string ("\n-------IN-TRANSIT-------\n");
   print_message_list 1 (List.map IM.message2term (!intransit)))

let print_messages msgs =
    List.iter
    (fun msg ->
      ((*print (NT.toStringTerm msg ^ "msg" ^ "\n");*)
	print_string (NT.nuprlTerm2eml msg ^ "\n")))
  msgs


(* ------ SOCKET INFORMATION ------ *)

let mk_sock_info id host port sock =
  {si_id   = Some id;
   si_host = Some host;
   si_port = Some port;
   si_sock = sock}

let mk_simple_sock_info sock =
  {si_id   = None;
   si_host = None;
   si_port = None;
   si_sock = sock}

let select_id     (nfo : sock_info) = nfo.si_id
let select_socket (nfo : sock_info) = nfo.si_sock

let idNfo2string nfo =
  match select_id nfo with
    Some id -> id
  | None -> "-"

let filter_socks id nfo =
  match select_id nfo with
    Some i -> id = i
  | None -> false

let get_socks = List.map select_socket

let add_socket sock (id,host,port) = mk_sock_info id host port sock

let add_socket_nfo sock (id,host,port,name,mem) =
  add_socket sock (id,host,port)

let add_file_desc fd = mk_simple_sock_info fd

let get_id_in_locs ident locations =
  match ident with
    Mac (ip, port) ->
      (try
	Some
	  (List.find
	     (fun (id,thost,tport) ->
	       let h = NT.dest_ihost thost in
	       let p = NT.dest_iport tport in
   (*val _ = print ("-" ^ h ^ "-" ^ Int.toString p ^ "-\n")*)
	       ip = h && port = p)
	     locations)
      with _ -> None)
  | Loc loc ->
      (try Some (List.find (fun (id,thost,tport) -> NT.dest_id id = loc) locations)
      with _ -> None)

let get_ip_port_in_locs loc locations =
    get_id_in_locs (Loc (NT.dest_id loc)) locations

let mk_client_sock_info sock addr =
  let (host,port) = dest_inet_sockaddr addr in
  let shost       = Unix.string_of_inet_addr host in
  mk_sock_info "client" shost port sock

let get_socket_info sock =
  let stats : Unix.stats = Unix.fstat sock in
  let kind  = stats.Unix.st_kind in
  if kind = Unix.S_SOCK
  then
    let name = Unix.getsockname sock in
    let peer = Unix.getpeername sock in
    let nstr = addr_to_string name in
    let pstr = addr_to_string peer in
    let str  = "(kind:socket,name:" ^ nstr ^ ",peer:" ^ pstr ^ ")" in
    let send =
      fun str ->
	let n = String.length str in
	let _ = Unix.set_nonblock sock in
	let m = Unix.send sock str 0 n [] in
	let _ = Unix.clear_nonblock sock in
	(n,m) in
    let rcv  = 
      fun n ->
	let str = String.create n in
	(str, Unix.recv sock str 0 n []) in
    (send,rcv,str)
  else if kind = Unix.S_FIFO
  then
    let str  = "(kind:pipe)" in
    let send =
      fun str ->
	let n = String.length str in
	let m = Unix.write sock str 0 n in
	(n,m) in
    let rcv  =
      fun n ->
	let str = String.create n in
	(str, Unix.read sock str 0 n) in
    (send,rcv,str)
  else failwith "get_socket_info:file_desc_kind_unrecognized"


(* ------ CONFIGURATION FILE ------ *)

let get_locs_from_groups groups =
  List.fold_right
    (fun (name,mem,locs) lst ->
      (List.map (fun (i,h,p) -> (i,h,p,name,mem)) locs) @ lst)
    groups
    []

let get_sp_locs_from_groups groups =
  List.fold_right
    (fun (name,mem,locs) lst -> locs @ lst)
    groups
    []

let get_internal_locs_from_groups groups =
  List.fold_right
    (fun (name,mem,locs) lst ->
      if mem
      then (List.map (fun (i,h,p) -> (i,h,p,name,mem)) locs) @ lst
      else lst)
    groups
    []

let get_sp_internal_locs_from_groups groups =
  List.fold_right
    (fun (name,mem,locs) lst -> if mem then locs @ lst else lst)
    groups
    []

let get_external_locs_from_groups groups =
  List.fold_right
    (fun (name,mem,locs) lst ->
      if mem
      then lst
      else (List.map (fun (i,h,p) -> (i,h,p,name,mem)) locs) @ lst)
    groups
    []

let get_sp_external_locs_from_groups groups =
  List.fold_right
    (fun (name,mem,locs) lst -> if mem then lst else locs @ lst)
    groups
    []

(* find the group from which (ip, port) belongs to *)
let find_group (ip, port) groups =
  try
    Some (List.find
	    (fun (name,mem,locs) ->
	      List.exists
		(fun (i,h,p) -> ip = h && port = p)
		locs)
	    groups)
  with _ -> None

let find_lt_groups name conns =
  List.fold_right
    (fun (f,t) names ->
      if name = t
      then f :: names
      else names)
    conns
    []

let find_gt_groups name conns =
  List.fold_right
    (fun (f,t) names ->
      if name = f
      then t :: names
      else names)
    conns
    []

let filter_groups groups names =
  List.filter
    (fun (name,mem,locs) -> List.exists (fun x -> x = name) names)
    groups

let get_group_op groups name =
  try Some (List.find (fun (n,mem,locs) -> n = name) groups)
  with _ -> None

let get_group groups name =
  match get_group_op groups name with
    Some (name,mem,locs) -> locs
  | None -> []

(*
let load_config_parse_locations locs =
  List.map
    (fun (loc, host, port) ->
      (NT.mk_nuprl_mkid_term  loc,
       NT.mk_nuprl_ihost_term host,
       NT.mk_nuprl_iport_term port))
    locs

let load_config_parse_groups groups =
  List.map
    (fun (name, mem, locs) -> (name, mem, load_config_parse_locations locs))
    groups
*)

let old_mk_nuprl_message hdr typ body =
  let pair = NT.mk_pair_term typ body in
  NT.mk_pair_term hdr pair

let mk_nuprl_message hdr typ body = NT.mk_pair_term hdr body

let load_config_parse_messages msgs =
  List.map
    (fun (id, (hdr, typ, body)) ->
      IM.mk_message 0 id (mk_nuprl_message hdr typ body))
    msgs

let load_config_parse config =
    let (grps, conns, params, msgs) = C.parse config in
    let groups   = (*load_config_parse_groups*) grps in
    let messages = load_config_parse_messages msgs in
    (groups, conns, params, messages)

let load_config_parse_str str =
  let (grps,conns,params,msgs) = C.parseString str in
  load_config_parse_messages msgs

let get_id_in_locs ident locations =
  match ident with
    Mac (ip, port) ->
      (try Some (List.find (fun (i,h,p) -> ip = h && port = p) locations)
      with _ -> None)
  | Loc loc ->
      try Some (List.find (fun (i,h,p) -> i = loc) locations)
      with _ -> None

let get_id_in_groups ident groups =
  get_id_in_locs ident (get_sp_internal_locs_from_groups groups)

let get_ip_port_in_locs loc locations =
  get_id_in_locs (Loc loc) locations


(* ------ SOCKETS CLEANING ------ *)

let same_desc sock1 sock2 =
  sock1 = sock2

let rec clean_sockets loc sock lst =
  match lst with
    [] -> []
  | entry :: sockets ->
      if same_desc (select_socket entry) sock
      then
	let _ = print_eml_id loc ("--removing " ^ idNfo2string entry ^ " from socket list")
	in clean_sockets loc sock sockets
      else entry :: (clean_sockets loc sock sockets)

let rec try_clean_sockets loc lst =
  match lst with
    [] -> (false,[])
  | entry :: socks ->
      let sock = select_socket entry in
      let id   = idNfo2string entry in
      try
	let timeout    = (float 1) /. (float 1000) in
	let _          = Unix.getsockname sock in
	let _          = Unix.getpeername sock in
	let _          = Unix.select [sock] [] [] timeout in
	let (b,socks') = try_clean_sockets loc socks in
	(b, entry :: socks')
      with _ -> (print_eml_id loc ("--socket seems dead (" ^ id ^ "), removing it from list");
		 (true, socks))


(* ------ RECEIVE ------ *)

let receive_integer_gen n pref prt_msg desc rcv loc =
  try
    let rec loop b lst =
      let (str,len) = rcv 1 in
      if b && len = 0
      then (print_eml_id loc ("--error, received empty vector(" ^ string_of_int n ^ ")");
	    raise (RCV_INT desc))
      else if len = 1
      then
	let char = String.get str 0 in
	if char = '!'
	then lst
	else loop false (lst @ [char])
      else failwith ("receive_integer(" ^ string_of_int n ^ ")"
		     ^ ":bad_length"
		     ^ ":expecting_message_length_1_received_" ^ string_of_int len
		     ^ ":received_so_far_" ^ implode lst
		     ^ ":" ^ prt_msg
		     ^ "\n") in
    let chars = loop true [] in
    let str   = pref ^ implode chars in
    let _     = print_eml_id loc ("--received integer " ^ str ^ " (" ^ prt_msg ^ ")") in
    (
     try Some (int_of_string str)
     with _ -> None
    )
  with err ->
    (print_eml_id loc ("--error, cannot receive integer, trying to recover(" ^ string_of_int n ^ ")");
     raise (RCV_SYS desc))

let receive_integer loc rcv str sock =
  receive_integer_gen
    1
    ""
    str
    sock
    rcv
    loc

let receive_one_message loc sock =
  let (_,rcv,debug) = get_socket_info sock in
  match receive_integer loc rcv debug sock with
    None -> failwith "receive_message:not_an_int"
  | Some n ->
      try
	let rec aux n s =
	  if n = 0
	  then s
	  else if n < 0
	  then failwith "receive_message_bad_length(negative)"
	  else
	    let (str,m) = rcv n in
	    let _ = print_eml_id loc ("--received " ^ string_of_int m ^ " out of " ^ string_of_int n) in
	    if m = 0
	    then aux n s
	    else aux (n - m) (s ^ (String.sub str 0 m)) in
	let _ = print_eml_id loc ("--ready to receive message of length " ^ string_of_int n) in
	aux n ""
      with err ->
	let _ =
	  (match err with
	    Failure str -> print_eml_id loc ("--error while receiving message: failure(" ^ str ^ ")")
	  | _ -> print_eml_id loc ("--error while receiving message: not a failure")) in
	let _ = print_eml_id loc ("--error while receiving vector, trying to recover") in
	raise (RCV_SYS sock)

let rec receive_messages loc sockets =
  match sockets with
    [] ->
      let str = "no more sockets to read from" in
      (print_eml_id loc str; failwith str)
  | _ ->
      try
	let (rds,wrs,exs) = Unix.select (get_socks sockets) [] [] (float (-1)) in
	(match rds with
	  [] -> failwith "receive_messages:select:nil"
	| socks ->
	    let _ = print_eml_id loc ("--ready to receive from " ^ string_of_int (List.length socks) ^ " sockets") in
	    let strings = List.map (receive_one_message loc) socks in
	    (String.concat "\n" strings, sockets))
      with
	RCV_INT desc ->
	  let _        = print_eml_id loc "--cleaning socket list" in
	  let sockets' = clean_sockets loc desc sockets in
	  let _        = print_eml_id loc "--retrying receive with new socket list" in
	  receive_messages loc sockets'
      | RCV_SYS desc ->
	  let _        = print_eml_id loc "--cleaning socket list" in
	  let sockets' = clean_sockets loc desc sockets in
	  let _        = print_eml_id loc "--retrying receive with new socket list" in
	  receive_messages loc sockets'
      | err ->
	  let _ = print_eml_id loc "--error, cannot receive message (sockets)" in
	  let _ = print_eml_id loc "--trying to find dead sockets" in
	  let (b,sockets') = try_clean_sockets loc sockets in
	  if b
	  then (print_eml_id loc "--retrying receive with new socket list";
		receive_messages loc sockets')
	  else (print_eml_id loc "--system error, but sockets seem fine, failing";
		raise err)

let receive_messages' loc sockets =
  let (msgs,_) = receive_messages loc sockets in msgs


(* ------ DEBUG ------ *)

let parse_message_debug n string prt =
  try PN.parseString prt [] string
  with exn ->
    (print_string ("\n--parse_message_debug:caught_error("
		   ^ string_of_int n
		   ^ ")----------------------\n"
		   ^ string
		   ^ "\n--------------------------\n"
		   ^ "--re-raising error----------------------\n");
     raise exn)


(* ------ MESSAGE PACKING ------ *)

let pack_message str =
  let len   = String.length str in
  let slen  = string_of_int len ^ "!" in
  (slen, str)

let pack_nuprl_message loc msg =
  let str = NT.spToStringTerm msg in
  (*let _ = print_eml_id loc ("--packing message:" ^ str) in*)
  pack_message str


(* ------ SEND ------ *)

let send_something loc sock str =
  try
    let (rds,wrs,exs) = Unix.select [] [sock] [] (float (-1)) in
    match wrs with
      [_] ->
	let (send,_,_) = get_socket_info sock in
	let (n,m) = send str in
	if n = m
	then print_eml_id loc ("--successfully sent " ^ string_of_int m ^ "bytes")
	else print_eml_id loc ("--send_message:bag length(sent " ^ string_of_int m ^ ", should have sent " ^ string_of_int n ^ ")")
    | lst -> (print_eml_id loc ("--send_something:error: " ^ string_of_int (List.length lst) ^ " sockets writable");
	      failwith "send_something")
  with _ -> print_eml_id loc ("--error, cannot send message")

let wait_before_sending loc delay =
  if delay > 0
  then
    let _ = print_eml_id loc ("sending message in " ^ string_of_int delay ^ "s") in
    let _ = Unix.sleep delay in
    ()
  else ()

let send_message id sock loc delay msg =
  try
    let _ = print_eml_id id ("sending message to " ^ loc) in
    let (slen, slice) = pack_nuprl_message loc msg in
    let n = Unix.fork () in
    if n = 0 (* then child *)
    then
      let _ = wait_before_sending id delay in
      let _ = send_something id sock slen in
      let _ = send_something id sock slice in
      exit 0
    else print_eml_id id "forked, child will send the message"
  with _ ->
    print_eml_id id "ignoring external send request because an unknown error occured"

(* id sends msg to loc after delay seconds *)
let send_nuprl_external_message id sockets loc delay msg =
  try
    let nfo = List.find (filter_socks loc) sockets in
    send_message id (select_socket nfo) loc delay msg
  with Not_found ->
    (*(print (sockets_to_string sockets); raise Fail "send_message")*)
    print_eml_id id "recipient does not exist anymore, ignoring send request"

let send_nuprl_internal_message loc outfd delay msg =
  try
    let _            = print_eml_id loc ("sending internal message (" ^ loc ^ ")") in
    let (slen,slice) = pack_nuprl_message loc msg in
    let (send,_,_)   = get_socket_info outfd in
    let n = Unix.fork () in
    if n = 0 (* then child *)
    then
      let _ = wait_before_sending loc delay in
      let _ = send slen in
      let _ = send slice in
      exit 0
    else (* parent *) print_eml_id loc "forked, child will send the message"
  with _ ->
    print_eml_id loc "ignoring internal send request because an unknown error occured"

let rec send_nuprl_messages loc outfd sockets terms =
  match terms with
    [] -> []
  | term :: terms ->
      let (delay,id,msg) = IM.dest_message (IM.term2message term) in
      if loc = id
      then
	let _ = send_nuprl_internal_message id outfd delay msg
	in send_nuprl_messages loc outfd sockets terms
      else
	let _ = send_nuprl_external_message loc sockets id delay msg in
	send_nuprl_messages loc outfd sockets terms

let rec send_nuprl_messages_to_itself loc outfd delay lst =
  match lst with
    [] -> print_eml_id loc ("sent internal messages (" ^ loc ^ ")")
  | msg :: msgs ->
      let _ = send_nuprl_internal_message loc outfd delay msg in
      send_nuprl_messages_to_itself loc outfd delay msgs

let send_port_number loc portop sock =
  match portop with
    Some port ->
      let pstr   = string_of_int port in
      let pslice = pstr ^ "!" in
      let _      = print_eml_id loc ("--sending port number " ^ pstr) in
      let _      = send_something loc sock pslice in
      ()
  | None -> ()


(* ------ NEW CONNECTIONS ------ *)

let handle_new_connection loc gc outfd sock addr =
  try
    let saddr = addr_to_string addr in
    let _     = print_eml_id loc ("--received new connection request from " ^ saddr) in
    let nfo   = mk_client_sock_info sock addr in
    let rec loop () =
      let msgstr = receive_messages' loc [nfo] in
      let msgs   = parse_message_debug 1 msgstr false in
      let _      = print_eml_id loc ("--received input messages from " ^ saddr) in
      let _      = print_messages msgs in
      (*val _      = print "[--closing socket]\n"
	val _      = Socket.close sock*)
      let _      = print_eml_id loc "--sending message to myself" in
      let delay  = 0 in (* no delay *)
      let _      = send_nuprl_messages_to_itself loc outfd delay msgs in
      loop () in
    loop ()
  with _ -> (print_eml_id loc "connection handler died"; exit 0)


(* ------ LISTENER ------ *)

let rec keep_listening_for_inputs_aux loc gc outfd server =
  try
    let (sock,addr) = Unix.accept server in
    let n = Unix.fork () in
    if n = 0
    then handle_new_connection loc gc outfd sock addr (* child *)
    else keep_listening_for_inputs_aux loc gc outfd server
  with _ -> (print_eml_id loc "listner died"; exit 0)

let keep_listening_for_inputs loc gc outfd server =
  let _ = reset_all () in
  let _ = if gc then Gc.minor () else () in
  keep_listening_for_inputs_aux loc gc outfd server

let rec split_in_and_out_locs lst =
  match lst with
    [] -> ([],[])
  | (((id,host,port,name,mem) as x) :: lst) ->
      let (inlocs,outlocs) = split_in_and_out_locs lst in
      if mem
      then (x :: inlocs, outlocs)
      else (inlocs, x :: outlocs)

let rec waiting_on_connection loc socket timer =
  if get_time timer > (float 60)
  then None
  else
    let sockaddrop = try Some (Unix.accept socket) with _ -> None in
    match sockaddrop with
      Some (sock, addr) ->
	let _ = add_a_sock_ref sock in
	let _ = print_eml_id loc ("--received connection request from " ^ addr_to_string addr) in
	let (_,rcv,debug) = get_socket_info sock in
	let client_port =
	  match receive_integer loc rcv debug sock with
	    Some p -> p
	  | None -> failwith "waiting_on_connection:received_non_int_data" in
	let _           = print_eml_id loc ("--received port number " ^ string_of_int client_port) in
	Some (sock,addr,client_port)
    | None ->
	let retry_limit = 2 in
	let st   = string_of_int retry_limit in
	let _    = print_eml_id loc ("--no pending connection request, will retry in " ^ st ^ "s") in
	let _    = Unix.sleep retry_limit in
	let _    = print_eml_id loc ("--ready to retry accept") in
	waiting_on_connection loc socket timer

let print_location_list lst =
  "[" ^ (String.concat "," (List.map (fun (i,h,p,name,mem) -> i) lst)) ^ "]"

let remove_address_from_list addr port lst =
  let (ip,_) = dest_inet_sockaddr addr in
  let ips = Unix.string_of_inet_addr ip in
  List.partition
    (fun (i,h,p,name,mem) -> h = ips && p = port)
    lst

let rec waiting_for_connections loc server lst =
  match lst with
    [] -> []
  | _ ->
      let n                = string_of_int (List.length lst) in
      let _                = print_eml_id loc ("--waiting on " ^ n ^ " connection(s)") in
      let timer            = start_timer () in
      match waiting_on_connection loc server timer with
	Some (sock,addr,port) ->
	  let (connected,lst') = remove_address_from_list addr port lst in
	  let sockets          = List.map (add_socket_nfo sock) connected in
	  sockets @ (waiting_for_connections loc server lst')
      | None ->
	  let (inlocs,outlocs) = split_in_and_out_locs lst in
	  let _ =
	    print_eml_id
	      loc
	      ("--waiting on connection timed out (1 minute)"
	       ^ ", did not connect to:"
	       ^ print_location_list inlocs
	       ^ ", still waiting for external connections") in
	  let _ = Unix.clear_nonblock server in
	  waiting_for_connections loc server outlocs

let create_new_socket ip port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in (* a file_descr *)
  let addr = 
    try Unix.inet_addr_of_string ip
    with _ -> failwith ("create_server_socket:not_a_internet_address(" ^ ip ^ ")") in
  let sock_addr = Unix.ADDR_INET (addr, port) in
  let saddr     = addr_to_string sock_addr in
  (sock, sock_addr, saddr)

(* connect to server using socket.
 * It differs from connect by the fact that if it fails to connect,
 * it creates a new socket. *)
let rec connect' loc socket id server timer =
  if get_time timer > (float 40)
  then
    (print_eml_id
       loc
       ("--aborting connection to "
	^ id
	^ ", already tried for 1 minute"); None)
  else
    try
      let s = addr_to_string server in
      let _ = print_eml_id loc ("--n-connecting to " ^ s ^ "(" ^ id ^ ")") in
      let _ = Unix.connect socket server in
      Some socket
    with _ ->
      let retry_limit = 2 in
      let st   = string_of_int retry_limit in
      let _    = print_eml_id loc ("--n-cannot connect, will retry in " ^ st ^ "s") in
      let _    = Unix.sleep retry_limit in
      let _    = print_eml_id loc ("--n-ready to retry connect, creating a new socket") in
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      connect' loc sock id server timer

(* loc connects to server of id using socket *)
let rec connect loc socket id server timer =
  if get_time timer > (float 40)
  then
    (print_eml_id
       loc
       ("--aborting connection to "
	^ id
	^ ", already tried for 1 minute"); None)
  else
    try
      let s = addr_to_string server in
      let _ = print_eml_id loc ("--connecting to " ^ s ^ "(" ^ id ^ ")") in
      let _ = Unix.connect socket server in
      Some socket
    with _ ->
      let retry_limit = 2 in
      let st = string_of_int retry_limit in
      let _  = print_eml_id loc ("--cannot connect, will retry in " ^ st ^ "s") in
      let _  = Unix.sleep retry_limit in
      let _  = print_eml_id loc ("--ready to retry") in
      connect loc socket id server timer

let rec connect_to loc port nsock lst =
  match lst with
    [] -> []
  | ((i,h,p,name,mem) as nfo) :: lst ->
      let n      = string_of_int (List.length lst + 1) in
      let _      = print_eml_id loc ("--still " ^ n ^ " machine(s) to connect to") in
      (* -- generates sock (a new socket) where h is the host we have to connect to -- *)
      let (sock, server, saddr) = create_new_socket h p in
      (* -- tries to connect to server using socket sock -- *)
      let timer  = start_timer () in
      let sockop =
	if nsock
	then connect' loc sock i server timer
	else connect  loc sock i server timer in
      match sockop with
	Some sock ->
	  (* -- sends port number to server -- *)
	  let _ = send_port_number loc port sock in
	  (add_socket_nfo sock nfo) :: (connect_to loc port nsock lst)
      | None -> connect_to loc port nsock lst

let create_server_socket loc (ip, port) =
  let (sock, sock_addr, saddr) = create_new_socket ip port in
  let _ = print_eml_id loc ("created server socket at " ^ saddr) in
  let _ = Unix.setsockopt sock Unix.TCP_NODELAY  true in
  let _ = Unix.setsockopt sock Unix.SO_REUSEADDR true in
  let _ = Unix.bind sock sock_addr in
  let _ = print_eml_id loc "binding" in
  let _ = Unix.listen sock 100 in
  let _ = print_eml_id loc "listening" in
  let _ = add_p_sock_ref sock in
  (sock, sock_addr)

let getParams params map_params =
  List.map
    (fun param ->
      try let (id,v) = List.find (fun (id, _) -> id = param) map_params in v
      with _ -> failwith ("get_params(" ^ param ^ ")"))
    (
     let r      = List.rev params in
     let last   = List.hd r in
     let firsts = List.rev (List.tl r) in
     if last = "i"
     then firsts
     else params
    )

let rec nuprl_all t   =
  if NT.is_nuprl_iclosure_term t
  then
    let (a,e) = NT.dest_iclosure t in
    NT.mk_nuprl_iclosure_term (nuprl_all a) e
  else NT.mk_nuprl_callbyvalueall_term t ("x", NT.mk_variable_term "x")

let rec nuprl_app p t =
  if NT.is_nuprl_iclosure_term p
  then
    let (a,e) = NT.dest_iclosure p in
    NT.mk_nuprl_iclosure_term (nuprl_app a t) e
  else NT.mk_apply_term p t

let fst (a,b) = a
let snd (a,b) = b

let load_program ev gc ident params loc extra =
  match !program with
    Some (prms, p) ->
      let id    = NT.mk_mkid_term loc in
      let args  = getParams prms params in
      let _     = print_eml_id loc (string_of_int (List.length args) ^ " parameters") in
      let prog1 = NT.mk_nuprl_applies_term p args in
      let prog2 =
	if newprog_extra extra
	then
	  let _ = print_eml_id loc "applying parameters to process" in
	  nuprl_app prog1 id
	else
	  let _ = print_eml_id loc "generating program meaning" in
	  NT.mk_nuprl_df_program_meaning_term (nuprl_app prog1 id) in
      let prog3 =
	if is_fold_extra extra
	then prog2
	else
	  let _     = print_eml_id loc "unfolding term" in
	  let lib   = EV.get_lib () in
	  let _     = NT.print_lib_stats lib in
	  let prog3 = NT.unfold_all lib prog2 in
	  let _     = print_eml_id loc "term unfolded" in
	  let _     = print_eml_id loc "unloading library" in
	  let _     = EV.reset_lib () in
	  let _     = if gc then Gc.minor() else () in
	  let _     = print_eml_id loc "library unloaded" in
	  prog3 in
      let prog4 = fst (ev [] (-1) prog3) in
      let _     = print_eml_id loc "component loaded" in
      let _     = print_eml_id loc ("size: " ^ string_of_int (NT.size prog4)) in
      if newprog_extra extra
      then
	let prog5 = NT.partial_ev_opt prog4 in
	let size  = string_of_int (NT.size prog5) in
	let _     = print_eml_id loc ("size(after partial evaluation):" ^ size) in
	prog5
      else prog4
  | None -> failwith "[no program]"

let rec split_locations_in_group (ip, port) name mem lst =
  match lst with
    [] -> ([], [])
  | (i,h,p) :: lst ->
    let (less, greater) = split_locations_in_group (ip, port) name mem lst in
    let n = String.compare ip h in
    if n < 0
    then (less, (i,h,p,name,mem) :: greater)
    else if n = 0
    then
      let m = compare port p in
      if m < 0
      then (less, (i,h,p,name,mem) :: greater)
      else if m = 0
      then (less, greater)
      else ((i,h,p,name,mem) :: less, greater)
    else ((i,h,p,name,mem) :: less, greater)

let split_locations (ip, port) groups conns =
  match find_group (ip, port) groups with
    Some (name, mem, locs) ->
      let (lt1, gt1) = split_locations_in_group (ip, port) name mem locs in
      let lt_groups = find_lt_groups name conns in
      let gt_groups = find_gt_groups name conns in
      let lt2 = get_locs_from_groups (filter_groups groups lt_groups) in
      let gt2 = get_locs_from_groups (filter_groups groups gt_groups) in
      (lt1 @ lt2, gt1 @ gt2)
  | None -> ([], [])

let load_program_from_file loc spec lib alldefs prt =
  (* -- load library and generates program -- *)
  let _     = print_eml_id loc ("loading program and library") in
  let terms =
    match PN.parse prt [] spec false with
      (prog :: rest) ->
	let (vars, _) = NT.dest_lambdas prog in
	let _ = program := Some (vars, prog) in
	rest
    | _ -> failwith "error, no program" in
  let _     =
    match alldefs with
      "" -> failwith "error, no library"
    | _ ->
	let _   = print_eml_id loc "parsing Nuprl library" in
	let ts  = PN.parse prt NT.to_filter_out alldefs false in
	let _   = print_eml_id loc "generating EML library" in
	let lib = NT.terms2map ts in
	let _   = NT.print_lib_stats lib in
	let _   = EV.start_session_lib lib in
	(*let _   = NT.dump_lib_termofs "output-termofs" lib in*)
	let _   = print_eml_id loc "library loaded" in
	(*let _   = print_eml_id loc (if NT.is_in_lib lib "at-prc" then "at-prc in lib" else "at-prc not in lib") in*)
	let _   = loaded := true in
	() in
  (* -- Add programs to library -- *)
  let _     = EV.add_to_session terms in
  ()

let boot_up_prog ev gc ident spec lib alldefs id params extra prt =
  let _ = load_program_from_file id spec lib alldefs prt in
  (* -- Load component -- *)
  let _ = print_eml_id id "loading component" in
  load_program ev gc ident params id extra

let boot_up_conn loc nsock ip port groups conns =
  (* -- Split the locations into those with lower ip/port and those with greater -- *)
  let (lt, gt) = split_locations (ip, port) groups conns in
  let lt_size  = string_of_int (List.length lt) in
  let gt_size  = string_of_int (List.length gt) in
  let _        = print_eml_id loc (lt_size ^ " lower machine(s), " ^ gt_size ^ " higher machine(s)") in
  (* -- create a server socket -- *)
  let _             = print_eml_id loc "starting sever socket" in
  let (server,addr) = create_server_socket loc (ip, port) in
  let _       = print_eml_id loc "connecting to higher machines" in
  let lst_gt  = connect_to loc (Some port) nsock gt in
  (* -- wait to get the connections from machines with lower ip/port -- *)
  let _       = print_eml_id loc "waiting on connections from lower machines" in
  let _       = Unix.set_nonblock server in
  let lst_lt  = waiting_for_connections loc server lt in
  let _       = Unix.clear_nonblock server in
  (server, lst_lt @ lst_gt)

let boot_up ev gc ident config spec lib alldefs extra prt =
  (* ------ reading configuration file ------ *)
  let idstr = ident_to_string ident in
  let _     = print_eml_id idstr "loading configuration file" in
  let (groups,conns,params,msgs) = load_config_parse config in
  let _     = print_eml_id idstr "configuration file loaded" in
  let (loc,ip,port) =
    match get_id_in_groups ident groups (* extract own location *) with
      Some (id,host,port) -> (id,host,port)
    | None -> failwith "[unknown location]\n" in
  (* -- *)
  (* ------ generating connections ------ *)
  let _     = print_eml_id loc "connecting" in
  let nsock = is_nsock_extra extra in
  let (server, sockets) = boot_up_conn loc nsock ip port groups conns in
  let (infd,outfd) = Unix.pipe () in
  (*let _     = Unix.set_nonblock infd in*)
  let n = Unix.fork () in
  let _ =
    if n = 0  (* then child *)
    then keep_listening_for_inputs loc gc outfd server
    else print_eml_id loc ("forking listner: " ^ string_of_int n) in
  (* -- *)
  (* ------ loading program ------ *)
  let _     = print_eml_id loc ("loading program") in
  let prog  = boot_up_prog ev gc ident spec lib alldefs loc params extra prt in
  (loc, prog, server, (add_file_desc infd) :: sockets)


(* ------ DUMP ------ *)

let dump_prog id n_terms prog fdump =
  let time   = Unix.time () in
  let output = "output-term-" ^ id ^ "-" ^ string_of_float time ^ "debug" in
  let _      = print_eml_id id ("dumping program in " ^ output) in
  let stout  = open_out output in
  let _      = output_string stout (fdump prog ^ "\004\n\n") in
  let _      = List.map (fun t -> output_string stout (fdump t ^ "\004\n")) n_terms in
  let _      = close_out stout in
  ()


(* ------ RUN PROCESS ------ *)

let get_non_halted_prog loc extra prog =
  if newprog_extra extra
  then
    if NT.is_nuprl_inl_term prog
    then Some (NT.dest_inl prog)
    else if NT.is_nuprl_inr_term prog
    then None
    else (dump_prog loc [] prog NT.toStringTerm;
	  failwith ("run_on_messages:newprog:not_inl_or_inr("
		    ^ NT.opid_of_term prog
		    ^ ")"))
  else Some prog

let rec run_on_messages extra loc outfd ev progop sockets messages =
  match progop, messages with
    None, _ -> None
  | _, [] -> progop
  | Some prog, msg :: msgs ->
      match get_non_halted_prog loc extra prog with
	None -> None
      | Some prog ->
	  let _             = print_eml_id loc "received message" in
	  let _             = print_eml2 (NT.nuprlTerm2eml msg) in
	  let toeval        = nuprl_all (nuprl_app prog msg) in
	  let (prog1,steps) = ev [] (-1) toeval in
	  let _             = print_eml_id loc (string_of_int steps ^ " steps") in
	  let (prog2,out)   = NT.dest_pair 7 prog1 in
	  let lstmsgs       = NT.dest_list out in
	  let slen          = string_of_int (List.length lstmsgs) in
	  let _             = print_eml_id loc ("sending messages from " ^ loc ^ " (" ^ slen ^ ")") in
	  let _             = print_message_list 1 lstmsgs in
	  let _             = send_nuprl_messages loc outfd sockets lstmsgs in
	  let _             = print_eml_id loc "messages sent" in
	  run_on_messages extra loc outfd ev (Some prog2) sockets msgs

let run_distributed_program2 ev gc ident config spec lib alldefs prt extra =
  let idstr = ident_to_string ident in
  let _     = print_eml_id idstr "booting up" in
  let (loc,process,server,sockets) =
    boot_up ev gc ident config spec lib alldefs extra prt in
  let (infd,outfd) = Unix.pipe () in
  let _ = print_eml_id loc "running process" in
  let rec loop sockets progop =
    match progop with
      None -> print_eml_id loc "program finished"
    | Some prog ->
	let _ = if gc then Gc.minor() else () in
	let _ = print_eml_id loc ("process " ^ loc ^ " waiting for a new message") in
	let (msgs_str,sockets') = receive_messages loc sockets in
	let msgs  = parse_message_debug 3 msgs_str prt in
	let _ = print_eml_id loc "parsed messages" in
	let prog' = run_on_messages extra loc outfd ev (Some prog) sockets' msgs in
	loop sockets' prog' in
  loop ((add_file_desc infd) :: sockets) (Some process)

let run_distributed_program extra ev gc ident config spec lib alldefs prt =
  let idstr = ident_to_string ident in
  (run_distributed_program2 ev gc ident config spec lib alldefs prt extra;
   print_eml_id idstr "process terminated without error")

let simulate_clients_server config prt =
  let (groups,_,_,_) = load_config_parse config in
  let _       = print_eml "--connecting to other machines" in
  let sockets = connect_to "clients" None false (get_external_locs_from_groups groups) in
  let _       = print_eml "--ready to receive messages from other machines" in
  let rec loop sockets =
    let (msgs_str,sockets') = receive_messages "client" sockets in
    let msgs = parse_message_debug 6 msgs_str prt in
    let _ = print_string ("\n-------------\n"
			  ^ "received: "
			  ^ String.concat "\n--\n" (List.map NT.nuprlTerm2eml msgs)
			  ^ "\n-------------\n") in
    loop sockets' in
  loop sockets

let rec send_intransit_messages locs lst =
  match lst with
    [] -> ()
  |  message :: msgs ->
      let (delay,id,msg) = IM.dest_message message in
      match get_ip_port_in_locs id locs with
	Some ((id,host,port) as nfo) ->
	  let (sock,server,s) = create_new_socket host port in
	  let _       = print_eml ("connecting to " ^ s) in
	  let _       = Unix.connect sock server in
	  let sockets = [add_socket sock nfo] in
	  let _       = print_eml ("sending message to " ^ s) in
	  let _       = send_nuprl_external_message "intransit" sockets id delay msg in
	  let _       = print_eml "message sent" in
	  let _       = print_eml "closing socket" in
	  let _       = Unix.close sock in
	  let _       = print_eml "socket closed" in
	  send_intransit_messages locs msgs
      | None ->
	  let _ = print_eml "unknown location" in
	  send_intransit_messages locs msgs

let simulate_clients_send config alldefs prt gc =
  let lib =
    match alldefs with
    | "" -> (print_eml "no library"; NT.emlib ())
    | f ->
      let _     = print_eml ("parsing Nuprl library") in
      let terms = PN.parse prt NT.to_filter_out f false in
      let _     = print_eml ("generating EML library") in
      let lib   = NT.terms2map terms in
      let _     = NT.print_lib_stats lib in
      lib in
  let cid = ref 1 in
  let input aux =
    let _    = print_string "send? " in
    (match read_line () with
      ""    -> aux None
    | "sp" ->
	let c = string_of_int (!cid) in
	let _ = cid := !cid + 1 in
	aux (Some ("rep1 : (``swap``, (Int * Tok List), (" ^ c ^ ",``paxos``))"))
    | "st" ->
	let c = string_of_int (!cid) in
	let _ = cid := !cid + 1 in
	aux (Some ("rep1 : (``swap``, (Int * Tok List), (" ^ c ^ ",``2/3``))"))
    | "c" ->
	let c = string_of_int (!cid) in
	let _ = cid := !cid + 1 in
	aux (Some ("rep1 : (``bcast``, (Int * Tok List), (" ^ c ^ ",``" ^ c ^ "``))"))
    | "p" ->
	let c = string_of_int (!cid) in
	let _ = cid := !cid + 1 in
	aux (Some ("ldr1 : (``propose``, (Int * Tok List), (" ^ c ^ ",``" ^ c ^ "``))"))
    | msg   -> aux (Some msg)) in
  let rec aux msgop =
    let (groups,_,_,msgs) = load_config_parse config in
    let locs = get_sp_internal_locs_from_groups groups in
    let _    = print_eml "selecting message" in
    let msgs =
      match msgop with
	None -> msgs
      | Some msg -> load_config_parse_str msg in
    let _    = print_eml "selected message" in
    let time = Unix.gettimeofday () in
    let _    = print_eml ("unfolding messages(" ^ string_of_float time ^ "s)") in
    let msgs = unfold_intransit_messages lib msgs in
    let _    = if gc then Gc.minor () else () in
    let time = Unix.gettimeofday () in
    let _    = print_eml ("sending intransit messages(" ^ string_of_float time ^ "s)") in
    let _    = send_intransit_messages locs msgs in
    input aux in
  input aux

let run_other_machine extra ident config prt =
  let (groups,_,_,_) = load_config_parse config in
  let locs  = get_internal_locs_from_groups groups in
  let locs' = get_sp_external_locs_from_groups groups in
  (* -- checks whethere ip/port is in the configuration file -- *)
  let nsock = is_nsock_extra extra in
  let str   = ident_to_string ident in
  let (loc,host,port) =
    match get_id_in_locs ident locs' with
      Some (id,host,port) -> (id,host,port)
    | None -> failwith ("[unknown location " ^ str ^ "]") in
  let _ = print_eml_id loc "starting sever socket" in
  (* -- creates a server socket -- *)
  let (sock,addr) = create_server_socket loc (host, port) in
  let _ = print_eml_id loc "connecting to machines" in
  (* -- connects to the other machines -- *)
  let sockets = connect_to loc (Some port) nsock locs in
  (* -- waits on a connection request from some client -- *)
  let (client,caddr) = Unix.accept sock in
  let scaddr  = addr_to_string caddr in
  let _       = print_eml_id loc ("--received connection request from " ^ scaddr) in
  (* -- forwards to client messages received from internal locations --  *)
  let rec internal sockets =
    let (msgs_str,sockets') = receive_messages loc sockets in
    let time = Unix.gettimeofday () in
    let _    = print_eml_id loc ("received message at " ^ string_of_float time ^ "s") in
    let msgs = parse_message_debug 7 msgs_str prt in
    let _    = print_string ("\n-------------\nreceived:\n"
			     ^ String.concat "\n--\n" (List.map NT.nuprlTerm2eml msgs)
			     ^ "\n-------------\n") in
    let delay = 0 (* just forwarding, no delay *) in
    let _ = List.iter (send_message loc client "client" delay) msgs in
    internal sockets' in
  internal sockets

let start_all_machines ev gc config spec lib alldefs prt extra =
  try
    let (groups,_,_,_) = load_config_parse config in
    let locs  = get_sp_internal_locs_from_groups groups in
    let locs' = get_sp_external_locs_from_groups groups in
    let _ =
      List.iter
	(fun (id,_,_) ->
	  let m = Unix.fork () in
	  if m = 0
	  then
	    let _ = run_other_machine extra (Loc id) config prt
	    in raise DONE
	  else ())
	locs' in
    let _ =
      List.iter (fun (id,_,_) ->
	let m = Unix.fork () in
	if m = 0
	then
	  let _ = run_distributed_program "" ev gc (Loc id) config spec lib alldefs prt
	  in raise DONE
	else ())
	locs in
    ()
  with DONE -> ()

(* ------ EVALUATORS ------ *)

let eval_wrap ev ts n t =
  let timer = start_timer () in
  let res   = ev ts n t in
  let _     = print_eml ("timer:" ^ string_of_time timer ^ "ms") in
  res

let ev1  = EV.run_ev1_map
let ev2  = EV.run_ev2_map
let ev2b = EV.run_ev2b_map
let ev2c = EV.run_ev2c_map
let ev2d = EV.run_ev2d_map
let ev3  = EV.run_ev3_map
let ev3b = EV.run_ev3b_map
let ev3c = EV.run_ev3c_map
let ev3d = EV.run_ev3d_map
let ev4  = EV.run_ev4_map
let ev4b = EV.run_ev4b_map
let ev5  = EV.run_ev5_map
let ev5b = EV.run_ev5b_map

let ev1  = eval_wrap ev1
let ev2  = eval_wrap ev2
let ev2b = eval_wrap ev2b
let ev2c = eval_wrap ev2c
let ev2d = eval_wrap ev2d
let ev3  = eval_wrap ev3
let ev3b = eval_wrap ev3b
let ev3c = eval_wrap ev3c
let ev3d = eval_wrap ev3d
let ev4  = eval_wrap ev4
let ev4b = eval_wrap ev4b
let ev5  = eval_wrap ev5
let ev5b = eval_wrap ev5b

let get_ev ev =
  match ev with
    "ev1"  -> ("1",  ev1)
	(* 2nd evalutator *)
  | "ev2"  -> ("2",  ev2)
  | "ev2b" -> ("2b", ev2b)
  | "2b"   -> ("2b", ev2b)
  | "ev2c" -> ("2c", ev2c)
  | "2c"   -> ("2c", ev2c)
  | "ev2d" -> ("2d", ev2d)
  | "2d"   -> ("2d", ev2d)
	(* 3rd evalutator *)
  | "ev3"  -> ("3",  ev3)
  | "ev3b" -> ("3b", ev3b)
  | "3b"   -> ("3b", ev3b)
  | "ev3c" -> ("3c", ev3c)
  | "3c"   -> ("3c", ev3c)
  | "ev3d" -> ("3d", ev3d)
  | "3d"   -> ("3d", ev3d)
	(* 4th evalutator *)
  | "ev4"  -> ("4",  ev4)
  | "ev4b" -> ("4b", ev4b)
  | "4b"   -> ("4b", ev4b)
	(* 5th evalutator *)
  | "ev5"  -> ("5",  ev5)
  | "ev5b" -> ("5b", ev5b)
  | "5b"   -> ("5b", ev5b)
	(* failure *)
  | _ -> failwith "get_ev"

(* ------ SIMULATOR ------ *)

let rec remove_nth lst n =
  if n < 1
  then failwith "remove_nth"
  else
    match lst with
      [] -> (None, [])
    | (x :: lst) ->
	if n = 1
	then (Some x,lst)
	else
	  let (r,keep) = remove_nth lst (n - 1) in
	  (r, x :: keep)

let update_component (i, s) cmps =
  List.map
    (fun (j, p) -> if i = j then (i, s) else (j, p))
    cmps

let run_stepper n ev gc debug remove config alldefs extra =
  if n < 0
  then
    let _ = configured := false in
    let _ = components := [] in
    let _ = intransit  := [] in
    ()
  else if !loaded
  then
    if !configured
    then
      match !intransit with
	[] -> print_eml "no message in transit"
      | lst ->
	  if n > List.length lst || n < 1
	  then print_eml "out of bound"
	  else if remove
	  then
	    let (rem_msg,new_msgs) = remove_nth lst n in
	    (*let _ =
	      match rem_msg with
		Some m -> print_endline ("-+-\n"
					 ^ NT.toStringTerm m
					 ^ "\n-+-")
	      | None -> () in*)
	    let _ = intransit := new_msgs in
	    ()
	  else
	    let (delay,id,msg) = IM.dest_message (List.nth lst (n - 1)) in
	    try
	      let (i,p) = List.find (fun (i,p) -> i = id) (!components) in
	      let _ = print_eml ("size(1): " ^ string_of_int (NT.size p)) in
	      let pop =
		if newprog_extra extra
		then
		  if NT.is_nuprl_inl_term p
		  then Some (NT.dest_inl p)
		  else if NT.is_nuprl_inr_term p
		  then None
		  else failwith ("run_stepper:newprog:not_inl_or_inr("
				 ^ NT.opid_of_term p
				 ^ ")")
		else Some p in
	      (match pop with
		Some p ->
		  let toeval = nuprl_all (nuprl_app p msg) in
		  let (p',stps) = ev [] (-1) toeval in
		  let (s,msgs) = NT.dest_pair 4 p' in
		  let _  = print_eml ("size(2): " ^ Big_int.string_of_big_int (NT.large_size s)) in
		  let lst_msgs = List.map IM.term2message (NT.dest_list msgs) in
		  let new_cmps = update_component (i, s) (!components) in
		  let new_msgs = snd (remove_nth lst n) @ lst_msgs in
		  let _ = components := new_cmps in
		  let _ = intransit := new_msgs in
		  ()
	      | None -> print_eml "program finished")
	    with Not_found -> print_eml "no recipient"
    else
      let (groups, conns, params, messages) = load_config_parse config in
      (* gets the locations and parameters from the input config file *)
      (*val (locations, params, messages) = load_config config*)
      (* generates the list of components *)
      match !program with
	Some (prms, p) ->
	  let args = getParams prms params in
	  let _    = print_eml ("size initial component: " ^ string_of_int (NT.size p)) in
	  let mng1 = NT.mk_lambda_term "p" (NT.mk_nuprl_df_program_meaning_term (NT.mk_variable_term "p")) in
	  let (prog0,mng2,messages,args) =
	    if is_fold_extra extra
	    then (p, mng1, messages, args)
	    else
	      let _    = print_eml "unfolding" in
	      let lib  = EV.get_lib () in
	      let _    = NT.print_lib_stats lib in
	      (*let b    = NT.is_in_lib lib "aneris_Replica-program" in*)
	      (*let _    = print_eml ("----aneris_Replica-program in library: " ^ string_of_bool b) in*)
	      let prog = NT.unfold_all lib p in
	      let mng2 = NT.unfold_all lib mng1 in
	      let msgs = unfold_intransit_messages lib messages in
	      let args = List.map (NT.unfold_all lib) args in
	      let _    = print_eml ("size after unfolding: " ^ string_of_int (NT.size prog)) in
	      let _    = print_eml ("term stats: " ^ NT.stats_term_to_string prog) in
	      let _    = print_eml "GC library" in
	      let _    = EV.reset_lib () in
	      (prog, mng2, msgs, args) in
	  let prog1 = prog0 in
	  let prog2 = NT.mk_nuprl_applies_term prog1 args in
	  let build id =
	    let mkid = NT.mk_mkid_term id in
	    if newprog_extra extra
	    then
	      let _ = print_eml ("getting process of " ^ id) in
	      nuprl_app prog2 mkid
	    else
	      let _ = print_eml "generating program meaning" in
	      nuprl_app mng2 (nuprl_app prog2 mkid) in
	  let cmps =
	    List.map
	      (fun (id,_,_) ->
		let _ = print_eml ("generatig program for " ^ id) in
		let b = build id in
		let (p,stps) = ev [] (-1) b in
		let _ = print_eml ("size component: " ^ string_of_int (NT.size p)) in
		let p' =
		  if newprog_extra extra
		  then
		    let p' = NT.partial_ev_opt p in
		    let _  = print_eml ("size after partial evaluation: " ^ string_of_int (NT.size p')) in
		    p'
		  else p in
		(id, p'))
	      (get_sp_internal_locs_from_groups groups) in
	  let _ = components := cmps in
	  let _ = intransit  := messages in
	  let _ = configured := true in
	  let _ = print_eml "components loaded" in
	  ()
      | None -> failwith "no program loaded in memory"
  else  (* loads the alldefs file *)
    let _ = EV.start_session true (Some default_alldefs) in
    let _ = loaded := true in
    let _ = print_eml "library loaded" in
    ()

let rec loop_gen_stepper ev gc conf alldefs extra =
  let _ = print_string "message? " in
  loop_stepper ev gc conf alldefs extra

and loop_ev_stepper ev str gc conf alldefs extra =
  let _ = print_string ("\nswitched to evaluator" ^ str ^ "\n") in
  loop_gen_stepper ev gc conf alldefs extra

and loop_help_stepper ev gc conf alldefs extra =
  let _ =
    print_string
      ("\n"
       ^ "  - To send a message enter an integer corresponding to a message in transit."
       ^ "\n"
       ^ "  - To quit, type quit."
       ^ "\n"
       ^ "  - To change the evaluator, type one of these: ev1, ev2, ev2b, ev2c, ev2d, ev3, ev3b, ev3c, ev3d, ev4, ev4b, ev5, ev5b"
       ^ "\n\n") in
  loop_gen_stepper ev gc conf alldefs extra

and loop_stepper ev gc conf alldefs extra =
  match read_line () with
    "quit"     -> (print_string "\nend of session\n"; run_stepper (-1) ev gc false false conf alldefs extra)
  | "exit"     -> (print_string "\nend of session\n"; run_stepper (-1) ev gc false false conf alldefs extra)
  | "aurevoir" -> (print_string "\nend of session\n"; run_stepper (-1) ev gc false false conf alldefs extra)
  | "ev1"      -> loop_ev_stepper ev1  "1"  gc conf alldefs extra
  | "ev2"      -> loop_ev_stepper ev2  "2"  gc conf alldefs extra
  | "ev2b"     -> loop_ev_stepper ev2b "2b" gc conf alldefs extra
  | "2b"       -> loop_ev_stepper ev2b "2b" gc conf alldefs extra
  | "ev2c"     -> loop_ev_stepper ev2c "2c" gc conf alldefs extra
  | "2c"       -> loop_ev_stepper ev2c "2c" gc conf alldefs extra
  | "ev2d"     -> loop_ev_stepper ev2d "2d" gc conf alldefs extra
  | "2d"       -> loop_ev_stepper ev2d "2d" gc conf alldefs extra
  | "ev3"      -> loop_ev_stepper ev3  "3"  gc conf alldefs extra
  | "ev3b"     -> loop_ev_stepper ev3b "3b" gc conf alldefs extra
  | "3b"       -> loop_ev_stepper ev3b "3b" gc conf alldefs extra
  | "ev3c"     -> loop_ev_stepper ev3c "3c" gc conf alldefs extra
  | "3c"       -> loop_ev_stepper ev3c "3c" gc conf alldefs extra
  | "ev3d"     -> loop_ev_stepper ev3d "3d" gc conf alldefs extra
  | "3d"       -> loop_ev_stepper ev3d "3d" gc conf alldefs extra
  | "ev4"      -> loop_ev_stepper ev4  "4"  gc conf alldefs extra
  | "ev4b"     -> loop_ev_stepper ev4b "4b" gc conf alldefs extra
  | "4b"       -> loop_ev_stepper ev4b "4b" gc conf alldefs extra
  | "ev5"      -> loop_ev_stepper ev5  "5"  gc conf alldefs extra
  | "ev5b"     -> loop_ev_stepper ev5b "5b" gc conf alldefs extra
  | "5b"       -> loop_ev_stepper ev5b "5b" gc conf alldefs extra
  | "help"     -> loop_help_stepper ev gc conf alldefs extra
  | line         ->
      let rec aux lst =
	match lst with
	  [] ->
	    (print_messages_intransit ();
	     print_string "message? ";
	     loop_stepper ev gc conf alldefs extra)
	| (input :: inputs) ->
	    let (input',debug,remove) =
	      if String.rcontains_from input 0 'd'
	      then (String.sub input 1 ((String.length input) - 1), true, false)
	      else if String.rcontains_from input 0 'r'
	      then (String.sub input 1 ((String.length input) - 1), false, true)
	      else (input, false, false)
	    in
	    try
	      let n = int_of_string input' in
	      let _ = run_stepper n ev gc debug remove conf alldefs extra in
	      aux inputs
	    with _ ->
	      let _ = print_string "please, enter a nat\n" in
	      let _ = print_string "message? " in
	      loop_stepper ev gc conf alldefs extra in
      aux (Str.split (Str.regexp "[ \n]+") line)

let start_stepper spec conf ev gc lib alldefs prt extra =
  (* loads library and generates program *)
  let _     = load_program_from_file "stepper" spec lib alldefs prt in
  (* evaluators *)
  let _     = print_eml "available evaluators: 1/2/2b/3/3b/3c/3d/4/4b/5/5b" in
  (* loads the components *)
  let _     = print_eml "loading components" in
  let _     = run_stepper 0 ev gc false false conf alldefs extra in
  (* print the list of messages initially in transit *)
  let _     = print_eml "running simulator" in
  let _     = print_messages_intransit () in
  let _     = print_string "message? " in
  (* starts simulator *)
  loop_stepper ev gc conf alldefs extra

(* ------ INTERFACE ------ *)

let run
    {input;  output;  lib;     time;    sub;      sanity;
     nuprl;  obid;    ascii;   tcheck;  parse;    split;
     prt;    eval;    alldef;  test;    session;  simul;
     step;   mono;    host;    port;    conf;     client;
     send;   other;   all;     ev;      id;       extra;
     gc} =
  let _ = print_eml "starting program" in
  (*let _ = print_extras extra in*)
  let _ = not mono in
  let _ = true in
  let (_, ev) = try (get_ev ev) with _ -> get_ev default_ev in
  let ident = if id = "" then Mac (host, port) else Loc id in
  if conf = ""
  then print_eml "no configuration file"
  else if simul
  then start_stepper input conf ev gc lib alldef prt extra
  else if client
  then simulate_clients_server conf prt
  else if send
  then simulate_clients_send conf alldef prt gc
  else if other
  then run_other_machine extra ident conf prt
  else if all
  then start_all_machines ev gc conf input lib alldef prt extra
  else run_distributed_program extra ev gc ident conf input lib alldef prt

let ref_default_el_output = ref "/tmp/eventml-output.el"
let ref_default_output    = ref ""
let ref_default_lib       = ref ""
let ref_default_input     = ref "test.esh"
let ref_default_time      = ref 1 (* 1sec by default *)
let ref_default_sub       = ref false
let ref_default_sanity    = ref false
let ref_default_nuprl     = ref false
let ref_default_obid      = ref ""
let ref_default_ascii     = ref false
let ref_default_tcheck    = ref false
let ref_default_parse     = ref false
let ref_default_split     = ref false
let ref_default_prt       = ref false
let ref_default_alldef    = ref ""
let ref_default_session   = ref false
let ref_default_mono      = ref false
let ref_default_host      = ref "127.0.0.0"
let ref_default_port      = ref 14567
let ref_default_conf      = ref ""
let ref_default_client    = ref false
let ref_default_send      = ref false
let ref_default_other     = ref false
let ref_default_all       = ref false
let ref_default_ev        = ref "ev2b" (* ev1 *)
let ref_default_id        = ref ""
let ref_default_extra     = ref ""
let ref_default_gc        = ref false
let ref_default_simul     = ref false

let x =
  let _ =
    Arg.parse
      [("--ascii",       Arg.Set ref_default_ascii,    "parse nuprl ascii file");
       ("-ascii",        Arg.Set ref_default_ascii,    "parse nuprl ascii file");
       ("--ascii-split", Arg.Set ref_default_split,    "split big ascii file");
       ("-ascii-split",  Arg.Set ref_default_split,    "split big ascii file");
       ("--sanitizer",   Arg.Set ref_default_sanity,   "runs sanity checker");
       ("-sanitizer",    Arg.Set ref_default_sanity,   "runs sanity checker");
       ("--sub",         Arg.Set ref_default_sub,      "turns on subtyping");
       ("-sub",          Arg.Set ref_default_sub,      "turns on subtyping");
       ("--print",       Arg.Set ref_default_prt,      "print debug information");
       ("-print",        Arg.Set ref_default_prt,      "print debug information");
       ("--nuprl",       Arg.Set ref_default_nuprl,    "converts EML to Nuprl");
       ("-nuprl",        Arg.Set ref_default_nuprl,    "converts EML to Nuprl");
       ("--session",     Arg.Set ref_default_session,  "starts an interactive EML session");
       ("-session",      Arg.Set ref_default_session,  "starts an interactive EML session");
       ("--mono",        Arg.Set ref_default_mono,     "monomorphic type checking");
       ("-mono",         Arg.Set ref_default_mono,     "monomorphic type checking");
       ("--client",      Arg.Set ref_default_client,   "runs clients");
       ("-client",       Arg.Set ref_default_client,   "runs clients");
       ("--send",        Arg.Set ref_default_send,     "send intransit messages");
       ("-semd",         Arg.Set ref_default_send,     "send intransit messages");
       ("--other",       Arg.Set ref_default_other,    "");
       ("-other",        Arg.Set ref_default_other,    "");
       ("--all",         Arg.Set ref_default_all,      "");
       ("-all",          Arg.Set ref_default_all,      "");
       ("--gc",          Arg.Set ref_default_gc,       "turns on GC");
       ("-gc",           Arg.Set ref_default_gc,       "turns on GC");
       ("--simul",       Arg.Set ref_default_simul,    "simulate processes");
       ("-simul",        Arg.Set ref_default_simul,    "simulate processes");
       ("--extra",       Arg.Set_string ref_default_extra,  "");
       ("-extra",        Arg.Set_string ref_default_extra,  "");
       ("--host",        Arg.Set_string ref_default_host,   "");
       ("-host",         Arg.Set_string ref_default_host,   "");
       ("--obid",        Arg.Set_string ref_default_obid,   "");
       ("-obid",         Arg.Set_string ref_default_obid,   "");
       ("--id",          Arg.Set_string ref_default_id,     "");
       ("-id",           Arg.Set_string ref_default_id,     "");
       ("--eval",        Arg.Set_string ref_default_ev,     "");
       ("-eval",         Arg.Set_string ref_default_ev,     "");
       ("--conf",        Arg.Set_string ref_default_conf,   "");
       ("-conf",         Arg.Set_string ref_default_conf,   "");
       ("--o",           Arg.Set_string ref_default_output, "");
       ("-o",            Arg.Set_string ref_default_output, "");
       ("--output",      Arg.Set_string ref_default_output, "");
       ("-output",       Arg.Set_string ref_default_output, "");
       ("--lib",         Arg.Set_string ref_default_lib,    "");
       ("-lib",          Arg.Set_string ref_default_lib,    "");
       ("--nuprl-defs",  Arg.Set_string ref_default_alldef, "");
       ("-nuprl-defs",   Arg.Set_string ref_default_alldef, "");
       ("--input",       Arg.Set_string ref_default_input,  "");
       ("-input",        Arg.Set_string ref_default_input,  "");
       ("--i",           Arg.Set_string ref_default_input,  "");
       ("-i",            Arg.Set_string ref_default_input,  "");
       ("--port",        Arg.Set_int ref_default_port, "");
       ("-port",         Arg.Set_int ref_default_port, "");
       ("--timelimit",   Arg.Set_int ref_default_time, "");
       ("-timelimit",    Arg.Set_int ref_default_time, "")]
      (fun str -> ())
      "EventML arguments" in
  let args =
    mk_args_ref
      ref_default_input   ref_default_output ref_default_lib    ref_default_time   ref_default_sub     ref_default_sanity
      ref_default_nuprl   ref_default_obid   ref_default_ascii  ref_default_tcheck ref_default_parse   ref_default_split
      ref_default_prt     (ref None)         ref_default_alldef (ref None)         ref_default_session ref_default_simul
      (ref None)          ref_default_mono   ref_default_host   ref_default_port   ref_default_conf    ref_default_client
      ref_default_send    ref_default_other  ref_default_all    ref_default_ev     ref_default_id      ref_default_extra
      ref_default_gc in
  run args
