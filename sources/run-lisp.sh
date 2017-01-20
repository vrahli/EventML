#!/bin/bash

TERMINAL=gnome-terminal
WINTITLE="runlisp"
DIR=`pwd`
SBCL=sbcl
RUN_LISP_FILE=run.lisp
RUN_FASL_FILE=run.fasl
RUN_LISP=${DIR}/${RUN_LISP_FILE}
RUN_FASL=${DIR}/${RUN_FASL_FILE}
#FILE=paxos-list2.lisp
#FILE=aneris_fastrep_opt4_2lisp2.lisp
#FILE=aneris_fastrep2_opt4_2lisp3.lisp
FILE=aneris-batching-control2.lisp

# echo "compiling yacc.lisp"
# sbcl --eval "(compile-file \"yacc.lisp\")" --eval "(quit)" > /dev/null

function compile() {
    echo "compiling ${FILE}"
    sbcl --load "prelude.lisp" --eval "(compile-file \"${FILE}\")" --eval "(quit)" > /dev/null

    echo "compiling ${RUN_LISP_FILE}"
    sbcl --eval "(require 'asdf)" --eval "(require 'cl-ppcre)" --eval "(require 'local-time)" --eval "(require 'cl-store)" --eval "(require 'cl-lex)" --eval "(require 'marshal)" --eval "(require 'yacc)"  --eval "(require 'ironclad)"  --eval "(require 'flexi-streams)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "prelude.lisp" --load "${FILE}" --eval "(compile-file \"${RUN_LISP_FILE}\")" --eval "(quit)" > /dev/null
}

function run() {
    echo "running processes"
    # --------- leaders ---------
    #emacs --title="ldr1" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-ldr1)\")" &
    #emacs --title="ldr2" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-ldr2)\")" &
    ${TERMINAL} --title=ldr1 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-ldr1)\"; exec bash" &
    ${TERMINAL} --title=ldr2 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-ldr2)\"; exec bash" &

    # --------- 2/3 locations ---------
    ${TERMINAL} --title=loc1 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-loc1)\"; exec bash" &
    ${TERMINAL} --title=loc2 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-loc2)\"; exec bash" &
    ${TERMINAL} --title=loc3 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-loc3)\"; exec bash" &
    ${TERMINAL} --title=loc4 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-loc4)\"; exec bash" &

    # --------- acceptors ---------
    #emacs --title="acc1" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-acc1)\")" &
    #emacs --title="acc2" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-acc2)\")" &
    #emacs --title="acc3" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-acc3)\")" &
    ${TERMINAL} --title=acc1 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-acc1)\"; exec bash" &
    ${TERMINAL} --title=acc2 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-acc2)\"; exec bash" &
    ${TERMINAL} --title=acc3 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-acc3)\"; exec bash" &

    # --------- replicas ---------
    #emacs --title="rep1" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-rep1)\")" &
    #emacs --title="rep2" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" --eval "(sleep-for 3)" --eval "(slime-interactive-eval \"(tst-rep2)\")" &
    ${TERMINAL} --title=rep1 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-rep1)\"; exec bash" &
    ${TERMINAL} --title=rep2 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-rep2)\"; exec bash" &

    # --------- clients (external) ---------
    ${TERMINAL} --title=client1 -x sh -c "sbcl --eval \"(require 'asdf)\" --eval \"(require 'sb-bsd-sockets)\" --eval \"(require 'sb-posix)\" --eval \"(require 'local-time)\" --eval \"(require 'cl-store)\" --load \"${RUN_FASL_FILE}\" --eval \"(tst-client1)\"; exec bash" &

    # replicas -- and then run (tst-sender)
    #emacs --title="client" --eval "(set-dark-colors)" --eval "(slime)" --eval "(sleep-for 3)" --eval "(slime-load-file \"${RUN_LISP}\")" &
    #${TERMINAL} --title=xxx -x sbcl --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --eval "(require 'local-time)" --load "${RUN_FASL_FILE}" &
}

case $1 in
    compile)
	compile
	;;
    *)
	compile; run
	;;
esac
