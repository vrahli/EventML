;; Copyright 2011 Cornell University
;; Copyright 2012 Cornell University
;;
;;
;; This file is part of EventML - a tool aiming at specifying
;; distributed protocols in an ML like language.  It is an interface
;; to the logic of events and is compiled into Nuprl.  It is written
;; by the NUPRL group of Cornell University, Ithaca, NY.
;;
;; EventML is a free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EventML is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EventML.  If not, see <http://www.gnu.org/licenses/>.
;;
;;  o Authors:     Vincent Rahli
;;  o Affiliation: Cornell University, NUPRL group
;;  o Date:        20 May 2011
;;  o File name:   eventml-main.el
;;  o Description: Main EventML Emacs UI file.
;;


(require 'comint)

(defvar eventml-output-buffer "*eventml-output*"
  "Name of the buffer used to display EventML's output.")

(defvar eventml-nuprl-constants-buffer "*nuprl-constants*"
  "Name of the buffer used to display Nuprl's constants' types.")

(defvar eventml-help-buffer "*eventml-help*"
  "Name of the buffer used to display EventML's help menu.")

(defvar eventml-nuprl-defs "alldefs"
  "Name of the file that contains some of Nuprl abstractions' types.")

(defvar eventml-nuprl-defs-esh "alldefs.eml"
  "Name of the file that contains some of Nuprl abstractions' types.")

(defvar eventml-nuprl-defs-temp nil
  "Name of the file that contains the EventML type of some of Nuprl abstractions.")

(defvar eventml-bin-name "eventml"
  "Name of the eventml binary.")

(defvar eventml-slicing-process "eventml"
  "Name of type inference process.")

(defvar eventml-nuprl-defs-process "eventml-constants"
  "Name of process in charge of displaying some of Nuprl's abstractions' types.")

(defvar eventml-session-process "eventml-session"
  "Name of process in charge of running an EventML session.")

(defvar eventml-session-buffer "eventml-session-buffer"
  "Name of the buffer hosting an EventML session.")

(defvar eventml-simulator-process "eventml-simulator"
  "Name of process in charge of running an EventML program simulator.")

(defvar eventml-simulator-buffer "eventml-simulator-buffer"
  "Name of the buffer hosting an EventML program session.")

(defvar eventml-subtyping nil
  "Switch controling whether or not subtyping is activated.")

(defvar eventml-linux-OS (not (string-equal system-type "windows-nt")))

;; On linux the tmp dir is "/tmp/"
(defvar eventml-tmp-dir temporary-file-directory)

(load (expand-file-name
       "sml-tes-main.el"
       eventml-emacs-directory))

(defun eventml-toggle-subtyping ()
  "Toggles the subtyping switch."
  (interactive)
  (setq eventml-subtyping (not eventml-subtyping)))

;; Same as sml-tes-trace but displays EventML instead of SML-TES
(defun eventml-trace (msg &rest data)
  "Writes a message and some data (optional) so that useful information can be displayed to the user"
  (apply #'message (concat "(EventML) " msg) data)
  (if (fboundp 'redisplay)
      (redisplay t)))

(defvar eventml-cmd-sep
  (if eventml-linux-OS
      ";"
    "&"
    )
  )


(defun eventml-echo (str)
  (if eventml-linux-OS
      (concat "echo " str)
    (concat "echo " str)
    )
  )

(defvar eventml-cmd-pref
  (if eventml-linux-OS
      "env nice -n 40 "
    ""
    )
  )

(defun eventml-run-inferencer (file option output-file)
  (let ((run-command (concat (eventml-echo "' '")
			     eventml-cmd-sep
			     (eventml-echo "' '")
			     eventml-cmd-sep
			     (eventml-echo "' '")
			     eventml-cmd-sep
			     (eventml-echo "' '")
			     eventml-cmd-sep
			     (eventml-echo "' '")
			     eventml-cmd-sep
			     (eventml-echo "========================================================================")
			     eventml-cmd-sep
			     (format "%s\"%s\" \"%s\" -lib \"%s\" -o \"%s\" -t \"%d\" %s"
				     eventml-cmd-pref
				     (expand-file-name eventml-bin-name eventml-bin-directory)
				     file
				     (expand-file-name eventml-nuprl-defs-esh eventml-lib-directory)
				     output-file
				     sml-tes-timelimit
				     option)
			     eventml-cmd-sep
			     (eventml-echo "' '"))))
    ;;(print run-command)
    (setq sml-tes-last-run-command run-command) ;; debugging trace
    ;; Kills the sml-tes process if it exists
    (eventml-stop-process)
    ;; Starts a new sml-tes process running the slicer.
    ;; There can be only one such process.
    ;; This is why we kill it before creating it again.
    (start-process-shell-command eventml-slicing-process eventml-output-buffer run-command))
  )

(defun eventml-run-slicer-option (option)
  "Runs the type error slicer using a stable executable of the slicer."
  (interactive)
  ;; Set the number of found errors to 0
  (setq sml-tes-item-count 0) ;; *** DEAD VARIABLE?
  ;; Set sml-tes-error-sount to 1 as the slicer produces files numbered 1,2,...
  (setq sml-tes-error-count 1)
  ;; Keep track of the the file being sliced
  (setq sml-tes-file-being-sliced (buffer-file-name))
  ;; If the file is not saved then we ask the user if he wants to save it
  (if (buffer-modified-p (get-file-buffer sml-tes-file-being-sliced))
      (save-some-buffers)
    )
  ;; If the file is still unsaved then we abord the slicing
  (if (buffer-modified-p (get-file-buffer sml-tes-file-being-sliced))
      (eventml-trace "FILE UNSAVED - SLICING ABORTED")
    ;; If the file to be sliced is the basis file we abord the slicing
    (if (string-equal sml-tes-file-being-sliced sml-tes-basis-file)
	(eventml-trace "BASIS FILE - SLICING ABORTED")
      (save-window-excursion
	;; Set up the command to run the slicer
	(let ((cur-file (file-name-nondirectory sml-tes-file-being-sliced))
	      (output-dir  nil)
	      (output-file nil)
              )
	  (sml-tes-set-temporary-directory)
	  (setq output-dir (sml-tes-get-temporary-directory))
	  (setq output-file (expand-file-name (format "%s.el" cur-file) output-dir))
	  ;; Remove all of the error information in the current file
	  (sml-tes-forget-all-slices)
	  ;; Create a temporary directory to store the output from the slicer
	  ;; *** use make-directory instead
	  ;;(shell-command (format "mkdir %s" output-dir))
	  (make-directory output-dir)
	  ;; Run the slicer
	  (eventml-trace "RUNNING SLICER")
	  (if eventml-subtyping
	      (setq option (concat option " --subtyping")))
	  (eventml-run-inferencer sml-tes-file-being-sliced option output-file)
	  (sml-tes-process-errors))))))

(defun eventml-run-slicer ()
  "Runs the EventML type checker."
  (interactive)
  (eventml-run-slicer-option "")
  )

(defun eventml-run-sanity-check-after-slicing (file)
  "Runs the sanity checker only if the slicer is not running and if the slicer hasn't found any error."
  (let ((proc (get-process eventml-slicing-process)))
    (if (equal nil proc)
	(if (< sml-tes-item-count 1)
	    (eventml-run-slicer-option "--sanitizer")
	  )
      (run-at-time 1 nil 'eventml-run-sanity-check-after-slicing file)
      )
    )
  )

(defun eventml-slice-then-sanity-check ()
  "Runs the type error slicer using a stable executable of the slicer."
  (interactive)
  (eventml-run-slicer)
  (eventml-run-sanity-check-after-slicing (buffer-file-name))
  )

(defun eventml-sanitizer ()
  "Runs the type error slicer using a stable executable of the slicer."
  (interactive)
  (eventml-run-slicer-option "--sanitizer")
  )

(defun eventml-compile-to-nuprl ()
  "Compiles the EventML code into NuPrl."
  (interactive)
  (let ((input-file (buffer-file-name))
	(output-file ""))
    (setq output-file (concat input-file ".nuprl"))
    (eventml-trace (format "COMPILING %s TO NUPRL" input-file))
    (eventml-run-inferencer input-file "--nuprl" output-file)
    (eventml-trace (format "COMPILED INTO %s" output-file))
    )
  )

(defun eventml-stop-process ()
  (interactive)
  "Stops the sml-tes process if it exists"
  (let ((proc (get-process eventml-slicing-process)))
    (if (not (equal nil proc))
	(delete-process proc)
      )
    )
  )

(defun eventml-forget-all-slices ()
  (interactive)
  "Forgets all slices."
  (sml-tes-forget-all-slices)
  (eventml-stop-process)
  )

(defun eventml-next-slice ()
  (interactive)
  "Focus on the next slice."
  (sml-tes-next-slice)
  )

(defun eventml-prev-slice ()
  (interactive)
  "Focus on the previous slice."
  (sml-tes-prev-slice)
  )

(defun eventml-next-part-of-slice ()
  (interactive)
  "Shows the next part of a slice."
  (sml-tes-next-part-of-slice)
  )

(defun eventml-display-h-output-buffer ()
  (interactive)
  "Displays the EventML output buffer."
  (delete-other-windows)
  (split-window-horizontally)
  (switch-to-buffer eventml-output-buffer)
  )

(defun eventml-display-v-output-buffer ()
  (interactive)
  "Displays the EventML output buffer."
  (delete-other-windows)
  (split-window-vertically)
  (switch-to-buffer eventml-output-buffer)
  )

(defun eventml-clear-output-buffer ()
  (interactive)
  "Clears the EventML output buffer."
  (let ((cur-buf (current-buffer)))
    (switch-to-buffer eventml-output-buffer)
    (erase-buffer)
    (switch-to-buffer cur-buf)
    )
  )

(defun eventml-get-temp-file (dir pref-file)
  "Generate the name of a file that does not exists in a given directory."
  (let ((rand (random))
	(file nil))
    (if (> 0 rand) (setq rand (- rand)))
    (setq file (expand-file-name (format "out-%s-%d" pref-file rand) dir))
    (while (file-exists-p file)
      (setq rand (+ rand 1))
      (setq file (expand-file-name (format "out-%s-%d" pref-file rand) dir))
      )
    file
    )
  )

(defun eventml-generate-nuprl-constants-out (file)
  (let ((proc (get-process eventml-nuprl-defs-process)))
    (if (not (equal nil proc))
	(progn
	  (eventml-trace (format "waiting for %s..." file))
	  (run-at-time 1 nil 'eventml-generate-nuprl-constants-out file))
      (if (file-exists-p file)
	  (progn
	    (delete-other-windows)
	    (split-window-horizontally)
	    (switch-to-buffer eventml-nuprl-constants-buffer)
	    (insert-file-contents file)
	    ;;(shell-command (format "rm \"%s\"" file)))
	    (delete-file file))
    	(print "The generation of Nuprl's abstractions' types is done but there is no output file.")
	)
      )
    )
  )

(defun eventml-generate-nuprl-constants ()
  (interactive)
  "Generates the types of Nuprl's abstractions from an ascii file
   and the display them in a buffer."
  (let ((output-file (eventml-get-temp-file eventml-tmp-dir "alldefs")))
    (setq eventml-nuprl-defs-temp output-file)
    (let ((command (format "%s\"%s\" \"%s\" -o \"%s\" --ascii"
			   eventml-cmd-pref
			   (expand-file-name eventml-bin-name   eventml-bin-directory)
			   (expand-file-name eventml-nuprl-defs eventml-lib-directory)
			   output-file)))
      (if (get-buffer eventml-nuprl-constants-buffer)
	  (kill-buffer eventml-nuprl-constants-buffer))
      (start-process-shell-command eventml-nuprl-defs-process nil command)
      (eventml-generate-nuprl-constants-out output-file)
      )
    )
  )

(defun eventml-display-nuprl-constants ()
  (interactive)
  "Displays Nuprl's abstractions' types."
  (let ((file (expand-file-name eventml-nuprl-defs-esh eventml-lib-directory)))
    (if (file-exists-p file)
	(progn
	  (delete-other-windows)
	  (split-window-horizontally)
	  (find-file file)
	  (toggle-read-only))
      (eventml-trace (concat file " doesn't exist."))
      )
    )
  )

;;(defvar inferior-eventml-mode-hook nil "")

;;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;(add-hook 'inferior-eventml-mode-hook 'ansi-color-for-comint-mode-on)

(define-derived-mode inferior-eventml-mode comint-mode "Inferior-EventML"
  "Inferior EventML mode."
  (setq comint-promp-regexp "^EventML# *")
  ;;(set-face-foreground 'comint-highlight-prompt "yellow")
  ;;(run-hooks 'inferior-eventml-mode-hook)
  ;;(custom-set-faces '(comint-highlight-prompt ((t (:foreground "red")))))
  ;; ;;  To send a string to the session process
  ;; ;; (process-send-string (get-buffer-process eventml-session-buffer) (buffer-substring (point) (point-max)))
  )

(define-derived-mode inferior-eventml-simul-mode comint-mode "Inferior-EventML-simul"
  "Inferior EventML mode for the program simulator."
  (setq comint-promp-regexp "^message? *")
  ;;(set-face-foreground 'comint-highlight-prompt "yellow")
  ;;(run-hooks 'inferior-eventml-mode-hook)
  ;;(custom-set-faces '(comint-highlight-prompt ((t (:foreground "red")))))
  ;; ;;  To send a string to the session process
  ;; ;; (process-send-string (get-buffer-process eventml-session-buffer) (buffer-substring (point) (point-max)))
  )

(defun eventml-session (&optional arg)
  (interactive)
  "Starts an EventML session."
  (let ((command (format "%s\"%s\" \"%s\" -lib \"%s\" -nuprl-defs \"%s\" --session"
			 eventml-cmd-pref
			 (expand-file-name eventml-bin-name       eventml-bin-directory)
			 (buffer-file-name)
			 (expand-file-name eventml-nuprl-defs-esh eventml-lib-directory)
			 (expand-file-name eventml-nuprl-defs     eventml-lib-directory))))
    (delete-other-windows)
    (split-window-horizontally)
    ;;(eventml-trace command)
    (start-process-shell-command eventml-session-process eventml-session-buffer command)
    (switch-to-buffer eventml-session-buffer)
    (inferior-eventml-simul-mode)
    )
  )

(defun eventml-simulator (&optional arg)
  (interactive)
  "Starts a program simulation."
  (let ((tmp-udb  use-dialog-box)
	(conf-file nil))
    (setq use-dialog-box nil)
    (setq conf-file (file-truename (read-file-name "Configuration file?" nil nil nil nil nil)))
    (setq use-dialog-box tmp-udb)
    (let ((command (format "%s\"%s\" --i \"%s\" --lib \"%s\" --nuprl-defs \"%s\" --simul --conf \"%s\""
			   eventml-cmd-pref
			   (expand-file-name eventml-bin-name       eventml-bin-directory)
			   (buffer-file-name)
			   (expand-file-name eventml-nuprl-defs-esh eventml-lib-directory)
			   (expand-file-name eventml-nuprl-defs     eventml-lib-directory)
			   conf-file)))
      (delete-other-windows)
      (split-window-horizontally)
      ;;(eventml-trace command)
      (start-process-shell-command eventml-simulator-process eventml-simulator-buffer command)
      (switch-to-buffer eventml-simulator-buffer)
      (inferior-eventml-mode)
      )
    )
  )

(defun eventml-program-skeleton-example ()
  (interactive)
  (insert
"specification test (prefix_)

 (* ------ IMPORTS ------ *)

constant pi1 (x : 'A * 'B) : 'A ;;
constant pi2 (x : 'A * 'B) : 'B ;;

import length ;;

 (* ------ PARAMETERS ------ *)

parameter n : Int ;;
parameter l : Loc ;;

 (* ------ MESSAGES ------ *)

MSGS internal (``test internal`` : Int,
                   base TestInternal,
                   send sendInternal)
     input    (``test input`` : Int,
                   base TestInput)
     output   (``test output`` : Int,
                   send sendOutput)
;;

 (* ------ PROTOCOL ------ *)

let P = TestInternal >> n >> Send(\_.{sendOutput l n}) ;;

main P ;;")
)

(defun eventml-ping-specification ()
  (interactive)
  (insert
"specification ping (ping_)

import bag-map;;

parameter p    : Loc;;
parameter locs : Loc Bag;;

MSGS
  input    (``start`` : Loc, base Start)
  internal (``ping``  : Loc, send ping, base Ping)
  internal (``pong``  : Loc, send pong, base Pong)
  output   (``out``   : Loc, send out)
;;

let Handler (s, loc) =
    Send(\\l.{ping loc l})
 || ((\\_.\\l.if l = loc then {out s l} else {})|Pong|);;

let P = (\\_.\\s.bag-map (\\l.(s,l)) locs)|Start| >>= Handler;;

let Reply = (\\loc.\\l.{pong l loc})|Ping| ;;

main P @ {p} || Reply @ locs")
)

(defun eventml-ping-configuration ()
  (interactive)
  (insert
"%locations p l1 l2

%parameters
p: LOC(p)
locs: {LOC(l1);LOC(l2)}

%messages
p: (``start``, Loc, LOC(client))")
)

(defun eventml-help ()
  (interactive)
  "Displays the EventML help buffer."
  (switch-to-buffer eventml-help-buffer)
  (delete-other-windows)
  )

(defun eventml-gen-export-to-eps (bbuf)
  (interactive)
  (let ((file (buffer-file-name)))
    (let ((dir   (file-name-directory      file))
	  (rfile (file-relative-name       file))
	  (base  nil)
	  (ext   nil))
      (setq base (file-name-sans-extension rfile))
      (setq ext  (file-name-extension      rfile))
      (let ((new-ps-file nil)
	    (new-base    nil)
	    (cmd         nil)
	    (temp-udb    use-dialog-box))
	(setq new-ps-file (concat base ".ps"))
	(setq use-dialog-box nil)
	(setq new-ps-file (read-file-name "Where do you want to save the ps file?" nil new-ps-file nil new-ps-file nil))
	(setq use-dialog-box temp-udb)
	(setq ps-print-header nil)
	(if bbuf
	    (ps-spool-buffer-with-faces)
	  (ps-spool-region-with-faces (region-beginning) (region-end)))
	(switch-to-buffer "*PostScript*")
	(setq new-ps-file (file-truename new-ps-file))
	(write-file new-ps-file)
	(eventml-trace (format "generated %s" new-ps-file))
	(setq new-base (file-name-sans-extension new-ps-file))
	(setq cmd (format "ps2eps -B -C %s %s" new-ps-file new-base))
	(start-process-shell-command "ps2eps" nil cmd)
	(eventml-trace (format "generated %s.eps" new-base))
	(kill-buffer)
	)
      )
    )
  )

(defun eventml-export-buffer-to-eps ()
  (interactive)
  (eventml-gen-export-to-eps t)
  )

(defun eventml-export-region-to-eps ()
  (interactive)
  (eventml-gen-export-to-eps nil)
  )

(defun eventml-merge-slices ()
  (interactive)
  (sml-tes-merge-slices)
  )

(defvar eventml-features
  '((letdec  . "let (* function name *) (* arguments *) = (* body *) ;;")
    (spec    . "specification (* name *) ;;")
    (param   . "parameter (* name *) : (* type *) ;;")
    (etparam . "parameter (* type name *), (* equality decider *) : Type * (* type name *) Deq ;;")
    (type    . "type (* name *) = (* type *) ;;")
    (letexp  . "let (* function name *) (* arguments *) = (* body *) in (* expression *)")
    (ite     . "if (* condition *) then (* true branch *) else (* false branch *)")
    (spcomb  . "(* function *) | (* class *); (* class *) (* ... *)|")
    (lspcomb . "(* function *) ^| (* class *); (* class *) (* ... *)|")
    (cspcomb . "(* function *) @| (* class *); (* class *) (* ... *)|")
    (msgs    . "MSGS (* headers *);;")
    (header  . "(``(* tokens *)`` : (*type *), base : (* id *), send : (* id *), msg : (* id *), broadcast : (* id *))"))
  "List of the eventml features."
  )

(defun eventml-display-feature (feature)
  (interactive)
  "Inserts an eventml feature."
  (let ((str (cdr (assoc feature eventml-features))))
    (insert str)
    )
  )

(defun eventml-completion ()
  (interactive)
  (cond
   ;; Conditional
   ((looking-back "ite")
    (progn
      (delete-backward-char 3)
      (eventml-display-feature 'ite))
    )
   ;; Conditional
   ((looking-back "ifthenelse")
    (progn
      (delete-backward-char 10)
      (eventml-display-feature 'ite))
    )
   ;; Let expression
   ((looking-back "letexp")
    (progn
      (delete-backward-char 6)
      (eventml-display-feature 'letexp))
    )
   ;; Let declaration
   ((looking-back "letdec")
    (progn
      (delete-backward-char 6)
      (eventml-display-feature 'letdec))
    )
   ;; Spec
   ((looking-back "spec")
    (progn
      (delete-backward-char 4)
      (eventml-display-feature 'spec))
    )
   ;; Parameter
   ((looking-back "param")
    (progn
      (delete-backward-char 5)
      (eventml-display-feature 'param))
    )
   ;; Type parameter with equality decider
   ((looking-back "etparam")
    (progn
      (delete-backward-char 7)
      (eventml-display-feature 'etparam))
    )
   ;; Messages
   ((looking-back "msgs")
    (progn
      (delete-backward-char 4)
      (eventml-display-feature 'msgs))
    )
   ;; Header
   ((looking-back "header")
    (progn
      (delete-backward-char 6)
      (eventml-display-feature 'header))
    )
   ;; Simple combinator
   ((looking-back "comb")
    (progn
      (delete-backward-char 4)
      (eventml-display-feature 'spcomb))
    )
   ;; Simple combinator with lifting
   ((looking-back "lcomb")
    (progn
      (delete-backward-char 5)
      (eventml-display-feature 'lspcomb))
    )
   ;; Simple combinator with lifting and concat
   ((looking-back "ccomb")
    (progn
      (delete-backward-char 5)
      (eventml-display-feature 'cspcomb))
    )
   )
  )

(defun eventml-current-slice-pointer-p ()
  (not (not sml-tes-current-slice-pointer))
  )

(defun eventml-define-keybindings-in-map (map)
  "Sets up key bindings for functions to use the EventML type inferencer."
  ;;
  ;; In all the following key bindings, e stands for EventML
  ;;
  ;; t stands for "type check"
  (define-key map "\C-c\C-et" 'eventml-sanitizer)
  ;;(define-key map "\C-c\C-es" 'eventml-slice-then-sanity-check)
  ;;
  ;; n stands for "next"
  (define-key map "\C-c\C-en" 'eventml-next-slice)
  ;;
  ;; p stands for "back"
  (define-key map "\C-c\C-eb" 'eventml-prev-slice)
  ;;
  ;; f stands for "forget"
  (define-key map "\C-c\C-ef" 'eventml-forget-all-slices)
  ;;
  ;; p stands for "part"
  (define-key map "\C-c\C-ep" 'eventml-next-part-of-slice)
  ;;
  ;; c stands for "compile"
  (define-key map "\C-c\C-ec" 'eventml-compile-to-nuprl)
  ;;
  ;; o stands for "output"
  (define-key map "\C-c\C-eo" 'eventml-display-h-output-buffer)
  ;;
  ;; h stands for "help"
  (define-key map "\C-c\C-eh" 'eventml-help)
  ;;
  ;; a stands for "automatic completion"
  (define-key map "\C-c\C-ea" 'eventml-completion)
  ;;
  ;; l stands for "library"
  (define-key map "\C-c\C-el" 'eventml-display-nuprl-constants)
  ;;
  ;; s stands for "session"
  (define-key map "\C-c\C-es" 'eventml-session)
  )

(eval-after-load (expand-file-name "eventml-mode.el" eventml-emacs-directory)
  '(eventml-define-keybindings-in-map eventml-mode-map))

(eval-after-load (expand-file-name "eventml-mode.el" eventml-emacs-directory)
  '(easy-menu-define eventml-mode-menu eventml-mode-map "Menu used in `EventML-mode'."
     '("EventML"
       ("Slicing"
	["Type check"                eventml-sanitizer                  t]
	["Next slice"                eventml-next-slice                 (eventml-current-slice-pointer-p)]
	["Previous slice"            eventml-prev-slice                 (eventml-current-slice-pointer-p)]
	["Next part of slice"        eventml-next-part-of-slice         (eventml-current-slice-pointer-p)]
	;;["Feeling lucky?"            eventml-merge-slices               (eventml-current-slice-pointer-p)]
	["Forget all slices"         eventml-forget-all-slices          t]
	;;["Subtyping"                 eventml-toggle-subtyping          :style toggle :selected eventml-subtyping]
	["Compile"                   eventml-compile-to-nuprl           f])
       ("View output"
	["Output buffer (H)"         eventml-display-h-output-buffer    t]
	["Output buffer (V)"         eventml-display-v-output-buffer    t]
	["Clear"                     eventml-clear-output-buffer        t])
       ("Nuprl constants"
	["Display"                   eventml-display-nuprl-constants    t]
	["Generate"                  eventml-generate-nuprl-constants   t])
       ;; ("Export to eps"
       ;; 	["Export region"             eventml-export-region-to-eps       t]
       ;; 	["Export buffer"             eventml-export-buffer-to-eps       t])
       ;; ("EventML features"
       ;; 	["EventML program skeleton"  eventml-program-skeleton-example   t]
       ;; 	["Ping specification"        eventml-ping-specification         t]
       ;; 	["Ping configuration"        eventml-ping-configuration         t]
       ;; 	["-----" nil nil]
       ;; 	["let declaration"           (eventml-display-feature 'letdec)  t]
       ;; 	["spec declaration"          (eventml-display-feature 'spec)    t]
       ;; 	["parameter"                 (eventml-display-feature 'param)   t]
       ;; 	["equality type parameter"   (eventml-display-feature 'etparam) t]
       ;; 	["messages"                  (eventml-display-feature 'msgs)    t]
       ;; 	["header"                    (eventml-display-feature 'header)  t]
       ;; 	["-----" nil nil]
       ;; 	["conditional"                      (eventml-display-feature 'ite)     t]
       ;; 	["let expression"                   (eventml-display-feature 'letexp)  t]
       ;; 	["simple combinator"                (eventml-display-feature 'spcomb)  t]
       ;; 	["lifting simple combinator"        (eventml-display-feature 'lspcomb) t]
       ;; 	["lifting concat simple combinator" (eventml-display-feature 'cspcomb) t])
       ["Session"                           eventml-session                    t]
       ["Simulator"                         eventml-simulator                  t]
       ["Completion"                        eventml-completion                 t]
       ["Help"                              eventml-help                       t])))
