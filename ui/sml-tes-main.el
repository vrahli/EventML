;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2009, 2010 Steven Shiells
;; Copyright 2009, 2010 Heriot-Watt University
;;
;; This file is part of smltesemacs - an Emacs User Interface for a
;; Type Error Slicer for Standard ML written by the ULTRA Group of
;; Heriot-Watt University, Edinburgh.
;;
;; smltesemacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; smltesemacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with smltesemacs.  If not, see <http://www.gnu.org/licenses/>.
;; * Authors:  Joe Wells, Steven Shiells, Vincent Rahli, Scott Fotheringham
;; * Affiliation: Heriot-Watt University, MACS
;; * Date: 10 November 2009
;; * Description:
;;     Main functions to interact with the slicer:
;;     run the slicer on a piece of code, switch from one slice to another, ...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; require Common Lisp package
;; do we actually need this? I don't think we do. Scott.
(require 'cl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;                            FACES                              ;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; STANDARD ERROR: FOCUS AND NON FOCUS

;; *** need better name ***
(defface sml-tes-standard-error-large-occ-focus
  '((((class color) (background dark))
     (:background "#aa0000" :foreground "#fefefe"))
    (t (:background "#ff0050" :foreground "#fefefe")))
  "Used to highlight a section of code that contributes
   to the type error when the error is in focus.")

;; *** need better name ***
(defface sml-tes-standard-error-focus
  '((((class color) (background dark))
     (:background "#9b0000" :foreground "#fefefe"))
    (t (:background "#ff0050" :foreground "#fefefe")))
  "Used to highlight a section of code that contributes
   to the type error when the error is in focus.")

;; *** need better name ***
(defface sml-tes-standard-error-non-focus
  '((((class color) (background dark))
     (:background "#500000"))
    (t (:background "#ffdcbd")))
  "Used to highlight a section of code that contributes
   to the type error when the error is not in focus.")



;;;; STANDARD ERROR BOX: FOCUS AND NON FOCUS

(defface sml-tes-standard-error-box-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#9b0000" :style nil)))
    (t (:box
	(:line-width 2 :color "#ff0050" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is in focus).")

(defface sml-tes-standard-error-box-non-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#500000" :style nil)))
    (t (:box
	(:line-width 2 :color "#ffdcbd" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is not in focus).")



;;;; STANDARD ERROR HEAD: FOCUS

(defface sml-tes-standard-error-head-focus
  '((((class color) (background dark))
     (:background "#9b0000" :foreground "white" :weight extra-bold))
    (t (:background "#ff8e8e" :foreground "white" :weight extra-bold)))
  "*********************************************")



;;;; END POINT ONE: FOCUS AND NON FOCUS

(defface sml-tes-end-point-one-focus
  '((((class color) (background dark))
     (:background "Blue" :foreground "#fefefe"))
    (t (:background "#5e76c1"  :foreground "#fefefe")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (in focus).")

(defface sml-tes-end-point-one-non-focus
  '((((class color) (background dark))
     (:background "#000050"))
    (t (:background "#dfdffb")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (not in focus).")



;;;; END POINT ONE BOX: FOCUS AND NON FOCUS

(defface sml-tes-end-point-one-box-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "Blue" :style nil)))
    (t (:box
	(:line-width 2 :color "#5e76c1" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is in focus).")

(defface sml-tes-end-point-one-box-non-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#000050" :style nil)))
    (t (:box
	(:line-width 2 :color "#dfdffb" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is not in focus).")



;;;; END POINT ONE HEAD: FOCUS

(defface sml-tes-end-point-one-head-focus
  '((((class color) (background dark))
     (:background "Blue" :foreground "white" :weight extra-bold))
    (t (:background "#9999ff" :foreground "white" :weight extra-bold)))
  "********************************************")



;;;; END POINT TWO: FOCUS AND NON FOCUS

(defface sml-tes-end-point-two-focus
  '((((class color) (background dark))
     (:background "#898989" :foreground "#fefefe"))
    (t (:background "#7b7a85" :foreground "#fefefe")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (in focus).")

(defface sml-tes-end-point-two-non-focus
  '((((class color) (background dark))
     (:background "#404040"))
    (t (:background "#e3e3e3")))
  "Used to highlight a section of code that is an end point of a type constructor clash, an arity clash, or a record clash (not in focus).")



;;;; END POINT TWO BOX: FOCUS AND NON FOCUS

(defface sml-tes-end-point-two-box-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#898989" :style nil)))
    (t (:box
	(:line-width 2 :color "grey" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is in focus).")

(defface sml-tes-end-point-two-box-non-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#404040" :style nil)))
    (t (:box
	(:line-width 2 :color "#e3e3e3" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not. This region identified by the box is an end point of a clash (when the error is not in focus).")



;;;; END POINT TWO HEAD: FOCUS

(defface sml-tes-end-point-two-head-focus
  '((((class color) (background dark))
     (:background "#898989" :foreground "white" :weight extra-bold))
    (t (:background "grey" :foreground "white" :weight extra-bold)))
  "**********************************************")



;;;; MERGED ERROR: FOCUS AND NON FOCUS

(defface sml-tes-merged-regions-focus
  '((((class color) (background dark))
     (:background "ForestGreen"))
    (t (:background "#af1de2" :foreground "#ffffff")))
  "Used to highlight a section of code that is an end point of a record clash (in focus). The highlited text appears in both clashing records.")

(defface sml-tes-merged-regions-non-focus
  '((((class color) (background dark))
     (:background "#004000"))
    (t (:background "#d8b1e2")))
  "Used to highlight a section of code that is an end point of a record clash (not in focus). The highlited text appears in both clashing records.")



;;;; MERGED ERROR BOX: FOCUS AND NON FOCUS

(defface sml-tes-merged-regions-box-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "ForestGreen" :style nil)))
    (t (:box
	(:line-width 2 :color "af1de2" :style nil))))
  "Used to surround a piece of code in a tuple clashing with a record.
  The piece of code is the the nth component of the tuple but the record does not mention n")

(defface sml-tes-merged-regions-box-non-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#004000" :style nil)))
    (t (:box
	(:line-width 2 :color "#d8b1e2" :style nil))))
  "Used to surround a piece of code in a tuple clashing with a record.
  The piece of code is the the nth component of the tuple but the record does not mention n")



;;;; FURTHER EXPLANATION: FOCUS AND NON FOCUS

;; *** need better name ***
(defface sml-tes-further-explanations-focus
  '((((class color) (background dark))
     (:background "DarkMagenta"))
    (t (:background "#FF9300")))
  "Used to highlight code provides information about the status of identifiers or that is expansive (in focus).")

;; *** need better name ***
(defface sml-tes-further-explanations-non-focus
  '((((class color) (background dark))
     (:background "#500050"))
    (t (:background "#FFBF77")))
  "Used to highlight code provides information about the status of identifiers or that is expansive (not in focus).")



;;;; FURTHER EXPLANATION BOX: FOCUS AND NON FOCUS

(defface sml-tes-further-explanations-box-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "DarkMagenta" :style nil)))
    (t (:box
	(:line-width 2 :color "#FF20AA" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is in focus).")

(defface sml-tes-further-explanations-box-non-focus
  '((((class color) (background dark))
     (:box
      (:line-width 2 :color "#500050" :style nil)))
    (t (:box
	(:line-width 2 :color "#FFB3E1" :style nil))))
  "Used to surround a piece of code for which the presence is relevant to the error but the contents are not (when the error is not in focus).")



;;;; PARSING: FOCUS AND NON FOCUS

(defface sml-tes-parsing-error-focus
  '((((class color) (background dark))
     (:background "#FFD700" :foreground "#010101"))
    (t (:background "#FFE50B" :foreground "#010101")))
  "Used to highlight the regions of code that cannot be parsed by the type error slicer.")

(defface sml-tes-parsing-error-non-focus
  '((((class color) (background dark))
     (:background "#5C3317"))
    (t (:background "#FFF68F")))
  "Used to highlight the regions of code that cannot be parsed by the type error slicer.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;                          VARIABLES                            ;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar tes-emacs-directory   eventml-emacs-directory)
(defvar tes-bin-directory     eventml-bin-directory)
(defvar tes-lib-directory     eventml-lib-directory)
(defvar tes-sources-directory eventml-sources-directory)


;;(defvar sml-tes-first t)

;; Variable to keep track of the number of times the slicer has been run
(defvar sml-tes-count 0
  "Variable to keep track of the number of times the type error slicer has been run")

(defvar sml-tes-fake-lines 0
  "Variable to keep track of the number lines added to the file being sliced")

;; Variable to keep track of the number of errors found by the slicer
;; *** DEAD VARIABLE?
(defvar sml-tes-item-count 0
  "Variable to keep track of the number of errors found by the tpye error slicer")

;; Variable to keep track of the number of errors processed
(defvar sml-tes-error-count 1
  "Variable to keep track of the number of errors processed")

;; The time limit that the slicer is allowed to run
;; 1 hour by default
(defvar sml-tes-timelimit 3600000
  "The time limit that the type error slicer is allowed to run")

;; *** fix to use expand-file-name and test
(defvar sml-tes-basis-file (expand-file-name "basis.sml" tes-lib-directory)
  "Variable to keep track of the SML/NJ basis file")

;; sml
;; (defvar sml-tes-sml-process "/home/vince/Work/smlnj-11070/bin/sml")
(defvar sml-tes-sml-process "sml" "The command used to run sml")

;; the option concerning the use of the basis
;; 0: no basis
;; 1: buildin environment
;; 2: basis file
(defvar sml-tes-basis-option 2
  "Variable to keep track of the basis option used by the type error slicer")

;; Variable to flag whether or not the interface is ready to process another error
(defvar sml-tes-ready t
  "Variable to flag whether or not the tpye error slicer's interface is ready to process another error")

;; Variable to flag when the user wishes to have the verbose error messages displayed
(defvar sml-tes-verbose-error-messages nil
  "Variable to flag when the user wishes to have the verbose error messages displayed")

;;(defvar sml-tes-slices-processed-mark)
;;
;;(put 'sml-tes-slices-processed-mark 'permanent-local t)

;; ???
(defvar sml-tes-slice-data nil)

;; The name of the binary (sometimes it is called slicer, sometimes smltes-bin THIS NEEDS FIXED!!) - Scott
(if (boundp 'sml-tes-bin-scott)
	(defvar sml-tes-slicer-bin "smltes-bin"))
(defvar sml-tes-slicer-bin "slicer")

;; Size of a tabulation
;; (2010-03-24)We can add that as an argument to the slicer binary
(defvar sml-tes-tab-size default-tab-width)

;; Variable to hold the message about the success or failure of the slicer
(defvar sml-tes-finished-message nil
  "Variable to hold the message about the success or failure of the type error slicer")

;; Variable to hold the list of slices found by the slicer
(defvar sml-tes-slices nil
  "Variable to hold the list of slices found by the type error slicer")

;; Variable to hold the list of slices found by the slicer that have been viewed by the user
(defvar sml-tes-slices-previous nil
  "Variable to hold the list of slices found by the type error slicer that have been viewed by the user")

;; Variable to hold the list of overlays that are used to highlight sections of the source file
(defvar sml-tes-overlays nil
  "Variable to hold the list of overlays that are used to highlight sections of the source file")

;; Variable to hold the overlays of merged slices.
(defvar sml-tes-merged-slice nil
  "Variable to hold the overlays of merges slices.")

;; Variable to keep track of the current file that is being sliced
(defvar sml-tes-file-being-sliced nil
  "Variable to keep track of the current file that is being sliced by the type error slicer")

;; Variable to point to the current slice
(defvar sml-tes-current-slice-pointer nil
  "Variable to point to the current slice")

;; Variable to keep track of the parts of the current slice which have not been viewed
(defvar sml-tes-current-slice-not-viewed nil
  "Holds the ")

(defvar sml-tes-tmp-dir temporary-file-directory)

;; Name of the temporary directory that will contain all the temporary files generated by the slicer
(defvar sml-tes-temporary-directory nil
  "Name of the temporary directory that will contain all the temporary files generated by the slicer")

;; Identifier of the slice currently highlighted
(defvar sml-tes-current-highlighted-slice nil
  "Identifier of the slice currently highlighted")

;; Keep temporary files between every time slicer runs
(defvar sml-tes-keep-temporary-files-for-debugging nil
  "If non-nil, then temporary files used for communication between
Emacs and the slicer back end are not deleted.  This makes
debugging easier.  In normal use, this variable should have the
value nil.")

;; Allows deleting or not the other windows before highlighting a slice.
(defvar sml-tes-delete-other-windows nil
  "This variable is true if one wants to delete the other windows before staring to
highlight a slice.")

;; Advice that allows the properties of tooltips to be manipulated so
;; that the error messages are displayed properly
(defadvice tooltip-show (around fix-stupid-face-idiocy activate)
  (let ((tooltip-show-original-text (ad-get-arg 0)))
    ad-do-it))

;; Advice that allows the properties of tooltips to be manipulated so
;; that the error messages are displayed properly
(defadvice x-show-tip (before fix-stupid-face-idiocy activate)
  (if (boundp 'tooltip-show-original-text)
      (ad-set-arg 0 tooltip-show-original-text))
  )

;; Define variable for user's preferred compiler, default to SML/NJ
(defvar sml-tes-user-compiler-smlnj nil)
(defvar sml-tes-user-compiler-mlton nil)
(defvar sml-tes-user-compiler-polyml nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;                          FUNCTIONS                            ;;;;;;;;;;
;;;;;;;;;;                                                               ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun sml-tes-unread-only-basis ()
  "Makes the basis file writable if it is read-only"
  (let ((cur-buf (current-buffer)))
    (set-buffer (get-file-buffer sml-tes-basis-file))
    (if buffer-read-only
	(toggle-read-only)
      )
    (set-buffer cur-buf)
    )
  )

(defun sml-tes-set-temporary-directory ()
  "Sets the sml-tes-temporary-directory to a new unused temporary file name"
  ;; We might want to use the tmpdir variable instead
  (let* ((smltes-tmp (expand-file-name "smltes" sml-tes-tmp-dir))
	 (rand (random))
	 (file nil))
    (if (> 0 rand) (setq rand (- rand)))
    (setq file (format "%s-%d" smltes-tmp rand))
    (while (file-exists-p file)
      (setq rand (+ rand 1))
      (setq file (format "%s-%d" smltes-tmp rand))
      )
    (setq sml-tes-temporary-directory file)
    )
  )

(defun sml-tes-get-temporary-directory ()
  "Builds the name of the temporary directory used by the slicer"
  ;;(format "%s-slices" sml-tes-file-being-sliced)
  (if (not sml-tes-temporary-directory)
      ;; This should never happen
      (sml-tes-set-temporary-directory))
  sml-tes-temporary-directory
  )

(defun sml-tes-delete-rec-dir (dir)
  (let ((files (directory-files dir)))
    (mapc (lambda (file)
	    (if (and (not (string-equal file "."))
		     (not (string-equal file "..")))
		(delete-file (expand-file-name file dir))))
	  files)
    (delete-directory dir)
    )
  )

(defun sml-tes-delete-temporary-directory ()
  "Deletes the temporary directory and files produced by the slicer if they exist"
  (let ((temp-dir (sml-tes-get-temporary-directory)))
    (if (file-exists-p temp-dir)
	;; *** Eeek!  Running rm -rf is dangerous!!!  We have not
        ;; really validated temp-dir.  If
        ;; (sml-tes-get-temporary-directory) can ever return a bad
        ;; value, we might remove all the user's files (or the entire
        ;; system if running as root)!
        (or sml-tes-keep-temporary-files-for-debugging
	    (sml-tes-delete-rec-dir temp-dir))
      ;;(shell-command (format "rm -rf %s" temp-dir)))
      )
    )
  )

(defun sml-tes-stop-sml-tes-process ()
  (interactive)
  "Stops the sml-tes process if it exists"
  (let ((proc (get-process "sml-tes")))
    (if (not (equal nil proc))
	(delete-process proc)
      )
    )
  )

(defun sml-tes-run-slicer ()
  "Runs the type error slicer using the 'commslicerp' command available from the Slicer.
   This function runs the type error slicer through the SML sources.
   This function can be used by developers of the type error slicer so that they can run
   the slicer without the need to produce a binary executbale of the slicer each time they
   update the slicer."
  (interactive)
  ;; Set sml-tes-error-sount to 1 as the slicer produces files numbered 1,2,...
  (setq sml-tes-error-count 1)
  (setq sml-tes-need-to-abandon nil)
  ;; Increase the variable that keeps track of the number of times the slicer has been run
  ;; since this file was loaded. This variable is used in the name of the process that runs
  ;; the slicer, allowing for multiple slicer processes
  (setq sml-tes-count (1+ sml-tes-count))
  ;; Keep track of the the file being sliced
  (setq sml-tes-file-being-sliced (buffer-file-name))
  (if (buffer-modified-p (get-file-buffer sml-tes-file-being-sliced))
      (save-some-buffers)
    )
  ;; If the file is still unsaved then we abord the slicing
  (if (buffer-modified-p (get-file-buffer sml-tes-file-being-sliced))
      (sml-tes-trace "FILE UNSAVED - SLICING ABORTED")
    ;; If the file to be sliced is the basis file we abord the slicing
    (if (string-equal sml-tes-file-being-sliced sml-tes-basis-file)
	(sml-tes-trace "BASIS FILE - SLICING ABORTED")
      (save-window-excursion
	;; Set up the command to run the slicer
	(let ((cur-file (file-name-nondirectory sml-tes-file-being-sliced))
	      (make-command (format "echo 'CM.make \"%s\"; "
				    (expand-file-name "sources.cm"
						      tes-sources-directory)))
	      (open-command "open Slicer; ")
	      (run-command (format "commslicerp \"%s\" [\"%s\"] \"\" \"\" \"\" \"" sml-tes-basis-file sml-tes-file-being-sliced))
	      (output-file nil)
	      (output-dir nil)
	      (rest-command (format "\" \"\" %d %d;' | %s" sml-tes-basis-option sml-tes-timelimit sml-tes-sml-process))
	      (timer nil) ;; *** DEAD VARIABLE?
              )
	  (sml-tes-set-temporary-directory)
	  (setq output-dir (sml-tes-get-temporary-directory))
	  (setq output-file (expand-file-name (format "%s.el" cur-file) output-dir))
	  ;; Format the command
	  (setq sml-tes-command (concat make-command open-command run-command output-file rest-command))
	  ;; Remove all of the error information in the current file
	  (sml-tes-forget-all-slices)
	  ;; Create a temporary directory to store the output from the slicer
	  ;;(shell-command (format "mkdir %s" output-dir))
	  (make-directory output-dir)
	  ;; Run the slicer
	  (sml-tes-trace "RUNNING SLICER: please wait..")
	  ;; Kills the sml-tes process if it exists
	  (sml-tes-stop-sml-tes-process)
	  ;; Starts a new sml-tes process running the slicer.
	  ;; There can be only one such process.
	  ;; This is why we kill it before creating it again.
	  ;; QUESTION: What if there is already a sml-tes process running which is not related to our slicer?
	  (start-process-shell-command "sml-tes" "*sml-tes-debugging-output*" sml-tes-command)
;;	  (start-process-shell-command (format "sml-tes-%d" sml-tes-count) nil sml-tes-command)
          ;; THE FOLLOWING CODE HAS BEEN DELIBERATELY DISABLED.  DO
          ;; NOT REENABLE IT!  SEE THE COMMENT AT THE OTHER SITE FOR
          ;; FURTHER EXPLANATION.
	  ;; ## ;; Set the time that the tooltip error messages are shown
	  ;; ## (setq tooltip-hide-delay 20)
	  ;; Process the errors
	  (sml-tes-process-errors))))))

(defun sml-tes-run-slicer-exec ()
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
      (sml-tes-trace "FILE UNSAVED - SLICING ABORTED")
    ;; If the file to be sliced is the basis file we abord the slicing
    (if (string-equal sml-tes-file-being-sliced sml-tes-basis-file)
	(sml-tes-trace "BASIS FILE - SLICING ABORTED")
      (save-window-excursion
	;; Set up the command to run the slicer
	(let ((cur-file (file-name-nondirectory sml-tes-file-being-sliced))
	      (run-command (format
                            ;; “env” is used to force the actual
                            ;; “nice” program to be used, not the
                            ;; “nice” (t)csh builtin command.  This is
                            ;; the only reasonably portable way I
                            ;; could think of that works to force
                            ;; this.
                            ;;
                            ;; I prefer the more understandable
                            ;; arguments to GNU “nice”, namely
                            ;; “--adjustment=+19”.  However that would
                            ;; not work for arbitrary POSIX “nice”.
                            ;;
                            ;; *** I am not sure 19 is the maximum
                            ;; niceness on all systems, so it would be
                            ;; nice if there was a portable way of
                            ;; saying “lowest possible priority”.
                            ;;
                            "env nice -n 19 %s \"%s\" \"%s\" \"\" \"\" \"\" \""
                            (expand-file-name sml-tes-slicer-bin tes-bin-directory)
                            sml-tes-basis-file
                            sml-tes-file-being-sliced))
	      (output-file nil)
	      (timer nil) ;; *** DEAD VARIABLE?
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
	  (sml-tes-trace "RUNNING SLICER: Please wait..")
          (setq run-command (concat run-command output-file (format "\" \"\" \"%d\" \"%d\" \"\" \"\" true" sml-tes-basis-option sml-tes-timelimit)))
          (setq sml-tes-last-run-command run-command) ;; debugging trace
	  ;; Kills the sml-tes process if it exists
	  (sml-tes-stop-sml-tes-process)
	  ;; Starts a new sml-tes process running the slicer.
	  ;; There can be only one such process.
	  ;; This is why we kill it before creating it again.
	  (start-process-shell-command "sml-tes" "*sml-tes-debugging-output*" run-command)
;;	  (start-process-shell-command (format "sml-tes-%d" sml-tes-count) nil run-command)
          ;; DO NOT DO THINGS LIKE THE FOLLOWING!  THIS IS AN EVIL
          ;; FORM OF TRAMPLING ON USER PREFERENCES!  THE EMACS USER IS
          ;; ENTITLED TO SET THIS VALUE AND EXPECT THAT THEIR SETTING
          ;; WILL PERSIST!  THERE ARE OTHER PARTS OF EMACS THAT ALSO
          ;; USE TOOLTIPS, AND THE SETTING OF THE VARIABLE
          ;; tootiip-hide-delay AFFECTS ALL OF THEM!  YOU CAN
          ;; **SUGGEST** TO THE USER THAT THEY ADJUST THEIR SETTING IN
          ;; THEIR .emacs FILE.  YOU CAN OFFER TO LET THEM CUSTOMIZE
          ;; THEIR SETTING WITH (customize-variable
          ;; 'tooltip-hide-delay).  BUT YOU MUST NOT DO THE FOLLOWING!
	  ;; ## Set the time that the tooltip error messages are shown
	  ;; ## (setq tooltip-hide-delay 20)
	  ;; Process the errors
	  (sml-tes-process-errors))))))



;; (defun sml-tes-dir (str)
;;   "This function takes in a filepath of a file and returns the filepath of the containing
;;    folder and the name of the file"
;;   (let ((string "/")
;; 	;; Split the full filepath everwhere there is a "/"
;; 	(split (split-string str "/")))
;;     ;; Loop through the list of the split file path to produce the file path of the directory
;;     (while (cdr split)
;;       (setq string (concat string (car split)  "/"))
;;       (setq split (cdr split)))
;;     ;; Return a list of the file path and file name
;;     (cons string (last split))))

;; ;; This was to test if there is a difference between that and file-readable-p
;; ;; It doesn't change anything
;; ;; And using readable instead of exists does not change anything either
;; (defun sml-tes-test-file (file)
;;   "true if the file can be accessed, nil otherwise"
;;   (condition-case nil
;;       (case (access-file file  "ARGH") ((nil) t))
;;     ('error nil)
;;     )
;;   )

;;(defvar sml-tes-temp-file "/tmp/sml-tes-temp-file.el")

(defun sml-tes-process-errors ()
  "This function processes the output files produced by the type error slicer"
  ;; Get the file path to the containing directory and the file name of the file being sliced
  (let ((cur-file (file-name-nondirectory sml-tes-file-being-sliced))
	(output-file nil)
	(finished-file nil)
	(curid sml-tes-current-highlighted-slice))
    ;; Create the name of the output files
    (setq output-file (expand-file-name (format "%s-%d.el" cur-file sml-tes-error-count)
					(sml-tes-get-temporary-directory)))
    (setq finished-file (expand-file-name (format "%s-finished.el" cur-file)
					  (sml-tes-get-temporary-directory)))
    ;; If the file indicating the slicer has finished exists ([filename-finished].el), and there are no other error files to be processed then ...
    (if (and (file-exists-p finished-file)
	     (not (file-exists-p output-file)))
	(progn
	  ;; ... Remove all of the temporary files produced by the slicer ...
	  (load-file finished-file)
	  (sml-tes-delete-temporary-directory)
	  (sml-tes-trace (concat "SLICING FINISHED WITH STATUS: " sml-tes-finished-message)))
      ;; ... else when there are no slices currently getting processed
      ;; and the next error file exists, then process the next error
      (if (and (file-exists-p output-file)
	       sml-tes-ready)
	  (progn
	    ;; We set sml-tes-slice-data to nil so that we don't have duplicate slices.
	    ;; We can then focus on the BIG problem that some errors are not loaded.
	    ;; Now I think we can safely get rid of this line.
	    ;;(setq sml-tes-slice-data nil)
	    ;;(copy-file output-file sml-tes-temp-file)
	    ;;(load-file sml-tes-temp-file)
	    (load-file output-file)
	    (if (buffer-modified-p (get-file-buffer sml-tes-file-being-sliced))
		(progn
		  (sml-tes-delete-temporary-directory)
		  (sml-tes-trace "BUFFER MODIFIFED - SLICING STOPPED"))
	      ;; Process the error
	      (sml-tes-process-error-file sml-tes-slice-data)
	      ;; Remove the error file that has just been processed
              (or sml-tes-keep-temporary-files-for-debugging
                  ;;(shell-command (format "rm %s " output-file)))
		  (delete-file output-file))
	      ;; Increment the counter that keeps track of the next error
	      (setq sml-tes-error-count (1+ sml-tes-error-count))
	      (setq sml-tes-current-slice-pointer (butlast sml-tes-slices))
	      ;; If an error was highlighted and has been replaced, we highlight the new one
	      (if sml-tes-current-highlighted-slice
		  (if (not (equal sml-tes-current-highlighted-slice curid))
		      (sml-tes-next-slice-by-id sml-tes-current-highlighted-slice)
		    )
		)
	      (sml-tes-trace "SEARCHING FOR OUTPUT: Please wait..")
	      ;; The next error is ready so process it
	      (sml-tes-process-errors)))
	;; Still waiting on the slicer, so wait a bit before trying again
	(run-at-time 1 nil 'sml-tes-process-errors)
	))))

;; for debugging only now, but we might include a use of this in the
;; delivered code.
(defun sml-tes-clean-timers ()
"for debugging only now, but we might include a use of this in the  delivered code."
  (let ((sml-tes-timer-list
         ;; make copy of timer-list containing only the sml-tes timers
         (remove* 'sml-tes-process-errors timer-list
                  :key (lambda (timer) (aref timer 5))
                  :test-not #'eq)))
    (dolist (timer sml-tes-timer-list)
      (cancel-timer timer))))
;;(sml-tes-clean-timers)

(defun sml-tes-process-error-file (list)
  "Processes the information in the file containing the details of the error"
  ;; Set the flag "sml-tes-ready" to show that an error is currently being processed
  (let ((sml-tes-ready nil) ;; can the timer really run during this function?
        (count 0))
    ;; Keep track of the number of errors in the file
    ;; ** Should only ever be 1 error per file, but the function allows for more (just incase) **
    (setq sml-tes-item-count (safe-length list)) ;; *** DEAD VARIABLE?
    ;; While there are to process
    (while (< count (safe-length list))
      (let ((item (nth count list)))
	;; Identify the different parts of the error
	(let* ((id          (cdr (assoc 'id item)))
	       (regs        (cdr (assoc 'regions item)))
	       (slice       (cdr (assoc 'slice item)))
	       (assumptions (cdr (assoc 'assumptions item)))
	       (kind        (cdr (assoc 'kind item)))
	       (remove      (cdr (assoc 'remove item)))
	       (new-slice   nil)
	       (info        nil))
	  ;; Highlight the slice
          ;; *** pass these as separate arguments!!!
	  (setq info (sml-tes-format-slice-info slice assumptions kind))
	  (setq new-slice `((id    . ,id)
			    (regs  . ,regs)
			    (slice . ,info)
			    (kind  . ,kind)))
	  (sml-tes-highlight-slice new-slice)
	  ;; ;; foucs the first slice
	  ;; (if sml-tes-first
	  ;;     (sml-tes-next-slice)
	  ;;     (sml-tes-next-slice)
	  ;;     )
	  ;; (setq sml-tes-first nil)
	  ;; Some slices may require some other slices to be removed (slices that are two or more minimal slices merged)
	  (when (not (equal 0 (safe-length remove)))
	    ;; Remove all of the slices that are no longer needed
	    (while remove
	      (if (equal (car remove) sml-tes-current-highlighted-slice)
	       	  ;; The we should highlight the current slice
		  ;; It is going to be done when returning from sml-tes-process-error-file in sml-tes-process-error
		  (setq sml-tes-current-highlighted-slice id)
		)
	      (sml-tes-forget-slice-by-id (car remove))
	      (setq remove (cdr remove))))
	  ))
      ;; Increment the counter "count" so that the next error will be processed
      (setq count (1+ count))))
  )

;; (defun sml-tes-highlight-slice (args)
;;   "Highlights a slice - a slice is made up of a number of regions"
;;   (sml-tes-trace "HIGHLIGHTING A SLICE ENTER [%s]" (car (cddr args)))
;;   ;; Identify the different parts of the error
;;   (let ((formattedRegs nil)
;; 	(regs  (car args))
;; 	(slice (car (cdr args)))
;; 	(id    (car (cddr args)))
;; 	(myovs nil))
;;     (while regs
;;       (let* ((file (caar regs))
;; 	     (regions (cdr (assoc file regs))))
;; 	(find-file file)
;; 	;; Put the information into a form in which it is easily processed
;; 	(setq slice (car (cdr args)))
;; 	(setq slice (cons id (cons (car slice) (cons (cons (car (cdr slice)) (cons (sml-tes-contains-box regions) nil)) nil))))
;; 	(setq formattedRegs nil)
;; 	(setq formattedRegs (sml-tes-mapslice formattedRegs regions slice))
;; 	;; Highlight the regions
;; 	(let ((ovs
;; 	       (apply #'append
;; 		      (mapcar #'sml-tes-highlight-region formattedRegs))))
;; 	  (mapc
;; 	   (lambda (ov) (overlay-put ov 'sml-tes-slice ovs))
;; 	   ovs)
;; 	  (setq myovs (append myovs ovs)))
;; 	(setq regs (cdr regs))
;; 	)
;;       )
;;     ;; Keep track of the error slices
;;     (when (not sml-tes-slices)
;;       (push nil sml-tes-slices))
;;     (when (not (equal myovs nil))
;;       (push (reverse myovs) sml-tes-slices))
;;     )
;;   (sml-tes-trace "HIGHLIGHTING A SLICE EXIT"))

;; *** Use this to track down errors in timer handler.
(defvar sml-tes-trace-signals-log nil)

(defmacro sml-tes-trace-signals (log &rest body)
  (declare (indent 2)
           (debug (sexp sexp body)))
  `(condition-case
       ;; *** should really generate a fresh symbol for this:
       sml-tes-trace-signals-data
       (progn ,@body)
     (error
      ;; log the signal
      (push sml-tes-trace-signals-data ,log)
      ;; re-raise the signal
      (signal (car sml-tes-trace-signals-data) (cdr sml-tes-trace-signals-data)))))

;; (pp (macroexpand '(sml-tes-trace-signals log-var x y z)))

(defun sml-tes-highlight-slice (item)
  "Highlights a slice - a slice is made up of a number of regions"
  (sml-tes-trace "HIGHLIGHTING A SLICE ENTER [%s]" (cdr (assoc 'id item)))
  ;; Identify the different parts of the error
  ;; *** Start using sml-tes-trace-signals.
  ;;(sml-tes-trace-signals sml-tes-trace-signals-log
  (let ((regs   (cdr (assoc 'regs  item)))
	(slice  (cdr (assoc 'slice item))) ;; *** why is this repeated below???
	(id     (cdr (assoc 'id    item)))
	(kind   (cdr (assoc 'kind  item)))
	(myovs  nil)
	(aslice nil))
    (while regs
      (let* ((file    (caar (last regs))) ;; *** why processing last item first???
	     (regions (cdr (assoc file regs)))
	     (formattedRegs nil))
        (with-current-buffer (find-file-noselect file)
          ;; Put the information into a form in which it is easily processed
          (setq aslice `((id    . ,id)
			 (slice . ,slice)
			 (kind  . ,kind)
			 (box   . ,(sml-tes-contains-box regions))))
          (setq formattedRegs (sml-tes-mapslice nil regions aslice))
          ;; Highlight the regions
          (let ((ovs
                 (apply #'append
                        (mapcar #'sml-tes-highlight-region formattedRegs))))
            (mapc
             (lambda (ov) (overlay-put ov 'sml-tes-slice ovs))
             ovs)
            (setq myovs (append myovs ovs)))
          ;;	(setq regs (cdr regs))
          (setq regs (butlast regs))
          ))
      )
    ;; Keep track of the error slices
    (when (not sml-tes-slices)
      (push nil sml-tes-slices))
    (when (not (equal myovs nil))
      (push (reverse myovs) sml-tes-slices))
    )
  ;; *** matching paren for commented out paren above:
  ;;)
  (sml-tes-trace "HIGHLIGHTING A SLICE EXIT"))

(defun sml-tes-add-overlay (ov ovs)
  "Adds an overlay to a list of overlays.  It avoids repetitions."
  (let ((found   nil)
	(new-ovs nil)
	(cur-ov  nil)
	(occ     nil))
    (while (and (not found) ovs)
      (setq cur-ov (car ovs))
      (if (and (= (overlay-start ov) (overlay-start cur-ov))
	       (= (overlay-end   ov) (overlay-end   cur-ov)))
	  (progn
	    (setq found t)
	    (setq occ (overlay-get cur-ov 'sml-tes-occ))
	    (overlay-put cur-ov 'sml-tes-occ (1+ occ))
	    (setq new-ovs (append new-ovs (cons cur-ov (cdr ovs)) nil))
	    )
	(setq ovs (cdr ovs))
	(setq new-ovs (append new-ovs (cons cur-ov nil) nil))
	)
      )
    (if found
	new-ovs
      (let ((cpy (copy-overlay ov)))
	(push cpy sml-tes-overlays)
	(setq new-ovs (append new-ovs (cons cpy nil) nil))
	)
      )
    )
  )

(defun sml-tes-merge-slices ()
  "Merges all the errors found so far."
  (let ((all-slices sml-tes-slices)
	(slice      nil)
	(cur-slice  nil))
    (while sml-tes-merged-slice
      (delete-overlay (pop sml-tes-merged-slice)))
    (while all-slices
      (setq cur-slice (car all-slices))
      (while cur-slice
	(setq slice (sml-tes-add-overlay (car cur-slice) slice))
	(setq cur-slice (cdr cur-slice))
	)
      (setq all-slices (cdr all-slices))
      )
    (setq sml-tes-merged-slice slice)
    ;; (print "***************")
    ;; (mapc '(lambda (ov) (progn (print ov)
    ;; 			       (print (overlay-get ov 'sml-tes-occ))))
    ;; 	  slice)
    ;; (print "***************")
    ;; (mapc '(lambda (ov) (progn (print ov)
    ;; 			       (print (overlay-get ov 'sml-tes-occ))))
    ;; 	  (car sml-tes-slices))
    ;; (print "***************")
    (sml-tes-unfocus-all-slices)
    (sml-tes-adjust-slice-focus slice t)
    (sml-tes-maybe-display-slice-as-much-as-possible slice)
    (setq sml-tes-current-slice-not-viewed slice)
    (sml-tes-focus-next-part-of-slice slice)
    ;;(setq sml-tes-current-highlighted-slice (sml-tes-get-id-slice slice))
    )
  )

(defun sml-tes-is-box (reg)
  "test is a regions is a box"
  (let ((ret nil))
    (case (car reg)
      ;; If the error code is "N" then it is a box
      (N (setq ret t)))
    ret))

(defun sml-tes-contains-box (regs)
  "search through a list of regions to determine if there is a box"
  (let ((ret "NOBOX"))
    (while regs
      (when (sml-tes-is-box (car regs))
	(setq ret "BOX"))
      (setq regs (cdr regs)))
    ret))

(defun sml-tes-mapslice (regs mylist slice)
  "format the list of regions mylist into regs: from ((reg1) (reg2) ...) to ((reg1 slice) (reg2 slice) ..).
  slice is more than slices, it contains extra information"
  (while mylist
    (push (cons (car mylist) (cons slice nil)) regs)
    (setq mylist (cdr mylist))
    )
  regs
  )

;; *** MAY NEED TO ADD BUFFER AS EN EXTRA INPUT PARAMTER
(defun sml-tes-line-and-char-to-buf-and-pos (line char)
  "Transforms a line number and column number into a buffer and the position that (line, char) is in the buffer"
  (prog1
      (let (file)
	(cond ((<= line sml-tes-fake-lines)
	       (setq file 'fake))
	      (t
	       (setq file 'main)
	       (setq line (- line sml-tes-fake-lines))))
	(save-excursion
	  (set-buffer
	   (case file
	     (fake (get-buffer-create sml-tes-fake-buffer))
	     (main (current-buffer))))
	  ;;	     (main (find-file-noselect sml-tes-file-being-sliced))))
	  (save-excursion
	    (goto-char (point-min))
	    (forward-line (1- line))
	    (condition-case nil
		(forward-char (1- char))
	      (error nil))
	    (cons (current-buffer)
		  (point)))))
    ))


(defun sml-tes-highlight-region (args)
  "Highlights a region"
  (let ((reg   (car args))
	(info  (car (cdr args)))
	(id    (cdr (assoc 'id    (car (cdr args)))))
	(slice (cdr (assoc 'slice (car (cdr args))))))
    (set-text-properties 0 (length slice) nil slice)
    (if (functionp 'font-lock-append-text-property)
        (font-lock-append-text-property 0 (length slice) 'face '(:family "fixed") slice))
    (prog1
	(destructuring-bind (type (bline bchar eline echar) color . subitems) reg
	  (let* ((buf-and-pos-beg
		  (sml-tes-line-and-char-to-buf-and-pos bline bchar))
		 (beg (cdr buf-and-pos-beg))
		 (buf-and-pos-end
		  (sml-tes-line-and-char-to-buf-and-pos eline echar))
		 (end (cdr buf-and-pos-end))
		 (buf (car buf-and-pos-beg))
		 (ov (make-overlay
		      beg
		      (1+ end)
		      (car buf-and-pos-beg)
		      t nil)))
	    (or (eq buf (car buf-and-pos-end))
		(error "impossible"))
	    ;; Add the overlay to the list of overlays
	    (push ov sml-tes-overlays)
	    ;; Add the necessary information to the properties of the overlays
	    (overlay-put ov 'sml-tes-type   type)
	    (overlay-put ov 'sml-tes-id     id)
	    (overlay-put ov 'sml-tes-info   slice)
	    (overlay-put ov 'sml-tes-color  color)
	    (overlay-put ov 'sml-tes-occ    1)
	    (sml-tes-set-overlay-properties ov nil)
	    ;; Calculate and store the inforamtion for the more detailed information
	    ;; ***LINE BELOW CAUSES THE PROGRAM TO BE A LOT SLOWER
	    ;;(overlay-put ov 'sml-tes-more-info (sml-tes-find-info info))
	    ;; If the user wishes to have the verbose error messages shown, then show them
	    ;; otherwise show the non verbose error messages
	    (if sml-tes-verbose-error-messages
		(overlay-put ov 'help-echo (concat (overlay-get ov 'sml-tes-info) (overlay-get ov 'sml-tes-more-info)))
	      (overlay-put ov 'help-echo (overlay-get ov 'sml-tes-info)))
	    (overlay-put ov 'evaporate t)
	    (cons ov
		  (when subitems
		    (apply #'append
			   (mapcar #'sml-tes-highlight-region
				   (sml-tes-mapslice nil (car subitems) info)))))
	    )
	  )
      )))

(defun sml-tes-set-overlay-properties (ov focus)
  "Sets the properties of the overlays that make up the highlighting of the regions of the slices"
  (let* ((color               (overlay-get ov 'sml-tes-color))
	 (occ                 (overlay-get ov 'sml-tes-occ))
	 (message-first-part  (overlay-get ov 'sml-tes-info))
	 (message-second-part (overlay-get ov 'sml-tes-more-info)))

    ;; If the overlay is in focus display the information about the error
    ;; otherwise display no information
    (if (equal t focus)
	(progn
	  (when sml-tes-verbose-error-messages
	    (setq message-first-part (concat message-first-part message-second-part)))
	  (overlay-put ov 'help-echo message-first-part))
      (overlay-put ov 'help-echo nil))
    (overlay-put ov 'sml-tes-focus focus)
    (overlay-put ov 'face
		 (case (overlay-get ov 'sml-tes-type)
		   (L (case color
			((R) (if focus
				 (if (> occ 1)
				     'sml-tes-standard-error-large-occ-focus
				   'sml-tes-standard-error-focus)
			       'sml-tes-standard-error-non-focus))
			((B) (if focus 'sml-tes-end-point-one-focus        'sml-tes-end-point-one-non-focus))
			((P) (if focus 'sml-tes-end-point-two-focus        'sml-tes-end-point-two-non-focus))
			((G) (if focus 'sml-tes-merged-regions-focus       'sml-tes-merged-regions-non-focus))
			((O) (if focus 'sml-tes-further-explanations-focus 'sml-tes-further-explanations-non-focus))
			((Y) (if focus 'sml-tes-parsing-error-focus        'sml-tes-parsing-error-non-focus))
			))
		   (H (case color
			((R) (if focus 'sml-tes-standard-error-head-focus 'sml-tes-standard-error-non-focus))
			((B) (if focus 'sml-tes-end-point-one-head-focus  'sml-tes-end-point-one-non-focus))
			((P) (if focus 'sml-tes-end-point-two-head-focus  'sml-tes-end-point-two-non-focus))
			))
		   (N (case color
			((R) (if focus 'sml-tes-standard-error-box-focus       'sml-tes-standard-error-box-non-focus))
			((B) (if focus 'sml-tes-end-point-one-box-focus        'sml-tes-end-point-one-box-non-focus))
			((P) (if focus 'sml-tes-end-point-two-box-focus        'sml-tes-end-point-two-box-non-focus))
			((G) (if focus 'sml-tes-merged-regions-box-focus       'sml-tes-merged-regions-box-non-focus))
			((O) (if focus 'sml-tes-further-explanations-box-focus 'sml-tes-further-explanations-box-non-focus))
			))
		   (t (error "impossible"))))
    (overlay-put ov 'priority
		 (case color
		   ((B G O P)
		    (if focus 4 2))
		   ((R Y)
		    (if focus 3 1))))))

(defun sml-tes-format-slice-info (slice assumptions kind)
  "Formats the details of the error for displaying to the user"
  ;; Report the appropriate mesage for the number of context dependencies in the error
  ;; 0 context dependencies
  (if (equal '0 (safe-length assumptions))
      (concat "Error kind: " (car (cdr kind))
	      "\n\nSlice:\n" slice)
    ;; 1 context dependency
    (if (equal '1 (safe-length assumptions))
	(concat "Error kind: " (car (cdr kind))
		"\n\n"
		"Slice:\n" slice
		"\n\n"
		"Context Dependency: " (prin1-to-string (car assumptions)) " is neither a datatype nor an exception constructor"
		)
      ;; 2 or more context dependencies
      (let ((dependencies ""))
	(while (> (safe-length assumptions) 0)
	  (setq dependencies (concat dependencies (prin1-to-string (pop assumptions)) ", ")))
	(concat "Error kind: " (car (cdr kind))
		"\n\n"
		"Slice:\n" slice
		"\n\n"
		"Context Dependencies: " (substring dependencies 0 (- (length dependencies) 2)) " are neither datatype\nnor exception constructors"
		)
	)))
  )

(defun sml-tes-find-info (info)
  "Calculates the extra details of the error messages that are used for the verbose error messages"
  ;; Determine the different details of the error
  (let ((kind (car info))
	(box  (car (cdr info)))
	(legendItems nil))
    ;; If the slice contains a box, add box to the list of items that need to be displayed in the legend
    (when (equal "BOX" box)
      (push 5 legendItems))
    ;; Add the extra infomation depending on the kind of error - retrieve the approprite items that need to be displayed in the legend
    (case kind
      ((CIR) (push 4 legendItems) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a type is ocnstrained to contain itself\n(an infinite type)" (sml-tes-get-legend legendItems)))
      ((OVE) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is overloaded and is used with a type that does not match the types specified by the overloading declaration" (sml-tes-get-legend legendItems)))
      ((ARI) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a sequence of length N is constrained to be of\nlength N where M is not equal to N" (sml-tes-get-legend legendItems)))
      ((TYP) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when two type constructors are constrained to be equal\nbut are not, for example int = bool" (sml-tes-get-legend legendItems)))
      ((REC) (push 4 legendItems) (push 2 legendItems) (push 1 legendItems) (push 0 legendItems)(concat   "\n\nAdditional info: Occurs when two two records are constrained to be equal but a\nlabel appears in one record and does not appear in the other" (sml-tes-get-legend legendItems)))
      ((UNM) (push 4 legendItems) (push 2 legendItems) (push 1 legendItems) (push 0 legendItems) (concat "\n\nAdditional info: Occurs when a structure does not declare an identifier that\nit is supposed to" (sml-tes-get-legend legendItems)))
      ((MSI) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat "\n\nAdditional info: Occurs when a constructor occurs in a structure but not in its signature" (sml-tes-get-legend legendItems)))
      ((MST) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat "\n\nAdditional info: Occurs when a constructor occurs in a signature but not in a structure with this signature" (sml-tes-get-legend legendItems)))
      ((DTC) (push 4 legendItems) (push 1 legendItems) (push 0 legendItems) (concat "\n\nAdditional info: Occurs when a type constructor is defined as a type function in a structure and as a datatype in its signature" (sml-tes-get-legend legendItems)))
      ((PAR) (push 3 legendItems) (concat  "\n\nAdditional info: Please refer to the details above" (sml-tes-get-legend legendItems)))
      ((MUL) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is bound more than once in one\ndeclaration" (sml-tes-get-legend legendItems)))
      ((VAL) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a value variable is supplied an argument in a\npattern" (sml-tes-get-legend legendItems)))
      ((EXV) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is declared as a value variable and used as an exception" (sml-tes-get-legend legendItems)))
      ((EXD) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is declared as datatype constructor and used as an exception" (sml-tes-get-legend legendItems)))
      ((LON) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is declared as a value variable and used as a datatype constructor" (sml-tes-get-legend legendItems)))
      ((DCE) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is declared as an exception and used as a datatype constructor" (sml-tes-get-legend legendItems)))
      ((UNG) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when SML does not allow generalisation of type variables at certain value declarations" (sml-tes-get-legend legendItems)))
      ((INC) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when there is a free type variable in a datatype\nor type declaration" (sml-tes-get-legend legendItems)))
      ((APP) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when an identifier is both applied and not applied to\nan argument inside a pattern" (sml-tes-get-legend legendItems)))
      ((CAN) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a constructor is non applied in a pattern and defined to take an argument" (sml-tes-get-legend legendItems)))
      ((CNA) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a constructor is applied in a pattern and defined to take no argument" (sml-tes-get-legend legendItems)))
      ((FUN) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a function is declared with two differing names" (sml-tes-get-legend legendItems)))
      ((ARG) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when a function is defined to take an\ninconsistent nuber of arguments" (sml-tes-get-legend legendItems)))
      ((FRE) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when there is a type variable in the top level\nenviroment that is not bound to anything (free)" (sml-tes-get-legend legendItems)))
      ((LAS) (push 0 legendItems) (concat  "\n\nAdditional info: Occurs when the identifier on teh left of an \"as\" is not a\nvalue variable" (sml-tes-get-legend legendItems)))
      ((FNE) (push 0 legendItems) (concat  "\n\nAdditional info: Expressions with recursive value must be functions" (sml-tes-get-legend legendItems)))
      ((REA) (push 0 legendItems) (concat  "\n\nAdditional info: A real cannot occurs within a pattern" (sml-tes-get-legend legendItems)))
      ((IDE) (push 3 legendItems) (concat  "\n\nAdditional info: Free identifier" (sml-tes-get-legend legendItems)))
      )
    )
  )

(defun sml-tes-get-legend (items)
  "Produces the legend that contains the items 'items' (a list that is taken in as a paramater)"
  (let ((legend "\n\nLEGEND:\n"))
    ;; For each item needed, retrieve the appropriate inforamtion
    (while items
      (setq legend (concat legend (sml-tes-get-legend-info (car items))))
      (setq items (cdr items)))
    ;; Return the legend
    legend))

(defun sml-tes-get-legend-info (item)
  "Returns the approprite legend item for 'item' (the paramater of the function)"
  (let ((red    "RED")
	(grey   "GRAY")
	(blue   "BLUE")
	(green  "GREEN")
	(purple "PURPLE")
	(yellow "YELLOW")
	(box    "BOX")
	(indent "\n\t   "))
    ;; Add the appropriate details for 'item' (the paramater of the function)
    (case item
      ((0) (concat (propertize red    'face 'sml-tes-standard-error-focus) ": Indicates that the highlighted text contributes to the error.\n"))
      ((1) (concat (propertize grey   'face 'sml-tes-end-point-two-focus) "/" (propertize blue 'face 'sml-tes-end-point-one-focus) ": Indicates that the highlighted text is an end point of a type\nconstructor clash, an arity clash or a record clash.\n"))
      ((2) (concat (propertize green  'face 'sml-tes-merged-regions-focus) ": Indicates the endpoint of a record clash. The highlighted text appears\nin both clashing records.\n" ))
      ((3) (concat (propertize yellow 'face 'sml-tes-parsing-error-focus) ": Indicates that either;" indent "1. The file contains a syntax error," indent "2. The file contains a feature of SML not yet handled by the type" indent "   error slicer," indent "3. The highlighted file is not a valid \".sml\" or \".tes\" file, or" indent "4. The highlighted file does not exit.\n"))
      ((4) (concat (propertize purple 'face 'sml-tes-further-explanations-focus) ": Indicates that the highlighted code provides information about the\nstatus of an identifier or that the code is expansive.\n"))
      ((5) (concat (propertize box    'face '(:box (:line-width 2 :color t :style nil)))": Can indicate:\n\t1: The application of a function to an argument (the contents of the" indent "box) takes part in an error. It is usually convenient to surround" indent "the argument of a function when the application participates in the" indent "error,or\n\t2: The contents of the box are the unique argument of a type name to" indent "make explicit that its arity is 1 (that is, has one argument). This" indent "is because there is no section of code to highlight, making explicit" indent "when its arity is 1 .\n" ))
      )))

(defun sml-tes-get-id-slice (slice)
  "Returns the identifier of the slice"
  ;; The slice is a list of overlays, and these overlays should all have the same identifier
  (let ((id nil))
    (if (car slice)
	(setq id (overlay-get (car slice) 'sml-tes-id))
      id)
    )
  )

(defun sml-tes-highlight-next-slice (slice)
  "Highlight the slice given in argument"
  ;; Set the next slice to be in focus
  (sml-tes-focus-slice slice)
  (sml-tes-maybe-display-slice-as-much-as-possible slice)
  ;; Set the parts of the slice that have not been viewed to be the whole slice
  (setq sml-tes-current-slice-not-viewed slice)
  ;; Focus on the begining of the slice
  (sml-tes-focus-next-part-of-slice slice)
  ;;(2010-06-14)DEBUG:(print (format "[CURRENTLY HIGHLIGHTING %d]" (sml-tes-get-id-slice slice)))
  ;; Assign the identifier of the newly highlighted slice to the variable "sml-tes-current-highlighted-slice"
  (setq sml-tes-current-highlighted-slice (sml-tes-get-id-slice slice))
  )

(defun sml-tes-next-slice ()
  "Highlights the next slice in the list"
  (interactive)
  ;; If the current slicer pointer is non nil
  (if sml-tes-current-slice-pointer
      ;; If there is a next slice
      (if (car sml-tes-current-slice-pointer)
	  (progn
	    ;; Add the first item of the current slice pointer to the end of the list
	    (setq sml-tes-current-slice-pointer (append sml-tes-current-slice-pointer (cons (car sml-tes-current-slice-pointer) nil)))
	    ;; Set current slice pointer to be the tail of itself
	    (setq sml-tes-current-slice-pointer (cdr sml-tes-current-slice-pointer))
	    (sml-tes-highlight-next-slice (car sml-tes-current-slice-pointer))
	    )
	;; The current slice pointer is nil, set it to point to all of the slices
	;; and unfocus all of the slices.
	(sml-tes-unfocus-all-slices)
	(setq sml-tes-current-slice-pointer (append sml-tes-current-slice-pointer (cons (car sml-tes-current-slice-pointer) nil)))
	(setq sml-tes-current-slice-pointer (cdr sml-tes-current-slice-pointer))
	)
    ;; The current slice pointer is nil, set it to point to all of the slices
    ;; and unfocus all of the slices.
    (setq sml-tes-current-slice-pointer (butlast sml-tes-slices))
    (sml-tes-unfocus-all-slices)))

(defun sml-tes-next-slice-by-id (id)
  "Highlights the next slice in the list that has the identifier id"
  (let ((done  nil)
	(firstid nil))
    (if sml-tes-current-slice-pointer
	(progn
	  (if (car sml-tes-current-slice-pointer)
	      (setq firstid (sml-tes-get-id-slice (car sml-tes-current-slice-pointer)))
	    (setq done t)
	    )
	  (while (not done)
	    (if (car sml-tes-current-slice-pointer)
		(progn
		  (setq sml-tes-current-slice-pointer (append sml-tes-current-slice-pointer (cons (car sml-tes-current-slice-pointer) nil)))
		  (setq sml-tes-current-slice-pointer (cdr sml-tes-current-slice-pointer))
		  ;;(sml-tes-trace (format "ID: %d - %d - %d" (sml-tes-get-id-slice (car sml-tes-current-slice-pointer)) id firstid))
		  (if (equal firstid (sml-tes-get-id-slice (car sml-tes-current-slice-pointer)))
		      ;; We didn't find the error id
		      (setq done t)
		    ) ;; END IF
		  (if (equal id (sml-tes-get-id-slice (car sml-tes-current-slice-pointer)))
		      (progn
			(sml-tes-highlight-next-slice (car sml-tes-current-slice-pointer))
			(setq done t)
			)
		    ) ;; END IF
		  ) ;; END PROGN
	      (setq done t)
	      ) ;; END IF
	    ) ;; END WHILE
	  ) ;; END PROGN
      ) ;; END IF
    ) ;; END LET
  ) ;; END FUN

(defun sml-tes-prev-slice ()
  "Highlights the previous slice in the list"
  (interactive)
  ;; If the current slicer pointer is non nil
  (let ((last (last sml-tes-current-slice-pointer)))
    (if last
	;; If there is a previous slice
	(if (car last)
	    (progn
	      ;; Set the previous slice to be in focus
	      (sml-tes-focus-slice (car last))
              (sml-tes-maybe-display-slice-as-much-as-possible (car last))
	      ;; Set the parts of the slice that have not been viewed to be the whole slice
	      (setq sml-tes-current-slice-not-viewed (car last))
	      ;; Focus on the begining of the slice
;;	      (sml-tes-next-part-of-slice)
	      (sml-tes-focus-next-part-of-slice (car sml-tes-current-slice-pointer))
	      ;; Remove the last slice from the list that current slice pointer points to
	      (setq sml-tes-current-slice-pointer (butlast sml-tes-current-slice-pointer))
	      ;; Add the last slice to the begining of the list that current slice pointer points to
	      (push (car last) sml-tes-current-slice-pointer))
	  ;; The current slice pointer is nil, set it to point to all of the slices
	  ;; and unfocus all of the slices.
	  (sml-tes-unfocus-all-slices)
	  (setq sml-tes-current-slice-pointer (butlast sml-tes-current-slice-pointer))
	  (push (car last) sml-tes-current-slice-pointer))
    ;; The current slice pointer is nil, set it to point to all of the slices
    ;; and unfocus all of the slices.
    (setq sml-tes-current-slice-pointer (butlast sml-tes-slices))
    (sml-tes-unfocus-all-slices))))

(defun sml-tes-overlay-in-view (overlay)
  "Tests whether or not the overlay is in view"
  (let ((ov-start (overlay-start overlay))
	(ov-end (overlay-end overlay)))
    ;; If the buffer of the overlay is not currently in view ...
    (if (not (get-buffer-window (overlay-buffer overlay)))
	;; ... then the overlay is not in view, so return nil ...
	nil
      ;; ... otherwise, if the whole of the overlay is in view ...
      (if (and (pos-visible-in-window-p ov-start) (pos-visible-in-window-p ov-end))
	  ;; ... return true ...
	  t
	;; ... otherwise return nil
	nil
	))))

(defun sml-tes-next-part-of-slice ()
  "Displays the next part of the slice that is not currently in view"
  (interactive)
  (let ((list sml-tes-current-slice-not-viewed))
    (setq sml-tes-current-slice-not-viewed nil)
    ;; If all parts of the current slice have been viewed
    ;; then set the list that contains the overlays which
    ;; have yet to be viewed to contain all of the overlays
    ;; in the current slice
    (when (not list)
      (setq sml-tes-current-slice-not-viewed (car sml-tes-current-slice-pointer))
      (sml-tes-next-part-of-slice)
      )
    ;; Foucs the next part of the current slice which is not currently visible
    (sml-tes-focus-next-part-of-slice list)
    ;; Update the list of the overlays in the current slice which have yet to be viewed
    (while list
      (when (not (sml-tes-overlay-in-view (car list)))
	(setq sml-tes-current-slice-not-viewed (append sml-tes-current-slice-not-viewed (cons (car list) nil)))
	)
      (setq list (cdr list))
      )
    )
  )

(defun sml-tes-focus-next-part-of-slice (list)
  "Focuses on the next part of the current slice which is not currently visible"
  (let ((overlay (car list)))
    (when overlay
      ;; When the next overlay has no buffer (i.e. the buffer has been killed)...
      (if (not (overlay-buffer overlay))
          ;; tidy up the list of slices so that it does not contain overlays which
          ;; exist in buffers which do not exist.
          ;; *** TO FIX: After tidying, if this slice still exists,
          ;; display part of it.  If this slice has been tidied away,
          ;; is it really okay to just silently do nothing?
          (sml-tes-tidy-slices)

        ;; switch-to-buffer and/or select-window will be enough to
        ;; arrange to change the buffer the next time the user is
        ;; asked for input, but we may still be in an old unrelated
        ;; buffer, so change now.
        (set-buffer (overlay-buffer overlay))

        ;; Set the selected window to show the buffer that contains the overlay
        (let ((w (get-buffer-window (current-buffer))))
          (if w
              (select-window w)
            (switch-to-buffer (current-buffer))
            (setq w (selected-window)))

          (goto-char (overlay-start overlay))

          ;; just in case already displaying buffer with different window-point
          (set-window-point w (point))

          (save-excursion
            (beginning-of-line 0) ;; moves to beginning of previous line
            (set-window-start w (point))))))))


(defun sml-tes-skip-past-word-if-in-one ()
  (when (and (not (bobp))
             (save-excursion
               (backward-char)
               (looking-at "[A-Za-z0-9_'][A-Za-z0-9_']")))
    (skip-chars-forward "A-Za-z0-9_'")
    (if (looking-at "\\s-")
        (forward-char))))

(defconst sml-tes-dec-spec-keyword-list
  '("datatype" "eqtype" "exception" "structure" "type" "val"
    "signature" "struct" "sig" "end"))

(defconst sml-tes-dec-spec-keyword-regexp
  (mapconcat #'identity
             sml-tes-dec-spec-keyword-list
             "\\|"))

(defconst sml-tes-backward-to-dec-regexp
  (format "\\(%s\\)\\s-" sml-tes-dec-spec-keyword-regexp))

(defun sml-tes-backward-to-dec ()
  (interactive)
  (sml-tes-skip-past-word-if-in-one)
  (while (and (re-search-backward
               "[A-Za-z0-9_']"
               nil
               'move)
              (skip-chars-backward "[A-Za-z0-9_']")
              (not (looking-at sml-tes-backward-to-dec-regexp)))))

(defconst sml-tes-forward-to-dec-regexp
  (format "[^A-Za-z0-9_']\\(%s\\)\\s-" sml-tes-dec-spec-keyword-regexp))

(defun sml-tes-forward-to-dec ()
  (interactive)
  (if (re-search-forward sml-tes-forward-to-dec-regexp nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun sml-tes-alist-put (sym key val)
  (let ((item (assoc key (symbol-value sym))))
    (if item
        (setcdr item val)
      (push (cons key val) (symbol-value sym)))))

(defun sml-tes-alist-get (sym key)
  (cdr (assoc key (symbol-value sym))))

(defvar sml-tes-auto-display-entire-slice t)

(defun sml-tes-maybe-display-slice-as-much-as-possible (ovs)
  (if sml-tes-auto-display-entire-slice
      (sml-tes-display-slice-as-much-as-possible ovs)))

;; *** TODO: Bind this to a key!
(defun sml-tes-display-slice-as-much-as-possible (ovs)
  "Function to fit as much of the current slice on the screen as possible"
  (interactive (list (car sml-tes-current-slice-pointer)))
  (let (bufs earliests latests overlays)
    (dolist (ov ovs)
      (let* ((buf (overlay-buffer ov))
             (old-earliest (sml-tes-alist-get 'earliests buf))
             ;;(old-latest (sml-tes-alist-get 'latests buf))
             )
        (pushnew buf bufs)
        (sml-tes-alist-put 'overlays
                           buf
                           (adjoin ov
                                   (sml-tes-alist-get 'overlays buf)))
        (sml-tes-alist-put 'earliests buf
                           (if old-earliest
                               (min old-earliest
                                    (overlay-start ov))
                             (overlay-start ov)))
        ;;(sml-tes-alist-put 'latests buf
        ;;                   (if old-latest
        ;;                       (max old-latest
        ;;                            (overlay-end ov))
        ;;                     (overlay-end ov)))
        ))
    (if sml-tes-delete-other-windows
	(delete-other-windows)
      )
    (let ((wins (list (selected-window)))
          next-level-wins)
      (while (< (+ (length wins)
                   (length next-level-wins))
                (length bufs))
        (when (null wins)
          (setq wins (nreverse next-level-wins))
          (setq next-level-wins nil))
        (let ((new-win (split-window (car wins))))
          (push (pop wins) next-level-wins)
          (push new-win next-level-wins)))
      (setq wins (append (nreverse next-level-wins) wins))
      (dolist (buf bufs)
        (with-current-buffer buf
          (let ((is-basis
                 ;; *** don't test against the string "basis.sml".  do this right!
		 ;; why don't we test: (equal sml-tes-basis-file buffer-file-name)
                 (equal sml-tes-basis-file buffer-file-name))
                (win (pop wins)))
            (set-window-buffer win buf)
            (set-window-start
             win
             (save-excursion
               (goto-char (sml-tes-alist-get 'earliests buf))
               (cond (is-basis
                      (forward-char 1)
                      (sml-tes-backward-to-dec))
                     (t
                      (beginning-of-line)))
               (point)))
            (when is-basis
              ;; *** instead of disabling undo, this should be done
              ;; with overlays instead of text properties.
              ;; *** Setting read only and disabling undo should be
              ;; done elsewhere.
              ;; *** read-only should be adjusted with let so it is
              ;; properly restored in case of abort.
              (setq buffer-read-only nil)
              (buffer-disable-undo)
              (put-text-property (point-min) (point-max) 'display nil)
              (goto-char (point-min))
              (let ((sorted-ovs
                     (sort (sml-tes-alist-get 'overlays buf)
                           (lambda (o1 o2)
                             (< (overlay-start o1)
                                (overlay-start o2))))))
                (while sorted-ovs
                  (let ((ov (pop sorted-ovs))
                        (pos (save-excursion (beginning-of-line) (point))))
                    (goto-char (1+ (overlay-start ov)))
                    (sml-tes-backward-to-dec)
                    (put-text-property pos (save-excursion (beginning-of-line) (point)) 'display "\n")
                    (sml-tes-forward-to-dec)
                    (while (and sorted-ovs
                                (< (overlay-start (car sorted-ovs))
                                   (point)))
                      (pop sorted-ovs)))))
              (put-text-property (point) (point-max) 'display "\n")
              (setq buffer-read-only t)
              (set-buffer-modified-p nil)
              ;; (goto-char (1+ (sml-tes-alist-get 'earliests buf)))
              ;; ;;(put-text-property (point) (1+ (point)) 'display "•")
              ;; (sml-tes-backward-to-dec)
              ;; ;;(put-text-property (point) (1+ (point)) 'display "‣")
              ;; (put-text-property (point-min) (point) 'invisible t)
              ;; (goto-char (sml-tes-alist-get 'latests buf))
              ;; ;;(put-text-property (1- (point)) (point) 'display "◦")
              ;; (sml-tes-forward-to-dec)
              ;; ;;(put-text-property (1- (point)) (point) 'display "⁃")
              ;; (put-text-property (point) (point-max) 'invisible t)
              )))))))

(defun sml-tes-adjust-slice-focus (ovs focus)
  "Adjust the focus of the slice that is made up of the overlays 'ovs'"
  (mapc
   (lambda (ov) (sml-tes-set-overlay-properties ov focus))
   ovs))

(defun sml-tes-focus-slice (slice)
  "Sets the slice 'slice' to be in focus"
  (sml-tes-adjust-slice-focus sml-tes-merged-slice nil)
  (mapc
   (lambda (sl)
     (sml-tes-adjust-slice-focus
      sl
      (or (null slice)
	  (eq slice sl))))
   sml-tes-slices)
  )

;; This function is not called at the present time
(defun sml-tes-print-error-message-buffer ()
  "Prints the error messages in the message buffer"
  (let ((error-message (overlay-get (caar sml-tes-current-slice-pointer) 'sml-tes-info))
	(error-message-extra-details (overlay-get (caar sml-tes-current-slice-pointer) 'sml-tes-more-info)))
    ;; Remove all of the text properties from the error messages
    (set-text-properties 0 (length error-message) nil error-message)
    (set-text-properties 0 (length error-message-extra-details) nil error-message-extra-details)
    ;; Print error message
    (print "****\tError Message (start)\t****")
    ;; If the user has the verbose error messgae option selected ...
    (if sml-tes-verbose-error-messages
	;; ... then print the verbose error messages ...
	(print (concat error-message error-message-extra-details))
      ;; ... otherwise print the shortened error message
      (print error-message))
    (print "****\tError Message (end)\t****")
    )
  )

(defun sml-tes-unfocus-all-slices ()
  "Sets all of the slices to be out of focus"
  (mapc
   (lambda (sl)
     (sml-tes-adjust-slice-focus
      sl
      nil))
   sml-tes-slices)
  )

(defun sml-tes-forget-all-slices ()
  "Removes all of the details about all of the slices"
  (interactive)
  (sml-tes-trace "FORGETTING ENTER")
  ;; stops the sml-tes process if it exists
  (sml-tes-stop-sml-tes-process)
  ;; deletes the temporary directory
  (sml-tes-delete-temporary-directory)
  ;; removes the highlightings
  (while sml-tes-overlays
    (delete-overlay (pop sml-tes-overlays)))
  (setq sml-tes-slices nil)
  (setq sml-tes-current-slice-pointer nil)
  (setq sml-tes-current-highlighted-slice nil)
  )

(defun sml-tes-forget-all-slices-file ()
  "Removes all of the details about all of the slices in the current file"
  (interactive)
  ;; Set pointer to be an exact copy of sml-tes-overlays
  (let ((pointer sml-tes-overlays)
	(buffer (current-buffer)))
    ;; Set sml-tes-overlays to be nil
    (setq sml-tes-overlays '())
    ;; While we have not looked at all of the overlays
    (while (car pointer)
      ;; If the overlay we are looking at is in the current buffer delete it...
      (if (equal (overlay-buffer (car pointer)) buffer)
	  (delete-overlay (car pointer))
	;; ...otherwise add it back to sml-tes-overlays
	(push (car pointer) sml-tes-overlays))
      ;; Increment the pointer to look at the next slice
      (setq pointer (cdr pointer)))
    ;; If all of the slices we in this buffer, then...
    (if (equal 0 (safe-length sml-tes-overlays))
	(progn
	  ;; Set the pointer to the current slice to nil
	  (setq sml-tes-current-slice-pointer nil)
	  ;; Set the list of Slices to nil
	  (setq sml-tes-slices nil)
	  ;; Set the identifier of the currently highlighted slice to nil
	  (setq sml-tes-current-highlighted-slice nil)
	  )
      )
    ;; Tidy up the slcies
    (sml-tes-tidy-slices)))

(defun sml-tes-forget-slice-by-id (id)
  "Removes the slice with id 'id'. Used to remove slices when they are no longer needed
     i.e. when the slicer merges 2 or more slices"
  ;; Set pointer to be an exact copy of sml-tes-overlays
  (let ((pointer sml-tes-overlays))
    ;; Set sml-tes-overlays to be nil
    (setq sml-tes-overlays '())
    ;; While we have not looked at all of the overlays
    (while (car pointer)
      ;; If the current overlay is in the slice with id 'id' delete it...
      (if (equal id (overlay-get (car pointer) 'sml-tes-id))
	  (delete-overlay (car pointer))
	;; ...otherwise add it back to the list of overlays
	(push (car pointer) sml-tes-overlays))
      ;; Look at the next overlay in the list
      (setq pointer (cdr pointer)))
    ;; If all of the overlays were in the slice with id 'id'
    (if (equal 0 (safe-length sml-tes-overlays))
	(progn
	  ;; Set the pointer to the current slice to nil
	  (setq sml-tes-current-slice-pointer nil)
	  ;; Set the list of Slices to nil
	  (setq sml-tes-slices nil)))
    ;; Tidy up the slcies
    (sml-tes-tidy-slices)))

;; *** TO FIX in sml-tes-tidy-slices:
;;
;; 1. This reverses the order of the slices, thereby confusing
;;     the user.
;;
;; 2. Why is this putting a nil entry in the list?  It can gets
;;    confused if nil exists in the list in the middle.  (Can this
;;    happen?)
;;
;; 3. This does not check that every overlay in each slice still
;;    exists.  It only checks the buffer of the first overlay in each
;;    slice.
;;
(defun sml-tes-tidy-slices ()
  "Ensures that the list of slices is tidy (there are no overlays that have been deleted)"
  ;; Set pointer to be an exact copy of sml-tes-slices
  (let ((pointer sml-tes-slices))
    ;; Set sml-tes-slices to be nil
    (setq sml-tes-slices nil)
    ;; Add 'nil' to the beginning of the list
    (push nil sml-tes-slices)
    ;; While we have not looked at all of the slices
    (while (car pointer)
      ;; When the current slice has not been deleted (exists in a buffer) add it back to sml-tes-slices
      (when (not (equal (overlay-buffer (caar pointer)) nil))
	(push (car pointer) sml-tes-slices))
      ;; Increment the pointer to look at the next slice
      (setq pointer (cdr pointer))
      ))
  ;; Set the current-slice-pointer to point to the list of slices
  (setq sml-tes-current-slice-pointer (butlast sml-tes-slices)))

(defun sml-tes-trace (msg &rest data)
  "Writes a message and some data (optional) so that useful information can be displayed to the user"
  (apply #'message (concat "(SML-TES) " msg) data)
  (if (fboundp 'redisplay)
      (redisplay t)))

(defun sml-tes-set-timelimit (time)
  "Changes the time that the slicer is run for"
  (interactive "nnew time limit:")
  (setq sml-tes-timelimit time))

(defun sml-tes-set-basis-option (option)
  "Changes the basis option (0/1/2 as explained above)"
  (interactive "nnew basis option:")
  (setq sml-tes-basis-option option))

(defun sml-tes-toggle-verbose ()
  "Toggle whether or not the verbose error messages are shown"
  (interactive)
  (setq sml-tes-verbose-error-messages (not sml-tes-verbose-error-messages)))

(defun sml-tes-show-help ()
  "Loads and displays the help file for the Type Error Slicer"
  (interactive)
  (let* ((cur-window (selected-window))
	 (help-window (split-window cur-window nil)))
    (select-window help-window)
    (view-file (expand-file-name "SML-TES-HELP" tes-emacs-directory))
    )
  )

(defun sml-tes-set-compiler-smlnj ()
  "Toggle SML/NJ as preferred compiler"
  (interactive)
  	(setq sml-tes-user-compiler-smlnj (not sml-tes-user-compiler-smlnj))
	(setq sml-tes-user-compiler-mlton nil)
	(setq sml-tes-user-compiler-polyml nil)
)

(defun sml-tes-set-compiler-mlton ()
  "Toggle MLton as preferred compiler"
  (interactive)
  	(setq sml-tes-user-compiler-mlton (not sml-tes-user-compiler-mlton))
	(setq sml-tes-user-compiler-smlnj nil)
	(setq sml-tes-user-compiler-polyml nil)
)

(defun sml-tes-set-compiler-polyml ()
  "Toggle PolyML as preferred compiler"
  (interactive)
  	(setq sml-tes-user-compiler-polyml (not sml-tes-user-compiler-polyml))
	(setq sml-tes-user-compiler-mlton nil)
	(setq sml-tes-user-compiler-smlnj nil)
)

(defun sml-tes-run-compiler-after-slicer (file)
	"Runs SML/NJ only if the slicer is not running and if the slicer hasn't found any error"
	(let ((proc (get-process "sml-tes")))
		(if (equal nil proc)
			(if (< sml-tes-item-count 1)
				(progn
             		(if sml-tes-user-compiler-smlnj
						(progn
							(split-window)
								(shell)
									(process-send-string "shell" "sml\n")
									(process-send-string "shell" (format "use \"%s\";\n" file))
						)
					)
               		(if sml-tes-user-compiler-mlton
						(progn
							(split-window)
                       			(shell)
						 			(process-send-string "shell" (format "mlton" file "-verbose 1"))
									(process-send-string "shell" "\n")
						)
               		)
					(if sml-tes-user-compiler-polyml
						(progn
							(split-window)
								(shell)
									(process-send-string "shell" "poly\n")
									(process-send-string "shell" (format "PolyML.use \"%s\"\n" file))
						)
					)
				)
			)
        	(run-at-time 1 nil 'run-compiler-after-slicer file)
		)
    )
)

(defun sml-tes-run-slicer-then-compiler ()
  "Runs TES then SML/NJ if the slicer does not find any error"
	(interactive)
	(sml-tes-run-slicer-exec)
	(run-compiler-after-slicer (buffer-file-name))
)