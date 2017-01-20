(defun print-eml-loc (loc str)
  (format t (concatenate 'string "[" loc ":" str "]~%"))
  (finish-output)
  )

;; ------ structures ------

(defstruct inl val)
(defstruct inr val)

;; ------ functions ------

(defun fix (str f)
  ;;(print-eml-loc "fix" str)
  (funcall f #'(lambda () (fix str f)))
  )
