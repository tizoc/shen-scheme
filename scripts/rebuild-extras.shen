(define license
  -> "c#34;Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clausec#34;

")

(defcc shen.<name>
  X := (if (symbol? X)
           X
           (error "~A is not a legitimate function name.~%" X)))

(define make-file
  ShenFile -> (let Message (output "compiling ~A~%" ShenFile)
                   Shen (read-file ShenFile)
                   KL (map (function make-kl-code) Shen)
                   StringKL (@s (license) (list->string KL))
                   KLFile (klfile ShenFile)
                   Write (write-to-file KLFile StringKL)
                   KLFile))

(define make-kl-code
  [define F | Rules] -> (shen.elim-def [define F | Rules])
  [defcc F | Rules] -> (shen.elim-def [defcc F | Rules])
  Code -> Code)

(define klfile
  ".shen" -> ".kl"
  (@s S Ss) -> (@s S (klfile Ss)))

(define list->string
  [] -> ""
  [[defun fail | _] | Y] -> (@s "(defun fail () shen.fail!)" (list->string Y))
  [X | Y] -> (@s (make-string "~R~%~%" X) (list->string Y)))

(define make
  -> (map (function make-file)
          ["shen/extensions/common.shen"
           "shen/extensions/chibi.shen"
           "shen/extensions/gauche.shen"
           "overwrites.shen"]))

(make)
