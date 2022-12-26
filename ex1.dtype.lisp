(include-sys "stdio.h")

(def (function (dd 0 code identity 0 1 "identity") (type char)) 
  (
   (return (char (dd 1 arg 0 0 1 "c")))
  )
)

(def (function (dd 0 code main 0 1 "main") (type int))
  (
    ((set arg)		1 (type char) (dd 1 (char constant) "x" 0 1 "x"))
    (call		(function (dd 0 code identity 0 1 "identity") (type char)))
    ((move result)	(type char) (dd 1 local 0 0 1 "x"))
    ((set arg)		1 (type string) (dd 1 (string constant) "result = %c\n" 0 1 nil))
    ((set arg)		2 (type char) (dd 1 local 0 0 1 "x"))
    (call		(function (dd 0 code printf 0 1 “printf”) (type void)))
  )
)

