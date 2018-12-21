;;;; -*- Mode: Lisp -*-

;;; store

(defun store (Lst Ind Val NewLst)
  (cond 
   ((null Lst) ())
    ((= Ind 0) (append NewLst (cons Val ()) (cdr Lst)))
    (t
     (store (cdr Lst) (1- Ind) Val (append NewLst (cons (car Lst) ()))))))


(defun build_instruction (List)
  (let* (
	(PC (nth 2 List))
	(Mem (nth 3 List)))
  (append (cons (decode_OpCode Pc Mem) ()) (cons (Decode_Immed PC Mem) ()))))


(defun set_Flag (Val)
  (cond
   ((< Val 0) "flag")
   ((> Val 999) "flag")
   (t
    "noflag")))

(defun build_state (Acc PC Mem In Out Flag Halt)
  (cond 
   ((= Halt 0)
   (append (cons "State" ()) (cons Acc ()) (cons PC ()) Mem In Out (cons Flag ())))
   ((= Halt 1)
   (append (cons "Halted_State" ()) (cons Acc ()) (cons PC ()) Mem In Out (cons Flag ()))
   )))

;;; fetch carica un dato dalla memoria
(defun fetch (Lst Ind) 
  (nth Ind Lst))


(defun decode_immed (PC Mem) 
  (let* (
	 (x (nth PC Mem)))
  (mod x 100)))


(defun decode_OpCode (PC Mem) 
  (let* (
	 (x (nth PC Mem)))
    (cond 
      ((= x 901) x)
      ((= x 902) x)
      (t
       (/ (- x (mod x 100)) 100)))))
  

(defun one_instruction (OpCode Immed List)
  (let* (
    (Acc (nth 1 List))
    (PC (nth 2 List))
    (Mem (nth 3 List))
    (In (nth 4 List))
    (Out (nth 5 List))
    (Flag (nth 6 List)))
     (cond
      ((= OpCode 1) 
       (build_state (add Acc Immed Mem) (UpPC PC) (cons Mem ()) In Out (set_flag Acc) 0))
      ((= OpCode 2) 
       (build_state (sub Acc Immed Mem) (UpPC PC) (cons Mem ()) In Out (set_flag Acc) 0))
      ((= Opcode 3) 
       (build_state (fetch Mem Immed) (UpPC PC) (cons Mem ()) In Out Flag 0))
      ((= OpCode 5) 
       (build_state Acc (UpPC PC) (cons (store Mem Immed Acc ())()) In Out Flag 0))
      ((= OpCode 6) 
       (build_state Acc Immed (cons Mem ()) In Out Flag 0))
      ((= OpCode 7) ())
      ((= OpCode 8) ())
      ((= OpCode 901)
       (build_state (Pop In) (UpPC PC) (cons Mem ()) In Out Flag 0))
      )))

(defun execution_loop (List)
  (let* (
         (Lst (build_instruction List))
         (OpCode (nth 0 Lst))
         (Immed (nth 1 Lst))
         (State (nth 0 List))
         (Out (nth 5 List)))
  (cond 
   ((equal State "State")
    (execution_loop (one_instruction OpCode Immed List)))
   ((equal State "halted_state") Out))))
  

(defun upPC (oldPC) 
  (cond 
   ((< oldPC 99) (1+ oldPC))
   ((= oldPC 99) 0 )))

(defun add (Acc PC Mem) 
  (let* (
	(Val (nth PC Mem)))
  (+ Acc Val)))


(defun sub (Acc PC Mem) 
  (let* (
	 (Val (nth PC Mem)))
  (- Acc Val)))


; (mod :istruzione 1000) ? XX
; (nth 0 '(foo bar baz)) =>  FOO

; funzioni built-in : defparameter, defun, cons, car, cdr, list, cond, nth, rest, first, second, ..., thenth, last, atom,

