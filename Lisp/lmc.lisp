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
    (append (cons "State" ()) (cons Acc ()) (cons PC ())
	    Mem In Out (cons Flag ())))
   ((= Halt 1)
    (append (cons "Halted_State" ()) (cons Acc ()) (cons PC ())
	    Mem In Out (cons Flag ()))
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
	(add_execute Acc PC (cons Mem ()) In Out))
      ((= OpCode 2)
       (sub_execute Acc PC (cons Mem ()) In Out))
      ((= Opcode 3)
       (build_state (fetch Mem Immed) (UpPC PC) (cons Mem ())
		    In Out Flag 0))
      ((= OpCode 5)
       (build_state Acc (UpPC PC) (cons (fetch Mem Immed) ()) In Out Flag 0))
      ((= OpCode 6)
       (build_state Acc Immed (cons Mem ()) In Out Flag 0))
      ((= OpCode 7)
       (branch-if-zero Immed Acc PC (cons Mem ())
					(cons In ()) (cons Out ()) Flag))
     ((= OpCode 8)
      (branch-if-positive Immed Acc PC (cons Mem ())
					(cons In ()) (cons Out ()) Flag))
      ((= OpCode 901)
       (build_state (Pop In) (UpPC PC) (cons Mem ())
		    (cons In ()) (cons Out ()) Flag 0))
      ((= OpCode 902) (build_state Acc (UpPC PC) (cons Mem ())
		    (cons In ()) (append (cons Out ()) (cons Acc ())) Flag 0))
      ((= OpCode 0) (build_state Acc PC Mem In (cons Out ())  Flag 1))
      )))

(defun execution_loop (List)
  (let* (
         (Lst (build_instruction List))
         (OpCode (nth 0 Lst))
         (Immed (nth 1 Lst))
         (State (nth 0 List))
         (Out (nth 5 List)))
  (cond 
   ((equal State "state")
    (execution_loop (one_instruction OpCode Immed List)))
   ((equal State "halted_state") Out))))
  

(defun upPC (oldPC) 
  (cond 
   ((< oldPC 99) (1+ oldPC))
   ((= oldPC 99) 0 )))

(defun add_execute (Acc PC Mem In Out)
  (let* (
	 (Val (nth PC Mem))
	 (XX (+ Acc Val)))
     (if (and (> XX 999)
	     (< XX 0))
	(build_state XX (UpPC PC) (cons Mem ()) (cons In ())
		     (cons Out ()) "noflag" 0)
	(build_state (mod XX 1000) (UpPC PC) (cons Mem ()) (cons In ())
		     (cons Out ()) "flag" 0))))


(defun sub_execute (Acc PC Mem In Out)
  (let* (
	 (Val (nth PC Mem))
	 (XX (- Acc Val)))
     (if (and (> XX 999)
	     (< XX 0))
	(build_state XX (UpPC PC) (cons Mem ()) (cons In ())
		     (cons Out ()) "noflag" 0)
	(build_state XX (UpPC PC) (cons Mem ()) (cons In ())
		     (cons Out ()) "flag" 0))))


(defun branch-if-positive (Immed Acc PC Mem In Out Flag)
    (if
     (equal Flag "noflag")
     (build_state Acc Immed (cons Mem ()) (cons In ())
		  (cons Out ()) Flag 0)
     (build_state Acc (UpPC PC) (cons Mem ()) (cons In ())
		  (cons Out ()) Flag 0)))


(defun branch-if-zero (Immed Acc PC Mem In Out Flag)
    (if (and (= Acc 0)
	     (equal Flag "noflag"))
	(build_state Acc Immed (cons Mem ()) (cons In ())
		     (cons Out ()) Flag 0)
	(build_state Acc (UpPC PC) (cons Mem ()) (cons In ())
		     (cons Out ()) Flag 0)))



