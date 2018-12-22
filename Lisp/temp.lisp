;;;; -*- Mode: Lisp -*-

;;;; Fetch-Decode-Execute

(defun fetch (PC Mem)
  (nth PC Mem))

(defun decode (IorD) ;; car ~> Opcode, cdr ~> Immed
  (cons (mod IorD 100) (rem IorD 100)))

;;;;

(defun set-flag (Val)
  (cond
    ((< Val 0) :flag)
    ((> Val 999) :flag)
    (t :noflag)))

(defun set-flag-and-acc (Val) ;; car ~> Flag, cdr ~> Val%1000
  (cond
    ((< Val 0)
     (cons :flag (mod Val 1000)))
    ((> Val 999)
     (cons :flag (mod Val 1000)))
    (t
     (cons :noflag Val))))

;;;; Store

(defun store (List Index Value NewList)
  (cond
    ((null List) ())
    ((= Index 0) (append NewList (cons Value ()) (cdr List)))
    (t
     (store
      (cdr List)
      (1- Index)
      Value
      (append NewList (cons (car List) ()))))))

;;;;

(defun build-state (Acc PC Mem In Out Flag Exc)
  (cond
    ((= Exc 0)
     (append
      (cons :state ())
      (cons Acc ())
      (cons PC ())
      Mem
      In
      Out
      (cons Flag ())))
    ((= Exc 1)
     (append
      (cons :halted_state ())
      (cons Acc ())
      (cons PC ())
      Mem
      In
      Out
      (cons Flag ()))
     )))

;;;; One Instr

(defun one-instruction (OpCode Immed List)
  (let* (
    (Acc (nth 1 List))
    (PC (nth 2 List))
    (Mem (nth 3 List))
    (In (nth 4 List))
    (Out (nth 5 List))
    (Flag (nth 6 List)))
     (cond
       ((= OpCode 1)
       (build-state (add Acc Immed Mem) (UpPC PC) (cons Mem ()) In Out (set-flag Acc) 0))
      ((= OpCode 2)
       (build-state (sub Acc Immed Mem) (UpPC PC) (cons Mem ()) In Out (set-flag Acc) 0))
      ((= Opcode 3)
       (build-state (fetch Mem Immed) (UpPC PC) (cons Mem ()) In Out Flag 0))
      ((= OpCode 5)
       (build-state Acc (UpPC PC) (cons (store Mem Immed Acc ())()) In Out Flag 0))
      ((= OpCode 6)
       (build-state Acc Immed (cons Mem ()) In Out Flag 0))
      ((= OpCode 7) ())
      ((= OpCode 8) ())
      ((= OpCode 901)
       (build-state (Pop In) (UpPC PC) (cons Mem ()) In Out Flag 0))
      )))

(defun execution-loop (List)
  (let ((OpcImm (decode (fetch (nth 1 List) (nth 2 List))))
	(State (first List))
	(Out (sixth List)))
    (cond
      ((equal State :state)
       (execution-loop (one-instruction (car OpcImm) (cdr OpcImm) List)))
      ((equal State :halted-state)
       Out))))

(defun upPC (oldPC)
  (cond
   ((< oldPC 99) (1+ oldPC))
   ((= oldPC 99) 0 )))

(defun add (Acc PC Mem)
  (+ Acc (nth PC Mem)))

(defun sub (Acc PC Mem)
  (- Acc (nth PC Mem)))


; (nth 0 '(foo bar baz)) =>  FOO
; funzioni built-in :
; defparameter, defun, cons, car, cdr, list, cond, nth, rest, first,
; second, ..., tenth, last, atom,

