;;;; -*- Mode: Lisp -*-

;; Funzioni di supporto

(defun increment-and-wrap (PC)
  (cond
   ((< PC 99) (1+ PC))
   ((= PC 99) 0)))

(defun set-flag (Val)
  (cond
    ((or (< Val 0) (> Val 999)) :flag)
    (t :noflag)))

(defun set-flag-and-acc (Val) ;; car ~> Flag, cdr ~> Val%1000
  (cond
    ((or (< Val 0) (> Val 999))
     (cons :flag (mod Val 1000)))
    (t
     (cons :noflag Val))))

(defun valid-state (State)
  ;; (state :acc Acc :pc Pc :mem Mem :in In :out Out :flag Flag)
  (cond
    ((eql (first State) 'state) t)
    (t NIL)
    ))

(defun get-acc (State) (third State))

(defun get-pc (State) (fifth State))

(defun get-mem (State) (seventh State))

(defun get-in (State) (ninth State))

(defun get-out (State) (nth 10 State))

(defun get-flag (State) (nth 12 State))

;; init-memory Program => Memory
;; Inizializza la memoria del simulatore LMP
;;     Program - Il programma in assembly LMP
;;     Memory - Una memoria di 100 posizioni adatta al simulatore LMP

(defun init-memory (Program)
  (append Program (make-list (- 100 (list-length Program)) :initial-element 0)))

;; build-check-exc State, Exc => NewState
;; Crea uno stato a partire da uno esistente, gestendo solo la presenza
;; o meno di un'eccezione.

(defun build-check-exc (State Exc)
  (cond
    ((eql Exc :noexc) State)
    ((eql Exc :exc)
     (list 'halted-state
	   :acc (get-acc State)
	   :pc (get-pc State)
	   :mem (get-mem State)
	   :in (get-in State)
	   :out (get-out State)
	   :flag (get-flag State)))
    (t (eabort 99))
    ))

;; build-state Acc, PC, Mem, In, Out, Flag, Exc => NewState
;; Crea uno stato a partire da una serie di parametri, tra cui la presenza o
;; meno di un eccezione.

(defun build-state (Acc PC Mem In Out Flag Exc)
  (cond
    ((eql Exc :noexc)
     (list 'state :acc Acc :pc PC :mem Mem :in In :out Out :flag Flag))
    ((eql Exc :exc)
     (list 'halted-state :acc Acc :pc PC :mem Mem :in In :out Out :flag Flag))
    (t (eabort 99)) ;; Non dovrebbe succedere mai
    ))

;; execution-loop State => Out | NIL
;; Esegue il programma contenuto nello stato in input e restituisce la lista
;; di Output o, nel caso di errori nell'esecuzione del programma, NIL

(defun execution-loop (State)
  (if (eql State NIL)
      (eabort 42)
      (let ((OpcImm (decode (fetch (get-pc State) (get-mem State)))))
	(if (valid-state State)
	     (execution-loop (one-instruction (car OpcImm) (cdr OpcImm) State))
	     (get-out State)))))

;; eabort MsgID => NIL
;; Stampa un messaggio di errore relativo al codice <MsgID> in ingresso e
;; restituisce NIL

(defun eabort (MsgID)
  (cond ((= MsgID 0)
	 (write-line "LMC00: Lettura di <Input> fallita."))
	((= MsgID 1)
	 (write-line "LMC01: Opcode non valido."))
	((= MsgID 42)
	 (write-line "LMC42: Esecuzione annullata."))
	(t
	 (write-line "LMC99: Eccezione non attesa")))
  NIL)

;; one-instruction OpCode Immed State => State | NIL
;; Esegue un'istruzione e restituisce un nuovo stato costruito in base ai
;; parametri modificati, oppure NIL nel caso di errori.

(defun one-instruction (OpCode Immed State)
  (cond
    ((= OpCode 1) (execute-alu :add Immed State))
    ((= OpCode 2) (execute-alu :sub Immed State))
    ((= Opcode 3) (execute-mem :load Immed State))
    ((= Opcode 4) (eabort 1))
    ((= OpCode 5) (execute-mem :store Immed State))
    ((= OpCode 6) (execute-branch :none Immed State))
    ((= OpCode 7) (execute-branch :zero Immed State))
    ((= OpCode 8) (execute-branch :positive Immed State))
    ((= OpCode 9)
     (cond ((= Immed 1) (execute-iop :in State))
	   ((= Immed 2) (execute-iop :out State))
	   (t (eabort 1))))
    (t (build-check-exc State :exc))))

;; fetch PC, Mem => IorD
;; Carica un dato o un'istruzione dalla memoria.

(defun fetch (PC Mem)
  (nth PC Mem))

;; decode IorD => OpcImm
;; Decodifica un'istruzione.

(defun decode (IorD) ;; car ~> Opcode, cdr ~> Immed
  (cons (truncate IorD 100) (mod IorD 100)))

;;;; Execute varie

(defun execute-alu (Parm Immed State)
  (cond ((eql Parm :add)
	 (let ((FlagAcc (negate-and-add NIL
					(get-acc State)
					(fetch Immed (get-mem State)))))
	      (build-state (cdr FlagAcc)
			   (increment-and-wrap (get-pc State))
			   (get-mem State)
			   (get-in State)
			   (get-out State)
			   (car FlagAcc)
			   :noexc
			   )))
	((eql Parm :sub)
	 (let ((FlagAcc (negate-and-add t
					(get-acc State)
					(fetch Immed (get-mem State)))))
	      (build-state (cdr FlagAcc)
			   (increment-and-wrap (get-pc State))
			   (get-mem State)
			   (get-in State)
			   (get-out State)
			   (car FlagAcc)
			   :noexc
			   )))
	(t (eabort 99))))

(defun execute-mem (Parm Immed State)
  (cond ((eql Parm :load)
	 (build-state (fetch Immed (get-mem State))
		      (increment-and-wrap (get-pc State))
		      (get-mem State)
		      (get-in State)
		      (get-out State)
		      (get-flag State)
		      :noexc
		      ))
	((eql Parm :store)
	 (build-state (get-acc State)
		      (increment-and-wrap (get-pc State))
		      (store (get-mem State) Immed (get-acc State) ())
		      (get-in State)
		      (get-out State)
		      (get-flag State)
		      :noexc
		      ))
	(t NIL)))

(defun execute-branch (Parm Immed State)
  (let ((Flag (get-flag State))
	(Acc (get-acc State)))
    (cond ((or (eql Parm :none) ;; Caso 1: Branch effettiva
	       (and (eql Parm :positive) (eql Flag :noflag))
	       (and (eql Parm :zero)
		    (eql Flag :noflag)
		    (= Acc 0)
		    ))
	   (build-state Acc
			Immed
			(get-mem State)
			(get-in State)
			(get-out State)
			Flag
			:noexc
			))
	  ((and (not (eql Parm :positive))   ;; Caso 2: Controllo (non dovrebbe
		(not (eql Parm :zero))       ;; verificarsi mai)
		(not (eql Parm :none))) (eabort 99))
	  (t (build-state Acc ;; Caso 3: Nessuna branch si verifica
			  (increment-and-wrap (get-pc State))
			  (get-mem State)
			  (get-in State)
			  (get-out State)
			  Flag
			  :noexc
			  )))))

(defun execute-iop (Parm State)
  (let ((Input (get-in State)) (Acc (get-acc State)))
    (cond ((eql Parm :in) ;; Aggiungi controllo lista vuota
	   (if (eql Input NIL) (eabort 0)
		(build-state (pop Input)
			     (increment-and-wrap (get-pc State))
			     (get-mem State)
			     Input
			     (get-out State)
			     (get-flag State)
			     :noexc
			     )))
	  ((eql Parm :out)
	   (build-state Acc
			(increment-and-wrap (get-pc State))
			(get-mem State)
			(get-in State)
			(append (get-out State) (list Acc))
			(get-flag State)
			:noexc
			))
	(t (eabort 99)))))

;; negate-and-add Neg, OpA, OpB => FlagAcc
;;     Neg - t o NIL
;; Permette di sommare due valori, o sottrarre il secondo al primo.

(defun negate-and-add (Neg OpA OpB)
  (if (eql Neg t)
      (set-flag-and-acc (+ OpA (- OpB)))
      (set-flag-and-acc (+ OpA OpB))))

;; store Mem, Addr, Value, Temp => NewMem
;;     Temp - sempre () all'inizio
;; Sostituisce l'elemento della lista <Mem> in poszione <Addr> con <Value>.

(defun store (Mem Addr Value Temp)
  (cond
    ((null Mem) ())
    ((= Addr 0) (append Temp (cons Value ()) (cdr Mem)))
    (t
     (store
      (cdr Mem)
      (1- Addr)
      Value
      (append Temp (cons (car Mem) ()))))))

;;; Sample Text Yolo

(defun initial-state (Program Input)
  (build-state 0 0 (init-memory Program) Input '() :noflag :noexc))

(defun load-run (Input)
  (let* (
	 (Program (LMC-Run
		   "/mnt/c/Users/Arthur/Desktop/Progetto LP/Lisp/test.txt")))
  (execution-loop (initial-state Program Input))))


(defun lmao ()
  (let* (
	 (Mem (LMC-Run
		"/mnt/c/Users/Arthur/Desktop/Progetto LP/Lisp/test.txt"))
	 (In (list 901 902 705 600 0 4 5 6 7 8 9 0)))
    (execution-loop
     (list 'state :acc 0 :pc 0 :mem Mem :in In :out '() :flag :noflag))))



;;; Parser (da sistemare)


;;; Step 1: Opening a file and reading lines into a list

(defun reading-from-file (File-Loc)
  (with-open-file
      (Stream File-Loc
	 :direction :input)
    (read-all-lines-helper Stream)))

(defun read-all-lines-helper (Stream)
  (let (
	(line (read-line Stream nil nil)))
    (when line
      (append (cons line ()) (read-all-lines-helper stream)))))



;;; Step 2: Cancello i commenti

(defun inst (File-Loc)
  (let* (
	 (splt (reading-from-file File-Loc)))
    (splitting splt '())))

(defun splitting (lst1 lst2)
  (cond
    ((null lst1) lst2)
    (t
	(let* (
	       (elem (pop lst1))
	       (qart (split-str elem "//")))
	  (splitting lst1 (append lst2 (cons (string-upcase (pop qart)) '()
					     )))))))



;;; Step 3: Creo una lista di liste, ogni sottolista corrisponde ad una riga
;;; del file

(defun second-split (File-Loc)
  (let* (
	 (l (white-spaces File-Loc)))
    (sec-spl-helper l '())))

(defun sec-spl-helper (lst1 lst2)
  (cond
    ((null lst1) lst2)
    (t
     (sec-spl-helper (cdr lst1)
			  (append lst2
				  (cons (split-str (pop lst1) " ") '()))))))



;;; Step 4: Deleting white spaces (solo quelli causati dai commenti)

(defun white-spaces (File-Loc)
 (let* (
	 (string (inst File-Loc)))
   (ws-helper string '() (list-length string))))

(defun ws-helper (lst1 lst2 ind)
  (let* (
	 (substr (car lst1)))
  (cond
    ((= ind 0) lst2)
    (t
     (ws-helper (cdr lst1)
		(append lst2 (cons (string-trim " " substr) '()))
		(1- ind))))))



;;; Step 5: Cerco la lables in posizione 0 e le separo dalla lista delle Op

(defun LMC-Run (File-Loc)
  (let* (
	 (lst (second-split File-Loc)))
    (LMC-Run-helper lst '() (make-list 100 :initial-element 0) 0)))

(defun LMC-Run-helper (lst newlst labels index)
  (let* (
	 (elem (car lst)))
    (cond
      ((= 100 index) (final-mem-state 0 newlst labels '()))
      ((is-label (car elem))
       (LMC-Run-helper (cdr lst)
		       (append newlst (list (cdr elem)))
		       (store labels index (car elem) '())
		       (1+ index)))
      (t
       (LMC-Run-helper (cdr lst)
		       (append newlst (list elem))
		       labels
		       (1+ index))))))



;;; Step 6: Final Memory State

(defun final-mem-state (Ind Oprt Labels Mem)
  (let* (
	 (Op (pop Oprt)))
    (cond
      ((= 100 Ind) Mem)
      (t
       (final-mem-state-hlp Oprt Op Labels (list-length Op) Mem Ind)))))

(defun final-mem-state-hlp (Oprt Op Lb Lenght Mem Ind)
  (cond
    ((= Lenght 1) (final-mem-state
		   (1+ Ind) Oprt Lb
		   (append Mem (list (bld-inst (car Op) "0" lb)))
		   ))
    ((= Lenght 2) (final-mem-state
		   (1+ Ind) Oprt Lb
		   (append Mem (list (bld-inst (car Op) (cadr Op) lb)))
		   ))))


;;; Splitter (funziona in modo strano ma funziona)

(defun split-str (string &optional separator)
  (split-str-1 string separator))

(defun split-str-1 (string &optional separator (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n)
 		     separator (cons (subseq string (1+ n)) r))
	(cons string r))))



;;; Controlla se la stringa in ingresso è un numero o meno

(defun is-numb (string)
    (cond
      ((numberp (parse-integer string :junk-allowed t)) t)))



;;; Controlla se la stringa in ingresso è un'etichetta o meno

(defun is-label (s)
  (cond
    ((or (is-istr s) (is-numb s)) nil)
    (t t)))



;;; Controlla se la stringa in ingresso è un'operazione o meno
;;; ritornando il suo OpCode

(defun is-istr (string)
  (cond
    ((equalp string "ADD") 100)
    ((equalp string "SUB") 200)
    ((equalp string "STA") 300)
    ((equalp string "LDA") 500)
    ((equalp string "BRA") 600)
    ((equalp string "BRZ") 700)
    ((equalp string "BRP") 800)
    ((equalp string "INP") 901)
    ((equalp string "OUT") 902)
    ((equalp string "HLT") 99)
    ((equalp string "DAT") 0)
    ))


(defun bld-inst (OpCode Immed Lb)
  (cond
    ((is-label Immed) (+ (is-istr OpCode) (pos lb Immed 0)))
    ((is-numb Immed) (+ (is-istr OpCode) (parse-integer Immed)))))

(defun pos (List Elem Ind)
  (cond
    ((equalp (pop List) Elem) Ind)
    (t
     (pos List Elem (1+ Ind)))))
