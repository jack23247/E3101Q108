;;;; -*- Mode: Lisp -*-

;;; lmc.lisp
;;; Implementazione in Lisp del Little Man's Computer
;;; Jacopo Maltagliati ~ 830110
;;; j.maltagliati@campus.unimib.it

;;;; Funzioni di supporto

;; eabort-dbg MsgID -> NIL
;; Stampa un messaggio di errore relativo al codice di errore in ingresso e
;; restituisce NIL.

(defun eabort-dbg (MsgID)
  (cond ((= MsgID 0)
	 (write-line "LMC00: Lettura di <Input> fallita."))
	((= MsgID 1)
	 (write-line "LMC01: Opcode non valido."))
	((= MsgID 2)
	 (write-line "LMC02: Etichetta non attesa dopo un'istruzione DAT."))
	((= MsgID 3)
	 (write-line
	  "LMC03: Errore nel risolutore delle Etichette: possibile duplicato."))
	((= MsgID 4)
	 (write-line "LMC04: Referenziazione etichetta mai dichiarata."))
	((= MsgID 5)
	 (write-line "LMC05: Operazione unaria con valore immediato."))
	((= MsgID 6)
	 (write-line "LMC06: Riga malformata."))
	(t
	 (write-line "LMC99: Eccezione non attesa")))
  NIL)


;; eabort MsgID -> NIL
;; Versione release di eabort-dbg: restituisce NIL senza stampare nulla.

(defun eabort (MsgID)
  NIL)

;; increment-and-wrap PC -> NewPC
;; Incrementa il Program Counter e ne restituisce il nuovo valore.
;; Se il PC raggiunge 100, restituisce 0.

(defun increment-and-wrap (PC)
  (cond
   ((< PC 99) (1+ PC))
   ((= PC 99) 0)))


;; set-flag Val -> Flag
;; Restituisce ':noflag' se Val e' compreso tra 0 e 999,
;; altrimenti restituisce ':flag'.

(defun set-flag (Val)
  (cond
    ((or (< Val 0) (> Val 999)) :flag)
    (t :noflag)))


;; set-flag-and-acc Val -> FlagAccCons
;; Restituisce, utilizzando le stesse regole di 'set-flag', sia il
;; valore di Flag che il modulo 1000 del valore in ingresso, in una cons-cell.

(defun set-flag-and-acc (Val)		; car ~> Flag, cdr ~> Val%1000
  (cond
    ((or (< Val 0) (> Val 999))
     (cons :flag (mod Val 1000)))
    (t
     (cons :noflag Val))))

;; statep State -> t | NIL
;; Stabilisce se uno 'state' e' uno stato valido oppure no (non e' uno stato
;; oppure e' un 'halted-state'.

(defun statep (State)
  ;; (state :acc Acc :pc Pc :mem Mem :in In :out Out :flag Flag)
  (cond
    ((equal (first State) 'state) t)
    (t NIL)
    ))


;; get-* State -> *
;; Restituisce, a partire da uno stato, il valore richiesto.
;; ATTENZIONE: Non effettua controlli sulla validita' dello stato!

(defun get-acc (State) (third State))

(defun get-pc (State) (fifth State))

(defun get-mem (State) (seventh State))

(defun get-in (State) (ninth State))

(defun get-out (State) (nth 10 State))

(defun get-flag (State) (nth 12 State))


;; init-memory Program -> Memory
;; Inizializza la memoria del simulatore LMP, dati:
;; Program - Il programma in assembly LMP,
;; Memory - Una memoria di 100 posizioni adatta al simulatore LMP.

(defun init-memory (Program)
  (append Program (make-list (- 100 (list-length Program)) :initial-element 0)))


;; initial-state Program, Input -> InitialState
;; Costruisce lo stato iniziale del simulatore LMP, dati il programma da
;; eseguire e la lista di input.

(defun initial-state (Program Input)
  (build-state 0 0 (init-memory Program) Input '() :noflag :noexc))


;; build-check-exc State, Exc -> NewState
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


;; build-state Acc, PC, Mem, In, Out, Flag, Exc -> NewState
;; Crea uno stato a partire da una serie di parametri, tra cui la presenza o
;; meno di un eccezione.

(defun build-state (Acc PC Mem In Out Flag Exc)
  (cond
    ((equal Exc :noexc)
     (list 'state :acc Acc :pc PC :mem Mem :in In :out Out :flag Flag))
    ((equal Exc :exc)
     (list 'halted-state :acc Acc :pc PC :mem Mem :in In :out Out :flag Flag))
    (t (eabort 99))))			; Non dovrebbe succedere mai


;;;; Funzioni Simulatore LMC

;; execution-loop State -> Out | NIL
;; Esegue il programma contenuto nello stato in input e restituisce la lista
;; di Output o, nel caso di errori nell'esecuzione del programma, NIL.

(defun execution-loop (State)
  (if (equal State NIL)
      (eabort 42)
      (let ((OpcImm (decode (fetch (get-pc State) (get-mem State)))))
	(if (statep State)
	     (execution-loop (one-instruction (car OpcImm) (cdr OpcImm) State))
	     (get-out State)))))


;; one-instruction OpCode, Immed, State -> State | NIL
;; Esegue un'istruzione e restituisce un nuovo stato costruito in base ai
;; parametri modificati, oppure NIL nel caso di errori.

(defun one-instruction (OpCode Immed State)
  (cond
    ((= OpCode 1) (execute-alu :add Immed State))
    ((= OpCode 2) (execute-alu :sub Immed State))
    ((= Opcode 3) (execute-mem :store Immed State))
    ((= Opcode 4) (eabort 1))
    ((= OpCode 5) (execute-mem :load Immed State))
    ((= OpCode 6) (execute-branch :none Immed State))
    ((= OpCode 7) (execute-branch :zero Immed State))
    ((= OpCode 8) (execute-branch :positive Immed State))
    ((= OpCode 9)
     (cond ((= Immed 1) (execute-iop :in State))
	   ((= Immed 2) (execute-iop :out State))
	   (t (eabort 1))))
    (t (build-check-exc State :exc))))


;; fetch PC, Mem -> IorD
;; Carica un dato o un'istruzione dalla memoria.

(defun fetch (PC Mem)
  (nth PC Mem))


;; decode IorD -> OpcImm
;; Decodifica un'istruzione.

(defun decode (IorD)			; car ~> Opcode, cdr ~> Immed
  (cons (truncate IorD 100) (mod IorD 100)))

;; execute-* Parm, Immed, State -> NewState
;; Esegue un'istruzione o un gruppo di istruzioni e restituisce lo stato
;; modificato.

(defun execute-alu (Parm Immed State)
  ;; Gruppo di istruzioni ALU (ADD, SUB)
  (cond ((equal Parm :add)
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
	((equal Parm :sub)
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
  ;; Gruppo di istruzioni ACC-MEM e MEM-ACC (LOAD, STORE)
  (cond ((equal Parm :load)
	 (build-state (fetch Immed (get-mem State))
		      (increment-and-wrap (get-pc State))
		      (get-mem State)
		      (get-in State)
		      (get-out State)
		      (get-flag State)
		      :noexc
		      ))
	((equal Parm :store)
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
  ;; Gruppo di istruzioni di salto (BRA, BRZ, BRP)
  (let ((Flag (get-flag State))
	(Acc (get-acc State)))
    (cond ((or (equal Parm :none)	; Caso 1: Branch effettiva
	       (and (equal Parm :positive) (equal Flag :noflag))
	       (and (equal Parm :zero)
		    (equal Flag :noflag)
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
	  ((and (not (equal Parm :positive)) ; Caso 2: Controllo (non dovrebbe
		(not (equal Parm :zero))     ; verificarsi mai)
		(not (equal Parm :none))) (eabort 99))
	  (t (build-state Acc	  ; Caso 3: Nessuna branch si verifica
			  (increment-and-wrap (get-pc State))
			  (get-mem State)
			  (get-in State)
			  (get-out State)
			  Flag
			  :noexc
			  )))))

(defun execute-iop (Parm State)
  ;; Gruppo di istruzioni di I/O (INP, OUT)
  (let ((Input (get-in State)) (Acc (get-acc State)))
    (cond ((equal Parm :in)	      ; TODO: Aggiungi controllo lista vuota
	   (if (equal Input NIL) (eabort 0)
		(build-state (pop Input)
			     (increment-and-wrap (get-pc State))
			     (get-mem State)
			     Input
			     (get-out State)
			     (get-flag State)
			     :noexc
			     )))
	  ((equal Parm :out)
	   (build-state Acc
			(increment-and-wrap (get-pc State))
			(get-mem State)
			(get-in State)
			(append (get-out State) (list Acc))
			(get-flag State)
			:noexc
			))
	(t (eabort 99)))))

;;;; Helper per execute-*

;; negate-and-add Neg, OpA, OpB -> FlagAcc
;;     Neg - t o NIL
;; Permette di sommare due valori, o sottrarre il secondo al primo.
;; Restituisce una cons-cell tramite 'set-flag-and-acc'

(defun negate-and-add (Neg OpA OpB)
  (if (equal Neg t)
      (set-flag-and-acc (+ OpA (- OpB)))
      (set-flag-and-acc (+ OpA OpB))))


;; store Mem, Addr, Value, Temp -> NewMem
;;     Temp - sempre () all'inizio
;; Sostituisce l'elemento della lista <Mem> in poszione <Addr> con <Value>.
;; Restituisce la lista modificata.

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


;;;; Funzioni principali del simulatore

;; lmc-run Path, Input -> Output | NIL
;; Carica un programma e si occupa di eseguirlo, elaborando la coda di input
;; e restituendo quella di output.

(defun lmc-run (Path Input)
  (execution-loop (initial-state (lmc-load Path) Input)))


;; lmc-load Path -> Program
;; Carica un file assembly, ne controlla la correttezza e si occupa di
;; assemblarlo, restituendo il programma sotto forma di lista.

(defun lmc-load (Path)
  (let* ((L3Cons (lbl-search-wrapper (split-line-wrapper
				     (fcomments-wrapper
				      (read-file Path))))))
    (assembler-wrapper (pop L3Cons) L3Cons)))


;; read-file Path -> LineList
;; Legge il file assembly e restituisce una lista di righe.

(defun read-file (Path)
  (with-open-file (Stream Path :direction :input)
    (rec-read-line Stream '())))

(defun rec-read-line (Stream LineList)
  (let ((Line (read-line Stream nil nil)))
    (if Line
        (rec-read-line Stream
		       (append LineList
			       (list (substitute #\Space #\Tab Line))))
	LineList)))


;; fcomments-wrapper LineList -> NiceLineList
;; Rimuove i commenti dalla lista di righe e restituisce la lista pulita.

(defun fcomments-wrapper (LineList)
  (rec-fcomments LineList '()))

(defun rec-fcomments (Lines NoComments)
  (cond
    ((null Lines) NoComments)
    (t
     (let* ((CurLine (pop Lines)))
       (rec-fcomments Lines
	 (append NoComments
		 (list (string-upcase (car (split-string CurLine "//"))))))))))


;; split-line-wrapper Lines -> TokenLists
;; Divide le linee pulite in liste di token.

(defun split-line-wrapper (Lines)
    (rec-split-lines Lines '()))

(defun rec-split-lines (Lines TokenLists)
  (if Lines
      (rec-split-lines (cdr Lines)
		       (append TokenLists
			       (list (split-all-wrapper (car Lines) " "))))
      TokenLists))


;; lbl-search-wrapper CleanLines -> L3Cons
;; Cerca e separa le Label dalla lista di tokens e restituisce una cons-cell
;; il cui car e' la lista dei token rimanenti e il cui cdr e' la lista delle
;; etichette, con "0INV" al posto di un'etichetta mancante.

(defun lbl-search-wrapper (CleanLines)
   (rec-lbl-search CleanLines '() '()))

(defun rec-lbl-search (Lines TmpLineList TmpLabelList)
  ;; car -> LineList
  ;; cdr -> LabelList
  (if (null Lines) (cons TmpLineList TmpLabelList)
      (let ((CurLine (pop Lines)))
	(let ((FirstToken (car CurLine)))
	  (cond
	    ((null FirstToken) (rec-lbl-search Lines TmpLineList TmpLabelList))
	    ((member FirstToken TmpLabelList :test #'string=) (eabort 3))
	    ((labelp FirstToken)
	     (rec-lbl-search Lines
			     (append TmpLineList (list (cdr CurLine)))
			     (append TmpLabelList (list (car CurLine)))))
	    (t (rec-lbl-search Lines
			       (append TmpLineList (list CurLine))
			       (append TmpLabelList (list "0INV")))))))))


;;;; Assemblatore LMC

;; assembler-wrapper Lines LabelList -> Program | NIL
;; Data una lista di liste di token e la lista di etichette, assembla e
;; restituisce il programma corrispondente.

(defun assembler-wrapper (Lines LabelList)
  (rec-assemble Lines LabelList '()))

(defun rec-assemble (Lines LabelList Program)
  (if Lines
      (let ((Line (pop Lines)))
	(if (null Line)
	    (rec-assemble Lines LabelList Program)
	    (let ((Asm (asm Line LabelList)))
	      (if Asm
		  (rec-assemble Lines LabelList (append Program (list Asm)))
		  (eabort 6)))))
      Program))

(defun asm (CurTokenList LabelList)
  (let ((Length (length CurTokenList)))
    (cond
      ((= Length 1) (instrp (first CurTokenList)))
      ((= Length 2) (build-binstruction (first CurTokenList)
					(second CurTokenList)
					LabelList))
      (t NIL))))

(defun build-binstruction (Mnemo IorL LabelList)
  (cond
    ((labelp IorL)
     (if (datp Mnemo) (eabort 2)
	 (lets-get-this-bread Mnemo IorL LabelList)))
    ((immedp IorL) (+ (instrp Mnemo) (parse-integer IorL)))
    (t (eabort 6))))

(defun lets-get-this-bread (Mnemo IorL LabelList)
  (let ((OpCode (instrp Mnemo))
	(Label (label-lookup LabelList IorL 0)))
	(if OpCode
	     (if Label
		 (+ OpCode Label) (eabort 4)) (eabort 6))))

(defun label-lookup (LabelList Label Pointer)
  (if LabelList
      (if (equalp (pop LabelList) Label) Pointer
	  (label-lookup LabelList Label (1+ Pointer)))
      NIL))


;; split-all-wrapper String, Separator -> TokenList
;; Restituise una lista di token, che corrispondono alle varie parti di una
;; stringa, divisa nel punto in cui si trova il separatore.

(defun split-all-wrapper (String Separator)
  (rec-split-all String Separator '()))

(defun rec-split-all (String Separator TmpTokenList)
  (let ((CurTokenRest (split-string
		       (string-trim '(#\Space #\Tab #\Return #\Linefeed) String)
		       Separator)))
    (let ((CurToken (cleanup (car CurTokenRest)))
	  (Rest (cdr CurTokenRest)))
      (if Rest
	  (rec-split-all Rest
			 Separator
			 (if CurToken (append TmpTokenList (list CurToken))
			     TmpTokenList))
	  (append TmpTokenList (list CurToken))))))

(defun cleanup (Token)
  (if (equal Token "") NIL Token))

(defun split-string (String Separator)
  ;; car -> Substring
  ;; cdr -> Rest
  (let
      ((SepIndex (search Separator String)))
    (if SepIndex
	(cons (subseq String 0 SepIndex)
	      (subseq String (+ SepIndex (length Separator)) NIL))
	(list String))))


;; immedp Token -> Immed | NIL
;; Se Token e' un valore numerico, ne restituisce il valore, altrimenti NIL

(defun immedp (Token)
    (cond
      ((numberp (parse-integer Token :junk-allowed T)) T)))


;; labelp Token -> OpCode | NIL
;; Se Token e' un label, restituisce T, altrimenti NIL

(defun labelp (Token)
  (if (or (instrp Token) (immedp Token)) NIL T))


;; instrp Token -> OpCode | NIL
;; Se Token e' un'istruzione, restituisce il suo opcode, altrimenti NIL

(defun instrp (Token)
  (cond
    ((equalp Token "ADD") 100)
    ((equalp Token "SUB") 200)
    ((equalp Token "STA") 300)
    ((equalp Token "LDA") 500)
    ((equalp Token "BRA") 600)
    ((equalp Token "BRZ") 700)
    ((equalp Token "BRP") 800)
    ((equalp Token "INP") 901)
    ((equalp Token "OUT") 902)
    ((equalp Token "HLT") 99)
    ((equalp Token "DAT") 0)
    (t NIL)))


;; datp Token -> OpCode | NIL
;; Se Token e' l'istruzione DAT, restituisce il suo opcode, altrimenti NIL

(defun datp (Token)
  (if (equalp Token "DAT") 0 NIL))
