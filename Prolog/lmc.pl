%%%% -*- Mode: Prolog -*-

% 
% Jacopo Maltagliati ~ 830110
% j.maltagliati@campus.unimib.it

%%%% SIMULATORE

%%%% Gestione dello stato del simulatore LMC

% Predicati:
%   state(Acc, Pc, Mem, In, Out, Flag).
%   halted_state(Acc, Pc, Mem, In, Out, Flag).
%

% Crea uno stato iniziale a partire da una lista di istruzioni.
initial_state(Asm, Input, State) :-	
    init_memory(Asm, Mem),
    State = state(0, 0, Mem, Input, [], noflag).

% Restituisce un nuovo 'state' a partire da una serie di parametri.
% Nel caso si sia  verificata un eccezione ('exc' invece di 'noexc'),
% restituisce un 'halting_state'.
build_state(Acc, Pc, Mem, In, Out, Flag, noexc, NewState) :- 
    NewState = state(Acc, Pc, Mem, In, Out, Flag).

build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState) :- 
    NewState = halted_state(Acc, Pc, Mem, In, Out, Flag).

% Computa il prossimo stato invocando il predicato 'one_instruction'.
% Si ferma e restituisce la coda di output se lo stato ricevuto è un
% 'halting state'
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), NewOut) :- 
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
    execution_loop(NewState, NewOut),
    !.

execution_loop(halted_state(_Acc, _Pc, _Mem, _In, Out, _Flag), Out).

% Esegue una istruzione tramite un ciclo fetch-decode-execute.
% Nel caso in cui l'opcode non sia valido costruisce un 'halted_state'.
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState) :- 
   fetch(Mem, Pc, Inst),
   decode(Inst, Opc, Immed),
   valid_opcode(Opc),
   execute(Opc, Immed, state(Acc, Pc, Mem, In, Out, Flag), NewState),
   !.

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState) :- 
   fetch(Mem, Pc, Inst),
   decode(Inst, Opc, _Immed),
   invalid_opcode(Opc),
   build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState).


%%%% Simulazione del Datapath

% Fase 1: Fetch
% Carica un dato dalla memoria usando un indice.
fetch(Mem, Addr, IorD) :- 
    %% TODO: Aggiungere controlli?
    nth0(Addr, Mem, IorD).

% Fase 2: Decode
% Riceve un istruzione e ne restituisce l'opcode e il valore immediato.
decode(Content, Opc, Immed) :- 
	divmod(Content, 100, Opc, Immed). 

% Fase 3: Execute + Write Back
%
% Predicati:
%    execute(Opc, Param, OldState, NewState).
%
% Esegue una singola istruzione in base all'opcode e al suo valore
% immediato. Invoca la 'build_state' per costruire e restituire un
% nuovo stato.

% Opcode: 1
% Istruzione: ADD
execute(1, Immed, state(Acc, Pc, Mem, In, Out, _Flag), NewState) :- 
    % print("DEBUG_ADD"),
    fetch(Mem, Immed, ToAdd),
    adder_add(Acc, ToAdd, NewFlag, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, In, Out, NewFlag, noexc, NewState), !.


% Opcode: 2
% Istruzione: SUB
execute(2, Immed, state(Acc, Pc, Mem, In, Out, _Flag), NewState) :- 
    % print("DEBUG_SUB"),
    fetch(Mem, Immed, ToNeg),
    adder_neg(ToNeg, ToAdd),
    adder_add(Acc, ToAdd, NewFlag, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, In, Out, NewFlag, noexc, NewState), !.


% Opcode: 3
% Istruzione: STA
execute(3, Immed, state(Acc, Pc, Mem, In, Out, Flag), NewState) :- 
    % print("DEBUG_STA"),
    lsubst(Immed, Acc,  Mem, [], NewMem),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, NewMem, In, Out, Flag, noexc, NewState), !.

% Opcode: 5
% Istruzione: LDA
execute(5, Immed, state(_Acc, Pc, Mem, In, Out, Flag), NewState) :- 
    % print("DEBUG_LDA"),
    fetch(Mem, Immed, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, In, Out, Flag, noexc, NewState), !.

% Opcode: 6
% Istruzione: BRA
execute(6, Immed, state(Acc, _Pc, Mem, In, Out, Flag), NewState) :- 
    % print("DEBUG_BRA"),
    build_state(Acc, Immed, Mem, In, Out, Flag, noexc, NewState), !.

% Opcode: 7
% Istruzione: BRZ
execute(7, Immed, state(0, _Pc, Mem, In, Out, noflag), NewState) :- 
    % print("DEBUG_BRZ"),
    build_state(0, Immed, Mem, In, Out, noflag, noexc, NewState), !.

execute(7, _Immed, state(Acc, Pc, Mem, In, Out, noflag), NewState) :- 
    % print("DEBUG_BRZ"),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, Out, noflag, noexc, NewState), !.

execute(7, _Immed, state(Acc, Pc, Mem, In, Out, flag), NewState) :- 
    % print("DEBUG_BRZ"),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, Out, flag, noexc, NewState), !.

% Opcode: 8
% Istruzione: BRP
execute(8, Immed, state(Acc, _Pc, Mem, In, Out, noflag), NewState) :- 
    % print("DEBUG_BRP"),
    build_state(Acc, Immed, Mem, In, Out, noflag, noexc, NewState), !.

execute(8, _Immed, state(Acc, Pc, Mem, In, Out, flag), NewState) :- 
    % print("DEBUG_BRP"),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, Out, flag, noexc, NewState), !.

% Opcode: 9
% Valore Immediato: 1
% Istruzione: INP
execute(9, 1, state(_Acc, Pc, Mem, In, Out, Flag), NewState) :-	
    % print("DEBUG_INP"),
    lpop(In, NewAcc, NewIn),
    between(0, 999, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, NewIn, Out, Flag, noexc, NewState), !.

execute(9, 1, state(Acc, Pc, Mem, In, Out, Flag), NewState) :-	
    % print("DEBUG_INP"),
    build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState), !.

% Opcode: 9
% Valore Immediato: 2
% Istruzione: OUT
execute(9, 2, state(Acc, Pc, Mem, In, Out, Flag), NewState) :- 
    % print("DEBUG_OUT"),
    lrpush(Out, Acc, NewOut),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, NewOut, Flag, noexc, NewState), !.

% Opcode: [0,4] ; 9
% Valore Immediato: [0..99] ; [0,3..99]
% Istruzione: <non valida>
execute(_OpCode, _Immed, state(Acc, Pc, Mem, In, Out, Flag), NewState) :- 
    % print("DEBUG_HLT"),
    build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState), !.


% Adder con Carry Flag
%
% Somma aritmeticamente due interi: se il risultato non è compreso
% tra 0 e 999 ne restituisce il modulo 1000 e 'flag', in caso
% contrario vengono restituiti il risultato e 'noflag'.
adder_add(OpA, OpB, Flag, Res) :- 
    Tmp is OpA + OpB,
    set_flag(Tmp, Flag),
    divmod(Tmp, 1000, _, Res).

% Prende in input un valore numerico e ne restituisce la negazione
% aritmetica.
adder_neg(OpA, Res) :- 
    Res is -OpA.

%%%% Predicati per funzioni di supporto

% Incrementa un registro, se viene raggiunto il valore 100,
% si riparte da 0.
increment_and_wrap(99, 0).

increment_and_wrap(Reg, NewReg) :- 
    between(0, 99, Reg),
    NewReg is Reg + 1.

% Controlla che il valore di un registro sia compreso tra 0 e 999:
% in questo caso restituisce 'noflag', altrimenti 'flag'
set_flag(Reg, noflag) :- 
    between(0, 999, Reg), !.

set_flag(_Reg, flag).

% Controlla che un opcode sia valido, ovvero compreso tra 0 e 9,
% escluso 4. Se non lo è, fallisce.
valid_opcode(Opc) :- 
    between(1,3,Opc), !.

valid_opcode(Opc) :- 
    between(5,9,Opc), !.

% Controlla che un opcode sia invalido, ovvero minore di 0, uguale
% a 4 o maggiore di 9. Se non lo è, fallisce.
invalid_opcode(0).

invalid_opcode(4).

invalid_opcode(Opc) :- 
    Opc @>= 10, !.

invalid_opcode(Opc) :- 
    Opc @=< -1, !.

% Dato un programma, ovvero una lista di istruzioni, genera una memoria
% valida per il simulatore. Se ci sono meno di 100 istruzioni, viene 
% generata una serie di valori di padding compresi tra 1 e 99, che vanno a
% riempire le celle di memoria mancanti. Se abbiamo piu' di 100 istruzioni,
% il predicato fallisce.
init_memory(Pgm, Pgm) :- 
    length(Pgm, PgmLength),
    PgmLength == 100, !.

init_memory(Pgm, Mem) :-
    length(Pgm, PgmLength),
    PadLength is 100 - PgmLength,
    PadLength @> 0,
    randseq(PadLength, 99, Pad),
    append(Pgm, Pad, Mem).


% Data una lista, ne restituisce il primo elemento e il resto della lista.
lpop([Top | Rest], Top, Rest).

% Data una lista, ne restituisce il primo elemento lasciandola invariata.
ltop([Top | _], Top).

% Aggiunge un elemento in testa ad una lista e ne restituisce la nuova
% versione.
llpush(List, Elem, ResList) :- 
    append([Elem], List, ResList).

% Aggiunge un elemento in coda ad una lista e ne restituisce la nuova
% versione.
lrpush(List, Elem, ResList) :- 
    append(List, [Elem], ResList).

% Sostituisce l'elemento della lista all'indice 'Index' con uno dato,
% e restituisce la lista modificata.
% lsubst(Index, Insert, Src, Temp, Dest).
lsubst(0, Insert, [ _SrcH | SrcT ], Temp, Dest) :- 
    append([Temp, [Insert], SrcT], Dest),
    !.

lsubst(Index, Insert, [ SrcH | SrcT ], Temp, Dest) :- 
    append(Temp, [SrcH], NewTemp),
    NewIndex is Index - 1,
    lsubst(NewIndex, Insert, SrcT, NewTemp, Dest).

%%%% ASSEMBLER/LOADER

% Crea una memoria compatibile con il formato LMC a partire da un file di
% testo.
lmc_load(Filename, Mem) :- 
    % Passo 1
    open(Filename, read, Stream),
    step1_loop(Stream, 0, [], TokenLists),
    close(Stream),
    % Passo 1 1/2
    build_label_ptr_map(TokenLists, 0, [], NoLabelTokenLists),
    % Passo 2
    step2_loop(NoLabelTokenLists, [], Mem).


% Passo 1: Stream -> Tokens <Label*, Mnemonic, IorL>
step1_loop(Stream, -1, Lines, Result) :- 
    read_string(Stream, "\n", "\r", _NewLn, Line),
    strip_and_push(Line, Lines, Result), !.

step1_loop(Stream, _Ln, Lines, Result) :- 
    read_string(Stream, "\n", "\r", NewLn, Line),
    strip_and_push(Line, Lines, TmpRes),
    step1_loop(Stream, NewLn, TmpRes, Result), !.


% Stabilisce la posizione di un commento, e ritorna l'indice dell'ultimo
% carattere utile della stringa
comment_pos(Line, Pos) :- 
    sub_string(Line, Pos, 2, _CPos, "//"), !.

comment_pos(Line, Pos) :- 
    sub_string(Line, _Zero, _Length, Pos, _Line).


% Trova e rimuove tutti i caratteri dopo la prima occorrenza di "//"
% nella riga corrente, per poi restituire una lista contenente i token
% di ogni riga.
strip_and_push(Line, List, Result) :- 
    comment_pos(Line, Pos),
    sub_string(Line, _, Pos, _, NoComment),
    string_upper(NoComment, UpComment),
    split_string(UpComment, "\s\t\n\r", "\s\t\n\r", Tokens),
    cleanup(Tokens, List, Result).


% Aggiunge i token alla lista, a patto che non siano vuoti.
cleanup([""], List, List).

cleanup(Tokens, List, Result) :- 
    append(List, [Tokens], Result).


% Costruisce in memoria una mappa delle label, togliendole dalla lista
% di token in ingresso.
build_label_ptr_map([], _Index, ResTokenLists, ResTokenLists) :- !.

build_label_ptr_map(TokenLists, Index, TmpTokenLists, ResTokenLists) :-	
    lpop(TokenLists, CurTokenList, Temp),
    lpop(CurTokenList, FirstToken, ModTokenList),
    what_is(FirstToken, DataType),
    is_label(DataType),
    lrpush(TmpTokenLists, ModTokenList, NewTokenLists),
    assertz((access_label(FirstToken, Index) :- !)),
    NewIndex is Index + 1,
    build_label_ptr_map(Temp, NewIndex, NewTokenLists, ResTokenLists), !.

build_label_ptr_map(TokenLists, Index, TmpTokenLists, ResTokenLists) :-	
    lpop(TokenLists, CurTokenList, Temp),
    NewIndex is Index + 1,
    lrpush(TmpTokenLists, CurTokenList, NewTokenLists),
    build_label_ptr_map(Temp, NewIndex, NewTokenLists, ResTokenLists).

% Passo 2: Tokens <Istruzione, IorL> -> Memoria LMC
step2_loop([], ResMem, ResMem) :- !.

step2_loop(TokenLists, TmpMem, ResMem) :- 
    lpop(TokenLists, CurTokenList, NewTokenLists),
    length(CurTokenList, CTLen),
    unpack_and_asm(CurTokenList, CTLen, Asm),
    lrpush(TmpMem, Asm, NewTmpMem),
    step2_loop(NewTokenLists, NewTmpMem, ResMem), !.


% Spacchetta una lista di token e usa la kb per riconoscere le istruzioni
% e le eventuali etichette, per poi restituire l'istruzione assemblata.
unpack_and_asm(TokenList, 2, Assembly) :- 
    lpop(TokenList, TokenA, TmpTLA),
    ltop(TmpTLA, TokenB),
    asm_binstruction(TokenA, TokenB, Assembly).

unpack_and_asm(TokenList, 1, Assembly) :- 
    ltop(TokenList, TokenA),
    asm_uinstruction(TokenA, Assembly).

% Assembla un'istruzione in base ai token che riceve in input, cercando
% di risolverli usando la kb.
asm_binstruction(TokenA, TokenB, Assembly) :- 
    what_is(TokenB, data, ValueB),
    what_is(TokenA, binstruction, ValueA),
    Assembly is ValueA + ValueB,
    between(0, 999, Assembly), !.

asm_binstruction(TokenA, TokenB, Assembly) :- 
    what_is(TokenB, label, ValueB),
    what_is(TokenA, binstruction, ValueA),
    Assembly is ValueA + ValueB,
    between(0, 999, Assembly).

asm_uinstruction(TokenA, Assembly) :- 
    what_is(TokenA, uinstruction, Assembly).

% Stabilisce il tipo di un token, a scelta tra:
%  - Istruzione Binaria (con Valore Immediato)
%  - Istruzione Unaria (senza Valore Immediato)
%  - Dato numerico (Valore Immediato)
%  - Etichetta
% Restituisce al chiamante i parametri del token.
% Canta anche una canzone...
what_is(Love, binstruction, Value) :- % Baby don't hurt me...
    mne_query(Love, Value, immed).

what_is(Love, uinstruction, Value) :- % Don't hurt me...
    mne_query(Love, Value, noimmed).

what_is(Love, data, Value) :- % No more!
    number_string(Value, Love).

what_is(Love, label, Value) :- % Uh-uh-oh-oh-oh-OH
    access_label(Love, Value).


% Stabilisce se il tipo di un token è quello richiesto dal chiamante.
% Utilizzato solo dal risolutore delle label.
% Canta anche una canzone...
% And so I wake in the morning and I get outside
what_is(GoingOn, binstruction) :- 
    mne_query(GoingOn, _Value, immed), !.

what_is(GoingOn, uinstruction) :- 
    mne_query(GoingOn, _Value, noimmed), !.

% And I say, HEY YEAH YEAH, HEY YEAH YEAH
what_is(GoingOn, data) :- 
    number_string(_Value, GoingOn), !.

% I said HEY!
what_is(_GoingOn, label).

is_label(label).

% Knowledge Base: Istruzioni Binarie 
mne_query("ADD", 100, immed).
mne_query("SUB", 200, immed).
mne_query("STA", 300, immed).
mne_query("LDA", 500, immed).
mne_query("BRA", 600, immed).
mne_query("BRZ", 700, immed).
mne_query("BRP", 800, immed).
mne_query("DAT", 000, immed).

% Knowledge Base: Istruzioni Unarie
mne_query("INP", 901, noimmed).
mne_query("OUT", 902, noimmed).
mne_query("HLT", 099, noimmed).
mne_query("DAT", 000, noimmed).

% Knowledge Base: Label
% Questo predicato viene asserito a runtime dal risolutore delle Label.
dynamic access_label/2.

%%%% Scratch

lmc_run(Filename, Input, Output) :-
    lmc_load(Filename, Mem),
    initial_state(Mem, Input, State),
    execution_loop(State, Output).

lmc_run_debug_load(Filename, Input, Output) :-
    leash(-all),
    trace,
    lmc_load(Filename, Mem),
    nodebug,
    initial_state(Mem, Input, State),
    execution_loop(State, Output).

lmc_run_debug_xloop(Filename, Input, Output) :-
    lmc_load(Filename, Mem),
    initial_state(Mem, Input, State),
    leash(-all),
    trace,
    execution_loop(State, Output),
    nodebug.
    

% Controlla TODOs

%%%% eof: lmc.pl

% lmc_run("./test_anto.lmc", [901, 902, 705, 600, 0, 4, 5, 6, 7, 8, 9, 0], Out).
