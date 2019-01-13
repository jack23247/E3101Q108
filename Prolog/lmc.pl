%%%% -*- Mode: Prolog -*-

%%% lmc.pl
%%% Implementazione in Prolog del Little Man's Computer
%%% Jacopo Maltagliati ~ 830110
%%% j.maltagliati@campus.unimib.it

%%%% Simulatore LMC

%% initial_state(+Mem, +Input, -InitialState)
%% Crea uno stato iniziale a partire da una memoria di 100 posizioni e da
%% una coda di input.

initial_state(Mem, Input, state(0, 0, Mem, Input, [], noflag)) :-
    length(Mem, 100). % qad hack


%% build_state(+Acc, +Pc, +Mem, +In, +Out, +Flag, +Exc, -NewState)
%% Restituisce un nuovo 'state' a partire da una serie di parametri.
%% Nel caso si sia  verificata un eccezione ('exc' invece di 'noexc'),
%% restituisce un 'halting_state'.

build_state(Acc, Pc, Mem, In, Out, Flag, noexc, NewState) :-
    NewState = state(Acc, Pc, Mem, In, Out, Flag).

build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState) :-
    NewState = halted_state(Acc, Pc, Mem, In, Out, Flag).


%% eabort(?MsgID)
%% Versione release di eabort-dbg(+MsgID): fallisce senza stampare nulla.

eabort(_MsgID) :-
    false.


%% execution_loop(+State, -Out)
%% Computa il prossimo stato invocando il predicato 'one_instruction'.
%% Si ferma e restituisce la coda di output se lo stato ricevuto e' un
%% 'halted_state'

execution_loop(state(Acc, Pc, Mem, In, Out, Flag), NewOut) :-
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
    execution_loop(NewState, NewOut),
    !.

execution_loop(halted_state(_Acc, _Pc, _Mem, _In, Out, _Flag), Out).

%% one_instruction(+State, -NewState)
%% Esegue una istruzione tramite un ciclo fetch-decode-execute.
%% Nel caso in cui l'opcode non sia valido, fallisce.

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState) :-
   fetch(Mem, Pc, Inst),
   decode(Inst, Opc, Immed),
   valid_opcode(Opc),
   execute(Opc, Immed, state(Acc, Pc, Mem, In, Out, Flag), NewState),
   !.

one_instruction(_State, _NewState) :-
    eabort(99).

%%%% Simulazione del Datapath

%% fetch(+Mem, +Addr, -IorD)
%% Carica un dato dalla memoria usando un indice.
fetch(Mem, Addr, IorD) :-
    nth0(Addr, Mem, IorD).

%% decode(+Content, -Opc, -Immed)
%% Riceve un istruzione e ne restituisce l'opcode e il valore immediato.
decode(Content, Opc, Immed) :-
    divmod(Content, 100, Opc, Immed).

%% execute(+Opc, +Param, +OldState, -NewState).
%% Esegue una singola istruzione in base all'opcode e al suo valore
%% immediato. Invoca la 'build_state' per costruire e restituire un
%% nuovo stato.

execute(1, Immed, state(Acc, Pc, Mem, In, Out, _Flag), NewState) :-
    %% Opcode: 1
    %% Istruzione: ADD
    fetch(Mem, Immed, ToAdd),
    adder_add(Acc, ToAdd, NewFlag, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, In, Out, NewFlag, noexc, NewState), !.

execute(2, Immed, state(Acc, Pc, Mem, In, Out, _Flag), NewState) :-
    %% Opcode: 2
    %% Istruzione: SUB
    fetch(Mem, Immed, ToNeg),
    adder_neg(ToNeg, ToAdd),
    adder_add(Acc, ToAdd, NewFlag, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, In, Out, NewFlag, noexc, NewState), !.

execute(3, Immed, state(Acc, Pc, Mem, In, Out, Flag), NewState) :-
    %% Opcode: 3
    %% Istruzione: STA
    lsubst(Immed, Acc,  Mem, [], NewMem),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, NewMem, In, Out, Flag, noexc, NewState), !.

execute(5, Immed, state(_Acc, Pc, Mem, In, Out, Flag), NewState) :-
    %% Opcode: 5
    %% Istruzione: LDA
    fetch(Mem, Immed, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, In, Out, Flag, noexc, NewState), !.

execute(6, Immed, state(Acc, _Pc, Mem, In, Out, Flag), NewState) :-
    %% Opcode: 6
    %% Istruzione: BRA
    build_state(Acc, Immed, Mem, In, Out, Flag, noexc, NewState), !.

%% Opcode: 7
%% Istruzione: BRA
execute(7, Immed, state(0, _Pc, Mem, In, Out, noflag), NewState) :-
    build_state(0, Immed, Mem, In, Out, noflag, noexc, NewState), !.

execute(7, _Immed, state(Acc, Pc, Mem, In, Out, noflag), NewState) :-
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, Out, noflag, noexc, NewState), !.

execute(7, _Immed, state(Acc, Pc, Mem, In, Out, flag), NewState) :-
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, Out, flag, noexc, NewState), !.

%% Opcode: 8
%% Istruzione: BRP
execute(8, Immed, state(Acc, _Pc, Mem, In, Out, noflag), NewState) :-
    build_state(Acc, Immed, Mem, In, Out, noflag, noexc, NewState), !.

execute(8, _Immed, state(Acc, Pc, Mem, In, Out, flag), NewState) :-
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, Out, flag, noexc, NewState), !.

%% Opcode: 9
%% Valore Immediato: 1
%% Istruzione: INP
execute(9, 1, state(_Acc, Pc, Mem, In, Out, Flag), NewState) :-
    lpop(In, NewAcc, NewIn),
    between(0, 999, NewAcc),
    increment_and_wrap(Pc, NewPc),
    build_state(NewAcc, NewPc, Mem, NewIn, Out, Flag, noexc, NewState), !.

execute(9, 1, state(Acc, Pc, Mem, In, Out, Flag), NewState) :-
    build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState), !.

%% Opcode: 9
%% Valore Immediato: 2
%% Istruzione: OUT
execute(9, 2, state(Acc, Pc, Mem, In, Out, Flag), NewState) :-
    lrpush(Out, Acc, NewOut),
    increment_and_wrap(Pc, NewPc),
    build_state(Acc, NewPc, Mem, In, NewOut, Flag, noexc, NewState), !.

%% Opcode: 0
%% Valore Immediato: [0..99]
%% Istruzione: HLT
execute(0, _Immed, state(Acc, Pc, Mem, In, Out, Flag), NewState) :-
    build_state(Acc, Pc, Mem, In, Out, Flag, exc, NewState), !.

%% Trap
%% Sistema catch-all per gli errori.
%% Non dovrebbe succedere mai una cosa del genere.
execute(_Opcode, _Immed, _State, _NewState) :-
    eabort(99), !.


%% adder_add(+OpA, +OpB, -Flag, -Res)
%% Adder con Carry Flag
%% Somma aritmeticamente due interi: se il risultato non e' compreso
%% tra 0 e 999 ne restituisce il modulo 1000 e 'flag', in caso
%% contrario vengono restituiti il risultato e 'noflag'.
adder_add(OpA, OpB, Flag, Res) :-
    Tmp is OpA + OpB,
    set_flag(Tmp, Flag),
    divmod(Tmp, 1000, _, Res).

%% adder_neg(+OpA, -Res)
%% Negatore per l'adder
%% Prende in input un valore numerico e ne restituisce la negazione
%% aritmetica.
adder_neg(OpA, Res) :-
    Res is -OpA.

%%%% Predicati di supporto

%% increment_and_wrap(+Reg, -NewReg)
%% Incrementa un registro, se viene raggiunto il valore 100,
%% si riparte da 0.
increment_and_wrap(99, 0).

increment_and_wrap(Reg, NewReg) :-
    between(0, 99, Reg),
    NewReg is Reg + 1.


%% set_flag(+Reg, -Flag)
%% Controlla che il valore di un registro sia compreso tra 0 e 999:
%% in questo caso restituisce 'noflag', altrimenti 'flag'
set_flag(Reg, noflag) :-
    between(0, 999, Reg), !.

set_flag(_Reg, flag).


%% valid_opcode(+Opc)
%% Controlla che un opcode sia valido, ovvero compreso tra 0 e 9,
%% escluso 4. Se non lo e', fallisce.
valid_opcode(Opc) :-
    between(0, 3, Opc), !.

valid_opcode(Opc) :-
    between(5, 9, Opc), !.

%% invalid_opcode(+Opc)
%% Controlla che un opcode non sia valido, ovvero minore di 0, uguale
%% a 4 o maggiore di 9. Se non lo e', fallisce.
invalid_opcode(4).

invalid_opcode(Opc) :-
    Opc @>= 10, !.

invalid_opcode(Opc) :-
    Opc @=< -1, !.


%% init_memory(+Pgm, -Mem)
%% Dato un programma, ovvero una lista di istruzioni, genera una memoria
%% valida per il simulatore. Se ci sono meno di 100 istruzioni, viene
%% generata una serie di valori di padding compresi tra 1 e 99, che vanno a
%% riempire le celle di memoria mancanti. Se abbiamo piu' di 100 istruzioni,
%% il predicato fallisce.
init_memory(Pgm, Pgm) :-
    length(Pgm, PgmLength),
    PgmLength == 100, !.

init_memory(Pgm, Mem) :-
    length(Pgm, PgmLength),
    PadLength is 100 - PgmLength,
    PadLength @>= 0,
    create_padding(PadLength, [], Pad),
    append(Pgm, Pad, Mem).

%% create_padding(+PadLength, [], -Pad)
%% Crea il padding per un programma di meno di 100 posizioni.
create_padding(0, Pad, Pad) :- !.

create_padding(PadLength, TmpPad, Pad) :-
    NewPadLength is PadLength - 1,
    append(TmpPad, [0], NewTmpPad),
    create_padding(NewPadLength, NewTmpPad, Pad).

%% lpop(+List, -Top, -Rest)
%% Data una lista, ne restituisce il primo elemento e il resto della lista.
lpop([Top | Rest], Top, Rest).

%% ltop(+List, -Top)
%% Data una lista, ne restituisce il primo elemento lasciandola invariata.
ltop([Top | _], Top).

%% llpush(+List, +Elem, -ResList)
%% Aggiunge un elemento in testa ad una lista e ne restituisce la nuova
%% versione.
llpush(List, Elem, ResList) :-
    append([Elem], List, ResList).

%% lrpush(+List, +Elem, -ResList)
%% Aggiunge un elemento in coda ad una lista e ne restituisce la nuova
%% versione.
lrpush(List, Elem, ResList) :-
    append(List, [Elem], ResList).

%% lsubst(+Index, +Insert, +Src, [], -Dest).
%% Sostituisce l'elemento della lista all'indice 'Index' con uno dato,
%% e restituisce la lista modificata.
lsubst(0, Insert, [_SrcH | SrcT], Temp, Dest) :-
    append([Temp, [Insert], SrcT], Dest),
    !.

lsubst(Index, Insert, [SrcH | SrcT], Temp, Dest) :-
    append(Temp, [SrcH], NewTemp),
    NewIndex is Index - 1,
    lsubst(NewIndex, Insert, SrcT, NewTemp, Dest).

%%%% ASSEMBLER/LOADER

%% lmc_run(+Filename, +Input, -Output)
%% Carica, assebla ed esegue un programma LMC, dati il suo percorso e una
%% coda di input.
lmc_run(Filename, Input, Output) :-
    lmc_load(Filename, Mem),
    initial_state(Mem, Input, State),
    execution_loop(State, Output).


%% lmc_load(+Filename, -Mem)
%% Crea una memoria compatibile con il formato LMC a partire da un file di
%% testo.
lmc_load(Filename, Mem) :-
    %% Passo 1
    open(Filename, read, Stream),
    step1_loop(Stream, 0, [], TokenLists),
    close(Stream),
    %% Passo 1 1/2
    assertz(access_label("0",0)), % evita l'errore di findall/3
    build_label_ptr_map(TokenLists, 0, [], NoLabelTokenLists),
    %% Passo 2
    step2_loop(NoLabelTokenLists, [], Pgm),
    retractall(access_label(_, _)),
    init_memory(Pgm, Mem).


%% step1_loop(+Stream, -1, [], -Result)
%% Passo 1: Stream -> Tokens <Label*, Mnemonic, IorL>
step1_loop(Stream, -1, Lines, Result) :-
    read_string(Stream, "\n", "\r", _NewLn, Line),
    strip_and_push(Line, Lines, Result), !.

step1_loop(Stream, _Ln, Lines, Result) :-
    read_string(Stream, "\n", "\r", NewLn, Line),
    strip_and_push(Line, Lines, TmpRes),
    step1_loop(Stream, NewLn, TmpRes, Result), !.


%% comment_pos(+Line -Pos)
%% Stabilisce la posizione di un commento, e ritorna l'indice dell'ultimo
%% carattere utile della stringa
comment_pos(Line, Pos) :-
    sub_string(Line, Pos, 2, _CPos, "//"), !.

comment_pos(Line, Pos) :-
    sub_string(Line, _Zero, _Length, Pos, _Line).

%% strip_and_push(+Line, +List, -Result)
%% Trova e rimuove tutti i caratteri dopo la prima occorrenza di "//"
%% nella riga corrente, per poi restituire una lista contenente i token
%% di ogni riga.
strip_and_push(Line, List, Result) :-
    comment_pos(Line, Pos),
    sub_string(Line, _, Pos, _, NoComment),
    string_upper(NoComment, UpComment),
    split_string(UpComment, "\s\t\n\r", "\s\t\n\r", Tokens),
    cleanup(Tokens, List, Result).


%% cleanup(+Tokens, +List, -Result)
%% Aggiunge i token alla lista, a patto che non siano vuoti.
cleanup([""], List, List).

cleanup(Tokens, List, Result) :-
    append(List, [Tokens], Result).


%% build_label_ptr_map(+TokenLists, +Index, [], -ResTLs)
%% Costruisce in memoria una mappa delle label, togliendole dalla lista
%% di token in ingresso.
build_label_ptr_map([], _Index, ResTLs, ResTLs) :- !.

build_label_ptr_map(TokenLists, Index, TmpTLs, ResTLs) :-
    lpop(TokenLists, CurTL, RestTLs),
    lpop(CurTL, FirstToken, ModTL),
    what_is(FirstToken, DataType),
    is_label(DataType), % Fail1: Non e' un label
    label_available(FirstToken), % Fail2: Esiste gia'
    lrpush(TmpTLs, ModTL, NewTLs),
    add_label(FirstToken, Index),
    NewIndex is Index + 1,
    build_label_ptr_map(RestTLs, NewIndex, NewTLs, ResTLs), !.

build_label_ptr_map(TokenLists, Index, TmpTLs, ResTLs) :-
    lpop(TokenLists, CurTL, RestTLs),
    ltop(CurTL, FirstToken),
    what_is(FirstToken, _DataType),
    mne_query(FirstToken, _OpCode, _Type), % Fail3: Non e' un'istruzione
    NewIndex is Index + 1,
    lrpush(TmpTLs, CurTL, NewTLs),
    build_label_ptr_map(RestTLs, NewIndex, NewTLs, ResTLs), !.

build_label_ptr_map(_TokenLists, _Index, _TmpTokenLists, _ResTokenLists) :-
    eabort(3).


%% step2_loop(+TokenLists, [], -ResMem)
%% Passo 2: Tokens <Istruzione, IorL> -> Memoria LMC
step2_loop([], ResMem, ResMem) :- !.

step2_loop(TokenLists, TmpMem, ResMem) :-
    lpop(TokenLists, CurTokenList, NewTokenLists),
    length(CurTokenList, CTLen),
    unpack_and_asm(CurTokenList, CTLen, Asm),
    lrpush(TmpMem, Asm, NewTmpMem),
    step2_loop(NewTokenLists, NewTmpMem, ResMem), !.


%% unpack_and_asm(+TokenList, +Length, -Assembly)
%% Spacchetta una lista di token e usa la kb per riconoscere le istruzioni
%% e le eventuali etichette, per poi restituire l'istruzione assemblata.

unpack_and_asm(TokenList, 2, Assembly) :-
    lpop(TokenList, TokenA, TmpTLA),
    ltop(TmpTLA, TokenB),
    asm_binstruction(TokenA, TokenB, Assembly).

unpack_and_asm(TokenList, 1, Assembly) :-
    ltop(TokenList, TokenA),
    asm_uinstruction(TokenA, Assembly).

unpack_and_asm(_TokenList, _Length, _Assembly) :-
    eabort(6).


%% asm_binstruction(+TokenA, +TokenB, -Assembly)
%% Assembla un'istruzione in base ai token che riceve in input, cercando
%% di risolverli usando la kb.

asm_binstruction(TokenA, TokenB, Assembly) :-
    what_is(TokenB, data, ValueB),
    what_is(TokenA, binstruction, ValueA),
    Assembly is ValueA + ValueB,
    between(0, 999, Assembly), !.

asm_binstruction(TokenA, TokenB, Assembly) :-
    what_is(TokenB, label, ValueB),
    what_is(TokenA, binstruction, ValueA),
    dif(ValueA, 000),
    Assembly is ValueA + ValueB,
    between(0, 999, Assembly), !.

asm_binstruction(TokenA, _TokenB, _Assembly) :- % Istruzioni unarie errate
    what_is(TokenA, uinstruction, _ValueA),
    eabort(5), !.

asm_binstruction(TokenA, TokenB, _Assembly) :- % Filtro per `DAT LABEL`
    what_is(TokenB, label, _ValueB),
    what_is(TokenA, binstruction, 000),
    eabort(2), !.

asm_uinstruction(TokenA, Assembly) :-
    what_is(TokenA, uinstruction, Assembly).


%% what_is(+Love, ?Type, ?Value)
%% Stabilisce il tipo di un token, a scelta tra:
%%  - Istruzione Binaria (con Valore Immediato)
%%  - Istruzione Unaria (senza Valore Immediato)
%%  - Dato numerico (Valore Immediato)
%%  - Etichetta
%% Restituisce al chiamante i parametri del token.

what_is(Love, binstruction, Value) :-
    mne_query(Love, Value, immed).

what_is(Love, uinstruction, Value) :-
    mne_query(Love, Value, noimmed).

what_is(Love, sinstruction, 000) :-
    mne_query(Love, 000, _Type).

what_is(Love, data, Value) :-
    number_string(Value, Love).

what_is(Love, label, Value) :-
    access_label(Love, Value), !.

what_is(_Love, label, _Value) :-
    eabort(4).


%% what_is(+GoingOn, ?Type)
%% Stabilisce se il tipo di un token e' quello richiesto dal chiamante.
%% Utilizzato solo dal risolutore delle label.

what_is(GoingOn, binstruction) :-
    mne_query(GoingOn, _Value, immed), !.

what_is(GoingOn, uinstruction) :-
    mne_query(GoingOn, _Value, noimmed), !.

what_is(GoingOn, data) :-
    number_string(_Value, GoingOn), !.

what_is(_GoingOn, label).


%% is_label(+Type)
%% Stabilisce se il tipo e' label
is_label(label).


%% mne_query(?Mnemo, ?OpCode, ?HasImmed)
%% Knowledge Base: Istruzioni Binarie
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


%% Knowledge Base: Label
%% Questo predicato viene asserito a runtime dal risolutore delle Label.
dynamic access_label/2.


%% label_available(+Label)
%% Stabilisce se una label puo' essere usata, oppure fallisce.
label_available(Label) :-
    findall(Pointer, access_label(Label, Pointer), PointerList),
    length(PointerList, 0), !.

label_unavailable(Label) :-
    findall(Pointer, access_label(Label, Pointer), PointerList),
    length(PointerList, Length),
    Length @> 0.

%% add_label(+Label, +Pointer)
%% Aggiunge una label alla kb
add_label(Label, Pointer) :-
    assertz((access_label(Label, Pointer) :- !)), !.

%%%% Funzioni di aiuto per il debug

%% eabort_dbg(+MsgID)
%% Stampa un messaggio di errore e fallisce.
%% NOTA: Occorre modifcare eabort/1 per usarla

eabort_dbg(0) :-
    print("LMC00: Lettura di <Input> fallita."),
    false.

eabort_dbg(1) :-
    print("LMC01: Opcode non valido."),
    false.

eabort_dbg(2) :-
    print("LMC02: Etichetta non attesa dopo un'istruzione DAT."),
    false.

eabort_dbg(3) :-
    print("LMC03: Errore nel risolutore delle Etichette: possibile duplicato."),
    false.

eabort_dbg(4) :-
    print("LMC04: Referenziazione etichetta mai dichiarata."),
    false.

eabort_dbg(5) :-
    print("LMC05: Operazione unaria con valore immediato."),
    false.

eabort_dbg(6) :-
    print("LMC06: Riga malformata."),
    false.

eabort_dbg(99) :-
    print("LMC99: Eccezione non attesa."),
    false.


%% dbg_load(+Filename, +Input, -Output)
%% Esegue una lmc_run/3 ma tracciando lmc_load/2

dbg_load(Filename, Input, Output) :-
    leash(-all),
    trace,
    lmc_load(Filename, Mem),
    nodebug,
    initial_state(Mem, Input, State),
    execution_loop(State, Output).


%% dbg_load(+Filename, +Input, -Output)
%% Esegue una lmc_run/3 ma tracciando execution_loop/2

dbg_xloop(Filename, Input, Output) :-
    lmc_load(Filename, Mem),
    initial_state(Mem, Input, State),
    leash(-all),
    trace,
    execution_loop(State, Output),
    nodebug.


%% print_memory(+Filename)
%% Stampa la'output di lmc_load/2

print_memory(Filename) :-
    lmc_load(Filename, Mem),
    write(Mem).


%% print_memory(+Filename)
%% Stampa la'output di lmc_load/2, tracciandola

dbg_print_memory(Filename) :-
    leash(-all),
    trace,
    lmc_load(Filename, Mem),
    nodebug,
    write(Mem).


%%%% eof: lmc.pl