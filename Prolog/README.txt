lmc.pl
Implementazione in Prolog del Little Man's Computer
Jacopo Maltagliati ~ 830110
j.maltagliati@campus.unimib.it


ABSTRACT

Il little man computer (LMC) e' un semplice modello di computer creato per scopi
didattici. Esso possiede 100 celle di memoria (numerate da 0 a 99) ognuna della
quali puo' contenere un numero da 0 a 999 (estremi inclusi). Il computer possiede
un solo registro, detto accumulatore, una coda di input ed una coda di output.
LMC possiede un numero limitato di tipi di istruzioni ed un equivalente assembly
altrettanto semplificato.


IMPLEMENTAZIONE

Questa direttrice contiene l'implementazione in Prolog del simulatore proposto
dal progetto. Per caricare ed eseguire il simulatore e' sufficiente consultare
il file 'lmc.pl' con l'ambiente SWI-Prolog. Il file e' stato sviluppato in
ambiente Emacs 24.5.1 + SWI-Prolog 7.2.3 (Debian x64).

Una volta consultato il file e' possibile eseguire una simulazione dell'LMC
eseguendo la seguente query nel prompt dell'ambiente Prolog in uso:

	  lmc-run("/path/to/file", [], Out).

Dove "/path/to/file" sia un percorso valido verso un file assembly LMC,
     [] sia una lista di input contenente numeri compresi tra 0 e 999 e
     Out sia la variabile che si vuole far unificare con la lista di output.


DEBUG

L'implementazione prevede alcune funzioni atte ad aiutare il debug: e' possibile
sostituire il predicato 'eabort/1' con il predicato 'eabort_dbg/1', al fine di
stampare un messaggio di errore quandunque il programma fallisca con risultato
NIL, al fine di aiutare nella stesura di programmi LMC.

Inoltre, sono disponibili i seguenti predicati:

	 dbg-load("/path/to/file", Mem).

	 dbg-xloop("/path/to/file", [], Out).

	 print-mem("/path/to/file").

	 dbg-print-mem("/path/to/file").

Dove "/path/to/file" sia un percorso valido verso un file assembly LMC,
     [] sia una lista di input contenente numeri compresi tra 0 e 999 e
     Out sia la variabile che si vuole far unificare con la lista di output.

Essi permettono di eseguire, rispettivamente:
     - Una trace automatica dell'Assemblatore LMC e conseguente esecuzione.
     - Una trace automatica del Simulatore LMC.
     - Una stampa della memoria preparata dall'Assemblatore LMC
     - Una trace dell'Assemblatore LMC e conseguente stampa.


NOTE

Tutti i file hanno line-endings in modalita' UNIX (LF).
