lmc.lisp
Implementazione in Lisp del Little Man's Computer
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

Questa direttrice contiene l'implementazione in Lisp del simulatore proposto dal
progetto. Per caricare ed eseguire il simulatore e' sufficiente compilare il
file 'lmc.lisp' con una qualsiasi implementazione di Lisp. Il file e' stato
sviluppato in ambiente Emacs 24.5.1 + SLIME + SBCL 1.3.14.debian e testato
anche con LispWorks 6.1 Personal Edition per Windows.

Una volta compilato il file e' possibile eseguire una simulazione dell'LMC
valutando la seguente funzione nel listener dell'ambiente Lisp in uso:

	  (lmc-run "/path/to/file" '())

Dove "/path/to/file" sia un percorso valido verso un file assembly LMC e
     '() sia una lista di input contenente numeri compresi tra 0 e 999.


DEBUG

L'implementazione prevede alcune funzioni atte ad aiutare il debug: sostituendo
le chiamate alla funzione 'eabort' con chiamate alla funzione 'eabort-dbg', e'
possibile stampare un messaggio di errore quandunque il programma fallisca con
risultato NIL, al fine di aiutare nella stesura dei programmi LMC.


NOTE

Tutti i file hanno line-endings in modalita' UNIX (LF).
