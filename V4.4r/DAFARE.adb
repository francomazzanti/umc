-----------------------------
-- WARNINS PER POSSIBILI SISTEMI DIVERGENTI  
---------------------------------
-- USARE DIRETTAMENTE ORE_DB nickname invece di SRC_DB nickname quando gli active objects sono solo 1
---------------------------------
-- LE INFORMAZIONIN SULLA ORE POSSONO ESSERE MESSE NELLA OCI e rosse dalla memoria una volta 
--  calcolate le objec evolutions --  NEL CASO INTERATTIVO !!!!!!!!!!!
-- NELLA ORE E' SUFFICENTE METTRCI UNA SECONDA KEY per identificare e risolvere i conflitti della KEY primaria.
---------------------------------
 - if UMC non-interactive  (not -w and with formulafile) do not generate ground info and Free DB info.
 - SMART HASHING and space-saving,  
---------------------

----------------------
When Single Object, in State  Obj.var = $1   we might omit "Obj."
----------------------

----------------------------------------
ASSERTION:    VALUTARE
 PRED vale  False   se PRED non e' una state label
 PRED = PRED invece value True anche se PRED non esiste come state label.
----------------------------------------

-----------------------
--   ELiminare ricorsione e andare su modello ITERATIVO???
Class Classname is
Vars v1:int :=0;
Behavior
s1 -> s1 {- [v1 < 3000000] / v1 := v1+1;}
end Classname;
Obj1: Classname
--  max Z: <true> Z  --  stack overflow
--   after 330_000 states ,  980_000   "max Z: <true> Z"  computations
--   after 1_77_000 states, 3_720_000  "EF FINAL" computations
------------------------

E[<> true {true} U {a(11,11)} true]  (==TRUE)  EXPLANATION DA IMPLEMENTARE

BUG:    (form or  PRINT_ONCE(..)    se form e' aborted, viene valutata PRINT_ONCE  !!!!!
 Quando e' presente PRINT*** gli and/or  dovrebbero diventare strettti!!!! (orelse/andthen)

BUG BUG
 obj.SetMode   --  se c'e' un errore nel nome del signal viene fatto un runtime error !?!"?"?

IMPROVEMENT
  if (..) {  };   la "{" e' obbligatoria altrimenti errore.   Niente "then"

  COUNT   ...   permetter COUNT(specifier), fa un qualcosa tipo specifier ++

PARSING:
   <a($1)> <b(%1+1)>   -- dare errore nel caso di espressioni, (o supportarle)

BUG:
Assign,var,indexes,value  -->  var := value,   var[index] := value,  var[index1,index2] := value 
L'index usato nel signal "assign" e' un vettore sempre! eventualmente emptyvector

IMPROVEMENT
State w0.message[0] = $1  -> $1   is OK  but may result in unverifiable labels 
  like   "123", "[1,2,3,]", "[]"
Should we extend UCTL syntax?  wouldn't "[...]" become umbiguos? ..   YES
Should we consider thisn a syntactic error?   -->$1   should become --> somelabel($1) YES
RULE:   RHS of StateRules cannot have $ as main label
But the same problem occurs also for action labels:
 Action oo.sig($1) -->  $1
We should identify $vars martching values and forbid them in the rhs as main labels


IMPROVEMENT:  
 allow composite literals like:
  x: int[] := [27 => OO, 25 => BB]
  OO:Class (v => [2=>OO])
  State oo.vv= []  ->  foo  
  State oo.vv= [1,2,3]  ->  foo  

GENERAZIONE E ESPORTAZIONE DI UN MODELLO NEL FORMATO .aut 
  (senza state labels se non ci sono)

IMPROVEMENT:
Allow composite parametric literals in State Abstractions:
>   State s0.message = [s0,*]  -> s0($2)
-           idsL[s0,message]  idsR = [s0,*] 
-   come distinguiamo  s0.message da [s0,*]  ???
--  potremmo aggiungere "[", "*", e "]" esplicitamente come item

VALORI DEFAULT: ripulire contettualmente!!
  var x;
  cosa dovrebbe restituire la valutazione di x?  e di x[1], e di x[x]?
  possiamo intridurtre un esplicito valore "undefined"?
  --
  var x: int[];  ha senso restituire [] come valode default per x, e "0" come valore default per x[n]
  cosa dobiamo restituire per x[undef]  o fare per x[undef] :=3;
  --
  v: int[[]];  deafult value of v should be [],  v[1000] should be [] !! not "0"!
---------------
  problema
  v: int:=  3;     v[2] :=4;  COSA DOVREBBE SUCCEDERE??
  --  attualmente v[2]:=4  genera un runtime_error
  --  NOTA PERO'  CHE  x := v[2]  NON da' errore ma assegna  x :=0 !!!!!  e' cio' incosistente!?!?!
  -- in alternativa  v potrebbe diventare [3,0,4]  v == v[0]
  --  se v= []    v[2] :=2  e' OK,   e y := v[3] e' OK
  --     v=123   v'length=0  ma anche v1.head!!!   e' cio' consistente!?!?!?
  --             forse v.length dovrebbe essere "-1" e v.head errore?!?
  - UNA ALTERNATIVA SAREBBE QUELLA DI CONSIDERARE <n> come uno zucchero sintattico per <[n]>
  --  per cui sarebbe  n == [n]  == [[n]] == [[[n]]]
  -- ed in questo caso la selezione su v sarebbe sempre ben definita !!
  --il tipo "int"  includerebbe per default i sottotipi int[], int[[]]  ...
  -- ed in pratica il significato del nome del tipo sarebbe l'identificazione del valore default per la
  -- selezione di componenti non esistenti.

--
RIDEFIBNIRE UGUAGLINZA  e ASEGNEMENTO FRA VETTORI: 
  [0,0,0] = [] (nullintvect)   [null,null,null] = [] (nullobjvect)
  [0,0,1][2] = 0  =A>   []  (non [0,0,0])
  ma in questo caso che significatoi hanno Head, Tail e soprattutto Length e Concatenazione  "+" ? 

RUNTIME_ERRORS MORE INFOMATIVE
COMUNICARE DOPO UNA VERFICA SE CI SONO STATI RUNTIME_ERRORS
Mettere parametri in Runtime_Error

  
----  Parametric propositions (in positive conjunctive contexts preceding implications)
AG ( (train1($1) and train0($0)) implies $0 /= $1)
-- whose semantics is: ExistsUnique $1, $2, s.t. train1($1) and train0($0) 


| Priority:   when there are no "-1" priorities, all negative priorities are shifted up of the same measure
| so that the higher becomes "-1" (suppoting round robin scheduling)

| IN PROGRESS  FULL RUNTIME VALUE TYPING
| DAFARE:  Type checking during dispatching = implicit guard
| DAFARE:  il valore "0" e' speciale e viene automaticamente convertito, in Bool e Object o Struct
| nel caso sia assegnato a variabili senza tipo:   
|  Vars V;     s1 -> s1 {- / OUT.([123] + V)   / V.tail}
| (onde evitare Runtime Errors!!)

| Introdurre il nome esplicito di tipo "any"  any[]" e definire un unico evento "return(evalue:any)"
|  (ma "any" puo' essere anche "composite??)  direi di si - default value = 0
| Utilizzare il Kind sono per l'analisi statica di variabili esplicite.
| creare variabili implicite di tipo "any".

| "0" automaticamente convertito in "null" object o "[]" o "False" quando usato in contesti altrimenti erronei??   FATTO

| All_Events non dove tenere overloading  (piu' eventi con lo stesso nome e numero di parametri)
| Dichiarazioni overloaded in classi diverse vengono fuse nella medesima dichiarazione polimorfa globale
| in All_Events.   Le singole dichiarazioni specializzate compaiono nella chart_Events (se c'e'^????)
| e vengono utlizzate per effettuare a runtime controlli aggiuntivi (implicit guards) sui parametri.
| Questo significa che non e' possibile type checking statico dei parametro al momento delle Call/Send
|  FATTO

| Capire overloading fra signals e operations (possibile?)  e nel caso lazy???

| EXTENSION !!!!!
|  Ask action/transition:  chide all'utente la azione da eseguire e la esegue.
|  Ask actions sono solo in oggetti speciali, che hanno priorita' minima e che cambiano continuamente stato
|  Ask actions possono essere usate solo quando UMC e' in modo interattivo
|

| IMPROVEMENT
|  EG True  /  AF False  -- l'insieme delle computationi lungo tutto il path
| poitrebbe  essere reso opzionalmente e selettivamente analizzabile definendo Explain_Path
| come una funzione che come Explain_Tree restituisce una tabella di sottocomputazioni

| IMPROVEMENT    LAZY PARSING   AND PRETTY PRINTING
|  Specialmente nel caso di automi sequenziali, gli states sono derivati dalle transizioni.
|  LAZY Parsing in modo da acquisire piu' facilmente modelli da altrio sistemi
| (State definitions, Variabili, Eventi e Signals generati automaticamente)
|  FATTO per gli stati di un automa sequenziale, da estendere al caso di automi paralleli.
| Da fare per le variabili locali.     FATTO
|  FATTO  DA COMPLETARE CON I TIPI  OBJ /VECTORS INT BOOL da dedurre per quanto possibile.
| Da fare per i signals/eventi di cui le transizioni hanno trigger   FATTO per SIGNALS
| DARE WARNINGS QUANDO SI AGGIUNGONO DECHIARAZIONI LAZY
|   obj.signal  -->  obj.kind=Object   
|   [guard]  -> guard.kind=Bool   (boolExpr -> bool type)
|   if (cond)  ->  cond.kind=Bool
|  DISTINGUARE LAZY PARSING w.r.t Vars and Events (opzionale) 
|  da Lazy Parsing w.r.t State definitions (con riconoscimento automatico
|  di stati concorrenti) QUESTO potrebbe diventare default!!!
|  Stato parallelo = stato con solo e piu' di uno stati composti e non Owner di transizioni.


| IMPROVEMENT
| Pretty printing di una modello(specialmente nel caso lazy in cui le dichiarazioni 
| mancanti sono aggiiunte implicitamente. Occorre fare il parsing e salvare i commenti
| sia end-of -line che full-line

| IMPROVEMENT
|  Se NOnInterctive, appena possibile sostituisce SRC_DB elements con fingferprints, sel ne vale la pena
|  UMC se NonInteractive,  mette a False Ground_Action_Labels_Needed
|  MC2DOT setta NonInteractive, e setta Ground_Action_Labels_Needed solo se realmente non needed.
| Questo risolve il caso di systemi con molti elementi.
| Come ottizzare il caso di sistemi con pochi elementi ma molto grandi?????  (ORC_DB)
| Rimuovere le code degli eventi da ORC e metterle in SRC

|IMPROVEMENT
|
|<$a> <%a> true
|Variable not allowed in the context   !!!!!! permetere Vars anche nel mail label!!

|MARK_STEP   called solo quando una computation termina in modo definitivo
||E.g.  AG true su un sistema che non ha stati finali  viene chiamata solo alla fine.

IMPROVEMENT:  in  CTL mode   (EX true)  le formula sono riscritte senza aggiungere {true};


 
|Entry/Exit Activities of States:
|
|State TT is  Entry {...};  -- no dependency ontrigger args
|State TT is  Exit {...};
||-- Commutative/associative Parallel composition 
|-- se due Entry sono associate a stati due regioni concorrenti NON devono interferire:
|a)  vars updated da 1) are nor updated by 2)
|b)  vars read by 1)   are not updated by 2)
|c)  var read by 2) are not updated by 1)
|d)  signals are sent by at most ONE of them
|--++  Se ho una system transition composta da due basic transitions (transizioni parallele)
|a) b) c) d)   devono valere per tutti gli stati in cui si Entra
|E.g:
|given t1:    src1 ->  tgt1-1.tgt1-2.tgt1-3 
|let lca = least_common_ancestror(src,tgt1-1.tgt1-2.tgt1-3)
|let targets1 = {ancestors of tgt1-3  and descendents of lca, and having Entry section)
|                + {default-initial descendents of tgt1-3, having Entry section}
|-- ide for T2
|FOR each pair <s1,s2>, s1 in targets1, s2 in targets2: NON_Interfere(s1,s2)
|--
|---  STEP by STEP   Entry/Exit only for Sequential Substates of TOP State
|-- 


|========================================
|
|NEGATIVE PRIORITIES ????  Seem OK, but maybe we should check


| IMPROVEMENT
| LE OBJECT QUEUE POTREBBERE ESSER OPARTE DELLA SYSCONF E NON DELLA OBJECTCONF.
| IN QUESTO MODO LE EVOLUZIONI obj --obj non dipendono, in caso di code FIFO dagli elemnti in coda
| e non occorre ricalcolarli ogni volta.

|----------------------------
|--   FATTO O NO!?!?!?!?
|----
|--Sarebbe meglio che All_charts avesse solo il nome degli eventi ed il numero
|--dei parametri, in modo da poter generare il codice per invio di messaggi.
|--Ma poi per la struttura dei parametri e dei tipi sarebbe meglio che la
|--classe facesse riferimento alle proprie definizioni.
|----------


|-- AGGISTATO O NO!?!?!?!
|UMC BUG --      Action  foo($1,$2) and $1=33 and $2=123 -> ...

|  IMPROVEMENT
| Permettere:   
|   int[3]   fixed vector, wrong read/write become Runtime_Errors
|   int[*]   oltre a int[]   (stesso significato)
|   int [*,*]   come matrice bidimensionale (implementato come vector di vector?)
|   int [*,*,*]   -- perche' no?!?

| IMPROVEMENT
| type foo = {int;   int[*];  int[*,*]; }
|           foo[0]   foo[1]  foo[2]

|----------------------------------------
|UCTL :  (%OO  =OO)   GIVE STATIC ERROR FOR UNDEFIEND VARSINSTANCES


| -------------------------------
| CHECK   Error in Defers clause  crate mess
| Transitions:
|   ^
| Error: Found use of undeclared event Get_Request


| -----------------------------
| DAFARE.       $1($*)   NON MATCHES   assign(var,i,val)
| BUG.       assign($var,$i,$vl)   mostra $i come intvalue (e' index !!! OK)

| IMPROVEMENT  (FATTO!?!?)
| UMC:  MULTIPLE ERROR DURIMNG PARSING.

|  IMPROVEMENT ?!?!?
| lostevent(ev)    senza ulteriori parametri come TEMPLATE matcha tutti i lostevent ground.


| ------------------------
| UCTL:   (foo($a) implies  bar(%a))   
| UCTL:   eq(obj,attr,$v) implies bat (%v)
| ---------------

| IMPROVEMENT
| umlString      IMPLEMENTARE STRING LITERALS !!!


| -- Constraints   {  label, label }
|   durante la minimizzazione taglia le evolutione quando incontra le labels
|  - Il TAB generato salta gia tutti i passi TAU 

Doc :     [ = ]   [  < ]   meaning of = > in the base of mixed types.
Doc : meaning of "+" in the case of non int types
Doc : meaining of  := in case of different types
UMC?

-----------------
dato x: Token := aa
     y: Token := bb
     c := x+y a cosa e' uguale?

SEMANTINS: al momento e' uguale a int(aa) + int(bb)  visuazzato come int o "undefined"
SEMANTICS:  Porrebbe essere un nuovo token creato dinamicamente = aabb
SEMANTICS:  Porrebbe essere una String = "aabb"
SEMANTICS   Potrebbe essere un vettore [dynvect, [operator, "+"] , [token, "aa"], [token, "bb"]]
--------------
?????????????
ground vs leftside rules should use STATIC literalvalues instead of Normalized literalimages
--  e.g.    "true" should match the value "1" when the parameter is nottyped.
--------------

UCTL:   (foo($a) implies  bar(%a))
UCTL:   eq(obj,attr,$v) implies bat (%v)

CODE: mc.adb  shared with cmc, moving all UMC dependeces (prompt, ..) inside mc-configurations.adb.

--------------------
[4,3,3]. sorted = [3,3,4]
--------------------


SEMANTICS: sending of  signal  SUSPENDS RTC STEP like CALLOP   (as optional semantics)
SEMANTICS:  this allows splitting big transitions in atomic segments

UMCSRC:  global constants
UMCSRC: types  enumeration and  record
UMCSRC: types   string  string []
----  E' possibile  definire una classe String predefinita (tipo Token)
----  E  considerare "aaaa" come Nomi di oggetti implicitamente dichiarati di tipo String?

Invece di usare classe Token ...  permettere la dichiarazioni di
constanti:     aa, bb: constant  (al di fuori delle classi)

-----------------
NON SAREBBE MEGLIO che tutte le classi fossero self-contained (senza visibilita' degli
oggetti dichiarati successivamente? (i bindings vanno fattin staticamente assegnando valori
a variabili locali);  Tutti i nomi NONLOCALI potrebbero essere costanti globali (Tokens)
senza bisogno di dichiarazione esplicita!!!!
--------------

------------------
Fully dynamically typed expressions (all values have structure [type,val]
------------------

------------------
CALLID in operation calls has the structure of [sender_obj, ....], _caller = callid[0]
------------------

------------------
MINIMIZATIONS
-- Constraints   {  label, label }
  durante la minimizzazione taglia le evolutione quando incontra le labels
-----------------

Dynchoice states followed by untriggered transitions (univoquely owned) (handled as completion transitions)

UMCSRC: Dynamic "new" object creation (passive objects)
UMCSRC: Dynamic "new" object creation (active objects)

UMCSRC: Support of Entry / Exit actions (maybe added to relevant transitions )

EXPLAIN: ADJUST THE EXPLANATIONS FOR AX (may contain too many subcomputations, we should consider
EXPLAIN:  only those with the same value of the main computation)
EXPLAIN: PROBABLY THE SAME ISSUE HOLDS FOR AF/AU1/AU2

UMCSRC: Entry/Exit: ACTIONS   - STATEFLOW COMPATIBILITY

-----------
Quando class1  definisce signal  foo(x)
e class2  definisce signal  foo(y)
al momento UMC  da errore: type mismatch for event ...
--
Sarebbe meglio che All_Events avesse solo il nome degli eventi ed il numero
dei parametri, in modo da poter generare il codice per invio di messaggi.
Ma poi per la struttura dei parametri e dei tipi sarebbe meglio che la
classe facesse riferimento alle proprie definizioni.
MA QUESTO CHE PROBLEMI DI VISUALIZZAZIONE IMPLICA?
Devo vedere la classe target per capire come visualizzare i parametri dei signals
----------

-----------------------
UMCSRC:   variables introduced/elborated incrementally ?????
v1:int  ;=2
v2:int  := 3
v3:int := v1*v2;
NOT DONE BECAUSE OTHERWISE SEMANTICALLY UNCLEAR IF VARS ARE REINITIALIZED
(OR NOT:  THEY COULDBE EVALUATED STATICALLY)
-----------------------

Nella GUI JAVA  se commandname= MacOS  OPPURE  REsources ....
settare la workingdir opportunemente.

---------------------------------------
GUI: EVAL AND MINIMIZE:  LOGGING of advancement with
MaxQuesize, maxint, maxvector, to observe divegence

------------------------
UCTL:   (foo($a) implies  bar(%a))
UCTL:   eq(obj,attr,$v) implies bat (%v)
------------------------



