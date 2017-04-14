--------------------------------------------------------------------------------------
CHANGES FROM  THE DOCUMENTATION OF UMC VERSION 3.6

--   selection of a not existing components returns the default null value (0,null,[])
--   e.g. v: int[] := [0,1,2];   N :=v[4]; not an ERROR!! but becomes  N=0 

--   assignment to not existing components extends the vector size!!
--   e.g. v: int[] := [0,1,2];   v[4] :=4; not an ERROR!! but becomes [0,1,2,0,4]

--  allowed indexing inside sending of signals and operatrion calls
--  vect[i].signalÃ(args);     vect[i].opcall(args):   var:=  vect[i].funcall(args)

--  opcalls  directed versus not existing objects are redirected towards ERR
--  e.g.  v:= 17;   n.signal(args) ;  --NOT a RUNTIME ERROR! (the execution continues)
--                  the signal(args) is sent to ERR
--------------------------------------------------------------------------------------
LAST
Version 4.2/4.3
4.3h added cpp support in loadmodel. (/usr/bin/cpp -w -P  model.txt > cppmodel.txt)
    redesigned html visualization of parallel states (filled lightgray)
4.3g Fixed Lazy Parsing bug and HTML dump bugs.
    Print_Fireables now print full name of source/target states omitting just Top.
    When only 1 active object present the obj name can be omitted inside State Abstractions
    Introduced display of ground assignments to transition variables
    Fixed bug in display of trigger when RANDOMQUEUES
    Fixedbug in handling of explicit "initial -> ss" transitions
    Introduced  -y -z +z options for reducing memory usage 
    Revised Kernel interface w.r.t OpenK to allow Configurations satisfies in body
    Introduced PRINT /PRINT_ONCE predicates
    Introduced strictness in and/or/imples operators when forumlas contain PRINT/PRINT_ONCE
    Improved Explanations (still missing the Until2 cases)
    Introduced experimental parallelism on model generation and verification (-c -m options)

Version 4.1 / 4.11:
    Removed dublication is abstract labels and added alfanumerical sorting
    Synchronized shared src files with fmc
    Redesigned mc2dot with linear complexity with option "-t"
    Revised Explanations for simple cases <> [] EF AF in great detail
    From hashed_Sets to Nicknames in Comp_DB
    Added Xi command to ask for explanation of specific subcomputation. 
Version 4.11b    introdotte COUNT and PRINT (evaluating false)
    Added  Ci  command  to display info of configuration i
    Increased taskstacksize at 8192 * 8192
    Improved  State Observations and Action Observations expressions
       Action    ...  and x >0 and ...   -> ..
    Disallowed "inState" State observations !?.
    Re-allowed "inState" State observations (why were have they been disallowed?)
    Redesigned compute system evolutions (bug improvemente is efficiency)
    Ajusted small bugs in parsing and event handling

Version 4.01:
     Allowed  var[ind1][ind2] := value;  if var is not a struct old value is replaced by new struct
     Allowed State oo.xx[ind] = $1  and $1 > 3 --> fooo
     Allowed State oo.xx[ind1][ind2] = $1  and $1 > 3 --> fooo
     BugFixed: action expression aa()  matches only label "aa" without params  ( *()  any simple label)
     CHANGED:  sending of a signal without explicit target means sending to OUT not to SELF !!!

Version 4.0
     Changed LOGIC:  
       logic action "foo" matches LTS labels: {foo, foo(a), foo(a,b,c)"
       logic action "foo()" matches LTS labels: {foo}"   (was {foo, foo(a,b,c)}
       logic action "foo(*)" matches LTS labels: {foo(a)" (was {foo(a), foo(a,b,c)})
     Added evaluation time after eval results.
     Adjusted abstract labels for observations like  "Action $1 -> $1"
     allowed strctured literal like [11,bb,cc] inside state predicates
Version 3.9
     Fixed major bug in Resume_Action (concurrent regions)
     Improved Explanations for AF {} EF{}  
     Allowaed implicit "true" subformula in all formulas.
     Improved responsiveness of cgi scripts.
     Allowed node selection in Abstract_LTS graph
Version 3.8
     Aggiustato bug in EG true (when aborted was considered true)     !!!! DA PROPAGARE
     Spostati Nicknames DynStore fuori.
     Rifatta Explanation per EG AG <> [] EX EF AF (in corso)
     Aggiuste stats e opzione -debug
     Aggiunto flag NonInteractive
Version 3.7d 
     Reintrodotta la visualizzazione degli statecharts delle classi.
     Iniziato a rivedere completamente le explanations (incrementali)
     Aggiustato bug in DumP_Sequential_Composite e in Get_Ground_Labels
Version 3.7
  build d:
      reintrodotta generazione degli statecharts delle classi.
      UCTL:  fixed parsing bug for  "E [[ true] ...]" 
      allowed    signal(x)    and  signal(y)   in different classes (arg names overloading)
  build c: 
     aggiustato bug  per attributi con valori negativi
     aggisutato big bug in Find_State(state names one containing the other )
     allowed:   vect[index].signal(args);  vect[index].opcall(args); v := vect[index].funcall(args)
  build b:
     aggiustato grosso bug in AUntil1  (logics and utilities)
     aggiustato minor parsing bug in fixpoints
  build a: 
     aggiunto mc2dot e aut2dot per connessione con ltsconvertt del framework mcrl2
     ristrutturato il pacchetto rendendo configurations e UCTL due packages generici indipenenti
     mc2dot traversa il modello in modo breadth first generando diversi tipi di grafi.

Version 3.6
  build z:  fixed bug in EF (lost assisgnment)
            mc2tab -  added "-t" to request "taU  generation ans possibly optimization
  build w:
     Fixed bug in logic engine (now equiv to cmc07o) in the case
     lf AG EF sometimes incorrectly returning False.
  build v: 
     Removed display of class statechart (to speed up loadting time)
     Different display of class defintion (inside text frame)
     -- minimization with TRACE_OPTIMIZATION ism plain wrong!!! (never used fortunately)
     Fixed bug in Transition vars updated inside loops
     Fixed BUG in AF verification (in case of abort) ***  MAJOR UPGRADE ***   
  build u:
     In case of binary nested inside a Mac application bundle the working directory
       and the model filename are adjusted.
     Added  support for Unary minus in integer expressions.
     Assignment to bigger index of vector extends the vector as needed.
     Priorities:  if one class defines "Priority" they are used
     "Priority" can be the first or the second vardecl.
     Improved error messages
     Removed case-sensitivenes for keywords (Class/ Vars ,..)
     Fixed bug in  RANDOMQUEUE
     Added   terse/verbose commands for line-oriented execution
     Added    $1 /= literal , $1 = $2  constraints for Action- rules
  build t:
     Fixed bug in ;--   (line comment not recognized)
     Fixed bug in  visualization of fireable transitions after Runtime_Error
  build s:
     Fixed bug in  {  - / i:int; for i in 1..2 {print(i) }  }  -- tvar hiding
  build r:
     Fixed bug  in Rec_Depth _EF
  build p:
     Adjusted [1,2,3] literals in UCTL logic (non $,% or* allowed insde vector literals)
     Fixed bugs in diplaying explicit "initial" substates (dot visualization)
     Fixed bug  in  "action or tau"  etc etc  ... (all cases of tau_allowed)
     RANDOMQUEUE as first or second Var trigger random queue object handling
     Allowed multiple error messages during a single model compilation.
     Fixed bug for  Action $1: -> $1  (in case of stuttering)
  build o: 
    allowed multiple conjunctions in State rules:  x=1 and y=2 and z=6 -> hhh
    fixed bug in "not expr"
    adjusted  "State: maxemptyqueue=$1 -> Q($1)", "State: obj.emptyqueue=$1 -> Q($1)"
    adjusted  negative values in vars and args,  Runtime_Error abstract labl
    adjusted [] (emptyguard)
    adjusted event();
  build n: 
    modified default action as "Action: $1:$2.$3($*) -> $3($1,$2,$*)" 
    -w  no mc> cursor, 
  build b:   
    fixed bug in AX w.r.t multiple bindings)
  ---
  introduced  "Action: $1($*) -> $1($*)"  as default observation rule
  allowed $*  as varbinding for a list of arguments in abstraction rules
  allowed  /* ... */    as comment delimiters
  smoothed HTML interfaces
  fixed errors in explanations
  allowed multiple bindings in the logic
  reshaping of abstractions matching rules ..    foo(*) is satisfied both by  foo and foo(a)
    ground "foo(a)" is 
         MATCHED by leftrule  foo,foo(a),foo(*),foo(a,*) and 
         NOTMATCHED by  foo(),foo(a,b), foo(a,b,*)
  Notice that in the logic  the actionexpr  "foo(a)" continue to match abstractlabels foo(a),foo(a,b)
  Code restrucuturing allowing code sharing with cmc  mc-configurations / mc-uctl  packages
  Added default AbstractionRules for the case in which the user does not explicitly defined them.
  Fixed bugs in AbstractView,AbstractTriggerView,AbstractLosteeventview.

Version 3.5
  STRONG UCTL UNTIL SUPPORTED !!!
  full alignment with CMC V0.5  (same uct_types, parser,utilities and logic)
  NEXT: parametric formulae,  EF {}  
  Introduced Abstraction rules in the model.
  Introduced   obj:accept(event, arg1,arg2.arg3n)  al label (trigging event)
  Fixed small bug if recording of subcomputations  in case of and/or/implies
  Intruduced Priorities  ("Priority" as first declared attribute)

Version 3.4

Optimization: N  parallel independent transitions generate only 1 evolution 
                instead of factorial(N)

Fixed bug in Check_AX (when final state)
Fixed bug in Check_AUntil2

Added: Weak until1, weak until2

Simple states can defer too
(deferring declaration separated from state structure definition)

Introduced compact description of state structure
  State Top = Foo(R1[s2,s3],R2[(S1(a,b,c),S3(d,e,f)])

UCTL:   Introduced weak next operator

More flexible syntax for conditional and loop
 if (connd) then ... else .. endif
 while  (cond) .... endloop

Operations:  return statement without parameters ADDED implicilty at the end of the
  transition effects for void returning operations.

Major logics optimizations for linear complexity UCTL fragment
Added predefined Token Class.

Version 3.3
------------------------------------------------------
Allowed  OBJECT.*  as UCTL action exporession
Allowed  if cond {..} else {cond}  as action
Allowed  while cond {.. } as action
Allowed  transition vars declarations
Removed  infinite loop checks in model abstractions
Allowed access to implicit _caller parameter in operations calls
Allowed vector declarations and literals. 
Fixed UML bug in evolutions of parallel regions.

Added Observation modes:  BLACK_BOX, WHITE_BOX, GRAY_BOX, CUSTOM, INTERACTIONS
Added * per unrelevant paramers of actions in action expressions
Added possibility of evaluating subattributes of a local variable or event parameter.

UCTL :  allowed    obj.x >= obj2.y+1  (/=, <= >=,  ids+ids relop ids+ids)
---------------------
Version 3.2
Towards Hugo compatibility:
 Region composition via  / instead  od //
 "//"  as alternative comment line  starter  (like "--")
---------------------
Version 3.1
Fixed major bug in logic algorithm (like fmc V4.4).
Minor bug fixes in valued ccs.

Max_Depth_Size  initialized at 0, then passed to 1 anf then
doubled at any subsequent aborted attempt.

---------------------
Version 3.0

Model, Storage, and Logic transformed into top level non-generic
packages.  Intraduced cadp_interface package (and dummy-main)

---------------------
Version 2.7

Added +, - and type casting  operators for integer expressions.
Allowed integer expressions in input/output arguments of actions,
inside guards, and as parameters for term instantiations

---------------------
Version 2.6
 BUG and fixes nedded: 
 
-- possible type violation not yet detected
-- given   type T = {vvv}
--         (aa?_x:T; stop) // (aa!kk; stop)  =  tau(aa#kk); stop 
--  

Already DONE!

Added the possibility of actions with multiple paramerers.
  !a?_x!b

Input parameters MUST now be VARIABLES
  (a?b e' illegale)

Lotos-Like parameter matching also for CCS coposition
  !a!b.nil // ?a!b.nil  =   tau(a#b).nil
  !a?_x.nil // ?a!b.nil  =   tau(a#b).nil
  ?a?_x.nil // !a!b.nil  =   tau(a#b).nil

";" substitutes ":"  for Action Prefixing


Allowed type declations for CCS values
   type T = {true, false}
   type T = mod 10
    ?a_X:T; stop

allowed "[]" as a replacement for "+"

allowed option "->"  after a guard.
    ([_x = aa] => stop)
---------------------
Could be done in 2.5

- Full recompilation with Tasking and Exception restrictions.

- Full recompilation with all optimizations on  (-O3) 

- Heavy rewriting of actl_eval-model, optimizing the case of a single-agent
  and two-agent rules. No state-expansion, just packed_state modification.

Just Done in 2.5

- Cache lists are kept sorted (ascending w.r.t. state)

- Stack_Pop moves the stack Item from Stack to Cache, setting the value. 

- rewritten actl_storage.  now stack and cache are a vector of lists.
  Cache-Max is now automatically adjusted from All_bits.
  There is no need to set this argument on the command line.

---------------------
JUST DONE: in 2.4

fmc, totab,  Eliminato Current_State: StateVector dal type Iterator;
            (ogni call a Find_Next_State, chiama Expand sul current state.

fmc, totab   Aggiunta l'opzione -et  che espande i "tau" in "tau(sync)".
            Oltre a rendere visibile ed analizzabile le sincronizzazioni 
            interne, questa opzione riduce NOTEVOLMENTE la quantita' di 
            memoria necessaria per lo stack di programma, in quando i
            next successors sono molti di meno per ogni chiamata di
            Find_Next_States.   Forse questa opzione dovebbe diventare
            quella di default?????

fmc       Aggiunta la possibilita di scrivere EX{tau} invece di ET
          e analogamente AX{tau} invece di AT.   <tau> e' invece proibito

totab:    L'opzione "-v" (verbose) induce la sostuzione degli indici
          delle azioni con il nome espanso delle medesime.
          (Per adesso solo nelle transizioni degli automi) 
 
fmc,totab: Le action labels adesso possono contenere anche il carattere "-"
           (modifica fatta per compatibilita con amc e atg)  

tofc2    non mette nelle tabelle delle azioni "tau"  ma tau
          senza doppi apici, altrimenti AMC non lo riconosce come tau.

FMC       "t"  e' un alias di "trace".  
          Quando avviene una transizione "tau", viene indicata
          fra parentesi anche la transizione che la ha generata.

FMC       se "aa" e' une azione tabellata AMC accetta la
          formula  EX {"aa"} true, mentre fmc NO. (fmc deve levare gli apici)
 
FMC       Viene accettata anche la sintassi EX {tau} true  e EX {"tau"} true.
          Cio' risolve un problema di compatibilita' con HAL, e permette
          di scrivere espressioni del tipo EX {tau | aa} true;

TBD:      tofc2    azioni del tipo aa!  devono essere normalizzate in !aa

- ccs-parser  permessa la definizione della composizione parallela ccs
   del tipo     NET =  T1 // T2 // T3 // Tn

- ccs_parser  permessa la definizione/uso di termini parameterici
   (E.g. come  T(_x) =  _x: stop)

- ccs_parser  permessa l'introduzione di guards per termini parametrici
   (E.g.  a?_x: b?_y ( _x=a ->  EXP1
                      []  _x=_y -> EXP2
                      []  EXP3)

Version 2.3
-  Prima esperimento di CCS con priorita'.
   IDEA: Azioni  Maiuscone donotano azioni ad alta priorita'.
   * Azioni maiuscole e minuscole sono DIVERSE per quanto riguarda
     renaming/restrictions/parameter-passing e sincronizzazioni.
   * Quando AAA!  si sincronizza solo con AAA?  il risultato DEVE 
     essere TAU  non tau, altrimenti viene persa la priorita'.
   -- questa sembra essere la versione minimale.
--
- disallowing "undef" for CCS (and lotos synchronizations)
    (verificare anche che tau? e tau!  non si sincronizzino, specialmente
    in lotots-like (e nemmeno "i").
--
- ccs_totab:  I gates su cui viaggiano parametri non DEVONO essere
                  visibili a top level (cioe' devono necessariamente 
                  restricted in seguito a qualche //).
   (E.g.   (a:?_x:stop // ... // ... )  deve finire con \a  ) 
   (il succo e' che l'espansione non deve essere visibile)
   (operativamente non deve esistere top-level nessuna azione  xx?undef)

Version 2.2
- Fc2-to_tab genera tabella di azioni della dimensione giusta.
- azione "i" = "t" = "tau" (quando letta da fc2 o tab)
- separatore di azioni '.'  equivalente a ':' .  **FATTO**

Version 2.1
-  Fixed major bug affecting the evaluation of actl formulas with
   alternance greater than one.
-  Introduced the tool "trace" (and the command "trace" inside fmc).
-  Introduced the check for undefined formula identifiers.
-  Introduced the check for action names non present in the model.
-  Small improvement in the use interface. Better error messages

Version 2.0
-  Introduced prototypal version of  parametric channels for CCS.
-  Modified the normalized structure of labels.  !aa  becomes aa!
-   (this to allow  more uniform actions as zz?  aa?_var  ,  aa!cc )


Version 1.1  
- Heavy Optization of active rules selection  (fmc, fmc.model, totab)
- "toauto"  functionalities moved to as "totab -a" (totab)
- Deleted -sg Stack_Gap and added Stack_Max (-sm) (fmc fmc.storage)
- Changed calling profile (totab, tofc2).
   totab  -a file.tab   generates   file.a.tab
   totab  file.fc2      generates   file.tab
   totab  file.tab      generates   file.tab.tab
   totab  -th file.tab  generates   file.h.tab, file.t.tab
   tofc2  file.fc2      generates   file.fc2.fc2
   tofc2  file.tab      generates   file.fc2
   tofc2  -ht file.tab  generates   file.a.fc2 by reading file.h.tab,file.t.tab
