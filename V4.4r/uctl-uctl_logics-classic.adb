with Ada.Exceptions; 
with System; use System;
with Ada.Unchecked_Deallocation;
with Ada.text_IO; use Ada.Text_IO;
---
separate(UCTL.UCTL_Logics)
package body Classic is
use UCTL_Types;
use UCTL_Utilities;
use Ada.Text_IO;
--use Computations_DB;
use Ada.Exceptions; 

-------------------------------------------------------------------------
-------------------------------------------------------------------------

  ----------------------------------------------------------------------
  --
  -- function Eval (Formula: UCTL_Types.Formula_Ref;
  --                State: System_Configuration) return Computation_Status;
  --
  ----------------------------------------------------------------------
  -- Commento generale: Quando la formula e lo stato corrente vengono
  -- trovati sullo stack di valutazione, si restituisce un risultato
  -- true o false (e seconda del tipo di punto fisso) che costituisce
  -- sono un risultato parziale della valutazione. Per questo motivo
  -- il risultato non va assolutamente messo nel cache.
  ----------------------------------------------------------------------


  -------------------------------------------
  
  procedure Check_EX (  Form: Formula_Ref;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);


  procedure Check_AX (  Form: Formula_Ref; 
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural); 

  procedure Check_Angle (Form: Formula_Ref; 
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_WAngle (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_Square ( Form: Formula_Ref; 
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_WSquare ( Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_AUntil1 ( Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_EUntil1 ( Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);
  
  procedure Check_AUntil2 ( Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_EUntil2 ( Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_Rec  (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_EF (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_EG (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);

  procedure Check_AF (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural);
                     

  procedure Check_AG (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: Out Natural);

  -------------------
  function Evolution_Satisfies_Tau (It: Evolutions_Iterator) return Boolean;

  function Match (It: Evolutions_Iterator;
               Action_Formula: UCTL_Types.Action_Ref) return  AllBindings;

  procedure  Evolution_Satisfies_with_Param
    (It: Evolutions_Iterator; Action_Formula: Basic_Action_Ref; Result: in out Boolean);

  function Configuration_Satisfies
     (This_Conf: System_Configuration; Assertion: Basic_Predicate_Ref) return Boolean; 

--  function   Evolution_Satisfies
--    (It: Evolutions_Iterator; Action_Formula: UCTL_Types.Action_Ref) return  Boolean;

  function Evolution_Bindings (It: Evolutions_Iterator;
      Action_Formula: UCTL_Types.Action_Ref) return VarBindings;

  function Evolution_Bindings (It: Evolutions_Iterator;
      Action_Formula: Basic_Action_Ref) return VarBindings;
-------------------------------------------------------------------------



-------------------------------------------------------------------------
----------------------------   ABOUT  FIXPOINTS -------------------------
-------------------------------------------------------------------------
--   ABSTRACT SYNTAX:
--
--   Form.Free_Vars (Form_Table_Ref)
--   Form.Closed_Context (Form_Table_Ref)
--   Form.Env_Selector (Num_Table_Ref)
--   Form.Context_Size  (ONLY for Kind=Fmax/Fmin) ( Natural)
--      
--   E.g.   max Z: ((AX{a}Z) or <b>Z)
--
--   max Z: ..  free_vars=()  closed_context=() env_selctor=(), context_size=0
--    .. or..   free_vars=("max") closed_context=("max"), env_selector=(1),
--   AX ..     free_vars=("max") closed_context=("max"), env_selector=(1)
--   (AX) Z    free_vars=("max") closed_context=("max"), env_selector=(1)    --  same
--   <b> ..    free_vars=("max") closed_context=("max"), env_selector=(1) 
--   (<b>) Z   free_vars=("max") closed_context=("max"), env_selector=(1)    --  same
--
--   E.g .  min X: max Y: (<b> X) or <a> Y
--
--   min X:    free_vars=()  closed_context=() env_selctor=(), context_size=0
--   max Y:    free_vars=("min")  closed_context=("min X") env_selctor=(1), context_size=1
--    ...or..  free_vars=("min","max")  closed_context=("min","max") env_selctor=(1,2)
--    <b> X    free_vars=("min") closed_context=("min X") env_selctor=(1),
--    (<b>) X  free_vars=("min") closed_context=("min X") env_selctor=(1),
--    <a> Y    free_vars=("max")  closed_context=("max Z","min X") env_selctor=(2,1), 
--    (<a>) Y  free_vars=("max")  closed_context=("max Z","min X") env_selctor=(2,1),
--
--    MODEL:                      FORMULA:  max Z: ((AX{a}Z) or <b>Z)
--           1                    VALUES:
--         /  \a                      Z(3[c2]) = Tmp_True
--       b|    2 <-                   Z(2[c1]) = False = Z(2[c2])
--        |  c/ \a \a                 Z(1) = False
--        |  4   \ /                  Z(2[c3]) = False
--         \_____ 3                   Z(3[c1]) = False
--
--
--    MODEL:                      FORMULA:  max Z: ((AX{a}Z) or <b>Z)
--           1<-----.             VALUES:
--         /  \a    |                 Z(3[c2]) = Tmp_True
--       b|    2    |                 Z(2[c1]) = False = Z(2[c2])
--        |  c/ \a  |a                Z(1[]) = True    (in questo caso si puo propagare il True da 1 a 3??)
--        |  4   \ /                  Z(1[c3]) = Tmp_True 
--         \_____ 3                   Z(3[c1]) = Tmp_True
--
--    QUESTA SITUAZIONE CGHIARISCE CHE I TMP_XXX NON VANNO RICICLATI MA DEVONO ESSERE RICALCOLATI ????
--      o NOOO?????     Nota che Z(2[c3]) e' inizialmente TMP_TRUE, e rimane tale acnhe dopo che Z(2[c1])=False
--    se propagassi il False ...  potrei anche riciclare i TMP ...  ma non e' detto che la propagazione si
--     possa fare a causa di and /or  che rendono la valutazione effettuata solo parziale.
--     Per poter essere riciclate il loro valore ed il Rec_Depth dovrebbe essere corretto!!! lo e'!?!?!?
--     Nel caso di tail recursion (EF AF U <<>> ) probabilmente si, in generale NO.
--   *** CONCLUSIONE AL MOMENTO  le TMP_XXX  NON VANNO RICICLATE!!!          ***
--   ***  PER OTTIMIZZARE,  QUANDO POSSIBILE VANNO TRASFORMATE IN FOUND_XXX  ***
--   
--  Durante la valutazione di un punto fisso, se il Rec_Depth restituito e' > C_Depth il risultato puo'
--     passare da TMP_X a FOUND_X per le supercomputations?  Sembrerebbe di si! (ottimizzazione non obbligatoria A). 
--  Cio' non implica nessun consolidamento (in particolare da TMP_X a FOUND_Y)!
--  E Soprattutto NON implica la possibilit‡ di riciclare i TMP (che va poibita) e tantomeno i TMP_DEPTH.
--  QUINDI:  
--  Check_Cpmputation in caso di TMP_XX   resttituisce NOT_YET_STARTED!!!
--     oppure Set_Status(TMP_XX)  fa in realta Set_Status(Not_Yet_Staretd)-- cio' pero' impedisce l'ottimizzazione A)  
--
--  CHECK COMPUTATIONS:
--
--  [form="max Z:", storedcontext=(""), owner=1, state=1, progressive=1, status=notyetstarted, rec_dept=1)
--  [form="..or..", storedcontext=(". 1"), owner=1, state=1, progressive=2, status=notyetstarted, rec_dept=0)
--  [form="AX..", storedcontext=(". 1"), owner=1, state=1, progressive=3, status=notyetstarted, rec_dept=1)
--
--  [form="(ax)Z", storedcontext=(". 1"), owner=2, state=2, progressive=4, status=notyetstarted, rec_dept=2)
--  [form="max Z", storedcontext=(""), owner=1, state=2, progressive=5, status=notyetstarted, rec_dept=2)
--  [form="..or..", storedcontext=(". 5"), owner=2, state=2, progressive=6, status=notyetstarted, rec_dept=0)
--  [form="AX..", storedcontext=(". 5"), owner=2, state=2, progressive=7, status=notyetstarted, rec_dept=2)
--
--  [form="(ax)Z", storedcontext=(". 5"), owner=3, state=3, progressive=8, status=notyetstarted, rec_dept=3)
--  [form="max Z", storedcontext=(""), owner=1, state=3, progressive=9, status=notyetstarted, rec_dept=3)
--  [form="..or..", storedcontext=(". 9"), owner=3, state=3, progressive=10, status=notyetstarted, rec_dept=0)
--  [form="AX..", storedcontext=(". 9"), owner=3, state=3, progressive=11, status=notyetstarted, rec_dept=3)
--
--  [form="(ax)Z", storedcontext=(". 9"), owner=2, state=2, progressive=12, status=notyetstarted, rec_dept=0)
--  [form="max Z", storedcontext=(""), owner=1, state=2, progressive=5, status=INPROGRESS, rec_dept=4)
--  [form="<b>Z", storedcontext=(". 5"), owner=2, state=2, progressive=13, status=notyetstarted, rec_dept=2)
--
--  [form="<b>Z", storedcontext=(". 1"), owner=1, state=1, progressive=14, status=notyetstarted, rec_dept=1)
--
--  [form="(b)Z", storedcontext=(". 1"), owner=3, state=3, progressive=11, status=notyetstarted, rec_dept=4)
--  [form="max Z", storedcontext=(""), owner=1, state=3, progressive=9, status=TMP_TRUE, rec_dept=3)
--  [form="..or..", storedcontext=(". 9"), owner=3, state=3, progressive=10, status=TMP_TRUE, rec_dept=0)
--  [form="AX..", storedcontext=(". 9"), owner=3, state=3, progressive=11, status=TMP_TRUE, rec_dept=3)
--
--  [form="(ax)Z", storedcontext=(". 9"), owner=2, state=2, progressive=12, status=TMP_TRUE, rec_dept=4)
--  [form="max Z", storedcontext=(""), owner=1, state=2, progressive=5, status=FOUND_FALSE, rec_dept=1)
--  [form="..or..", storedcontext=(". 9"), owner=2, state=2, progressive=6, status=FOUND_FALSE, rec_dept=0)
--  [form="<b>Z", storedcontext=(". 9"), owner=2, state=2, progressive=13, status=notyetstarted, rec_dept=2)
--
--   EVALUATION
--  La rivalutazione una formula "max" restitruisce TMP_TRUE  se versione esistente nel DB ha stato INPROGRESS,
--   e tale valore pu√≤ essere propagato all'indietro.
--  La rivalutazione di una formula (max o flat) RICHIEDE la RIPETIZIONE dell'elaborazione se la versione precedente
--   nel DB ha stato TMP_TRUE
--  Quando owner=state (cioe' quando una max/min e' completamente valutata) il risultato da TMP_TRUE
--   viene trasformato in FOUND_TRUE (vero?)

--  Il CONTEXT  e' un array VUOTO  per tutte le formule che non annidate dentro punti fissi.
--  Per le formule annidate dentro punti fissi  e' il vettore delle computazioni corrispondenti 
--     alle formule max /min (la piu' interna)  dentro le quali e' annidata la formula 
--  Il context serve per dare una sematica precisa alle eventuali variabili di fixpoint presenti nella formula.
--  In particolare serve per distinguere due  variabili di punto fisso   Z    e Z che sono sintatticamente uguali
--    ma che denotatano max/min  valutati in stati diversi (computazioni diverse)
--  Se usassi lo stato (owner) invece della computazione,    non distinguarei diversi punti fissi con lo stesso nome
--    (magaii valutati in valuazioni diverse - visto che le computations sono conservate)
--    E.g    (max Z: (<b> Z)) and max Z: <c> Z   -- la computazione distingue, gli stati no.
--  Nelle COMPUTAZIONI salvate nel DB una formula e' salvata assieme al suo "trimmed context" che e' il sottoinsieme
--   del contesto necessrio per dare la sematica alle sue variabili libere (dirette e indirette), e al suo stato.
--   E.g.  max Y:   min X: ( (<a>X) or (<b>Y) or (<c>true) )  
--      trimmedcontex(<a>X)=[maxY,minX] , trimmedcontext(<b>Y)=[maxY], trimmedcontext(<c>)=[]
--
--  l'OWNER e' lo stato in cui inizia una valutazione ricorsiva di una formula
--  e coincide con  FROMSTATE se le formula non e' ricorsiva, (o all'inizio di una formaula ricorsiva).
--
--  lo HASHING di unc computatione viene calcolato partendo da FORMIMAGE, CONTEXTIMGE e STATEIMAGE
--   nel caso:   "max Z: ((<a>EF Z) or <b>EF Z)"   le due formule "EF Z", anche se sintatticamente uguali
--   sono trattate come diverse perche hanno diversi contesti.
--
--  Il Form.Context_Size e' presente solo per le formula max/min ed  denota il suo livello di
--    annidamento sintattico (considerando solo le formula max/min)
-------------------------------------------------------------------------

------------------------------------------------------------------
--  domanda:  Una sottoformula NON RICORSIVA come  "<b> Z"  
--   (sottoformula di    min Z:(<a> <b> Z)
--   PUO essere RItrovata come IN_PROGRESS nel DB durante la valutazione ???
--   (sopponedo che <..> si setti come INPROGRESS dall'inizio fino al risultato finale, (cosa che adesso non fa).
--  E.g.  consideriamo  
--     s1 -> s2 {-/a}
--     s1 -> s2 {-/a}
--     s2 -> s1 {-/b}  
--  La valutazione della sottoformula <b>Z viene iniziata in s2.  
--  L valutazione di <a> viene fatta una volta sola ... quella di <b> due volte, ma la seconda
--  avviene dopo che la prima e' terminata (TMP_FALSE).
--  SEMBREREBBE DI NOOOOOOOO!!!  (per fortuna!!)
------------------------------------------------------------------

-----------------------  ABOUT DEPTH ---------------------
--
-- N_Depth   incrementata ogni volta che durante la valutazione di avanza di
-- un passo nel LTS.
-- Usata per triggerare gli ABORT e per la valutazione delle asserzioni
--   DEPTH_GT_nnn    e DEPT_LT_nnn
--
-- C_Depth   incrementata ogni volta che si chiama un sottoprogramma di valutazione,
--  anche rimanendo nello stato stato. Cresce molto piu' velocemente. (INUTILE???)
--
-- Rec_Depth  incrementatata all'interno di chiamate ricorsive. Usata in antiche
--  implementazioni di valutazioni ricorsive (INUTILE). Forse ancora in uso
--   nel caso di fix-points.
------------------------------------------------------------------
-------------------------------------------------------------------------
-- "Check_Rec" viena chiamata quando viene incontrata "dall'esterno" una
-- definizione ricorsiva di max/min punto fisso.
-- In questo caso Owner=State e Context non contiene ancora la computazione stessa.
--  (e.g.  puo' essere anche vuoto se il punto fisso non e' nested in altri punti fissi)
--
-- "Check_Rec" viena chiamata anche quando viene valutato un identificatore
--  di punto fisso (da Check_Apply).
--  In questo caso l'owner e' la computation del punto fisso iniziale.
--  Nota che l'owner e' anche un elemento del Context che, come minumo, contiene la
--  definizione del punto fisso iniziale.
-------------------------------------------------------------------------
procedure Check_Rec  (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural) is
   --------------------------------------------------
   --  FORM  =   max ID: FORM1(ID)     
   --  FORM  =   min ID: FORM1(ID) 
   --------------------------------------------------
   --------------------------------------------------
   --  FORM = Form, FORM1 = Form.FDef)
   --
   --  Form  =  (Fmax, IDef, FDef) or (Fmin, IDef, FDef)
   --------------------------------------------------
   --  Called also during the evaluation of Apply
   --------------------------------------------------
   --------------------------------------------------------------
   --  max Y:  FORM(Y)      e' equivalente a:
   -- FORM(true) & FORM(FORM(true)) & FORM(FORM(FORM(true))) & ... & ...
   --------------------------------------------------------------
   --  min Y:  FORM(Y)      e' equivalente a:
   -- FORM(false) | FORM(FORM(false)) | FORM(FORM(FORM(false))) | ... | ...
   --------------------------------------------------------------
   Tmp_Result: Computation_Status;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
--   My_Depth: Natural := Get_Depth(FromState);
--
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
   --
   if My_Status = IN_PROGRESS then
     -- notice that the computation still remains In_Progress
     if Form.Kind = Fmax then
        Result :=  TMP_TRUE;          -- this is a MAXIMUM fixed point
     else
        Result :=  TMP_FALSE;         ---this is a  minimum fixed point
     end if;
     Rec_Depth := Tmp_Depth;
     --
   elsif My_Status = ABORTED or else
         My_Status = FOUND_TRUE  or else
         My_Status = FOUND_FALSE then
        -- start evaluation again is Result is not FOUND_TRUE / FOUND_FALSE
     Result :=   My_Status;
     --
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=   ABORTED;
      --
   else
     -- brand new (or unreliable) computation
     -- NOT_YET_STARTED/TMP_TRUE/TMP_FALSE
     Set_Status (Cref, IN_PROGRESS);
     --
     -- we re-evaluate the fulldef
     Check_True (Form.FDef, FromState, 
                 Context & (1..1 => Cref),SubCref, Tmp_Result, N_Depth, C_Depth+1,Tmp_Depth); 
     if Tmp_Depth < C_Depth then 
         Rec_Depth := Tmp_Depth; 
     else
        -- Tmp_Depth >= C_Depth
        if Tmp_Result = TMP_FALSE then
          Tmp_Result := FOUND_FALSE;
        elsif Tmp_Result = TMP_TRUE then
          Tmp_Result := FOUND_TRUE;
       end if;
     end if;
     --
     if Result /= ABORTED then
       Set_Subcomputation (Cref, SubCref, No_Evolution);
     end if;
     Set_Status(Cref,Tmp_Result);
     Result :=   Tmp_Result;
     --
   end if;
   --
   return;
   --
end Check_Rec;


------------------------------------------------------------------------
-- "Check_Apply" viene chiamata quando viene incontrato l'identificatore di un punto
-- fisso Z. Il parametro "Context" contiene la computation iniziale del punto fisso,
-- cioe' quella iniziata quando viene incontrata dall'esterno la formula max/min.
------------------------------------------------------------------------
procedure Check_Apply (Form: Formula_Ref; 
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural) is
   --------------------------------------------------
   --  FORM1  =   FINAL 
   --  FORM1  =   DEPTH_GT_nnn  moved into ASSERTIONS
   --  FORM1  =   DEPTH_LT_nnn  moved into ASSERTIONS
   --  FORM1  =   COUNT 
   --  FORM1  =   STEPS_GT_nnn ??????
   --  FORM1  =   PRINT
   --  FORM1  =   PRINT_ONCE  moved into ASSERTIONS
   --  FORM1  =   REC_ID 
   --------------------------------------------------
   --  FORM1 = Form
   --
   --  Form  =  (Fapply, IDen, FullDef, Optargs)
   --------------------------------------------------------------
   -- NOTA: Quando la full def (per questo stato) viene trovata sullo stack
   -- si restituisce True o False a secondo del tipo di punto fisso,
   -- ma il risultato NON deve essere salvato sul cache perche'
   -- questo e' solo un risultato parziale della valutazione
   -- della full-def, il cui risultato finale puo' anche essere
   -- diverso.
   --------------------------------------------------------------
   --
   --------------------------------------------------
   --  Form  =  (Fapply, IDen, FullDef, Optargs)
   --------------------------------------------------
   --
   --
   Id_String: String(1..Form.IDen.all'Length) := Form.IDen.all;
   --
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, FromState, FromState, Cref,My_Status,Tmp_Depth);
   if My_Status = FOUND_TRUE or else
      My_Status = FOUND_FALSE  or else
      My_Status = IN_PROGRESS or else -- (ACTYUALLY CANNOT OCCUR!)
      --  non essendo strutturalmente ricorsiva My_Status non puo essere trovato IN_PROGRESS ? 
      My_Status = ABORTED then
        Result :=   My_Status;
        return;
   elsif N_Depth > Max_LTS_Depth then
      -- TMP_TRUE  / TMP_FALSE /  NOT_YET_STARTED
      Set_Status (Cref, ABORTED);
      Result :=   ABORTED;
      return;
   else
      -- if My_Status is TMP_TRUE  / TMP_FALSE /  NOT_YET_STARTED 
      --  we can start a new evaluation
      Set_Status (Cref, IN_PROGRESS);  -- probabilmente INUTILE
   end if;
   --
   --  FINAL
   --
   if Id_String = "FINAL" then 
      declare
        My_Iter: Evolutions_Iterator;
      begin
        Iterator_Initialize (My_Iter,System_Configuration(FromState));
        if Has_System_Transition (My_Iter)  then
          Result := FOUND_FALSE;
       else 
          Result := FOUND_TRUE;
       end if;
       Iterator_Finalize (My_Iter);
     end;
     Set_Status (Cref, Result);
     return;
   end if;
   --
   --  COUNT
   --
   if Id_String = "COUNT" then
     Result := FOUND_FALSE;
     Set_Status (Cref, Result);
     Requested_Counts := Requested_Counts + 1;
--     Counting_Requested := True;
     return;
   end if;
   --
   --  PRINT
   --
   if Id_String = "PRINT" then
     Result := FOUND_FALSE;
     Set_Status (Cref, Result);
     Print_Message(FromState,Form.Optargs);
     return;
   end if;
   --
   --  ID: check for fixed point recursions
   --
   Check_Rec (Form.FullDef, 
              Get_Owner(Context(Form.FullDef.Context_Size+1)),FromState, 
              Context(1..Form.FullDef.Context_Size), SubCref, Result, N_Depth,C_Depth+1,Tmp_Depth);
   if Tmp_Depth < C_Depth then
       -- the only case in which we return rec_dept /= c_depth
       Rec_Depth := Tmp_Depth;
   end if;
   --
   if Result /= ABORTED then
     Set_Subcomputation (Cref, SubCref, No_Evolution);
   end if;
   Set_Status (Cref,Result);
   return;
   --
 end; -- end Check_Apply;


-------------------------------------------------------------------
--  Durante la valutazione ricorsiva di una formula EF P i valore Tmp_False(D) generati
-- NON possono essere riciclati, ma e' necessario far ripartire una nuova valutazione.
-- (A meno che  quando una formula passa de Tmp_False(D) a Tmp_false(DD) il nuovo valore di DD
-- non venga consolidato ai discendenti Tmp_False(D)).
-- Se cio' non viene fatto, il riciclo di Tmp_False(D) scorretto puo' portare a valutazioni errate.
-- E.g.
--  s1->s2, s2->s1, s2->s3, s3->s2, s1->s4, s4->s3, s1->OK
-- EF(s1,d1) = EF(s2,2)= EF(s3,3)= EF(s2,4)=TF(2)
--                       +EF(s1,3)=TF(1)
--             +EF(s4,2)=EF(s3,3) == se riciclo il TMP, TF(2)
--            *=FF !!! aarebbe scorretto
--  = TT
-- IL problema NON COMPARE CON QUESTA VALUTAZIONE, ma con la sua ripatizione in un contesto diverso
-- dove EF(s4) viene rivalutata prendendo per buona la FF mentre e' invece TT
--------------
-- QUINDI:  i TMP VALUES NON VANNOP MAI RICICLATI  A MENO CHE (?):
--  +) vengano correttamente consolidate le Depth dei TT TF
--  +) vengono sempre e completamente consolidati i FF e i FT
--------------
-- LA SOLUZIONE ATTUALE E' DI NON RICICLARE MAI I TMP.
-- Se i FF  o FT  non vengono consolidati e' solo un problema di efficenza, non di risultato.
--
--  FARE solo consolidameti parziali (a depth limitata)  o in modo BREADTH FIRST potrebbe essere un
-- modo per eliminare problemi di STACK OVERFLOW DURANTE i CONSOLIDAMENTI
-------------------------------------------------------------------
-- SOLUZIONE CORRETTA:  ELIMINARE I CONSOLIDAMENTI MODELLANDO LO STATO DI UNA COMPUTAZIONE COME
--  CONTENENTE UN VALORE CHE PUO' ESSERE UN VALORE BOOL OPPURE UN PUNTATORE AD N ALTRO ELEMENTO SIMILE
-------------------------------------------------------------------
-----------------------------------------------------------------------
-- Check_EF viene chiamata quando viene iniziata la valutazione "dall'esterno" 
-- di una formula  EF phi (in questo caso Owner=State), come PURE quando la valutazione
-- viene continuata in modo ricorsivo sui discendenti  dello stato da cui e' iniziata
-- Nel prima caso, quando termina, il risultato diventa DEFINITIVO (se e' una FORMULA CHIUSA)
-- Nel secondo caso, in presenza di cicli tra i sottostati, il risultato puo' non
-- essere stabile (tmp_true / tmp false) anche se la formula e' chiusa (puro uctl).
--  C_Depth (IN) e' la profondita a cui siamo arrivati con questa valutazione, 
--    partendo dalla formula iniziale, e contando solo i passi di valutazione
--  Rec_Depth (OUT) e' la profondita' dalla formula iniziale, rispetto alla quale dipende la stabilita'
--   di questo risultato. Se Rec_Depth >= C_Depth il risultato di questa valutazione ricorsiva NON dipende da
--   stati precedenti non ancora completamente analizzati, quindi TMP_FALSE puo' diventare subito FOUND_FALSE.
-----------------------------------------------------------------------
procedure Check_EF (Form: Formula_Ref; 
                   WithOwner: System_Configuration;
                   FromState: System_Configuration;
                   Context: Computations_Table;
                   Cref: Out Computation_Step;
                   Result: Out Computation_Status;
                   N_Depth: Natural;
                   C_Depth: Natural;
                   Rec_Depth: out Natural) is
     --------------------------------------------------
     --  FORM  =   EF  FORM1     
     --
     --  e' per definizione uguale a:
     --   FORM  = E [ true {true} U FORM1 ]
     --
     --  ed e' equivalente a:
     --   min Z = ( FORM1 or EX{true} Z) ) 
     ------------------------------------------------------------
     -- Se FORM1 e' True adesso, ritorna True.
     --
     -- Se FORM1 e' False adesso, valuta FORM per tutti i next states
     --   restituendo True appena un risultato True viene trovato.
     -- Altrimenti ritorna False.
     --
     --------------------------------------------------
     --  FORM = Form, FORM1 = Form.PRef.TForm
     --
     --  Form  =  (Fexist, Form.PRef)
     --  Pref  =  (Eventually, PRef.Tform)
     --------------------------------------------------
   Tmp_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
begin
   Rec_Depth := C_Depth;
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
   --
   --  check if now closing a fixpoint loop
   --
   ------------------------------------
   if My_Status = IN_PROGRESS then  
   ------------------------------------
     Result :=   FOUND_FALSE;         ---this is a  minimum fixed point
     return;
   ------------------------------------
   elsif My_Status = FOUND_TRUE or 
         My_Status= FOUND_FALSE then
   ------------------------------------
      Result :=  My_Status;
      return;
   ------------------------------------
   elsif (My_Status = TMP_TRUE or 
          My_Status= TMP_FALSE ) then 
   ------------------------------------
      -- Notice that TMP_XXX values can only be found if the formulas contains fixpoint variables.
      -- (i.e. is part of a more external fixpoint formula), and the nested fixpoint id still
      --  does not have a definitive value.
      -- THIS PART HAS TO BE BETTER ANALYZED
      --
      if (Get_Owner(CRef)=WithOwner or Get_Owner(CRef)=FromState) then
        Result :=  My_Status;
        if Tmp_Depth < Rec_Depth then
          Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
        end if;
        return;
      else
        -- My_Status= TMP_TRUE / TMP_FALSE leftover from previous completed computations
         Set_Status (Cref, IN_PROGRESS,C_Depth);
         -- Result will be set by this computation
      end if;
   ------------------------------------
   elsif (My_Status = ABORTED) then
   ------------------------------------
      -- Notice that if the Max_LTS_Depth has been encreased since the time the formula was
      -- evaluated and this result saved, it is the Check_Computation itself which return
      -- a new status NOT_YET_STARTED instead of an obsolete ABORTED.
      --
--      if N_Depth < Tmp_Depth/2 and then N_Depth > 20 then
--        -- continue , maybe we are luckier, since we have more depth available
--        Set_Status (Cref, IN_PROGRESS,C_Depth);
--      else
        Result :=  ABORTED;
        return;
--      end if;
   ------------------------------------
   elsif N_Depth > Max_LTS_Depth then
   ------------------------------------
     Set_Status (Cref, ABORTED,N_Depth);
     Result :=  ABORTED;
     return;
   ------------------------------------
   else --  NOT_YET_STARTED 
   ------------------------------------
     Set_Status (Cref, IN_PROGRESS);
      -- Result will be set by this computation
   ------------------------------------
   end if;
   ------------------------------------
   --
   --  LA LOGICA E':  PER DEFAULT LA FORMULA E' FALSE, SOLO NEL CASO CHE VENGA TRIVATA TRUE, 
   --   QUESTA COMPUTAZIONE E TUTTE LE SUE SUPERCOMPUTAZIONI DA FALSE VENGONO MESSE A TRUE !!!
   --
   --
   -- Se FORM1 e' True adesso, ritorna True.
   --
   Check_True (Form.PRef.Tform, FromState,Context,SubCref, Tmp_Result, N_Depth, C_Depth+1,Tmp_Depth); 
   if Tmp_Result = FOUND_TRUE then
      Set_Subcomputation(Cref,SubCref,No_Evolution);
      Set_Status (Cref, Tmp_Result);
      Result :=  Tmp_Result;                --- immediate RETURN
      return;
   elsif Tmp_Result = TMP_TRUE then    -- TOBEDELETED
      if Tmp_Depth < C_Depth then 
        Rec_Depth := Tmp_Depth;
      else
         -- QUESTO dovvrebbe essere inutile perche' Check_True restituisce Tmp_Depth=C_Depth 
         --  solo nel caso di max/min fixpoint fourmulas (in ogni caso non fa male!)
         Tmp_Result := FOUND_TRUE;
      end if;
      Set_Subcomputation(Cref,SubCref,No_Evolution);
      Set_Status (Cref, Tmp_Result);
      Result :=  Tmp_Result;                --- immediate RETURN
      return;
   end if;
   if Tmp_Result = FOUND_FALSE then 
     Add_Subcomputation(Cref,SubCref,No_Evolution);
   elsif Tmp_Result = TMP_FALSE  then
     Add_Subcomputation(Cref,SubCref,No_Evolution);
      if Tmp_Depth < C_Depth then
        Rec_Depth := Tmp_Depth;
      end if;
   elsif Tmp_Result = ABORTED then
       null; -- just continue
   elsif Tmp_Result = NOT_YET_STARTED or else 
         Tmp_Result = IN_PROGRESS then
     null;  -- SHOULD NOT HAPPEN
   end if;
   --
   -- Se e' TMP_FALSE / FOUND_FALSE  si prova a quardare nei discendenti.
   -- Anche se e' ABORTED si prova a cercare, magari si riesce a risolvere lo stesso.
   --
   -- Se FORM1 e' False adesso (oppure la sua valutazione e' stata abortita),
   -- valuta FORM per tutti i next states
   --  restituendo True appena un risultato True viene trovato.
   -- Altrimenti ritorna False (o Aborted).
   --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   --
   -- Tmp_Result e'  TMP_FALSE o FOUND_FALSE o ABORTED, Rec_Depth <= C_Depth
   -- Rec_Depth < C_Depth se FORM1 = Tmp_False (e con punti fissi)
   --
   while Has_System_Transition(My_Iter) loop
     declare
       Last_Result: Computation_Status;
       This_Next: System_Configuration := 
           Get_Target_Configuration (My_Iter);
     begin
       --
       Check_EF(Form, WithOwner, This_Next, Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
       --
       if Last_Result=FOUND_TRUE then     --  recursion definitely true
--         Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
--         -- we first add so that the definitive result can be consolidated
--         Set_Status(Cref, FOUND_TRUE);
         Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
         Set_Status(Cref, FOUND_TRUE);
         --  Truncated := False;   -- no more, in the case
         Iterator_Finalize (My_Iter);
--         Rec_Depth := C_Depth;      -- we can forget of fixpoints
         Result := FOUND_TRUE;
         return;
         --
       elsif Last_Result=TMP_TRUE then       -- Env_selector'Length >0   -- TOBEDELETED
         Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
         -- we first add so that the definitive result can be consolidated
         Set_Status(Cref, TMP_TRUE);
         Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
         --  Truncated := False;   -- no more, in the case
         Iterator_Finalize (My_Iter);
         --  in questo caso Tmp_Depth  >= Rec_Depth
         --   perche'  Depth(SUBFORM)  <=  Depth (EF SUBFORM)
         --  quindi il test sotto e' ridondante, ma in generale e' difficile da capire ...
         if Tmp_Depth < Rec_Depth then 
            Rec_Depth := Tmp_Depth; 
         end if;
         Result := TMP_TRUE;
         return;
         --
       elsif Last_Result = ABORTED then
         Tmp_Result := ABORTED;
         -- devo continuare a veder le possibili altre transizioni ...
         --
       elsif Last_Result=TMP_FALSE then  -- TODELETED
         if Tmp_Depth < Rec_Depth then
            Rec_Depth := Tmp_Depth;
         end if;
         if Tmp_Result=FOUND_FALSE then
            Tmp_Result := TMP_FALSE;   
         end if;
         --  if Tmp_Result is ABORTED, it remains unchanged.
         --  Tmp_Result cannot be  XX_TRUE because would have already returned.
         --
         Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
         -- devo continuare a veder le possibili altre transizioni ...
         --
       else   
         -- Last_result=FOUND_FALSE
         Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
         -- devo continuare a veder le possibili altre transizioni ...
         --
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;  -- for each rule
   --
   Iterator_Finalize (My_Iter);
   --
   ---------------------------------
   --  Il completamento della valutazione di una EF non causa mai la trasformazione del valore della
   --  formula da TMP_XXX a FOUND_XXX.  Se il valore e' TMP_XXX vuol dire che la formula contiene
   --  delle sottoformule con variabili di punti fissi la cui valutazione e' ancora in progress.
   --  (quandi Form.Env_Selector.all'Length >0 and Rec_Depth < C_Depth)
   ---------------------------------
   if Tmp_Result = Aborted then
      Set_Status(Cref, Tmp_Result,N_Depth);  
   else   
      Set_Status(Cref, Tmp_Result,Rec_Depth);
   end if;
   Result :=  Tmp_Result;
   return;
   --
end Check_EF;

-----------------------------------------------------------------------
-- Check_EG viene chiamata quando viene iniziata la valutazione "dall'esterno"
-- di una formula  EG phi (in questo caso Owner=State), come PURE QUANDO la valutazione
-- viene continuata in modo ricorsivo sui discendenti  dello stato da cui e' iniziata
-- Nel prima caso, quando termina, il risultato diventa DEFINITIVO (se e' una FORMULA CHIUSA)
-- Nel secondo caso, in presenza di cicli tra i sottostati, il risultato puo' non
-- essere stabile (tmp_true / tmp false) anche se la formula e' chiusa (puro uctl).
-----------------------------------------------------------------------
procedure Check_EG (Form: Formula_Ref; 
                   WithOwner: System_Configuration;
                   FromState: System_Configuration;
                   Context: Computations_Table;
                   Cref: Out Computation_Step;
                   Result: Out Computation_Status;
                   N_Depth: Natural;
                   C_Depth: Natural;
                   Rec_Depth: out Natural) is
     --------------------------------------------------
     --  FORM  =   EG  FORM1     
     --
     --  e' per definizione uguale a:
     --   FORM =  ~ (AF ~FORM1)
     --
     --  ed e' equivalente a:
     --   max Y = ( FORM1 & 
     --       ( FINAL | (ET Y) | (EX{true} Y) ) )
     ------------------------------------------------------------
     --
     -- Se FORM1 e' False adesso ritorna False.
     --
     -- Se FORM1 e' True adesso, e non ci sono next states, 
     --   o per almeno uno di quelli esistenti FORM1 e' True, 
     --   ritorna True. Altrimenti ritorna False.
     --
     --------------------------------------------------
     --  FORM = Form, FORM1 = Form.PRef.TForm
     --
     --  Form  =  (Fexist, Form.PRef)
     --  Pref  =  (Always, PRef.Tform)
     --------------------------------------------------
   Tmp_Result: Computation_Status;
   Tmp_Result2: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;  
   Tmp_Depth2: Natural;  
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
   --  check for fixed point recursions
   --
   if My_Status = IN_PROGRESS then
       --
       -- If a loopback is found the formula EG Form  becomes now DEFINITIVELY TRUE
       --  hence the recursive result is stable, and there is no need to manage Rec_Depth.
       --
       Result :=  FOUND_TRUE;         ---this is a  maximum fixed point
       return;
   elsif (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
     Result :=  My_Status;
     return;
   elsif N_Depth > Max_LTS_Depth then
     Set_Status (Cref, ABORTED);
     Result :=  ABORTED;
     return;
   else
      -- My_Status= TMP_TRUE / TMP_FALSE / NOT_YET_STARTED
      Set_Status (Cref, IN_PROGRESS);
   end if;
   --
   -- Se FORM1 e' False adesso ritorna False.
   --
   Check_True (Form.PRef.Tform, FromState,Context, SubCref, Tmp_Result, N_Depth, C_Depth+1,Tmp_Depth); 
   if Tmp_Result = FOUND_FALSE then
      Set_Subcomputation(Cref,SubCref, No_Evolution);
      Set_Status (Cref, Tmp_Result);
      Result :=  Tmp_Result;
      return;
   elsif Tmp_Result = TMP_FALSE then
      if Tmp_Depth < C_Depth then
        Rec_Depth := Tmp_Depth;
      else
         -- QUESTO dovvrebbe essere inutile perche' Check_True restituisce Tmp_Depth=C_Depth
         --  solo nel caso di max/min fixpoint fourmulas (in ogni caso non fa male!)
         Tmp_Result := FOUND_TRUE;
      end if;
      Set_Subcomputation(Cref,SubCref, No_Evolution);
      Set_Status (Cref, Tmp_Result);
      Result :=  Tmp_Result;
      return;
   elsif Tmp_Result = FOUND_TRUE then
      Add_Subcomputation(Cref,SubCref, No_Evolution);
  elsif Tmp_Result = TMP_TRUE then
      Add_Subcomputation(Cref,SubCref, No_Evolution);
      if Tmp_Depth < C_Depth then
        Rec_Depth := Tmp_Depth;
      end if; 
   elsif Tmp_Result = ABORTED then
      null;  -- still continue, just in case we could later return a definitive False!!
   elsif Tmp_Result = NOT_YET_STARTED or else
         Tmp_Result = IN_PROGRESS then
      null;  -- SHOULD NOT HAPPEN after a call to Check_True
   end if;
   --
   -- Se FORM1 e' True adesso, e non ci sono next states, 
   --   o per almeno uno di quelli esistenti FORM e' True, 
   --   ritorna True. Altrimenti ritorna False.
   --
   -- Se FORM1 e' ABORTED, si continusa nel caso fosse comunque possibile
   --  restituire un risultato FALSE
   -- 
   -- Se FORM1= ABORTED si puo' restituire solo FALSE
   --  se tutte le sub sono false 
   --
   -- Se FORM1= TRUE si puo' restituire TRUE 
   --  se tutte le sub sono TRUE 
   --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   --
   if not Has_System_Transition (My_Iter) then
     Iterator_Finalize(My_Iter);
     Set_Status (Cref, Tmp_Result);
     Result :=  Tmp_Result;
     -- Rec_Depth = C_Depth
     return;
   end if;
   --
   -- Tmp_Result e'  TMP_TRUE o FOUND_TRUE o ABORTED
   -- Se Tmp_Result =true allora tutti i risultati sono ancora possibili
   -- Se Tmp_Result = Aborted allora alla fina potro restituire solo false o aborted
   --
   Tmp_Result2 := FOUND_FALSE;  -- rimane tale anche se NON ci sono transizioni
   Tmp_Depth2 := Rec_Depth;
   --
   while Has_System_Transition (My_Iter) loop    
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       Last_Result: Computation_Status;
       Last_Depth: Natural;
     begin
       --
       Check_EG(Form, This_Next, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Last_Depth);
       --
       if  Last_Result = FOUND_TRUE  then
         if (Tmp_Result = TMP_TRUE) or (Tmp_Result = FOUND_TRUE) then
           Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
           Set_Status (Cref, Tmp_Result);
           Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Iterator_Finalize(My_Iter);
           Result :=  Tmp_Result;   -- found_true or Tmp_true
           return;
         else
           --  Tmp_Result = ABORTED
           -- we have lost the possibility of returning False instead of aborted
           exit;
         end if;
         --
       elsif  (Last_Result = TMP_TRUE ) then
         if (Tmp_Result = TMP_TRUE) or (Tmp_Result = FOUND_TRUE) then
           if  Tmp_Result = FOUND_TRUE then
             Tmp_Result := Last_Result;   -- at most  FOUND_TRUE -> TMP_TRUE
           end if;
           Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
           Set_Status (Cref, Tmp_Result);
           Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Iterator_Finalize(My_Iter);
           Result :=  Tmp_Result;   -- found_true or Tmp_true
           --  take this path as good, consider its depth
           if Last_Depth < Rec_Depth then
              Rec_Depth := Last_Depth;
           end if;
           return;
         else
           --  Tmp_Result = ABORTED
           -- we have lost the possibility of returning False instead of aborted
           --  Rec_Depth := C_Depth;    -- done at the end
           exit;
         end if;
         --
       elsif Last_Result = ABORTED and
             (Tmp_Result = TMP_TRUE or Tmp_Result = FOUND_TRUE or Tmp_Result= ABORTED) then
          --
          -- In the case FORM1=True we must record the failure 
          --  so that we can return ABORTED if no true path exist
          -- We continue looking for possibly returning TRUE
          --
          Tmp_Result2 := ABORTED;
          --
       elsif (Last_Result = FOUND_FALSE or Last_Result = TMP_FALSE)  then
         --
         if Tmp_Result2= FOUND_FALSE then
           -- se era aborted rimane tale
           -- se era foundfalse divente tmpfalse se necessario
           Tmp_Result2 := Last_Result;
         end if;
          --
         if Last_Depth < Tmp_Depth2 then
              Tmp_Depth2 := Last_Depth;
              -- we do not overwrite Rec_Depth because we may still neeed it if we find a true path
         end if;
         --
         Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
         -- continue the analisys of evolutions
         --
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;  -- for each rule
   --
   Iterator_Finalize(My_Iter);
   --
   if Tmp_Result2 = ABORTED then
      Tmp_Result := ABORTED;
      Rec_Depth := C_Depth;
   elsif Tmp_Result2 = TMP_FALSE or else Tmp_Result2=FOUND_FALSE then
      -- Tmp_result: (ABORTED/TMP_TRUE/FOUND_TRUE) -> XXX_FALSE
      Tmp_Result := Tmp_Result2;
      Rec_Depth := Tmp_Depth2;
   else
     -- (Tmp_Result2=TMP_TRUE or FOUND_TRUE) then
     if Tmp_result=ABORTED  then
       Rec_Depth := C_Depth;
     else 
       --  Tmp_Result =TMP_TRUE / FOUND_TRUE
       if Tmp_Depth2 < Rec_Depth then
          Rec_Depth := Tmp_Depth2;
       end if;
--       if Rec_Depth >= C_Depth then
--          Tmp_Result := FOUND_TRUE;
--          Rec_Depth := C_Depth;
--       end if;         
     end if;
   end if;
   Set_Status (Cref, Tmp_Result);
   Result :=  Tmp_Result;
   return;
   --
end Check_EG;

-----------------------------------------------------------------------
-- Check_Angle viene chiamata quando viene iniziata la valutazione "
-- di una formula  <act> phi (in questo caso vale sempre che Owner=State).
-- Nota che il diamond ha semantica STRONG in uctl (diversamente da actl).
-----------------------------------------------------------------------
procedure Check_Angle (Form: Formula_Ref;
                   FromState: System_Configuration;
                   Context: Computations_Table;
                   Cref: Out Computation_Step;
                   Result: Out Computation_Status;
                   N_Depth: Natural;
                   C_Depth: Natural;
                   Rec_Depth: out Natural) is
   --------------------------------------------------
   --  FORM  =   EX {ACTION}  FORM1
   --  FORM  =   ET {ACTION}  FORM1
   --
   -- (nel caso ET  Action = null)
   --------------------------------------------------
   -- Se esiste un next state raggiungibile con una azione
   --  visibile che soddisfa ACTION in cui FORM2 vale True
   --  allora restituisce True.
   -- Altrimenti restituisce False.
   --
   -- (in particolare vale False se lo stato e' finale, o non
   --   puo' fare azioni che soddisfino ACTION)
   --------------------------------------------------
   --  FORM1 = Form
   --------------------------------------------------
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, FromState, FromState,Cref,My_Status,Tmp_Depth);
--   if (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
     if  (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
      Result :=  My_Status;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   end if;
   --
   -- MY_Status=INPROGRESS (cannot happen!!!)
   -- My_Status=NOT_YET_STARTED, TMP_XXX (can only occour with Env>0)
   --
   -- Se esiste un next state raggiungibile con una azione
   --  visibile che soddisfa ACTION in cui FORM2 vale True
   --  allora restituisce True.
   -- Altrimenti restituisce False.
   --
   Tmp_Result := FOUND_FALSE;
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings: AllBindings := Match(My_Iter,Form.ARef);
       NewFormula: Formula_Ref;
     begin
       -- if Evolution_Satisfies (My_Iter, Form.ARef) then
       if TheBindings.MatchOK then
          --
         if TheBindings.VarsCount = 0 then
            -- the subformula is NOT parametric
            Check_True(Form.FormRef, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
            if Last_Result = FOUND_TRUE then
              Tmp_Result := FOUND_TRUE;
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status(Cref, Tmp_Result);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Iterator_Finalize (My_Iter);
              Result :=  FOUND_TRUE;
              Rec_Depth := C_Depth;
              return;   -- exit has_system_transition loop
              --
            elsif Last_Result = TMP_TRUE then
              Tmp_Result := TMP_TRUE;
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status(Cref, Tmp_Result);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Iterator_Finalize (My_Iter);
              Rec_Depth := C_Depth;
              if Tmp_Depth < C_Depth then
                 Rec_Depth := Tmp_Depth;
              end if;
              Result :=  TMP_TRUE;
              return;   -- exit has_system_transition loop
              --
            elsif Last_Result = FOUND_FALSE then
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              -- nota che se Tmp_Result=ABORTED/FOUND_FALSE/TMP_FALSE, rimane tale!
              --
            elsif Last_Result = TMP_FALSE then
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              if Tmp_Result = FOUND_FALSE then
                 Tmp_Result := TMP_FALSE;
                 if Tmp_Depth < Rec_Depth then
                   Rec_Depth := Tmp_Depth;
                 end if;
              end if;
              --
            elsif Last_Result=ABORTED then
              Tmp_Result := Last_Result;
              Rec_Depth := C_Depth;
              --
            end if;
         --
         else
           --the subformula IS PARAMETRIC
           -- we should check if at least one BINDING satisfies he subformula
           for I in 1..TheBindings.BindCount loop
             NewFormula := NewInstance(Form.FormRef,
                                         (TheBindings.VarsCount,
                                          TheBindings.VarNames,
                                          TheBindings.AllValues(I).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
             if Last_Result = FOUND_TRUE  then
               Tmp_Result := Last_Result;
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Tmp_Result);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize (My_Iter);
               Result :=  Tmp_Result;
               Rec_Depth := C_Depth;
               return;   -- exit has_system_transition loop
             elsif Last_Result = TMP_TRUE then
               Tmp_Result := Last_Result;
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Tmp_Result);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize (My_Iter);
               Result :=  Tmp_Result;
               Rec_Depth := C_Depth;
               if Tmp_Depth < Rec_Depth then
                 Rec_Depth := Tmp_Depth;
               end if;
               return;   -- exit has_system_transition loop
               --
             elsif Last_Result = TMP_FALSE then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               if Tmp_Result = FOUND_FALSE then
                 Tmp_Result := TMP_FALSE;
               end if;
               if Tmp_Depth < Rec_Depth then
                 Rec_Depth := Tmp_Depth;
               end if;
               -- nota che se Tmp_Result=ABORTED,TMP_FALSE rimane tale!
               --
             elsif Tmp_Result = FOUND_FALSE then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               -- nota che se Tmp_Result=ABORTED,TMP_FALSE/FOUND_FALSE rimane tale!
               --
             elsif Last_Result=ABORTED then
               Tmp_Result := Last_Result;
               Rec_Depth := C_Depth;
             end if;
           end loop;
         end if;    --parametric subformula?
       end if;  -- MatchOK
     end;  -- This_Next /  TheBindings context
     Iterator_Advance (My_Iter);
   end loop;  -- has_system_transition loop
   --
   Iterator_Finalize (My_Iter);
   --
   Set_Status(Cref, Tmp_Result);
   Result :=  Tmp_Result;
   return;
end Check_Angle;


-----------------------------------------------------------------------
-- Check_WAngle viene chiamata quando viene iniziata la valutazione "dall'esterno"
-- di una formula  <<act>> phi (in questo caso Owner=State), come pure quando la valutazione
-- viene continuata in modo ricorsivo sui discendenti  dello stato da cui e' iniziata
-- Nel prima caso, quando termina, il risultato diventa DEFINITIVO (se e' una FORMULA CHIUSA)
-- Nel secondo caso, in presenza di cicli tra i sottostati, il risultato puo' non
-- essere stabile (tmp_true / tmp false) anche se la formula e' chiusa (puro uctl).
-----------------------------------------------------------------------

procedure Check_WAngle (Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural) is
     ------------------------------------------------------------
     --  FORM  =   <ACTION>  FORM1   
     --
     --  e' per definizione uguale a::  
     --   FORM  = E [true {false} U  {ACTION} FORM1 ]
     --
     --  ed e' equivalente a:
     --   min Z = ((EX {ACTION} FORM1) | (ET Z) )
     ------------------------------------------------------------
     --
     -- Se esiste un next state raggiungibile con una azione
     --  visibile che soddisfa ACTION in cui FORM1 vale True
     --  allora ritorna True. 
     --
     -- Se esiste un next state raggiungibile con un tau in cui
     --   FORM1 vale True  allora ritorna True.
     -- Altrimenti ritorna False.
     --
     ------------------------------------------------------------
     --  FORM1 = Form, FORM2 = Form.FormRef)
     --
     --  Form  =  (Fangle2, ARef, FormRef)
     ------------------------------------------------------------
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   --
   My_Iter: Evolutions_Iterator; 
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
   --  check for fixed point recursions
   --
   if My_Status = IN_PROGRESS then
     Result :=  FOUND_FALSE;         ---this is a  minimum fixed point
     Rec_Depth := Tmp_Depth;      -- THE VALUE STORED WITH THE BEGINING OF THE COMPUTATION
     return;
   elsif My_Status /= NOT_YET_STARTED or
         My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE then
      Result :=  My_Status;
      if Result= TMP_FALSE and then 
          Tmp_Depth < Rec_Depth and then Form.Env_Selector'Length =0 then
        Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
      end if;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   else
      Set_Status (Cref, IN_PROGRESS);
   end if;
   --
   -- Se esiste un next state raggiungibile con una azione
   --  visibile che soddisfa ACTION in cui FORM2 vale True
   --  allora ritorna True. 
   --
   Tmp_Result := FOUND_FALSE;
   Iterator_Initialize (My_Iter,System_Configuration(FromState));
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings: AllBindings := Match(My_Iter,Form.ARef);
       NewFormula: Formula_Ref;
     begin
       --
       if TheBindings.MatchOK then
         if TheBindings.VarsCount = 0 then
            Check_True(Form.FormRef, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
            if Last_Result = FOUND_TRUE or Last_Result = TMP_TRUE then
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status(Cref, Last_Result);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Iterator_Finalize(My_Iter);
              Result :=  Last_Result;
              return;
            elsif Last_Result = TMP_FALSE or Last_Result = FOUND_FALSE then
              if Tmp_Result = FOUND_FALSE then
                Tmp_Result := Last_Result;
              end if;
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
            elsif Last_Result = ABORTED then
               Tmp_Result := Last_Result;
            end if;
         else
           -- AT LEAST ONE of the binding must HOLD
           for B in 1..TheBindings.BindCount loop
             NewFormula := NewInstance(Form.FormRef,(TheBindings.VarsCount,
                                                     TheBindings.VarNames,
                                                     TheBindings.AllValues(B).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
             if Last_Result = FOUND_TRUE or Last_Result = TMP_TRUE then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Last_Result);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize(My_Iter);
               Result :=  Last_Result;
               return;
             elsif Last_Result = TMP_FALSE or Last_Result = FOUND_FALSE then
               if Tmp_Result = FOUND_FALSE then
                 Tmp_Result := Last_Result;
                 -- nota che se Tmp_Result=ABORTED, rimane tale!
               end if;
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             elsif Last_Result = ABORTED then
                Tmp_Result := Last_Result;
             end if;
           end loop;
         end if;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop;
   --
   -- Se esiste un next state raggiungibile con un tau in cui
   --   FORM1 vale True  allora ritorna True.
   -- Altrimenti ritorna False.
   --
   -- Tmp_Result e' FOUND_FALSE /TMP_FALSE o ABORTED 
   --
   Iterator_Restart (My_Iter);
   while Has_System_Transition (My_Iter) loop   
     -- check this rule and the postcondition;  exit when true
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
       if Evolution_Satisfies_Tau (My_Iter) then
         --
         Check_WAngle(Form, WithOwner, This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
         --
         if Tmp_Depth < Rec_Depth then   -- Last_Result = TMP_FALSE
           Rec_Depth := Tmp_Depth;
         end if;
         --
         if Last_Result = FOUND_TRUE or      -- Form definitely true
              Last_Result = TMP_TRUE then    -- Form true and Env_selector >0
           --
           Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
           Tmp_Result := Last_Result;
           Iterator_Finalize(My_Iter);
           Set_Status(Cref,Tmp_Result,C_Depth);
           Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
           Result :=  Tmp_Result;
           Rec_Depth := C_Depth;
           return;
            --
         elsif Last_Result = TMP_FALSE then
           if Tmp_Result=FOUND_FALSE then
             Tmp_Result := Last_Result;
           end if;
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           --
         elsif Last_Result = ABORTED then
           Tmp_Result := Last_Result;
           Rec_Depth := C_Depth;
         --
         else   -- Last_Result = FOUND_FALSE
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
         end if;
         --
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;  -- for each rule
   --
   Iterator_Finalize(My_Iter);
   if WithOwner = FromState and then
        Form.Env_Selector.all'Length =0 and then
        Tmp_Result = TMP_FALSE then
        Tmp_Result := FOUND_FALSE;
--   elsif C_Depth <= Rec_Depth and then  
--          Form.Env_Selector.all'Length =0 and then
--          Tmp_Result = TMP_FALSE then
--      Tmp_Result := FOUND_FALSE;
   end if;
   Set_Status(Cref,Tmp_Result,Rec_Depth);
   Result :=  Tmp_Result;
   return;
   --
end Check_WAngle;

procedure Check_EX (Form: Formula_Ref; 
                   FromState: System_Configuration;
                   Context: Computations_Table;
                   Cref: Out Computation_Step;
                   Result: Out Computation_Status;
                   N_Depth: Natural;
                   C_Depth: Natural;
                   Rec_Depth: out Natural) is
   --------------------------------------------------
   --  FORM  =   EX {ACTION}  FORM1
   --  FORM  =   ET {ACTION}  FORM1
   --  
   -- (nel caso ET  Action = null)
   --------------------------------------------------
   -- Se esiste un next state raggiungibile con una azione
   --  visibile che soddisfa ACTION in cui FORM2 vale True
   --  allora restituisce True.
   -- Altrimenti restituisce False.
   -- 
   -- (in particolare vale False se lo stato e' finale, o non
   --   puo' fare azioni che soddisfino ACTION) 
   --------------------------------------------------
   --  FORM1 = Form
   --------------------------------------------------
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, FromState, FromState,Cref,My_Status,Tmp_Depth);
   if (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
      Result :=  My_Status;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   end if;
   --
   -- MY_Status=INPROGRESS (cannot happen!!!)
   -- My_Status=NOT_YET_STARTED, TMP_XXX (can only occour with Env>0)
   --
   -- Se esiste un next state raggiungibile con una azione
   --  visibile che soddisfa ACTION in cui FORM2 vale True
   --  allora restituisce True.
   -- Altrimenti restituisce False.
   -- 
   Tmp_Result := FOUND_FALSE;
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings : AllBindings := Match(My_Iter, Form.PRef.AARef);
       NewFormula: Formula_Ref;
     begin
       if TheBindings.MatchOK then
         if TheBindings.VarsCount = 0 then
           Check_True(Form.PRef.AFormRef,This_Next,Context,SubCref,Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
           if Last_Result = FOUND_TRUE then
             Tmp_Result := Last_Result;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Tmp_Result);
             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Iterator_Finalize (My_Iter);
             Rec_Depth := C_Depth;
             Result :=  Tmp_Result;
             return;
             --
           elsif Last_Result = TMP_TRUE then
             Tmp_Result := Last_Result;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Tmp_Result);
             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Iterator_Finalize (My_Iter);
              Rec_Depth := C_Depth;
              if Tmp_Depth < C_Depth then
                 Rec_Depth := Tmp_Depth;
              end if;
             Result :=  Tmp_Result;
             return;
             --
           elsif Last_Result = FOUND_FALSE then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             -- nota che se Tmp_Result=ABORTED/FOUND_FALSE/TMP_FALSE, rimane tale!
             --
           elsif  Last_Result = TMP_FALSE then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             if Tmp_Result = FOUND_FALSE then
                 Tmp_Result := TMP_FALSE;
                 if Tmp_Depth < Rec_Depth then
                   Rec_Depth := Tmp_Depth;
                 end if;
             end if;
             --
           elsif Last_Result=ABORTED then
             Tmp_Result := Last_Result;
             Rec_Depth := C_Depth;
             --
           else
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           end if;
         --
         else
           --the subformula IS PARAMETRIC
           -- we should check if at least one BINDING satisfies he subformula
           for B in 1.. TheBindings.BindCount loop
             NewFormula := 
                 NewInstance(Form.PRef.AFormRef,(TheBindings.VarsCount,
                                                 TheBindings.VarNames,
                                                 TheBindings.allValues(B).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
             if Last_Result = FOUND_TRUE then
               Tmp_Result := Last_Result;
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Tmp_Result);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize (My_Iter);
               Result :=  Tmp_Result;
               Rec_Depth := C_Depth;
               return;
               --
             elsif Last_Result = TMP_TRUE then
               Tmp_Result := Last_Result;
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Tmp_Result);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize (My_Iter);
               Result :=  Tmp_Result;
               Rec_Depth := C_Depth;
               if Tmp_Depth < Rec_Depth then
                 Rec_Depth := Tmp_Depth;
               end if;
               return;
               --
             elsif Last_Result = FOUND_FALSE then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               -- nota che se Tmp_Result=ABORTED,TMP_FALSE/FOUND_FALSE rimane tale!
               --
             elsif Last_Result = TMP_FALSE then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               if Tmp_Result = FOUND_FALSE then
                 Tmp_Result := TMP_FALSE;
               end if;
               if Tmp_Depth < Rec_Depth then
                 Rec_Depth := Tmp_Depth;
               end if;
               -- nota che se Tmp_Result=ABORTED,TMP_FALSE rimane tale!
               --
             elsif Last_Result=ABORTED then
               Tmp_Result := Last_Result;
               Rec_depth := C_Depth;
             end if;
           end loop;            
         end if;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;
   Iterator_Finalize (My_Iter);
   --
   Set_Status(Cref, Tmp_Result);
   Result :=  Tmp_Result;
   return;
end Check_EX;



---------------------------------------------
--Class Classname is
--Behavior
----  s1 -> s2 {-/b} 
--  s2 -> s3
--  s3 -> s2    -- Tmp_true
--  s2 -> s5 {-/a}  -- FALSE
--  
--  s1 -> s20 {-/b}
--  s20 -> s3  -- dovrebbe essere FALSE
--             -- se ricicla Tmp_True e' sbagliato
--  
--end Classname;
--
--// object instatiations
--Obj1: Classname 
-- <b> AG [a] false
---------------------------------------------
procedure Check_AG (Form: Formula_Ref; 
                   WithOwner: System_Configuration;
                   FromState: System_Configuration;
                   Context: Computations_Table;
                   Cref: Out Computation_Step;
                   Result: Out Computation_Status;
                   N_Depth: Natural;
                   C_Depth: Natural;
                   Rec_Depth: Out Natural) is
   --------------------------------------------------
   --  FORM  =   AG  FORM1
   --
   --  e' per definizione uguale a:
   --   FORM =  ~ (EF ~FORM1)
   --
   --  ed e' equivalente a:
   --    max Y = (FORM1 & (~ET ~Y)  & (~EX{true} ~Y) )
   --
   --  !!! max FORM1 = (FORM1 & (~(EX{true|tau} ~Y) ) !!!!
   --------------------------------------------------
   --
   -- Se FORM1 e' False adesso, ritorna False.
   --
   -- Se FORM1 e' True adesso, e esiste almeno un next state
   --  in cui  FORM1 e' False, ritorna False.
   --
   -- Altrimenti ritorna True.
   -- 
   -- (Nota: se lo stato e' finale il risultato e' True 
   --  se FORM1 vale adesso, altrimenti il risultato e' False)
   --------------------------------------------------
   --  FORM = Form, FORM1 = Form.PRef.TForm
   --
   --  Form  =  (Fexist, Form.PRef)
   --  Pref  =  (Eventually, PRef.Tform)
   --------------------------------------------------
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
   --  check for fixed point recursions
   ----------------------------------
   if My_Status = IN_PROGRESS then
   ----------------------------------
      --
      -- in caso di ricorsione, restituisco il C_Depth del punto di partenza.
      --
      Result :=  FOUND_TRUE;         ---this is a  maximum fixed point
      return;
      --
   ----------------------------------
   elsif My_Status = ABORTED then
   ----------------------------------
      -- Notice that if the Max_LTS_Depth has been encreased since the time the formula was 
      -- evaluated and this result saved, it is the Check_Computation itself which return
      -- a new status NOT_YET_STARTED instead of an obsolete ABORTED.
      --
--      if (N_Depth < Tmp_Depth/2 and then N_Depth > 20)  then
--        --
--        -- retry, maybe we are luckier, since we have more depth available
--        -- (but only if we have a minimal additional depth)
--        --
--        Set_Status (Cref, IN_PROGRESS,C_Depth);
--      else
        Result :=  ABORTED;
        return;
--      end if;
   ----------------------------------
   elsif My_Status = FOUND_TRUE or 
         My_Status = FOUND_FALSE then
   ----------------------------------
      Result :=  My_Status;
      return;
   ----------------------------------
   elsif (My_Status = TMP_TRUE or 
          My_Status = TMP_FALSE)  then
   ----------------------------------
      -- Notice that TMP_XXX values can only be found if the formulas contains fixpoint variables.
      -- (i.e. is part of a more external fixpoint formula), and the nested fixpoint id still 
      --  does not have a definitive value.
      -- THIS CASE DOES NOT OCCURR IF THE FORMULA HAS NO FIX POIN VARIABLES AS REQUESTED AND CHECKED
      --
      if (Get_Owner(CRef)=WithOwner or Get_Owner(CRef)=FromState) then
        Result :=  My_Status;
        if Tmp_Depth < Rec_Depth then
          Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
        end if;
        return;
      else
        -- My_Status= TMP_TRUE / TMP_FALSE unusable leftover from previous completed computations
         Set_Status (Cref, IN_PROGRESS,C_Depth);  
      end if;
   ----------------------------------
   elsif N_Depth > Max_LTS_Depth then
   ----------------------------------
      Set_Status (Cref, ABORTED,N_Depth);
      Result :=  ABORTED;
      return;
   ----------------------------------
   else -- My_Status = NOT_YET_STARTED
   ----------------------------------
      Set_Status (Cref, IN_PROGRESS,C_Depth);  
      -- Result will be set by this computation
   ----------------------------------
   end if;  
   ----------------------------------
   --
   -- Se FORM1 e' False adesso, ritorna False.
   --
   --
   if Form.PRef.Tform.Kind = Ftrue then
     Tmp_Result := FOUND_TRUE;
   else
   Check_True (Form.PRef.Tform, FromState,Context, SubCref, Tmp_Result, N_Depth,C_Depth+1,Tmp_Depth); 
   --
   if Tmp_Result = FOUND_FALSE or         -- Form defintely false
       Tmp_Result = TMP_FALSE then        -- Env_selector'Length >0
      --
      -- a false result here is definitive
      --
      Set_Subcomputation(Cref,SubCref, No_Evolution);
      Set_Status(Cref, Tmp_Result);
      --        Rec_Depth remains C_Depth
      Result :=  Tmp_Result;
      return;
      --  IN EFFETTI NEL CASO TMP_FALSE POTREMMO ANCHE CONTINUARE PER VEDERE SE ESISTE
      -- un path che rende la formula FOUND_FALSE. Se non lo facciamo e successivamente la
      -- sottoformula da TMP_FALSE passa a FOUND_TRUE il risultato restituito diventa inconsistente
      -- OVVIAMENTE risultati del tipo TMP_.. sono possibili sono in presenza di punti fissi
      -- ATTUALMENTE NON SUCCEDE PERCHE NON PERMETTIAMO A VARIABILI DI PUNTI FISSI DI COMPARIRE
      -- ALL'INTERNO DEGLI OPERATORI G F U W
      --
   elsif Tmp_Result /= ABORTED then    
     Add_Subcomputation(Cref,SubCref, No_Evolution);
     end if;
   end if;
   --
   -- Tmp_Result = ABORTED / TMP_TRUE / FOUND_TRUE
   --  (even if ABORTED, we continue in case we can  prove the falsity)
   --
   -- Se FORM1 e' True adesso, e esiste almeno un next state
   --  in cui  FORM e' False, ritorna False.
   --
   -- Tmp_Result is already TRUE or ABORTED
   --   even if ABORTED we continue in case we could return a FALSE instead.
   --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   --
    while Has_System_Transition (My_Iter) loop  
      declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
       --
       Check_AG(Form, WithOwner, This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
       --
       --  if Tmp_Depth < Rec_Depth then Rec_Depth := Tmp_Depth; end if;
       --
       if Last_Result = FOUND_FALSE or    --  recursion definitely FALSE
            Last_Result = TMP_FALSE then  -- Env_selector'Length >0
          Tmp_Result := Last_Result; 
--          Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));  -- TOBEDELETED
--          Set_Status(Cref, Tmp_Result,C_Depth);
          Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
          Set_Status(Cref, Tmp_Result,C_Depth);
          Iterator_Finalize (My_Iter);
          Result :=  Tmp_Result;
          return;
          --
       elsif Last_Result = ABORTED then
         Tmp_Result := Last_Result;
--         Rec_Depth := C_Depth;
         --
       elsif Last_Result = TMP_TRUE then
         if Tmp_Result=FOUND_TRUE then
            Tmp_Result := TMP_TRUE;
         end if;
--         if Tmp_Result /= ABORTED and Tmp_Depth < Rec_Depth then
--           Rec_Depth := Tmp_Depth;
--         end if;         
         if Tmp_Result /= ABORTED  then
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
         end if;
         --
       elsif Last_Result = FOUND_TRUE then
         if Tmp_Result /= ABORTED then
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
         end if;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;  -- for each rule
   --
--   if WithOwner = FromState and then  -- NO LONGER NECESSARY
--        Form.Env_Selector.all'Length =0 then
--        -- Form.Env_Selector.all'Length >0 allora  nella formula AG FF, la formula FF contiene
--        --  delle variabili di punti fissi, il cui valore puo' non essere (anzi non e' sicuramente 
--        --  un valore stabile, quindi non possiamo trasformate il TMP_XX in FOUND_XX )
----        Tmp_Result = TMP_TRUE then
--      Tmp_Result := FOUND_TRUE;    
      --
--  SOFAR THIS IS BUGGED!!!!!!!  
--   elsif C_Depth <= Rec_Depth and then    --  (ACTUALLY ONLY "=" since it cannot be "<")
--          Form.Env_Selector.all'Length =0 and then
--       -- there are no dependencies from outer computations
--       Tmp_Result = TMP_TRUE then
--      Tmp_Result := FOUND_TRUE;        
--  MA SE NON FACCIAMO COSI'  NON RICONOSCIAMO QUANDO PER UN SOTTOALBERO  AG Form  e' vera, con
--  il risultato che se poi AG Form diventa false per l'owner, quando consolido il risultato
--  trasformo tutti i risultati veri del sottoalbero in risultati falsi!!!!
--  e.g.  s1 -> s2,  s2 -> s2, s2 -> s3 {-/a},  AG [a] False, 
--     se in s2 non metto il risultato a FOUND_TRUE quando consolido in s1 lo metto a FALSE!!!!
--   end if;
--
---------------------------------
--  Il completamento della valutazione di una AG non causa mai la trasformazione del valore della
--  formula da TMP_XXX a FOUND_XXX.  Se il valore e' TMP_XXX vuol dire che la formula contiene
--  delle sottoformule con variabili di punti fissi la cui valutazione e' ancora in progress.
--  (quandi Form.Env_Selector.all'Length >0 and Rec_Depth < C_Depth)
---------------------------------
   if Tmp_Result = ABORTED then
      Set_Status(Cref, ABORTED,N_Depth);
   else 
      Set_Status(Cref, Tmp_Result,Rec_Depth);
   end if;
   Result :=  Tmp_Result;
   Iterator_Finalize (My_Iter);
   return;
   --
end Check_AG;


-------------------------------------------------------------------
-- Non posso mettere lo stato iniziale a TRUE perche' in caso di ricorsione devo restituire FALSE,
-- e devo quindi essere in grado di riconoscere quando avviene.
-- Quindi inizialmente lo stato e' IN_PROGRESS, quando tutte le subbompuations sonon TRUE
--  diventa TRUE (sensa bisogno di consolidare nulla). Quando una subcomputation restituisce
-- FALSE diventa FALSE.  In caso di ricorsione si restituisce FALSE.
-- Non c'e' mai nulla da dover consolidare
-------------------------------------------------------------------
procedure Check_AF (Form: Formula_Ref; 
                    WithOwner: System_Configuration;
                    FromState: System_Configuration;
                    Context: Computations_Table;
                    Cref: Out Computation_Step;
                    Result: Out Computation_Status;
                    N_Depth: Natural;
                    C_Depth: Natural;
                    Rec_Depth: out Natural) is
   --------------------------------------------------
   --  FORM  =   AF  FORM1     
   --
   --  e' per definizione uguale a:
   --   FORM =  [ true { true } U  FORM1 ]
   --
   --  ede' equivalente a:
   --   min Z = (FORM1 | 
   --           ((~FINAL) & (~ET ~Z) & (~EX{true} ~Z))
   --
   --  !!! min Z = (FORM1 or (AX{true} Y) ) !!!
   --------------------------------------------------
   --
   -- Se FORM1 e' True adesso ritorna True.
   --
   -- Se FORM1 e' False adesso e non ci sono next states, o ne esiste 
   -- almeno uno in cui FORM vale False, ritorna False.
   -- Altrimenti ritorna True.
   --
   -- Nota: Se FORM1 ritorna True solo se o FORM1 e' True subito,
   -- o per ogni next state (e ne deve esistere almeno uno) FORM e' True.
   -------------------------------------------------
   --  FORM = Form, FORM1 = Form.PRef.TForm
   --
   --  Form  =  (Fexist, Form.PRef)
   --  Pref  =  (Eventually, PRef.Tform)
   --------------------------------------------------
   Tmp_Result: Computation_Status;
   Tmp_Result2: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
    --
    Rec_Depth := C_Depth; -- DELETABLE
    -- guardiamo se ci sono dei risultati gia' pronti
    --
    Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --
    -- se la computazione era in progresso, allora abbiamo trovato un ciclo. 
   if My_Status = IN_PROGRESS then
       Result :=  FOUND_FALSE;         ---this is a  minimum fixed point
       -- in PRESENCE of LOOPS BACK exists at least an infinite path not satisfying Form
       --  hence AF is DEFINITIVELY FALSE  hence no need to handle rec_depth.
      return;
   elsif (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
      -- possiamo riciclare un risultato gia' calcolato in precedenza
      Result :=  My_Status;
      return;
   elsif ((My_Status = TMP_TRUE) or My_Status = TMP_FALSE) then -- DELETABLE?!?!?
      --    -- and Form.Env_Selector'Length =0) then
      -- PERCHE MAI NON DOVREMMO RICICLARE TMP? se Form.Env_Selector'Length >0 ?????
      --
      -- possiamo riciclare un risultato gia' calcolato in precedenza
      Result :=  My_Status;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   else
       -- dobbiamo iniziare una prima a nuova valutazione della formula. 
      Set_Status (Cref, IN_PROGRESS,C_Depth);
   end if;
   --
   --Primo passo; valutiamo FORM1 in questo stato.
   -- Se FORM1 e' True adesso ritorna True.
   --
   Check_True (Form.PRef.Tform, FromState,Context, SubCref, Tmp_Result, N_Depth,C_Depth+1,Tmp_Depth); 
   --
   if Tmp_Result = FOUND_TRUE or Tmp_Result= TMP_TRUE then
     -- Se FORM1 e' gia' TRUE in questo stato, abbiamo gia' il risultato definitivo
     -- Se Form e' vera qui non e' necessario osservare i discendenti
     Set_Subcomputation(Cref, SubCref, No_Evolution);
     Set_Status(Cref, Tmp_Result);
     --  Rec_Depth remains C_Depth
     Result :=   Tmp_Result;
     return;
   elsif Tmp_Result /= ABORTED then  -- TMP_FALSE  / FOUND_FALSE
     Add_Subcomputation(Cref, SubCref, No_Evolution);
   end if;
   -- 
   -- FORM1 pero' essere anche false o aborted,
   -- in entrambi i casi si va a vedere come procede la ricorsione nei discendenti.
   --
   -- Se FORM1 e' False adesso e non ci sono next states, o ne esiste 
   -- almeno uno in cui FORM vale False, ritorna False.
   -- Altrimenti ritorna True.
   --
   -- Tmp_Result =  ABORTED / TMP_FALSE /FOUND_FALSE
   --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
    --
    if not Has_System_Transition (My_Iter)  then
      --  Set_Subcomputation(Cref, SubCref, No_Evolution);
      Iterator_Finalize (My_Iter);
      Set_Status(Cref, Tmp_Result);
      Result :=   Tmp_Result;
      return;      
    end if;
    --
    -- se Tmp_Result=ABORTED  sicuramente non si potra restiture TRUE, possiamo pero'
    ---  continuare per vedere se si puo' restiture FALSE
    -- 
    --  se Tmp_Result = FALSE potremo ancora restituire True/False/Aborted a seconda dei casi
    --  Appena Tmp_Result2 diventa Aborted, non potremo piu' restituire TRUE, 
    --    ma al piu' FALSE o ABORTED
    --
   Tmp_Result2 := FOUND_TRUE;    --  sara' sempre  TMP_TRUE / FOUND_TRUE / ABORTED
   while Has_System_Transition (My_Iter) loop  
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
        --
        Check_AF(Form, This_Next,  This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
        --
        --  if Tmp_Depth < Rec_Depth then Rec_Depth := Tmp_Depth; end if;
        --
        if Last_Result = FOUND_FALSE or Last_Result = TMP_FALSE then
          --
          -- se AF FORM e' falsa per un discendente, essa divanta DEFINITIVAMENTE FALSE
          --  anche per questo stato. Il ciclo viene abbandonato
          --
          if Tmp_Result= ABORTED or else Tmp_Result=FOUND_FALSE then
            -- se Tmp_Result=TMP_FALSE  rimane tale
            Tmp_Result := Last_Result;
          end if;
          Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
          Set_Status(Cref, Tmp_Result);
          Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
          Iterator_Finalize (My_Iter);
          Result :=  Tmp_Result;
          return;
          --
        elsif Last_Result = ABORTED then
          --
          -- se AF FORM e' aborted per un discendente tutta la computazione
          -- non puo' che essere aborted  o false. decidiamo di continuare per verificxare
          -- fino in fondo se per caso possa essre false.
          --
          Tmp_Result2 := ABORTED;
           --
        elsif  Last_Result = TMP_TRUE or Last_Result = FOUND_TRUE then 
          --
          -- se il risultato e' true, continuiamo l'analisi delle altre transizioni,
          -- eventualmente raffinando il risultato complessivo trovato finora.
          --
          if  Tmp_Result2 = FOUND_TRUE then
          -- if Tmp_Result2=ABORTED ... rimane tale
          -- if Tmp_Result2=TMP_TRUE .. rimane tale
          --
            Tmp_Result2 := Last_Result;    -- Tmp_result2 maybe become TMP_TRUE
          --
          end if;
          Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
        end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;
   --
   Iterator_Finalize (My_Iter);
   --
   -- Tmp_Result = ABORTED / TMP_FALSE / FOUND_FALSE   -- the value of current Form
   -- Tmp_Result2 = ABORTED / TMP_TRUE / FOUND_TRUE    -- the value of recursive application 
   --
   if Tmp_Result = ABORTED or Tmp_Result2=ABORTED  then
     Result := ABORTED;
   else
     Result := Tmp_Result2;
   end if;
   if (Result = FOUND_TRUE or Result = TMP_TRUE) and then
      Product_Families and then Live_Model then
      Check_Liveness(FromState);
   end if;
   Set_Status(Cref, Result);
   return;
   --
end Check_AF;


procedure Check_WSquare (Form: Formula_Ref; 
                       WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: Out Natural) is
     --------------------------------------------------
     --  FORM  =   [[ ACTION ]] FORM1     
     --
     --  e' per definizione uguale a:
     --   FORM =  ~ ( << ACTION >> ~ FORM1 )
     -- 
     --  ed e' equivalente a:
     --   max Y = ( ~(EX{act} ~FORM1) & (~ET ~Y) )
     --------------------------------------------------
     --  Richiede quindi che PER OGNI BINDING soddisfatto da act FORM1 sia vera.
     --------------------------------------------------
     -- Se e' possibile fare ora, o dopo una eventuale sequenza di tau,
     --   una azione compatibile con ACTION, allora nello stato
     --   raggiunto deve valere FORM1.
     --
     -- Quindi:
     --
     -- Se esiste adesso transizione con azione compatibile con ACTION che
     --    porta in uno stato in cui FORM1 e' False, allora ritorna False.
     --
     -- Altrimenti si verifica che ogni transizione tau porti un stato
     --  in cui FORM1 sia vera.
     --
     -- (Nota: Ritorna subito True se lo stato e' Finale)
     -- (Nota: Ritorna subito True se lo stato fa solo azioni non compatibili)
     -- (Nota: Ritorna True se puo' fare solo un cicli infiniti di tau)
     ---------------------------------------------------------
     --  FORM = Form, FORM1 = Form.FormRef
     --
     --  Form  =  (Check_Square, ARef, FormRef, NotFormRef)
     ---------------------------------------------------------
     --
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   --
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --  check for fixed point recursions
    --
   if My_Status = IN_PROGRESS then
       Result :=  FOUND_TRUE;         ---this is a  maximum fixed point
       Rec_Depth := Tmp_Depth;      -- THE VALUE STORED WITH THE BEGINING OF THE COMPUTATION
      return;
   elsif (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
         (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
      -- TMP_TRUE / TMP_FALSE /FOUND_TRUE / FOUND_FALSE /ABORTED
      Result :=  My_Status;
      if Result= TMP_TRUE and then 
        Tmp_Depth < Rec_Depth and then Form.Env_Selector'Length =0  then
        Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
      end if;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   else
      Set_Status (Cref, IN_PROGRESS);
   end if;
     --
     -- Se esiste adesso transizione con azione compatibile con ACTION che
     --    porta in uno stato in cui FORM1 e' False, allora ritorna False.
     --
   Tmp_Result := FOUND_TRUE;   -- if there are no transitions result is definitely true
   Iterator_Initialize (My_Iter,System_Configuration(FromState));
   --
   while Has_System_Transition (My_Iter) loop
     --  Check il all IMMEDIATE transitions satisfying ACT satisfy FORM
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings : AllBindings := Match(My_Iter, Form.ARef);
       NewFormula: Formula_Ref;
     begin
       if TheBindings.MatchOK then
         --
         if TheBindings.VarsCount = 0 then
           Check_True(Form.FormRef, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
           if Last_Result=FOUND_FALSE or Last_Result=TMP_FALSE then
              -- in case of failure, this is a definitive failure
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status(Cref, Last_Result);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Iterator_Finalize (My_Iter);
              Result :=   Last_Result;
              return; 
              --
           elsif Last_Result = ABORTED then
             --  if aborted  we lose the possibility of returning TRUE
             Tmp_Result := ABORTED;
             Rec_Depth := C_Depth;
              --
           elsif  Last_Result=TMP_TRUE then 
             if Tmp_Result /= ABORTED then
               Tmp_Result := TMP_TRUE;
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             end if;
             --
           else  --  Last_result=FOUND_TRUE
             if Tmp_Result /= ABORTED then    -- hence TMP_TRUE / FOUND_TRUE
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             end if;
           end if;
         else
           -- ALL THE INSTANTIATIONS MUST HOLD
           for B in 1.. TheBindings.BindCount loop
             NewFormula := 
                NewInstance(Form.FormRef,(TheBindings.VarsCount,
                                          TheBindings.VarNames,
                                          TheBindings.AllValues(B).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
             if Last_Result=FOUND_FALSE or Last_Result=TMP_FALSE then
                Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                Set_Status(Cref, Last_Result);
                Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                Iterator_Finalize (My_Iter);
                Result :=   Last_Result;
                return; 
                --
             elsif Last_Result = ABORTED then
               Tmp_Result := ABORTED;
               Rec_Depth := C_Depth;
                --
             elsif  Last_Result=TMP_TRUE then 
               if Tmp_Result /= ABORTED then
                 Tmp_Result := TMP_TRUE;
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               end if;
             else  --  Last_result=FOUND_TRUE
               if Tmp_Result /= ABORTED then
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               end if;
             end if;
           end loop;
         end if;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop;
     --
     -- Altrimenti si verifica ricorsivamente che ogni transizione tau porti un stato
     --  in cui FORM sia vera.
     --
     -- Tmp_Result = ABORTED / FOUND_TRUE / TMP_TRUE / Rec_Depth=C_Depth
     --
   Iterator_Restart (My_Iter);
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
      begin
         if Evolution_Satisfies_Tau (My_Iter) then
           --
           Check_WSquare(Form,WithOwner,This_Next,Context,SubCref,Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
           --
           if Last_Result = FOUND_FALSE or   -- recursion definitely FALSE
                Last_Result = TMP_FALSE then   -- Env_selector'Length >0
             Rec_Depth := C_Depth;
             Tmp_Result := Last_Result;
             --
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Tmp_Result);
             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Iterator_Finalize (My_Iter);
             Result :=   Tmp_Result;
             return;
             --
           elsif Last_Result = Aborted then
             Tmp_Result := Last_Result;
             Rec_Depth := C_Depth;
             -- we continue just in case we can prove it is false
             --
           elsif Last_Result=TMP_TRUE then
             if Tmp_Depth < C_Depth then
               Rec_Depth := Tmp_Depth;
             end if;
             if Tmp_Result =FOUND_TRUE then
               Tmp_Result := Last_Result;
             end if;
             if Tmp_Result /= ABORTED then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             end if;
             -- if Tmp_result is aborted, it remains untocuhed.
           else -- Last_Result=FOUND_TRUE
             if Tmp_Result /= ABORTED then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             end if;
           end if;
         end if;   -- else is not tau
       end;
       Iterator_Advance (My_Iter);
   end loop ;  -- for each rule
   --
   Iterator_Finalize (My_Iter);
    --
    if WithOwner = FromState and then
        Form.Env_Selector.all'Length =0 and then
        Tmp_Result = TMP_TRUE then
        Tmp_Result := FOUND_TRUE;
--   elsif C_Depth <= Rec_Depth and then    --  (ACTUALLY ONLY "=" since it cannot be "<")
--          Form.Env_Selector.all'Length =0 and then
--          Tmp_Result = TMP_TRUE then
--       -- there are no dependencies from outer computations
--      Tmp_Result := FOUND_TRUE;
    end if;
    Set_Status(Cref, Tmp_Result);
    Result :=   Tmp_Result;
    return;
    --
end Check_WSquare;


procedure Check_Square (Form: Formula_Ref;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: out Natural) is
     --------------------------------------------------
     --  FORM  =   [ ACTION ] FORM1
     --
     --  e' per definizione uguale a:
     --   FORM =  ~ ( < ACTION > ~ FORM1 )
     --
     --  ed e' equivalente a:
     --   max Y = ( ~(EX{act} ~FORM1) )
     --------------------------------------------------
     --  Richiede quindi che PER OGNI BINDING soddisfatto da act FORM1 sia vera.
     --------------------------------------------------
     -- Se e' possibile fare ora
     --   una azione compatibile con ACTION, allora nello stato
     --   raggiunto deve valere FORM1.
     --
     -- Quindi:
     --
     -- Se esiste adesso transizione con azione compatibile con ACTION che
     --    porta in uno stato in cui FORM1 e' False, allora ritorna False.
     --
     -- (Nota: Ritorna subito True se lo stato e' Finale)
     -- (Nota: Ritorna subito True se lo stato fa solo azioni non compatibili)
     ---------------------------------------------------------
     --  FORM = Form, FORM1 = Form.FormRef
     --
     --  Form  =  (Check_Square, ARef, FormRef, NotFormRef)
     ---------------------------------------------------------
     --
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator ;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, FromState, FromState,Cref,My_Status,Tmp_Depth);
    --
   if (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
      Result :=  My_Status;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   end if;  
   -- MY_Status=INPROGRESS (cannot happen!!!)
   -- My_Status=NOT_YET_STARTED, TMP_XXX (can only occour with Env>0)
   --
   -- Se esiste adesso transizione con azione compatibile con ACTION che
   --    porta in uno stato in cui FORM1 e' False, allora ritorna False.
   --
   Tmp_Result := FOUND_TRUE;
   Iterator_Initialize (My_Iter,System_Configuration(FromState));
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings : AllBindings := Match(My_Iter, Form.ARef);
       NewFormula: Formula_Ref;
     begin
       if TheBindings.MatchOK then
         if TheBindings.VarsCount = 0 then
           Check_True(Form.FormRef, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
           if Last_Result = FOUND_FALSE then
             Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Last_Result);
             Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
             Iterator_Finalize (My_Iter);
             Result :=  Last_Result;
             Rec_Depth := C_Depth;
             return;
           elsif Last_Result=TMP_FALSE then
             Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Last_Result);
             Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
             Iterator_Finalize (My_Iter);
             Result :=  Last_Result;
              Rec_Depth := C_Depth;
              if Tmp_Depth < C_Depth then
                 Rec_Depth := Tmp_Depth;
              end if;
             return;
           elsif Last_Result = ABORTED then
             Tmp_Result := Last_Result;
              Rec_Depth := C_Depth;
           elsif Last_Result=FOUND_TRUE then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             -- nota che se Tmp_Result=ABORTED/FOUND_TRUE/TMP_TRUE, rimane tale
           elsif Last_Result =TMP_TRUE  then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Tmp_Result := Last_Result;
              if Tmp_Result = FOUND_FALSE then
                 Tmp_Result := TMP_FALSE;
              end if;
             if Tmp_Depth < Rec_Depth then
               Rec_Depth := Tmp_Depth;
             end if;
           end if;
         else
           for B in 1.. TheBindings.BindCount loop
             -- ALL new formulas must by satisfied by next state
             NewFormula := 
                    NewInstance(Form.FormRef,(TheBindings.VarsCount,
                                              TheBindings.VarNames,
                                              TheBindings.AllValues(B).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
             if Last_Result = FOUND_FALSE  then
               Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Last_Result);
               Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize (My_Iter);
               Result :=  Last_Result;
               Rec_Depth := Tmp_Depth;
               -- we override any previous *_TRUE pr ABORTED partial result
               return;
             elsif Last_Result=TMP_FALSE then
               Add_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Last_Result);
               Set_Subcomputation(Cref,SubCref, Get_Evolution_Data(My_Iter));
               Iterator_Finalize (My_Iter);
               -- we override any previous *_TRUE pr ABORTED partial result
               Result :=  Last_Result;
               Rec_Depth := C_Depth;
               if Tmp_Depth < Rec_Depth then
                 Rec_Depth := Tmp_Depth;
               end if;
               return;
             elsif Last_Result = ABORTED then
               -- we may override any previous partial _*_TRUE reult
               Tmp_Result := ABORTED;
                 Rec_Depth := Tmp_Depth;
             elsif Last_Result =FOUND_TRUE then
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 -- nota che Tmp_Result rimane tale!
             elsif Last_Result =TMP_TRUE then
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               if Tmp_Result = FOUND_TRUE then
                 Tmp_Result := TMP_TRUE;
               end if;
               if Tmp_Depth < Rec_Depth then
                   Rec_Depth := Tmp_Depth;
               end if;
             end if;
           end loop;
         end if;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop;
   --
   Iterator_Finalize (My_Iter);
   --
   Set_Status(Cref, Tmp_Result);
   Result :=   Tmp_Result;
   return;
   --
end Check_Square;


procedure Check_AX (Form: Formula_Ref; 
                   FromState: System_Configuration;
                   Context: Computations_Table;
                    Cref: Out Computation_Step;
                    Result: Out Computation_Status;
                    N_Depth: Natural;
                    C_Depth: Natural;
                    Rec_Depth: out Natural) is
   --------------------------------------------------
   --  FORM  =   AX {ACTION} FORM1   
   --  FORM  =   AT {ACTION} FORM1    
   --
   --  (nel caso AT  Action = null)
   --------------------------------------------------
   --
   -- Le uniche azioni possibili (ed almeno una anche obbligatoria)
   -- sono quelle compatibili con ACTION. Inoltre nello stato raggiunto
   -- deve valere FORM1.
   --
   -- Quindi:
   --
   -- Se lo stato e' finale ritorna subito False.
   -- Se esiste una transizione con una azione tau o una azione 
   --  non compatibile con ACTION ritorna subito False.
   -- Altrimenti si verifica che ogni transizione con azione compatibile
   -- porti un stato in cui FORM1 sia vera.
   --  
   ------------------------------------------------------------------
   --  Nota sui Bindings multipli:
   --  volgiamo una semantica per cui se esiste un unico path,
   --   AX  ==  EX
   -- Quindi questa sara' una semantica per cui, per ogni possibile
   --- evoluzione, basta che esista un binding (fra  tutti quelli possibili)
   --  per cui FORM(Binding)  sia true.
   --
   -- Questo  significa che AX {act($)} FORM(%)  NON E' EQUIVALENTE A:
   --   EX{act($}) FORM(%)   
   --    and  not EX{not act(*)} true
   --    and  not EX{act($)} not FORM(%)    cioe [$] FORM(%)
   ------------------------------------------------------------------
   --   FORM1 = Form
   ------------------------------------------------------------------
   Tmp_Result: Computation_Status;
   Tmp_Result1: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator; 
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
   Tmp_Depth1: Natural := C_Depth;
 begin
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, FromState, FromState, Cref,My_Status,Tmp_Depth);
   --
   if (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
      Result :=  My_Status;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   end if;  
   --
   -- MY_Status=INPROGRESS (cannot happen!!!)
   -- My_Status=NOT_YET_STARTED, TMP_XXX (can only occour with Env>0)
   --
   -- Se lo stato e' finale ritorna subito False.
   -- Se esiste una transizione con una azione tau o una azione 
   -- non compatibile con ACTION ritorna subito False.
   -- Altrimenti si verifica che ogni transizione con azione compatibile
   -- porti un stato in cui FORM1 sia vera.
   -- 
   Tmp_Result := FOUND_TRUE;
   --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   --
   --      
   -- Se lo stato e' finale ritorna subito False.
   --
   if not Has_System_Transition (My_Iter) then
     Tmp_Result := FOUND_FALSE;
     Set_Status(Cref, Tmp_Result);
     Result :=  Tmp_Result;
     Iterator_Finalize (My_Iter);
     return;
   end if;
   --
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings : AllBindings := Match(My_Iter, Form.PRef.AARef);
       -- NewFormula: Formula_Ref;
     begin
       if not TheBindings.MatchOK then
         -- there MUST be NO states not satisfying action
         -- MAYBE WE SOULD SAY RECORD THAT THIS EVOLUTION MAKES THE EVALUATION FAIL
        Tmp_Result := FOUND_FALSE;
        Set_Status(Cref, Tmp_Result);
        Result :=  Tmp_Result;
        Iterator_Finalize (My_Iter);
        return;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;
   --
   Iterator_Restart (My_Iter);
   --
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings : AllBindings := Match(My_Iter, Form.PRef.AARef);
       NewFormula: Formula_Ref;
     begin
       if TheBindings.MatchOK then
         --
         -- All the states reachable with Action MUST satisfy This_Rule
         --
         if TheBindings.VarsCount = 0 then
           --
           -- if there are no bindings ... we just check if FORM holds ..
           --
           Check_True (Form.PRef.AFormRef, This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
           if Last_Result = FOUND_FALSE then
             Tmp_Result := Last_Result;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Tmp_Result);
             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             -- a transition failure is sufficient to complete the evaluation
             Iterator_Finalize (My_Iter);
             Result :=  Tmp_Result;
             Rec_Depth := C_Depth;
             return;
           elsif Last_Result = TMP_FALSE then
             Tmp_Result := Last_Result;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Tmp_Result);
             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             -- a transition failure is sufficient to complete the evaluation
             Iterator_Finalize (My_Iter);
             Result :=  Tmp_Result;
              Rec_Depth := C_Depth;
              if Tmp_Depth < C_Depth then
                 Rec_Depth := Tmp_Depth;
              end if;             
             return;
             --
           elsif Last_Result = ABORTED then
              Tmp_Result := Last_Result;
              Rec_Depth := C_Depth;
              -- after becoming aborted ... the Tmp_Result cannot anymore become TRUE,
              -- but, at most, definitely FALSE
              --  in that case we will save the counterexample
           elsif Last_Result = FOUND_TRUE then
             if  Tmp_Result= FOUND_TRUE then
                Tmp_Result := Last_Result;
                -- if Tmp_Result in not changed
             end if;
             if Tmp_Result /= ABORTED then
                Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             end if;
           elsif Last_Result = TMP_TRUE then
             if  Tmp_Result= FOUND_TRUE then
                Tmp_Result := Last_Result;
                -- if Tmp_Result=ABORTED in not changed
             end if;
             if Tmp_Depth < Rec_Depth then
               Rec_Depth := Tmp_Depth;
             end if;
             if Tmp_Result /= ABORTED then
                Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             end if;
           end if;
           --
         else
           --  Tmp_Result1  ranges over temporary binding related subformulas for THIS evolution
           -- at least one binding must satisfy FORM1
           Tmp_Result1 := FOUND_FALSE;
           Tmp_Depth1 := C_Depth;
           for B in 1.. TheBindings.BindCount loop
             NewFormula :=
                  NewInstance(Form.PRef.AFormRef,(TheBindings.VarsCount,
                                                  TheBindings.VarNames,
                                                  TheBindings.AllValues(B).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth+1,C_Depth+1,Tmp_Depth);
             -- 
             if Last_Result = FOUND_FALSE or Last_Result = TMP_FALSE then
               -- if one binding fails, we must check for the others
               -- (we should save the counterexample in case of definitive fauilure)
               if Tmp_Result1 /= ABORTED then
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 -- The main Tmp_Result can be TRUE or ABORTED
                 -- if it is TRUE, we are saving temporary binding related subcomputations
                 --  because of ALL of them fail we have a definitive failure for which
                 -- we need the explanation.
                 -- Unfortunately, these subcomputations remains here even if 
                 -- we later find a successfull binding, because we cannot remove all
                 -- the already saved successfull subcomnputations.
                 -- SPRECIAL care should be used in presenting the explanation.
                 if Tmp_Depth < Tmp_Depth1 then
                      Tmp_Depth1 := Tmp_Depth;               
                 end if;
               end if;
               if Tmp_Result1 = FOUND_FALSE and then Last_Result = TMP_FALSE then
                 Tmp_Result1 := TMP_FALSE;
               end if;
               --
             elsif Last_Result = ABORTED then
               -- Tmp_Result1 was already FALSE or ABORTED
               Tmp_Result1 := ABORTED;
               Tmp_Depth1 := C_Depth;
               --
             elsif Last_Result = TMP_TRUE or Last_Result = FOUND_TRUE then
               -- we have found a successfull binding
               Tmp_Result1 := Last_Result;
                 if Tmp_Depth < Tmp_Depth1 then
                      Tmp_Depth1 := Tmp_Depth;
                 end if;
               if Tmp_Result /= ABORTED then
                 -- unless any positive result are to be discarded because we have
                 -- already ecounted an aborted transition evaluation
                 --  Tmp_Result can only be TRUE OR ABORTED                  
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 if Tmp_Depth1 < Rec_Depth then
                    Rec_Depth := Tmp_Depth1;
                 end if;
               end if;
               exit;  -- from binding cycle for this evaluation
             end if;
           end loop; -- loop of all binding for a single evolution
           -- Tmp_Result1  = ABORTED, *_TRUE, *_FALSE
           if Tmp_Result1 = FOUND_FALSE or Tmp_Result1 = TMP_FALSE then
             Tmp_Result := Tmp_Result1;
             if Tmp_Depth1 < Rec_Depth then
                    Rec_Depth := Tmp_Depth1;
              end if;             
             -- a transition failure is sufficient to complete the evaluation
             exit;
           elsif Tmp_Result1 = ABORTED then
              Tmp_Result := Tmp_Result1;
              Rec_Depth := C_Depth;
           elsif Tmp_Result1 = TMP_TRUE or Tmp_Result1 = FOUND_TRUE then
             if  Tmp_Result= FOUND_TRUE then
                Tmp_Result := Tmp_Result1;
                -- if Tmp_Result=ABORTED in not changed
             end if;
           end if;
         end if;
       else
         -- there MUST be NO states not satisfying action
         -- MAYBE WE SOULD SAY RECORD THAT THIS EVOLUTION MAKES THE EVALUATION FAIL
         Tmp_Result := FOUND_FALSE;
         exit ;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;  
   --
   Iterator_Finalize (My_Iter);
   --
   Set_Status(Cref, Tmp_Result);
   Result :=  Tmp_Result;
   return;
end Check_AX;


procedure Check_AUntil1 ( Form: Formula_Ref; 
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: Out Natural) is
   ------------------------------------------------------------------
   --
   --  FORM  =   A [ FORM1 { ACTION } U FORM2 ] 
   --
   --  e' equivalente a:
   --   min Z: FORM2 | ( FORM1 & 
   --                       (~EX{~ACTION) true) &
   --                       (~EX {ACTION} ~Z)&
   --                       (~ET ~ Z) )
   --                        &  ~ FINAL !!!!!!!!!!!!!!!!!!!!!!!!!
   ------------------------------------------------------------------
   -- Se FORM2 e' True in questo stato, ritorna subito True.
   --
   -- Se FORM2 e' False, e anche FORM1 e' False, ritorna subito False.
   --
   -- Se FORM2 e' False, FORM1 e' true, ma esiste una transizione con azione 
   --    non tau e non compatibile con ACTION  ritorna subito False.
   --
   -- Se FORM2 e' False, FORM1 e' true, e ogni transizione tau o 
   --   compatibile con ACTION porta un stato in cui FORM e' vera allora 
   --   ritorna True.
   --   (in caso di cicli infiniti pero' ritorna False).
   ------------------------------------------------------------------
   -- Ogni path completo contiene uno stato in cui vale "Form2",
   -- e tale stato viene raggiunto attraverso una sequenza (eventualmente
   -- vuota) di "tau" o azioni compatibili con ACTION e attraversando stati 
   -- in cui vale sempre "Form1".
   ------------------------------------------------------------------
   --  FORM = Form, FORM1 = Form.Pref.U1FormRef1, FORM2 = Form.Pref.U1FormRef2
   --
   --  Form = (All,  Form.PRef)
   --  PRef = (Until1, U1FormRef1, U1ARef, U1FormRef2)
   --------------------------------------------------
   Tmp_Result2: Computation_Status;
   Tmp_Result1: Computation_Status;
   Tmp_Result: Computation_Status;  
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator;
   SubCref: Computation_Step;
   FirstCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   --
   Rec_Depth := C_Depth;
   --
   --  Guarda se c'e' gia' un risultato riciclabile displonibile.
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --  check for fixed point recursions
    --
    ----------------------------------
    if My_Status = In_PROGRESS then
   ----------------------------------
      Result :=  FOUND_FALSE;         ---this is a  minimum fixed point
      return;
   ----------------------------------
   elsif My_Status = ABORTED then
   ----------------------------------
      -- Notice that if the Max_LTS_Depth has been encreased since the time the formula was
      -- evaluated and this result saved, it is the Check_Computation itself which return
      -- a new status NOT_YET_STARTED instead of an obsolete ABORTED.
      --
--      if (N_Depth < Tmp_Depth/2 and then N_Depth > 20)  then
--        --
--        -- retry, maybe we are luckier, since we have more depth available
--        -- (but only if we have a minimal additional depth)
--        --
--        Set_Status (Cref, IN_PROGRESS,C_Depth);
--      else
        Result :=  ABORTED;
        return;
--      end if;
   ----------------------------------
   elsif My_Status = FOUND_TRUE or
         My_Status = FOUND_FALSE then
   ----------------------------------
      Result :=  My_Status;
      return;
   ----------------------------------
   elsif (My_Status = TMP_TRUE or
          My_Status = TMP_FALSE)  then
   ----------------------------------
      -- Notice that TMP_XXX values can only be found if the formulas contains fixpoint variables.
      -- (i.e. is part of a more external fixpoint formula), and the nested fixpoint id still
      --  does not have a definitive value.
      -- THIS PART HAS TO BE BETTER ANALYZED
      --
      if (Get_Owner(CRef)=WithOwner or Get_Owner(CRef)=FromState) then
        Result :=  My_Status;
        if Form.PRef.kind = Until1 and then
            Result= TMP_False and then 
             Tmp_Depth < Rec_Depth then
          Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
        elsif Form.PRef.kind = WUntil1 and then 
               Result= TMP_TRUE and then 
                Tmp_Depth < Rec_Depth then
          Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
        end if;
      else
        -- My_Status= TMP_TRUE / TMP_FALSE unusable leftover from previous completed computations
         Set_Status (Cref, IN_PROGRESS,C_Depth);
      end if;
      return;
   ----------------------------------
   elsif N_Depth > Max_LTS_Depth then
   ----------------------------------
      Set_Status (Cref, ABORTED,N_Depth);
      Result :=  ABORTED;
      return;
   ----------------------------------
   else  -- My_Status /= NOT_YET_STARTED then
   ----------------------------------
      Set_Status (Cref, IN_PROGRESS,C_Depth);
      -- Result will be set by this computation
   ----------------------------------
   end if;
   ----------------------------------
 
   --
   -- se non c'e' bisogna mettere il piedi la valutazione della formula.
    --
    Check_True (Form.PRef.U1FormRef2, FromState,Context, SubCref, Tmp_Result,N_Depth,C_Depth+1,Tmp_Depth);
    FirstCref := SubCref;
    --
    -- se FORM2 e' vera subito, il risultato e' definitivamente true.
    --
    if Tmp_Result = FOUND_TRUE or Tmp_Result = TMP_TRUE then
       --
       Set_Subcomputation(Cref, SubCref, No_Evolution);
       Set_Status(Cref, Tmp_Result);
       Result :=  Tmp_Result;
      return;
    else
      Add_Subcomputation(Cref,SubCref, No_Evolution);
    end if;
    --
    -- Se Form2 e' FALSE o ABORTED si continua con la
    --  valutazione di form1 e di form2 sui discendenti.
    -- Tutti i rusultati sono ancora possibili.
    --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
    --
   if not Has_System_Transition (My_Iter)  then
     -- no recursion ixs needed. We find now the result
     -- se non ci sono discendenti il risultato di FORM2 adesso diventa
     --  definitivo (Tmp_Result).
     Set_Status(Cref, Tmp_Result);
     Iterator_Finalize (My_Iter);
     Result :=  Tmp_Result;
     return;
   end if;
    --
   -- se ci sono discendendenti si valuta FORM1 prima di vedere
   --   se continuare la ricorsione.
    --
    Check_True (Form.PRef.U1FormRef1, FromState,Context, SubCref,Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth); 
    --
    -- se FORM1 e' false subito, il risultato della valutazione di FORM2 qui
    --  diventa definitivo.
    --
    if Tmp_Result1 = FOUND_FALSE or else Tmp_Result1=TMP_FALSE then
      Add_Subcomputation(Cref,SubCref, No_Evolution);
      Set_Status(Cref, Tmp_Result1);
      Iterator_Finalize (My_Iter);
      Result :=  Tmp_Result1;
      return;
      --
    elsif Tmp_Result1 /= ABORTED then
      Add_Subcomputation(Cref, SubCref, No_Evolution);
    end if;
    --
    -- se FORM1 e VERA si procede con la valutazione di FORM sui discendenti
    --  (e tutti i risultati sono ancora possibili).
    -- 
    -- se FORM1 e' ABORTED si procede lo stesso con la valutazione di form
    --  sui discendenti, giusto per vedere se per caso non si possa restituire
    --  invece che un aborted, un risultato FALSE
    --
    -- The possibilities are:
    -- Tmp_Result  (Form2) = Found_False / Tmp_False / Aborted
    -- Tmp_Result1 (Form1) = Found_True  / Tmp_True  / Aborted 
    --  se Tmp_Result1 = Aborted non potro piu' restituire True, ma al massimo False
    --
   Tmp_Result2 := FOUND_TRUE;   --  (FORM, recursively on all descendents)
   -- se Tmp_Result2 diventa Aborted non potro piu' restituire True, ma al massimo False
   -- se Tmp_Result2 diventa *_False allora restituisco False
   -- fintanto che Tmp_Result2 rimane *_True continuo ad analizzare evoluzioni
   --
   while Has_System_Transition (My_Iter) loop 
      --
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
--       Set_Depth(My_Depth+1,This_Next);
       --
       if Evolution_Satisfies (My_Iter, Form.PRef.U1ARef) or else
            (ACTL_Compatibility and then Evolution_Satisfies_Tau(My_Iter)) then
         --
         -- Se FORM2 e' False, FORM1 e' true, e esiste una transizione tau o 
         --  compatibile con ACTION che porta in un stato in cui FORM 
         --   e' falsa allora ritorna False.
         --
         Check_AUntil1 (Form, WithOwner,This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
         --
         --
         if Last_Result = FOUND_FALSE or else
             Last_Result = TMP_FALSE then
           -- se FORM e' falsa per qualche subpath, FORM e' definitivamente falsa.
           --
           Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Set_Status(Cref, Last_Result,C_Depth);
           Iterator_Finalize (My_Iter);
           Result :=  Last_Result;
           return;
           --
         elsif Last_Result = FOUND_TRUE then
           --
           if Tmp_Result2 /= ABORTED then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           end if;
           --
         elsif Last_Result = TMP_TRUE then
           -- se form e' vera' su un subpath, evntualmente raffiniamo Tmp_Result2. 
           -- 
           if  Tmp_Result2= FOUND_TRUE then
             Tmp_Result2 := TMP_TRUE;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
--             if Form.Pref.Kind = WUntil1 and then
--                 Form.Env_Selector.all'Length =0 then
--               Rec_Depth := Tmp_Depth;
--             end if;
           end if;
           if Tmp_Result2 = TMP_TRUE then
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
--             if Form.Pref.Kind = WUntil1 and then
--                 Form.Env_Selector.all'Length =0 then
--               Rec_Depth := Tmp_Depth;
--            end if;
           end if;
           -- if Tmp_Result2 = ABORTED we do nothing.
           --
         elsif Last_Result = ABORTED then
           -- se form e' aborted su un subpath, sicumante non potremmo piu'
           -- restituire un risultato true, pero' forse potremmo restituire false
           -- invece che aborted (vedremo poi).
           --
           Tmp_Result2 := ABORTED;
           Rec_Depth := C_Depth;
           --
         end if;
         --
       else
         -- BUG BUG QUESTO TEST ANDREBBE FATTO IN UN CICLO A PARTE PRIMA DI PROVARE LA RICORSIONE
         -- Se esiste una transizione con azione non tau e 
         -- non compatibile con ACTION   il risultato della valutazione di 
         -- FORM2 (Tmp_Result) in questo stato diventa il risultato definitivo.
         -- (sia esso false, aborted)
         --
         --  NELLA EXPLANATION PERO' ANDREBBE MESSO LO STATO RAGGIUNTO
         --  DA QUESTA TRANSIZIONE (OPPURE SE NE PUO' FARE A MENO!?!?)
         --  BUG BUG!!!  IN REALTA' NON C'E' NESSUNA SUB_CREF!! ED IL VALORE ATTUALE E'
         --  QUELLO DI FORM1 o QUELLO DI FORM IN UN PATH PRECEDENTE!!!!
         --
         --  Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter)); -- But subcref is randomly the last
         Set_Subcomputation(Cref, FirstCref, No_Evolution);  -- But we lose the info on the wrong transition
         -- We fake a furher final step, which can be identified by observing that evolution action /= act1
         -- Set_Subcomputation(Cref, Cref, Get_Evolution_Data(My_Iter));  -- but messes path explantions ...
         Set_Status(Cref, Tmp_Result);
         --
         Iterator_Finalize (My_Iter);
         Result :=  Tmp_Result;
         return;
       end if; -- evolution satisfies 
     end;
     Iterator_Advance (My_Iter);
    end loop ;
    --
   Iterator_Finalize (My_Iter);
   --
   -- Tmp_Result1=  FOUND_TRUE  TMP_TRUE  ABORTED   (FORM1 now)
   -- Tmp_Result2=  FOUND_TRUE  TMP_TRUE  ABORTED   (FORM recursively)
   --
   -- in the WUntil case, the case of recursive loops back to this state are OK
--   if Tmp_Result2=TMP_TRUE and then
--       Form.PRef.kind = WUntil1 and then
--       Form.Env_Selector.all'Length =0 and then
--       C_Depth <= Rec_Depth then
--       -- TANTO NON SUCCEDE MAI!!!
--       -- se ho un risultato TMP e' perche' la formula ha delle veriaibili di punti fissi
--       -- non ancora risolte in modo definitivo
--     Tmp_Result2 := FOUND_TRUE; 
--   end if;
   -- 
   if  Tmp_Result1=ABORTED or else Tmp_Result2=ABORTED then
     Set_Status(Cref, ABORTED);
     Result :=  ABORTED;
     return;
   elsif Tmp_Result1=TMP_TRUE or Tmp_Result2=TMP_TRUE then
     Result :=  TMP_TRUE;
     Check_Liveness(FromState);
     if Form.PRef.kind = WUntil1 and then
         Tmp_Result2 = TMP_TRUE and then
          Form.Env_Selector.all'Length =0 then
        Set_Status(Cref, Result, Rec_Depth);
     end if;
     Set_Status(Cref, Result);
     return;
   else 
     Set_Status(Cref, FOUND_TRUE);
     Result :=  FOUND_TRUE;
     Check_Liveness(FromState);
     return;
   end if;
   --
end Check_AUntil1;

procedure Check_AWUntil1 ( Form: Formula_Ref;
                     WithOwner: System_Configuration;
                     FromState: System_Configuration;
                     Context: Computations_Table;
                     Cref: Out Computation_Step;
                     Result: Out Computation_Status;
                     N_Depth: Natural;
                     C_Depth: Natural;
                     Rec_Depth: Out Natural) is
   ------------------------------------------------------------------
   --
   --  FORM  =   A [ FORM1 { ACTION } W FORM2 ]
   --
   ------------------------------------------------------------------
   --  FORM = Form, FORM1 = Form.Pref.U1FormRef1, FORM2 = Form.Pref.U1FormRef2
   --
   --  Form = (All,  Form.PRef)
   --  PRef = (WUntil1, U1FormRef1, U1ARef, U1FormRef2)
   --------------------------------------------------
   Tmp_Result2: Computation_Status;
   Tmp_Result1: Computation_Status;
   Tmp_Result: Computation_Status;
   Last_Result: Computation_Status;
   My_Iter: Evolutions_Iterator;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Tmp_Depth: Natural := C_Depth;
 begin
   --
   Rec_Depth := C_Depth;
   --
   --  Guarda se c'e' gia' un risultato riciclabile displonibile.
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --  check for fixed point recursions
    --
    ----------------------------------
    if My_Status = In_PROGRESS then
   ----------------------------------
      Result := FOUND_TRUE;  -- Form.Kind = Wuntil1  !!!!  max fix point
      return;
   ----------------------------------
   elsif My_Status = ABORTED then
   ----------------------------------
      -- Notice that if the Max_LTS_Depth has been encreased since the time the formula was
      -- evaluated and this result saved, it is the Check_Computation itself which return
      -- a new status NOT_YET_STARTED instead of an obsolete ABORTED.
      --
--      if (N_Depth < Tmp_Depth/2 and then N_Depth > 20)  then
--        --
--        -- retry, maybe we are luckier, since we have more depth available
--        -- (but only if we have a minimal additional depth)
--        --
--        Set_Status (Cref, IN_PROGRESS,C_Depth);
--      else
        Result :=  ABORTED;
        return;
--      end if;
   ----------------------------------
   elsif My_Status = FOUND_TRUE or
         My_Status = FOUND_FALSE then
   ----------------------------------
      Result :=  My_Status;
      return;
   ----------------------------------
   elsif (My_Status = TMP_TRUE or
          My_Status = TMP_FALSE)  then
   ----------------------------------
      -- Notice that TMP_XXX values can only be found if the formulas contains fixpoint variables.
      -- (i.e. is part of a more external fixpoint formula), and the nested fixpoint id still
      --  does not have a definitive value.
      -- THIS PART HAS TO BE BETTER ANALYZED
      --
      if (Get_Owner(CRef)=WithOwner or Get_Owner(CRef)=FromState) then
        Result :=  My_Status;
--        if Form.PRef.kind = Until1 and then
--            Result= TMP_False and then
--             Tmp_Depth < Rec_Depth then
--          Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
--        elsif Form.PRef.kind = WUntil1 and then
        if  Result= TMP_TRUE and then
                Tmp_Depth < Rec_Depth then
          Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
        end if;
      else
        -- My_Status= TMP_TRUE / TMP_FALSE unusable leftover from previous completed computations
         Set_Status (Cref, IN_PROGRESS,C_Depth);
      end if;
      return;
   ----------------------------------
   elsif N_Depth > Max_LTS_Depth then
   ----------------------------------
      Set_Status (Cref, ABORTED,N_Depth);
      Result :=  ABORTED;
      return;
   ----------------------------------
   else  -- My_Status /= NOT_YET_STARTED then
   ----------------------------------
      Set_Status (Cref, IN_PROGRESS,C_Depth);
      -- Result will be set by this computation
   ----------------------------------
   end if;
   ----------------------------------

      --
      -- if Form.PRef.U1FormRef2 is true now, this is the definitive result
      --
    Check_True (Form.PRef.U1FormRef2, FromState,Context, SubCref,Tmp_Result,N_Depth,C_Depth+1,Tmp_Depth);
    if Tmp_Result = FOUND_TRUE or Tmp_Result = TMP_TRUE then
      Set_Subcomputation(Cref,SubCref, No_Evolution);
      Set_Status(Cref, Tmp_Result);
      Result :=  Tmp_Result;
      return;
    elsif Tmp_Result /= ABORTED then
      Add_Subcomputation(Cref, SubCref, No_Evolution);
    end if;
   
     --
     -- Se non ci sono stati successivi il valore della condizione finale
     -- diventa il risultato della valutazione di U1FormRef1
     --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   if not Has_System_Transition (My_Iter)  then
     Check_True (Form.PRef.U1FormRef1, FromState,Context, SubCref,Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth);
     Set_Subcomputation(Cref,SubCref, No_Evolution);
     Set_Status(Cref, Tmp_Result1);
     Iterator_Finalize (My_Iter);
     Result :=  Tmp_Result1;
     return;
   end if;
   --
   -- se ci sono discendendenti si valuta FORM1 prima di vedere
   --   se continuare la ricorsione.
    --
    Check_True (Form.PRef.U1FormRef1, FromState,Context, SubCref,Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth);
    --
    -- se FORM1 e' false subito, il risultato della valutazione di FORM2 qui
    --  diventa definitivo.
    --
    if Tmp_Result1 = FOUND_FALSE or else Tmp_Result1=TMP_FALSE then
      Set_Subcomputation(Cref,SubCref, No_Evolution);
      Set_Status(Cref, Tmp_Result1);
      Iterator_Finalize (My_Iter);
      Result :=  Tmp_Result1;
      return;
      --
    elsif Tmp_Result1 /= ABORTED then
      Add_Subcomputation(Cref, SubCref, No_Evolution);
    end if;
    --
    -- se FORM1 e VERA si procede con la valutazione di FORM sui discendenti
    --  (e tutti i risultati sono ancora possibili).
    --
    -- se FORM1 e' ABORTED si procede lo stesso con la valutazione di form
    --  sui discendenti, giusto per vedere se per caso non si possa restituire
    --  invece che un aborted, un risultato FALSE
    --
    -- The possibilities are:
    -- Tmp_Result  (Form2) = Found_False / Tmp_False / Aborted
    -- Tmp_Result1 (Form1) = Found_True  / Tmp_True  / Aborted
    --  se Tmp_Result1 = Aborted non potro piu' restituire True, ma al massimo False
    --
   Tmp_Result2 := FOUND_TRUE;   --  (FORM, recursively on all descendents)
   -- se Tmp_Result2 diventa Aborted non potro piu' restituire True, ma al massimo False
   -- se Tmp_Result2 diventa *_False allora restituisco False
   -- fintanto che Tmp_Result2 rimane *_True continuo ad analizzare evoluzioni
   --
   while Has_System_Transition (My_Iter) loop
      --
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
--       Set_Depth(My_Depth+1,This_Next);
       --
       if Evolution_Satisfies (My_Iter, Form.PRef.U1ARef) or else
            (ACTL_Compatibility and then Evolution_Satisfies_Tau(My_Iter)) then
         --
         -- Se FORM2 e' False, FORM1 e' true, e esiste una transizione tau o
         --  compatibile con ACTION che porta in un stato in cui FORM
         --   e' falsa allora ritorna False.
         --
         Check_AWUntil1 (Form, WithOwner,This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
         --
         --
         if Last_Result = FOUND_FALSE or else
             Last_Result = TMP_FALSE then
           -- se FORM e' falsa per qualche subpath, FORM e' definitivamente falsa.
           --
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Set_Status(Cref, Last_Result,C_Depth);
           Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Iterator_Finalize (My_Iter);
           Result :=  Last_Result;
           return;
           --
         elsif Last_Result = FOUND_TRUE then
           --
           if Tmp_Result2 /= ABORTED then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           end if;
           --
         elsif Last_Result = TMP_TRUE then
           -- se form e' vera' su un subpath, evntualmente raffiniamo Tmp_Result2.
           --
           if  Tmp_Result2= FOUND_TRUE then
             Tmp_Result2 := TMP_TRUE;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             if -- Form.Pref.Kind = WUntil1 and then
                 Form.Env_Selector.all'Length =0 then
               Rec_Depth := Tmp_Depth;
             end if;
           end if;
           if Tmp_Result2 = TMP_TRUE then
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             if -- Form.Pref.Kind = WUntil1 and then
                 Form.Env_Selector.all'Length =0 then
               Rec_Depth := Tmp_Depth;
             end if;
           end if;
           -- if Tmp_Result2 = ABORTED we do nothing.
           --
         elsif Last_Result = ABORTED then
           -- se form e' aborted su un subpath, sicumante non potremmo piu'
           -- restituire un risultato true, pero' forse potremmo restituire false
           -- invece che aborted (vedremo poi).
           --
           Tmp_Result2 := ABORTED;
           Rec_Depth := C_Depth;
           --
         end if;
         --
       else
         -- Se esiste una transizione con azione non tau e
         -- non compatibile con ACTION   il risultato della valutazione di
         -- FORM2 (Tmp_Result) in questo stato diventa il risultato definitivo.
         -- (sia esso false, aborted)
         --
         --  NELLA EXPLANATION PERO' ANDREBBE MESSO LO STATO RAGGIUNTO
         --  DA QUESTA TRANSIZIONE
         --
         Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
         Set_Status(Cref, Tmp_Result);
         Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
         --
         Iterator_Finalize (My_Iter);
         Result :=  Tmp_Result;
         return;
       end if; -- rule satisfies
     end;
    Iterator_Advance (My_Iter);
    end loop ;
    --
   Iterator_Finalize (My_Iter);
   --
   -- Tmp_Result1=  FOUND_TRUE  TMP_TRUE  ABORTED   (FORM1 now)
   -- Tmp_Result2=  FOUND_TRUE  TMP_TRUE  ABORTED   (FORM recursively)
   --
   if  Tmp_Result1=ABORTED or else Tmp_Result2=ABORTED then
     Set_Status(Cref, ABORTED);
     Result :=  ABORTED;
     return;
   elsif Tmp_Result1=TMP_TRUE or Tmp_Result2=TMP_TRUE then
     Result :=  TMP_TRUE;
     Check_Liveness(Fromstate);
     if -- Form.PRef.kind = WUntil1 and then
         Tmp_Result2 = TMP_TRUE and then
          Form.Env_Selector.all'Length =0 then
        Set_Status(Cref, Result, Rec_Depth);
     end if;
     Set_Status(Cref, Result);
     return;
   else
     Set_Status(Cref, FOUND_TRUE);
     Result :=  FOUND_TRUE;
     Check_Liveness(Fromstate);
     return;
   end if;
   --
end Check_AWUntil1;


procedure Check_EUntil1 ( Form: Formula_Ref; 
                      WithOwner: System_Configuration;
                      FromState: System_Configuration;
                      Context: Computations_Table;
                      Cref: Out Computation_Step;
                      Result: Out Computation_Status;
                      N_Depth: Natural;
                      C_Depth: Natural;
                      Rec_Depth: out Natural) is
   ------------------------------------------------------------------
   -- EUNtil1  simile a EF   (recloop false)
   -- EWntil1  simile a EG   (recloop true)
   ------------------------------------------------------------------
   --
   --  FORM  =   E [ FORM1 { ACTION } U FORM2 ] 
   --
   --  e' equivalente a:
   --  min Z :   FORM2 | ( FORM1 & 
   --                          ( EX{ACTION} Z) | 
   --                            ET Z ) )
   ------------------------------------------------------------------
   --
   -- Se FORM2 e' True in questo stato, ritorna subito True.
   --
   -- Se FORM2 e' False, e anche FORM1 e' False, ritorna subito False.
   --
   -- Se FORM2 e' False, FORM1 e' true, e esiste una transizione con azione 
   --    tau o compatibile con ACTION  che porta un stato in cui FORM e' vera 
   --    allora ritorna True.  (in caso di cicli infiniti pero' ritorna False).
   --
   -- Altrimenti ritorna False.
   --
   -- (Nota: Se FORM2 e' False e lo stato e' finale ritorna False)
   ------------------------------------------------------------------
   -- Esiste un path etichettato solo con "tau" o con una azione compatibile 
   -- con ACTION che porta in uno stato in cui vale "EX{ACTION} FORM2" (il path 
   -- puo' anche essere lungo 0). In tutti gli stati intermedi attraversati 
   -- vale "FORM1".
   ------------------------------------------------------------------
   --  FORM = Form, FORM1 = Form.Pref.U1FormRef1, FORM2 = Form.Pref.U1FormRef2
   --
   --  Form = (Exist,  Form.PRef)
   --  PRef = (Until1, U1FormRef1, U1ARef, U1FormRef2)
   ------------------------------------------------------------------
     -- REC Y : U1FormRef2 or (U1FormRef1 and (EX{act} Y) or (ET Y))
   ------------------------------------------------------------------
   Tmp_Result: Computation_Status;
   Tmp_Result1: Computation_Status;
   Tmp_Result2: Computation_Status;
   Last_Result: Computation_Status;
   SubCref: Computation_Step;
   My_Iter: Evolutions_Iterator;
   My_Status: Computation_Status;
--   My_Depth: Natural := Get_Depth(FromState);
   Tmp_Depth: Natural := C_Depth;
 begin
   --
   Rec_Depth := C_Depth;
   --
   -- controlliamo se questa computatione e' gia' stata fatta o e' gia
   -- in corso ...
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --
    --  check for fixed point recursions
    --
   if My_Status = IN_PROGRESS then
     if Form.PRef.Kind = Until1 then
       Result :=  FOUND_FALSE;         ---this is a  minimum fixed point   -- like   EF FORM
       Rec_Depth := Tmp_Depth;      -- THE VALUE STORED WITH THE BEGINING OF THE COMPUTATION
       return;
     else   --     Form.Pref.Kind=Wuntil1  --  this is a max fixpoint        -- like  EG FORM
       Result :=  FOUND_TRUE;         --  in case of loop the result is definitively true
       return;
     end if;
--   elsif My_Status /= NOT_YET_STARTED then
   elsif (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
         (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
     -- we already have a previous definitive or still temporary value.
     --  it can be ABORTED /FOUND_TRUE / FOUND_FALSE
     --    and possibly TMP_TRUE /TMP_FALSE if env_selector.all'length >0  (DEFINITVE)
     --    but also TMP_FALSE if the subcomputation has been completed but
     --      the main recursion is still in progress.
      Result :=  My_Status;
      if Result= TMP_FALSE and then
         Tmp_Depth < Rec_Depth and then
          Form.Env_Selector'Length =0 then
       Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
      end if;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   else
      Set_Status (Cref, IN_PROGRESS);
   end if;
   -- se non e' gia' in corso ne gia' fatta iniziano le danze.
   --
   -- Se FORM2 e' True in questo stato, ritorna subito True.
   --
    Check_True (Form.PRef.U1FormRef2, FromState,Context, SubCref, Tmp_Result2,N_Depth,C_Depth+1,Tmp_Depth); 
    --
    if Tmp_Result2 = FOUND_TRUE             -- Form2 defintely true
       or else Tmp_Result2 = TMP_TRUE then  -- Env_selector'Length >0
      Set_Status(Cref, Tmp_Result2);
      Set_Subcomputation(Cref, SubCref, No_Evolution);
      Result :=  Tmp_Result2;
      return;
    elsif Tmp_Result2 /= ABORTED then
      Add_Subcomputation(Cref, SubCref, No_Evolution);
    end if;
    --
    -- se FORM2 e False o aborted bisogna guardare i discendenti e la ricorsione
    --  su di essi.
    --
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   --
   -- Form2 puo' essere *_FALSE, ABORTED
   -- Form1 non e' stato ancora valutato
   --
   if not Has_System_Transition (My_Iter)  then
     if Form.PRef.kind = Until1 then
       -- se non ci sono discendenti il risultato di FORM2 adesso diventa
       --  definitivo (Tmp_Result2).
       Set_Status(Cref, Tmp_Result2);
       Iterator_Finalize (My_Iter);
       Result :=  Tmp_Result2;
       return;
     else   -- Form.PRef.Kind = Wuntil1  !!!!
       -- se non ci sono discendenti la valutatione di form1 in questo stato
       -- diventa il risultato definitivo.
       --
--     if Form.PRef.U1FormRef1.Kind=Ftrue then
--         Tmp_Result1 := FOUND_TRUE;
--       else
         Check_True (Form.PRef.U1FormRef1, FromState,Context, SubCref,Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth);
         Add_Subcomputation(Cref,SubCref, No_Evolution);
         Set_Status(Cref, Tmp_Result1);
         Set_Subcomputation(Cref,SubCref, No_Evolution);
--       end if;
       Iterator_Finalize (My_Iter);
--       Set_Status(Cref, Tmp_Result1);
       Result :=  Tmp_Result1;
       return;
     end if;
   end if;
    --
    -- se ci sono discendenti iniziamo con la valutazione di FORM1
    --
    Check_True(Form.PRef.U1FormRef1,FromState,Context,SubCref, Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth); 
    --
    -- se FORM1 in queso stato ' false, allora il risultato di form2 in 
    -- questo stato (gia noto: *_FALSE o ABORTED) diventa definitivo.
    -- Form2= Tmp_Result2 = *_FALSE o ABORTED
    -- Form1= Tmp_Result1 = *_TRUE, *_FALSE o ABORTED
    --
    if (Tmp_Result1 = FOUND_FALSE or Tmp_Result1 = TMP_FALSE) then
      --  se Form1 e' False la valutazione termina qui.
      --  Tmp_Result1 = FOUND_FALSE, Tmp_Result2 = ABORTED -> return Tmp_Result2 (ABORTED)
      --  Tmp_Result1 = FOUND_FALSE, Tmp_Result2 = *_FALSE -> return Tmp_Result2 (*_FALSE)
      --  Tmp_Result1 = TMP_FALSE, Tmp_Result2 =TMP_FALSE -> return Tmp_Result2 (TMP_FALSE)
      --  Tmp_Result1 = TMP_FALSE, Tmp_Result2 =FOUND_FALSE -> return TMP_FALSE
      --  Tmp_Result1 = TMP_FALSE, Tmp_Result2 =ABORTED -> return Tmp_Result2 (ABORTED)
      --  Tmp_Result1 = ABORTED, Tmp_Result2 =*_FALSE -> return Tmp_Result2 (*_FALSE)
      --  Tmp_Result1 = ABORTED, Tmp_Result2 = ABORTED -> return Tmp_Result2 (ABORTED)
      --
      if Tmp_Result2=FOUND_FALSE and Tmp_Result1 =TMP_FALSE then
         Tmp_Result2 := TMP_FALSE;
      end if;
      Add_Subcomputation(Cref, SubCref, No_Evolution);
      Set_Status(Cref, Tmp_Result2);
      Set_Subcomputation(Cref, SubCref, No_Evolution);   -- necessaria !?!?!?!
      Iterator_Finalize (My_Iter);
      Result :=   Tmp_Result2;  -- ABORTED , TMP_FALSE, FOUND_FALSE
      return;
    elsif Tmp_Result1 /= ABORTED then
      Add_Subcomputation(Cref, SubCref, No_Evolution);
    end if;
   --
   -- se FORM1 e' vera o aborted, andiamo a vedere cosa succede ricorsivasmnte
   -- nei discendenti.
   -- se FORM1 e' vera tutti i risultati sono ancora possibili
   -- se FORM1 e' aborted osserviamo i discendenti sono per vedere se per caso
   --  non sia possibile restiture un false invece che un aborted.
   -- Form2= Tmp_Result2 = *_FALSE o ABORTED
   -- Form1= Tmp_Result1 = *_TRUE, o ABORTED
   --   
   Tmp_Result := FOUND_FALSE;
   --
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
--         Set_Depth(My_Depth+1,This_Next);
         if Evolution_Satisfies (My_Iter, Form.PRef.U1ARef) or else
            (ACTL_Compatibility and then Evolution_Satisfies_Tau(My_Iter)) then
           --
           -- guardiamo come si comporta con la ricorsione ... 
           Check_EUntil1(Form, WithOwner, This_Next,Context, SubCref, Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
           --
           -- se esiste un risultato *_TRUE (della valutazione di form su un discendete)
           -- allora possiamo gia' dare un risultato conclusivo.
           -- In particolare, se FORM1 era true possiamo dire che la computazione era true
           --  se FORM1 era aborted possiamo dire che il risultato e' aborted (non potendo piu'
           --  sperare di sostituirlo con un risultato definitivo FALSE).
           --
           -- Nota: Last_Result = TMP_FALSE in caso di recloop (Until1)
           -- Nota: Last_Result = FOUND_TRUE in caso di recloop (Wuntil1)
           -- 
           if Last_Result=TMP_TRUE and then
              Form.Env_selector'Length =0 and then
              Form.PRef.Kind=Wuntil1 and then
              C_Depth <= Tmp_Depth then
              Last_Result := FOUND_TRUE;
           end if;
           if (Last_Result = FOUND_TRUE  or -- recursion definitely true  (WUntil)
                Last_Result = TMP_TRUE) and -- Env_selector'Length >0 
               Tmp_Result1= TMP_TRUE then    -- Env_selector'Length >0
              Rec_Depth := C_Depth;     -- we can forget of fixpoints
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status (Cref, TMP_TRUE,C_Depth);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter)); -- neceaaria !?!?!?!
              -- PERDO TUTTE LE SUBCOMPUTATIONS RELATIVE A Form1 !!!!
              Iterator_Finalize (My_Iter);
              Result :=   TMP_TRUE;
              return;
              --
           elsif Last_Result = TMP_TRUE and   -- Env_selector'Length >0 
                 (Tmp_Result2= TMP_TRUE  or Tmp_Result1= FOUND_TRUE) then
              Rec_Depth := C_Depth;     -- we can forget of fixpoints
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status (Cref, TMP_TRUE,C_Depth);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Iterator_Finalize (My_Iter);
              Result :=   TMP_TRUE;
              return;
              --
           elsif Last_Result = FOUND_TRUE  -- recursion definitely true  (WUntil)
                  and Tmp_Result1= FOUND_TRUE then
              Rec_Depth := C_Depth;     -- we can forget of fixpoints
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
              Set_Status (Cref, FOUND_TRUE,C_Depth);
              Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter)); -- necessario?!?!?!
              -- PERDO TUTTE LE SUBCOMPUTATIONS RELATIVE A Form1 !!!!
              Iterator_Finalize (My_Iter);
              Result :=  FOUND_TRUE;
              return;
              --
           elsif (Last_Result = FOUND_TRUE or Last_Result = TMP_TRUE) and then
              Tmp_Result1= ABORTED then
              Set_Status (Cref, ABORTED);
              Iterator_Finalize (My_Iter);
              Result :=  ABORTED;
              return;
           -- 
           --  se la ricorsione ritorna TMP_FALSE e Form.Kind=Until1  il risultato
           --   potrebbe essere temporaneo e quiandi bisogna settare il Rec_Depth giusto
           elsif Last_Result = TMP_FALSE and Tmp_Result=FOUND_FALSE then
             Tmp_Result := TMP_FALSE;
             if Form.PRef.Kind=Until1 and Tmp_Depth < Rec_Depth then
               Rec_Depth := Tmp_Depth;
             end if;
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           --
           elsif Last_Result = ABORTED then
              Tmp_Result := ABORTED;
               Rec_Depth := C_Depth; 
              --
           else -- Last_result=FOUND_FALSE
              Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           end if;
       end if;
     end;
     Iterator_Advance (My_Iter);
   end loop ;
    --
   Iterator_Finalize (My_Iter);
   --
   -- a questo punto abbiamo che:
   --
   -- Tmp_Result2 = ABORTED, tmp_FALSE found_FALSE (valutazione di form2 qui)
   -- Tmp_Result1 = ABORTED  tmp_TRUE found_TRUE   (valutazione di form1 qui)
   -- Tmp_Result  = ABORTED, tmp_FALSE, found_FALSE (valutazione di form sui discendenti).
   --
   -- se per tutti i discendenti FORM e' false, indipendentemente dagli aborted risults di
   -- form2 qui e form1, il risulato e' false.
   --
   if Tmp_Result /= ABORTED then
     if WithOwner = FromState and then
        Form.Env_Selector.all'Length =0 and then
        Tmp_Result = TMP_FALSE then 
        Tmp_Result := FOUND_FALSE;
     end if;
     if Form.PRef.Kind=Until1 and then
         C_Depth <= Rec_Depth and then
          Form.Env_Selector.all'Length =0 then
--       if Tmp_Result = TMP_FALSE then -- (hence Wuntil1)
--         Tmp_Result := FOUND_FALSE;
--       end if;
       Set_Status(Cref, Tmp_Result,Rec_Depth);
     else
       Set_Status(Cref, Tmp_Result,C_Depth);
     end if;
     Result :=  Tmp_Result;
     return;
   --
   -- altrimenti il risultato e' aborted.
   -- (eventuai risultati true li avrei gia' segnalati appena incontrati)
   --
   else
     Set_Status(Cref,ABORTED);
     Result :=  ABORTED;
      return;
   end if;
   --
end Check_EUntil1;



procedure Check_EUntil2 ( Form: Formula_Ref; 
                       WithOwner: System_Configuration;
                       FromState: System_Configuration;
                       Context: Computations_Table;
                       Cref: Out Computation_Step;
                       Result: Out Computation_Status;
                       N_Depth: Natural;
                       C_Depth: Natural;
                       Rec_Depth: out Natural) is
     -----------------------------------------------------------------
     -- EUntil2  simile a EF    (recloop false)
     -- EWntil2 simile a EG    (recloop true)
     -----------------------------------------------------------------
     --  FORM = E [ FORM1 { ACTION1 } U { ACTION2 } FORM2 ]
     --
     --  e' equivalente a:
     --  min Z : FORM1  and
     --          ( EX{ACTION2} FORM2    or
     --            EX {ACTION1}  Z   or 
     --            ET Z)
     -----------------------------------------------------------------
     -- Se FORM1 e' false ritorna False.
     --
     -- Se FORM1 e' True, ed e' possiibile fare un azione compatibile 
     --  con ACTION2 che porti in uno stato in cui valga FORM2 ritorna True.
     --
     -- Se FORM1 e' True, ed e' possibile fare un azione tau o una azione
     -- compatibile con ACTION1 che porti in uno stato in cui valga FORM 
     -- ritorna true. Altrimenti ritorna False.
     -- 
     -----------------------------------------------------------------
     --  Esiste un path etichettato solo con "tau" o con azioni compatibili 
     --  con ACTION che porta in uno stato in cui vale "EX{ACTION} FORM2" 
     --  (il path puo' anche essere lungo 0). In tutti gli stati intermedi 
     --  attraversati vale "FORM1".
     -----------------------------------------------------------------
     --  FORM = Form, FORM1 = Form.PRef.U2FormRef1, FORM2 = Form.PRef.U2FormRef2
     --
     --  Form = (Exist,  Form.PRef)
     --  PRef = (Until2, U2FormRef1, U2ARef1,U2ARef2, U2FormRef2)
     -----------------------------------------------------------------
   Tmp_Result: Computation_Status;
   Tmp_Result1: Computation_Status;
   Tmp_Result2: Computation_Status;
   Last_Result: Computation_Status;
   SubCref1: Computation_Step;
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   -- My_Iter: Evolutions_Iterator;
   Truncated: Boolean := False;
--   My_Depth: Natural := Get_Depth(FromState);
   Tmp_Depth: Natural := C_Depth;
 begin
   --
   Rec_Depth := C_Depth;
   --
   -- controlliamo se questa computatione e' gia' stata fatta o e' gia
   -- in corso ...
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --
    --  check for fixed point recursions
    --
   if My_Status = IN_PROGRESS then
      if Form.PRef.Kind = Until2 then
       Result :=  FOUND_FALSE;        -- this is a  minimum fixed point  like EF Form
       Rec_Depth := Tmp_Depth;      -- THE VALUE STORED WITH THE BEGINING OF THE COMPUTATION
       return;
      else   --     Form.Kind=Wuntil2
       Result :=  FOUND_TRUE;         -- this is a  maximum fixed point   like EG Form
       return;
      end if;
--   elsif My_Status /= NOT_YET_STARTED then
   elsif (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
         (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
     -- we already have a previous definitive or still temporary value.
     --  it can be ABORTED /FOUND_TRUE / FOUND_FALSE
     --    and possibly TMP_TRUE /TMP_FALSE if env_selector.all'length >0  (DEFINITVE)
     --    but also TMP_FALSE if the subcomputation has been completed but
     --      the main recursion is still in progress.
      Result :=  My_Status;
      if Form.PRef.Kind = Until2  and then
         Result= TMP_FALSE and then
         Tmp_Depth < Rec_Depth and then
          Form.Env_Selector'Length =0 then
        Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
      end if;
      return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   else
      Set_Status (Cref, IN_PROGRESS);
   end if;
   -- se non e' gia' in corso se ne inizia la valutazione
   --
   -- iniziamo con la valutazione di Form1, che deve in ogni caso essere vera.
   -- if Form.PRef.U2FormRef1.Kind /= Ftrue then  
   -- (OTTIMIZZAZIONE RIMOSSA PER NON COMPICARE LE EXPLANATIONS)
      Check_True (Form.PRef.U2FormRef1, FromState,Context, SubCref1,Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth); 
      --
      if Tmp_Result1 = FOUND_FALSE or Tmp_Result1=TMP_FALSE then
        Add_Subcomputation(Cref, SubCref1, No_Evolution);
        Set_Status(Cref, Tmp_Result1,C_Depth);
        Set_Subcomputation(Cref, SubCref1, No_Evolution);
        Result :=  Tmp_Result1;
        return;
      elsif Tmp_Result1 /= ABORTED then
        -- Se FORM1 e' true procediamo normalmente.
        -- Se FORM1 e' aborted possiamo al piu continuare a vedere se
        -- caso fosse possibile valutare l'intera formula come false.
        -- (invece che come aborted)
        Add_Subcomputation(Cref, SubCref1, No_Evolution);
      end if;
   --  else    
       -- se  Form1 = "True" si evita la valutazione e si restituisce subito FOUND_TRUE
       Tmp_Result1 := FOUND_TRUE;
    -- end if;
     --
     -- Se FORM1 e' *_TRUE, ed e' possiibile fare un azione compatibile 
     --  con ACTION2 che porti in uno stato in cui valga FORM2 ritorna True.
     ---
     -- Tmp_Result1 = ABORTED,  TMP_TRUE,  FOUND_TRUE
     --
   declare
     My_Iter: Evolutions_Iterator;
   begin
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   --
   if not Has_System_Transition (My_Iter)  then
     if Form.PRef.kind = Until2 then   -- EF like
       -- se non ci sono discendenti il risultato di Until2 e' definitivamente FALSE
       Set_Status(Cref, FOUND_FALSE);
       Iterator_Finalize (My_Iter);
       Result :=  FOUND_FALSE;
       return;
     else   -- Form.Kind = Wuntil1  -- EG like
       -- se non ci sono discendenti il risultato di Wuntil2 e' quello di Form1
       Set_Status(Cref, Tmp_Result1);
       Iterator_Finalize (My_Iter);
       Result :=  Tmp_Result1;
       return;
     end if;
   end if;
   --
   Tmp_Result := Tmp_Result1; -- ABORTED,  TMP_TRUE,  FOUND_TRUE
   Tmp_Result2 := FOUND_FALSE;
   --  FIRST CYCLE -- if possible we check for an immediate successfull completion
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings: AllBindings := Match(My_Iter, Form.PRef.U2ARef2);
       NewFormula: Formula_Ref;
     begin
--       Set_Depth(My_Depth+1,This_Next);
       --
       if Evolution_Satisfies (My_Iter, Form.PRef.U2ARef2) then
         --
         if TheBindings.VarsCount = 0 then
           Check_True(Form.PRef.U2FormRef2,This_Next,Context, SubCref,Last_Result,N_Depth,C_Depth+1,Tmp_Depth);
          -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
          -- THEREFORE WE MIGHT OMIT THEM  (!!! FALSE)
           if Last_Result = FOUND_TRUE or Last_Result=TMP_TRUE then
             -- se Form2  e'  *_TRUE la comp√utazione in ogni caso termina
             if Tmp_Result1 = ABORTED then
               -- se Tmp_Result1 = ABORTED un Last_Result =_TRUE elimina la possibilita'
               -- di restituire FALSE anziche ABORTED, quindi ABORTED diventa subito DEFINITIVO
                Set_Status(Cref, ABORTED);
                Iterator_Finalize (My_Iter);
                Result :=  ABORTED;
                return;
                --
             else -- Tmp_Result1 = true
               if  Tmp_Result1 = FOUND_TRUE then
                  Tmp_Result2 := Last_Result;
               end if;
                -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
                -- THEREFORE WE MIGHT OMIT THEM  (FALSE)
                --
                -- DOBBIAMO ELIMINARE LE FALSE SUBCOMPUATIONS  GIA INCONTRATE LASCIANDO SOLO
                -- LA TRUE SUBCOMPUTATION RELATIVA A FROM1 + QUESTA FORM2. (O NO????!!!!??
                --
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Tmp_Result2,C_Depth);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               --Set_Subcomputation(Cref, SubCref1, No_Evolution);
               --Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               --Set_Status(Cref, Tmp_Result2,C_Depth);
               Iterator_Finalize (My_Iter);
               Result :=   Tmp_Result2;
               return;
             end if;
           elsif Last_Result = ABORTED then
             -- se Form2 e' aborted ... dobbiamo continuare solo se FORM1 = *_TRUE
             if Tmp_Result1=ABORTED then
                --  se Form1 = ABORTED non esiste piu la possibilita' di restituire FALSE
                --   quindi si termina qui.
                Set_Status(Cref, ABORTED);
                Iterator_Finalize (My_Iter);
                Result :=   ABORTED;
                return;
             else
               -- se FORM1 = *_TRUE  si continua ...
               Tmp_Result2 := ABORTED;
             end if;
           elsif Last_Result=TMP_FALSE and Tmp_Result2=FOUND_FALSE then
             Tmp_Result2 := TMP_FALSE;
              -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
              -- THEREFORE WE MIGHT OMIT THEM
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           else
             -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
             -- THEREFORE WE MIGHT OMIT THEM
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           end if;
         else
           for B in 1.. TheBindings.BindCount loop
             NewFormula :=
                NewInstance(Form.PRef.U2FormRef2,(TheBindings.VarsCount,
                                                  TheBindings.VarNames,
                                                  TheBindings.AllValues(B).VarValues));
             Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth,C_Depth+1,Tmp_Depth);
             --
             if Last_Result = FOUND_TRUE or Last_Result=TMP_TRUE then
               -- una subcomputation e' TRUE.
               -- se Form2  e'  *_TRUE la computazione in ogni caso termina
               -- se Tmp_Result1=Aborted allora l'intero risultato e' aborted.
               --
               if Tmp_Result1=ABORTED then
                 Set_Status(Cref, ABORTED);
                 Iterator_Finalize (My_Iter);
                 Result := ABORTED;
                 return;
                 --
               else -- Tmp_Result1=true
                 -- se anche tmp_result= true allora si puo' restituire
                 -- il risultato definitivo true (raffinato)
                 --
                 if  Tmp_Result1 = FOUND_TRUE then
                    Tmp_Result2 := Last_Result;
                 end if;
                  -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
                  -- THEREFORE WE MIGHT OMIT THEM
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 Set_Status(Cref, Tmp_Result2,C_Depth);
                 Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 Iterator_Finalize (My_Iter);
                 Result :=   Tmp_Result2;
                 return;
               end if;
               --
             elsif Last_Result = ABORTED then
               -- se la valutazione di {b} FORM2 e' aborted
               --   ... dobbiamo continuare solo se FORM1 = *_TRUE
               if Tmp_Result=ABORTED then
                 --
                 -- se anche Tmp_result era aborted, allora
                 -- aborted e' il risultato definitivo, perche non
                 -- puo' essere piu' ne true ne false.
                 --
                 Set_Status(Cref, ABORTED);
                     Iterator_Finalize (My_Iter);
                    Result :=   ABORTED;
                    return;
               else
                 --
                 -- se tmp_result era true, questo caso di aborted
                 -- viene registrato, ma la analisi dei discendenti continua.
                 --
                 Tmp_Result2 := ABORTED;
               end if;
               --
             elsif Last_Result=TMP_FALSE and Tmp_Result2=FOUND_FALSE then
               -- Se il risultato e' false si continua ...
               -- raffinando il riassundo delle subcompuations se necessario
               Tmp_Result2 := TMP_FALSE;
               -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
               -- THEREFORE WE MIGHT OMIT THEM
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             --
             else
               -- SUBCOMPUTATIONS ACTUALLY UNUSED  NEITHER FOR EXPLANATIONS NOR FOR RECURSION
               -- THEREFORE WE MIGHT OMIT THEM
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             --
             end if;
           end loop;   -- bindings loop
         end if;   -- varscount=0
           --
       end if;  -- Evolution Satisfies 
     end;
     Iterator_Advance (My_Iter);
   end loop ;
    Iterator_Finalize(My_Iter);
   end;
   --
   -- A questo punto abbiamo che:
   --
   -- Tmp-Result1 = ABORTED e  Tmp_Result2 = *_FALSE
   -- Tmp_Result1 = *_TRUE  e  Tmp_Result2= ABORTED or *_FALSE
   --
   -- Se FORM1 e' True, ed e' possibile fare un azione tau o una azione
   -- compatibile con ACTION1 che porti in uno stato in cui valga FORM 
   -- ritorna true. Altrimenti ritorna False.
   -- 
   -- guardiamo adesso come si comporta la ricorsione ....
   --
   Tmp_Result := FOUND_FALSE;
   declare
     My_Iter: Evolutions_Iterator;
   begin
--   Iterator_Restart(My_Iter);
   Iterator_Initialize (My_Iter, System_Configuration(FromState));
   while Has_System_Transition (My_Iter) loop
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
     begin
--       Set_Depth(My_Depth+1,This_Next);
       if Form.PRef.U2ARef1=null or else
          Form.PRef.U2ARef1.all =  True_Action.all or else
           Evolution_Satisfies (My_Iter, Form.PRef.U2ARef1) or else
           (ACTL_Compatibility and then Evolution_Satisfies_Tau(My_Iter)) then
         -- 
         Check_EUntil2 (Form, WithOwner, This_Next,Context, SubCref,Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
         if  Form.PRef.Kind=WUntil2 and then
              Last_Result=TMP_TRUE and then
              C_Depth <= Tmp_Depth and then
              Form.Env_Selector.all'Length =0 then
            Last_Result := FOUND_TRUE;
         end if;
         --
         if Form.PRef.Kind=Until2 and then
            Tmp_Depth < Rec_Depth and then
              Form.Env_Selector.all'Length =0 and then
               Last_Result = TMP_FALSE then
            Rec_Depth := Tmp_Depth;
         end if;
         --
         if Last_Result = FOUND_TRUE or Last_Result=TMP_TRUE then
           -- una subcomputation e' TRUE. 
           -- se Tmp_Result1=Aborted allore il risultato
           -- e da considerare aborted non potendo piu' essere FALSE
           if Tmp_Result1=ABORTED or Tmp_Result2=ABORTED then
             Set_Status(Cref, ABORTED);
             Iterator_Finalize (My_Iter);
             Result :=   ABORTED;
             return;
           else -- Tmp_Result1= *_TRUE
             -- se anche tmp_result1= true allora si puo' restituire
             -- il risultato definitivo true (raffinato)
             Result :=  Tmp_Result1;
             if Tmp_Result1 = FOUND_TRUE then
                Result := Last_Result;
             end if;
--             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
--             Set_Status(Cref, Result);
--             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
             Set_Status(Cref, Result);
             Iterator_Finalize (My_Iter);
             return;
           end if;
           --
         elsif Last_Result = ABORTED then
           -- se la valutazione di {b} FORM2 e' aborted
           if Tmp_Result1=ABORTED then
             -- se anche Tmp_result1 era aborted, allora
             -- aborted e' il risultato definitivo, perche non
             -- puo' essere piu' ne true ne false.
             Set_Status(Cref, ABORTED);
             Iterator_Finalize (My_Iter);
             Result :=   ABORTED;
             return;
           else
             -- se tmp_result era true, questo caso di aborted
             -- viene registrato, ma la analisi dei discendenti continua.
             Tmp_Result := ABORTED;
           end if;
           --
         else   --  Last_Result=*_FALSE 
           -- Se il risultato e' false si continua ...
           -- raffinando il riassunto delle subcompuations se necessario
           if Last_Result=TMP_FALSE and Tmp_Result=FOUND_FALSE  then 
             Tmp_Result := TMP_FALSE;
           end if;
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           --
         end if;
       end if; -- evolution satisfies
     end;
     Iterator_Advance (My_Iter);
   end loop ;
   --
   Iterator_Finalize (My_Iter);
   end;
   --
   --      FORM1                 FORM2                         RICORSIONE
   -- Tmp-Result1=ABORTED e  Tmp_Result2=*_FALSE          e  Tmp_Result=*_FALSE 
   -- Tmp_Result1=TRUE    e  Tmp_Result2=ABORTED or FALSE e  Tmp_Result=ABORTED or FALSE
   --
--   if Form.PRef.Kind=Until2 and then WithOwner = FromState and then
--       Form.Env_Selector.all'Length =0 and then
--         Tmp_Result = TMP_FALSE then
--     Tmp_Result := FOUND_FALSE;
--   end if;
--   if Form.PRef.Kind=Until2 and then
--       C_Depth <= Rec_Depth and then  -- actually, at most "="
--          Form.Env_Selector.all'Length =0 then
--     --  Rec_Depth := C_Depth;   -- actually useless ... 
--     if  Tmp_Result = TMP_FALSE then
--       Tmp_Result := FOUND_FALSE;
--     end if;
--   end if;
   --
   if Tmp_Result2 /= ABORTED and then Tmp_Result /= ABORTED  then
     -- l' ABORTED INIZIALE di Form1 PUO' ESSERE SOSTITUITO DA UN *_FALSE
     -- il *_TRUE INIZIALE e' sostituito dai false successivi
     Result := Tmp_Result;   -- il risultato della ricorsione (*_FALSE)
     if Result=FOUND_FALSE then
       Result := Tmp_Result2;
     end if;
     Set_Status(Cref, Result,Rec_Depth);
     --
   else -- (Tmp_Result2 = ABORTED or  Tmp_Result = ABORTED)  then
     Set_Status(Cref,ABORTED);
     Result :=  ABORTED;
     return;
   end if;
end Check_EUntil2;


procedure Check_AUntil2 ( Form: Formula_Ref; 
                      WithOwner: System_Configuration;
                      FromState: System_Configuration;
                      Context: Computations_Table;
                      Cref: Out Computation_Step;
                      Result: Out Computation_Status;
                      N_Depth: Natural;
                      C_Depth: Natural;
                      Rec_Depth: out Natural) is
     -----------------------------------------------------------------
     --  FORM = A [ FORM1 { ACTION1 } U { ACTION2 } FORM2 ]
     --
     --  e' equivalente a:
     --    min Z:  FORM1 &
     --               ~( EX { ACTION1 & ~ ACTION2 } ~Z ) &
     --               ~( ET  ~Z ) &
     --               ~( EX { ACTION2 & ~ ACTION1 } ~FORM2 ) &
     --               ~( EX { ACTION2 & ACTION1 } ~( Z or FORM2 ) )
     --               ~ FINAL  !!!!!!!!!
     --           deve fare act1, act2, o tau,  e  BASTA ..
     -----------------------------------------------------------------
     
     ----------------------------------------------------------
     --  1)  Se FORM1 e' False ritorna False.
     --
     --  2)  Se e' FINALE  si restituisce TRUE(Wuntil1)  o FALSE (Until2)
     --        Altrimenti per ogni transizione uscente
     --  
     --  3)  Se una transizione soddisfa (not A1 and not A2) si restituisce FALSE
     --
     --  4)  Se una transizione soddisfa (not A1 and A2 ), il target DEVE soddisfare FORM2 
     --
     --  5a) se soddisfa (A1 and not A2), il target DEVE soddisfare ricorsivamentre FORM.
     --  5b) Se una transizione soddisfa (A1 and A2) si pre-valutvaluta FORM2 nel target 
     --  5c)    e se FORM2=false DEVE valere ricorsivamente FORM nel target.
     --
     --  6) se tutte le transizioni uscenti sono OK si restituisce TRUE
     --
     --  nota:  andrebbero PRIMA DI TUTTO verificate le consizioni 3a,3b.
     --         poi la soddisfazione di di FORM2/FORM nei casi 4a/4b/4c
     -- se non si fa' così in caso di sistemi grossi/infiniti non si garantisce il risultato
     --   bounded quando il realtà esso esiste,
     ----------------------------------------------------------
     --  Se FORM1 e' False ritorna False.
     --
     --  Se e' possible fare una azione tau, o una azione compatibile
     --   con ACTION1 & ~ ACTION2, che porta in uno stato in cui FORM e'
     --    False ritorna False.
     --
     --  Se e' possible fare una azione compatibile con ACTION2 & ~ ACTION1, 
     --  che porta in uno stato in cui FORM2 e' False ritorna False.
     --
     --  Se e' possible fare una azione compatibile con ACTION2 & ~ ACTION1,
     --  che porta in uno stato in cui non vale ne FORM2 ne FORM ritorna False.
     --
     --  Se e' possibile fare altri tipi di azioni (non tau e incompatibili 
     --  con ACTION1 e ACTION2)  ritorna False.
     --
     --  Altrimenti ritorna True.
     -----------------------------------------------------------------
     -- Ogni path massimale contiene uno stato in cui vale "EX{ACTION2}FORM2",
     -- e tale stato viene raggiunto attraverso una sequenza (eventualmente
     -- vuota) di "tau" o azioni compatibili con ACTION1 e attraversando stati 
     -- in cui vale sempre "FORM1".  
     -----------------------------------------------------------------
     --  FORM = Form, FORM1 = Form.PRef.U2FormRef1, FORM2 = Form.PRef.U2FormRef2
     -- 
     --  Form = (FAll,  Form.PRef)
     --  PRef = (Until2, U2FormRef1, U2ARef1,U2ARef2, U2FormRef2,
     --            A1andnota2, A2andnota1, A1anda2, YorF2)
     -----------------------------------------------------------------
   Tmp_Result: Computation_Status;
   Tmp_Result1: Computation_Status;
   Tmp_Result2: Computation_Status;
   Last_Result: Computation_Status;
   Is_Final: Boolean := True;
   My_Iter: Evolutions_Iterator; 
   SubCref: Computation_Step;
   My_Status: Computation_Status;
   Truncated: Boolean := False;
--   My_Depth: Natural := Get_Depth(FromState);
   Tmp_Depth: Natural := C_Depth;
 begin
   --
   Rec_Depth := C_Depth;
   --
   Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
    --
    --  check for fixed point recursions
    --
    --
   if My_Status = IN_PROGRESS then
      if Form.PRef.Kind = Until2 then
        Result :=  FOUND_FALSE;    -- this is a  minimum fixed point  -- Like AF
        return;
      else   --     Form.PRef.Kind=Wuntil2
        Result :=  FOUND_TRUE;     -- this is a  maximum fixed point   -- Like AG
        Rec_Depth := Tmp_Depth;  -- THE VALUE STORED WITH THE BEGINING OF THE COMPUTATION
        return;
      end if;
   elsif (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
         (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
     -- TMP_TRUE  recursion loopback
     --  TMP_FALSE /FOUND_TRUE / FOUND_FALSE /ABORTED other ...
     Result :=  My_Status;
     if Result= TMP_TRUE and then
          Tmp_Depth < Rec_Depth and then Form.Env_Selector'Length =0 then
       Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
     end if;
     return;
   elsif N_Depth > Max_LTS_Depth then
      Set_Status (Cref, ABORTED);
      Result :=  ABORTED;
      return;
   else
      Set_Status (Cref, IN_PROGRESS);
   end if;
   -- no previous result available
   -- we must begin a real evaluation of the formula
   --
   ----------------------------------------------------------
   --  STEP 1):  Se FORM1 e' False ritorna False.
   ----------------------------------------------------------
   --
   -- if Form.PRef.U2FormRef1.Kind /= FTrue then
   -- (OTTIMIZZAZIONE RIMOSSA PER NON COMPICARE LE EXPLANATIONS)
      Check_True (Form.PRef.U2FormRef1, FromState,Context, SubCref, Tmp_Result1,N_Depth,C_Depth+1,Tmp_Depth); 
      if Tmp_Result1 = FOUND_FALSE or          -- definitive result
           else Tmp_Result1 = TMP_FALSE  then  -- Env_selector'Length >0
           Add_Subcomputation(Cref, SubCref, No_Evolution);
           Set_Status(Cref, Tmp_Result1);  -- Rec_Depth remains C_Depth
           Set_Subcomputation(Cref, SubCref, No_Evolution);
           Result :=  Tmp_Result1;
        return;
         --
      elsif Tmp_Result1 /= ABORTED then
        -- se Tmp_Result1 = FOUND_TRUE, o TMP_TRUE registro la subcomputation
        --  e continuo an analizzare le transizioni possibili
         Add_Subcomputation(Cref, SubCref, No_Evolution);
      end if;
--    else
      -- se Form1 = "True" considera il Tmp_Result1 direttamente True
--   Tmp_Result1 := FOUND_TRUE;
--   end if;
   --
   ----------------------------------------------------------
   -- STEP 2):  SE non ci sono discendenti si conclude.
   ----------------------------------------------------------
   --
   Iterator_Initialize (My_Iter,System_Configuration(FromState));
   --
   -- Tmp_Result1 =  ABORTED, *_TRUE
   --
   if not Has_System_Transition (My_Iter)  then
     -- se non ci sono transitioni uscenti (stato finale)
     if Form.PRef.kind = Until2 then
       --  se non sono discendenti il risultato e' FOUND_FALSE.
       Set_Status(Cref, FOUND_FALSE);
       Iterator_Finalize (My_Iter);
       Result :=  FOUND_FALSE;
       return;
     else   -- Form.PRef.Kind = Wuntil2  
       -- se finale il risultato di Form e' quello definitivo di questo path
       Set_Status(Cref, Tmp_Result1);
       Iterator_Finalize (My_Iter);
       Result :=  Tmp_Result1;
       if Result=TMP_TRUE or Result=FOUND_TRUE then
          Check_Liveness(Fromstate);
       end if;
       return;
     end if;
   end if;
   --
   -- Tmp_Result1  a questo punto puo' essere  *_TRUE or ABORTED
   -- se Tmp_Result1 = true tutti i risultati sono anora possibili
   -- se Tmp_Result1 = aborted, AL PIU' potremo restuire FALSE
   --  ci sono transitioni in uscita.
   --
   ----------------------------------------------------------
   --  STEP 2b:  Se ci sono discendenti, per ogni transitione in uscita si
   --  verifica cio' essa DEVE soddisfare.
   --  Appena una transizione fallisce si restituisce FALSE
   --  Fintanto che Tmp_Result = *_TRUE or ABORTED si continua ad analizzare transizioni
   ----------------------------------------------------------
   --
   Tmp_Result := Tmp_Result1;    -- TMP_RESULT e' il risultato incrementale della valutazione
   --

   if Form.PRef.U2ARef1.Kind /= Atrue then  
     --
     -- Skip in the cae of "AF {act}"
     --   
     ----------------------------------------------------------
     -- STEP 3):  Se una transizione soddisfa (not A1 and not A2) si restituisce FALSE 
     ----------------------------------------------------------
     while Has_System_Transition (My_Iter) loop 
       declare
         This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
         TheBindings : AllBindings := Match(My_Iter, Form.Pref.U2ARef2);
         -- NewFormula: Formula_Ref;
       begin
         if not (Evolution_Satisfies_Tau (My_Iter) or else
               Evolution_Satisfies (My_Iter,Form.PRef.U2ARef1) ) and then
             not TheBindings.MatchOK then
           --
           --------------------------------------------------------------------------
           --  se esistono transizioni le cui labels soddisfano  not (tau or A1) and not A2 
           --    allora la formula e' falsa
           --------------------------------------------------------------------------
           --
           -- CI VORREBBE  CLEAR SUBCOMPUTATIONS ....
           --  o  aggiungere   esplicitamente una "<not (tau or A1) and not A2>false" subcomputation 
           Set_Status(Cref, FOUND_FALSE);
           Result :=  FOUND_FALSE;
           Iterator_Finalize (My_Iter);
           return;
         end if;
       end;
       Iterator_Advance (My_Iter);
     end loop;
     --
     Iterator_Restart(My_Iter);   
     ----------------------------------------------------------
     -- STEP 4) Se una transizione soddisfa (not A1 and A2 ), il target DEVE soddisfare FORM2 
     ----------------------------------------------------------
     while Has_System_Transition (My_Iter) loop 
       declare
         This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
         TheBindings : AllBindings := Match(My_Iter, Form.Pref.U2ARef2);
         NewFormula: Formula_Ref;
       begin
         if not (Evolution_Satisfies_Tau (My_Iter) or else
                 Evolution_Satisfies (My_Iter,Form.PRef.U2ARef1) ) and then
               TheBindings.MatchOK then
           --
           --------------------------------------------------------------------------
           --  tutte le transizioni che  soddisfano ( not (tau or A1)and (A2) )   
           --  DEVONO soddisfare FORM2 (per almeno un varbinding)
           --------------------------------------------------------------------------
           -- Se FORM2 e' *_False si restituisce definitivamente false.
           -- Se FORM2 e' _TRUE si continua a guaratare tutte queste transitioni, e poi si fa il resto.
           -- Se FORM2 e' ABORTED si continua per vedere se si potesse comunque restiture FAlse
           --
           -- Se ci sono piu' di un binding, per restituire False e' necessario che TUTTI i binding
           --    siano False.  (( E' GIUSTO?)))   
           --    vedi  AF {b($i)} <c(%i)>  in   s1 -{b(1),b(2)}-> s2 -{c(2)}-> s3  OK???
           -- nel caso immediato dovrebbe esser equivalente a:  AX{b($i)} <c(%i)>  E LO E'!!!
           -- (note che  AX{b($i)}FF  e [b($i)]FF  sono DIVERSE nel caso s1 -{b(1),b(2)}-> s2
           -- Se almeno uno dei binding e' TRUE si continua con *_TRUE
           -- Se tutti sono ABORTED si continua con ABORTED
           --
           if TheBindings.VarsCount = 0 then
             --
             Check_True(Form.Pref.U2FormRef2,This_Next,Context,SubCref,Last_Result,N_Depth,C_Depth+1,Tmp_Depth);
             if Last_Result = FOUND_FALSE or else Last_Result=TMP_FALSE then
               Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               Set_Status(Cref, Last_Result);
               Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               --
               Iterator_Finalize (My_Iter);
               Result :=  Last_Result;
               return;
               --
             elsif Last_Result = ABORTED then
               Tmp_Result := ABORTED;
             elsif Last_Result=TMP_TRUE or Last_Result=FOUND_TRUE then
               if  Tmp_Result=FOUND_TRUE then
                 -- hence Tmp_Result /= ABORTED
                 Tmp_Result := Last_Result;
               end if;
               if Tmp_Result /= ABORTED then
                 Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
               end if;
             end if;
             --
           else --  VarsCount > 0
             --
             Tmp_Result2 := FOUND_FALSE;
             for B in 1 ..TheBindings.BindCount loop
                 NewFormula :=
                    NewInstance(Form.Pref.U2FormRef2,(TheBindings.VarsCount,
                                                    TheBindings.VarNames,
                                                    TheBindings.AllValues(B).VarValues));
               Check_True(NewFormula, This_Next,Context, SubCref, Last_Result, N_Depth,C_Depth+1,Tmp_Depth);
               if Last_Result=TMP_TRUE or Last_Result=FOUND_TRUE then
                 Tmp_Result2 := Last_Result;
                 if Tmp_Result /= ABORTED and Tmp_Result2 /= ABORTED then
                   Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 end if;
                 exit;   -- found a successfull binding
               elsif Last_Result = FOUND_FALSE or else Last_Result=TMP_FALSE then
                 if  Tmp_Result2=FOUND_FALSE then
                   Tmp_Result2 := Last_Result;
                 end if;
                 if Tmp_Result /= ABORTED and Tmp_Result2 /= ABORTED and TheBindings.BindCount =1 then
                    Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 elsif Tmp_Result /= ABORTED and Tmp_Result2 /= ABORTED then
                   Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
                 end if;
               else -- Last_Result=ABORTED
                  Tmp_Result2 := ABORTED; 
               end if;
             end loop;   --  end loop on theBindings
             --
             if Tmp_Result2=TMP_TRUE or Tmp_Result2=FOUND_TRUE then
                -- if at least one of the binding is satisfied we contyinue with
                -- the orther transitions
                if  Tmp_Result = FOUND_TRUE then
                  Tmp_Result := Last_Result;
                end if;
             elsif Tmp_Result2 = FOUND_FALSE or else Tmp_Result2=TMP_FALSE then
               -- if all the bindings are False we terminate the evaluation
               if Tmp_Result /= TMP_FALSE then  -- ABORTED or FOUND_FALSE
                   Tmp_Result := Tmp_Result2;
               end if;
               Set_Status(Cref, Tmp_Result);
               --
               Iterator_Finalize (My_Iter);
               Result :=  Tmp_Result;
               return;
               --
             elsif Tmp_Result2 = ABORTED then
               -- if all bindings are Aborted, we continue to see we can return FALSE
               Tmp_Result := ABORTED;
             end if;
           end if;  -- bindings.VarsCount =0
         end if;
       end;
       Iterator_Advance (My_Iter);
     end loop;
     --
   end if;  --  Act1 /= Atrue
   
   ----------------------------------------------------------
   -- STEP 5)
   --  5a) se soddisfa (A1 and not A2), il target DEVE soddisfare ricorsivamentre FORM.
   --  5b) Se una transizione soddisfa (A1 and A2) si pre-valutvaluta FORM2 nel target 
   --  5c)    e se FORM2=false DEVE valere ricorsivamente FORM nel target.
   ----------------------------------------------------------
   Iterator_Restart(My_Iter);
   while Has_System_Transition (My_Iter) loop 
     declare
       This_Next: System_Configuration := Get_Target_Configuration (My_Iter);
       TheBindings : AllBindings := Match(My_Iter, Form.Pref.U2ARef2);
       NewFormula: Formula_Ref;
     begin
       --
       if ((ACTL_Compatibility and then Evolution_Satisfies_Tau (My_Iter)) or else
            Evolution_Satisfies (My_Iter,Form.PRef.U2ARef1) ) and then
            not TheBindings.MatchOK then     -- (Match(My_Iter, Form.Pref.U2ARef2)
         --
         --------------------------------------------------------------------------
         -- 5a)  tutte le transizioni che soddisfano  (tau or A1) and (not A2) ,   
         --    DEVONO soddisfare FORM (ricorsivamene)
         --------------------------------------------------------------------------
         --
         Check_AUntil2 (Form, WithOwner, This_Next,Context, SubCref,Last_Result,N_Depth+1,C_Depth+1,Tmp_Depth);
         --
         -- se le ricorsione fallisce, il risultato e' definitivamente false
         --  sia che form1 sia *_TRUE che ABORTED
         --
         if Last_Result = FOUND_FALSE or  Last_Result = TMP_FALSE  then
           Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Set_Status(Cref, Last_Result);
           Set_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           Result :=  Last_Result;
           Iterator_Finalize (My_Iter);
           return;
           --
         elsif Last_Result = ABORTED then
           -- se il risultato e' aborted,
           -- PROVO A VEDERE SE PER CASO FORM POTESSE ESSERE DIMOSTRATA FALSE UGUALMENTE
           Tmp_Result := ABORTED;
           --
         elsif Last_Result=TMP_TRUE  or Last_Result=FOUND_TRUE then
           -- se il risultato della ricorsione e' true. si continua a
           -- guardare le altre transizioni, eventualmente raffinando il risultato.
           if Last_Result= TMP_TRUE and then     -- lookback of recursion , AG like
              Form.Pref.Kind= WUntil2 and then
               Tmp_Depth < Rec_Depth and then 
                Form.Env_Selector'Length =0 then
             Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
           elsif Last_Result = TMP_TRUE and then 
               Form.Pref.Kind= WUntil2 and then
               C_Depth <= Tmp_Depth then  -- actually just "=" would be suffiicient..
               Last_Result := FOUND_TRUE; 
           end if;
           if  Tmp_Result=FOUND_TRUE then
             Tmp_Result := Last_Result;
           end if;
           if Tmp_Result /= ABORTED then
             Add_Subcomputation(Cref, SubCref, Get_Evolution_Data(My_Iter));
           end if;
         end if;
       --
       -- Tmp_Result  =  ABORTED,  *_TRUE
       --
           --
       elsif (Evolution_Satisfies_Tau (My_Iter) or else
               Evolution_Satisfies (My_Iter,Form.PRef.U2ARef1) ) and then
               TheBindings.MatchOK then   --(Match(My_Iter, Form.Pref.U2ARef2)
         --
         --------------------------------------------------------------------------
         -- 5b)5c) tutte le transizioni le cui labels soddisfano   (tau or A1) and (A2) 
         --  DEVONO soddisfare o FORM(ricorsivamente) o FORM2 (per almeno un binding)
         --------------------------------------------------------------------------
         --
         -- FORM1 = Tmp_Result1 =  ABORTED,  *_TRUE
         -- FORM  = Tmp_Result  =  ABORTED,  *_TRUE
         --
         declare
           R2 : Computation_Status;
           SubCref1: Computation_Step;
           SubCref2: Computation_Step;
         begin
           --  guardiamo prima se FORM2 e' soddisfatta (in modo da evitare la ricorsione, se possibile)
           --  salvando il risultato su Tmp_Result2 e SubCref1
           --
           if TheBindings.VarsCount = 0 then
             --
             Check_True(Form.PRef.U2FormRef2, This_Next,Context, SubCref1, Tmp_Result2, N_Depth,C_Depth+1,Tmp_Depth);
             if  Tmp_Result2 /= ABORTED  then
               Add_Subcomputation(Cref, SubCref1, Get_Evolution_Data(My_Iter));
             end if;
            --
           else
             for B in 1.. TheBindings.BindCount loop
               Tmp_Result2 := FOUND_FALSE;
               NewFormula :=
                    NewInstance(Form.PRef.U2FormRef2,(TheBindings.VarsCount,
                                                    TheBindings.VarNames,
                                                    TheBindings.AllValues(B).VarValues));
               Check_True(NewFormula, This_Next,Context, SubCref1, Last_Result, N_Depth,C_Depth+1,Tmp_Depth);
               if Last_Result=TMP_TRUE or Last_Result=FOUND_TRUE then
                 Tmp_Result2 := Last_Result;
                 if  Tmp_Result /= ABORTED  then
                   Add_Subcomputation(Cref, SubCref1, Get_Evolution_Data(My_Iter));  
                 end if;
                 exit;   -- found a successfull binding
               elsif Last_Result = FOUND_FALSE or else Last_Result=TMP_FALSE then
                 if  Tmp_Result2=FOUND_FALSE then
                   Tmp_Result2 := Last_Result;
                 end if;
                 -- also if aborted we must record this subcomputation   (REALLY ?!?!?!?)
                 Add_Subcomputation(Cref, SubCref1, Get_Evolution_Data(My_Iter));
               else -- Last_Result=ABORTED
                  Tmp_Result2 := ABORTED;
               end if;
             end loop;
           end if;
           -- 
           if Tmp_Result2 = TMP_TRUE or Tmp_Result2 = FOUND_TRUE then
             -- guardare le altre transizioni, eventualmente raffinando il risultato.
             if Tmp_Result2 = TMP_TRUE and Tmp_Result = FOUND_TRUE then
               Tmp_Result := TMP_TRUE;
             end if;
             --
           else  --  Tmp_Result2 = Aborted   or *_False
             --
             -- se il risultato di FORM2 non e' *_TRUE si
             -- deve guardare se vale la ricorsione su FORM 
             --  se fosse *_TRUE potremmo andare avanti con le transizioni
             --
             --  Tmp_Result2 =  *_FALSE  /  ABORTED
             --
             Check_AUntil2 (Form, WithOwner, This_Next,Context, SubCref2,R2,N_Depth+1,C_Depth+1,Tmp_Depth); 
             if R2 = TMP_TRUE or R2 = FOUND_TRUE then
                -- 
                if R2 = TMP_TRUE and then     -- lookback of recursion , AG like
                    Form.Pref.Kind= WUntil2 and then
                      Tmp_Depth < Rec_Depth and then
                         Form.Env_Selector'Length =0 then
                  Rec_Depth := Tmp_Depth;   --  THE FINAL VALUE SAVED WITH THE COMPUTATION
                elsif R2 = TMP_TRUE and then 
                      Form.Pref.Kind= WUntil2 and then
                        C_Depth <= Tmp_Depth then  -- actually just "=" would be suffiicient..
                     R2 := FOUND_TRUE;  -- a loop back TMP_TRUE is Definitively OK for itself
                end if;
                if R2=TMP_TRUE and Tmp_Result=FOUND_TRUE then
                   Tmp_Result := R2;
                end if;
                if Tmp_Result /= ABORTED then
                  Add_Subcomputation(Cref, SubCref2, Get_Evolution_Data(My_Iter));
                end if;
                --
             elsif R2 = ABORTED then
               --  Tmp_Result2 is  ABORTED or *_FALSE
               Tmp_Result := ABORTED;
               --
             elsif R2 = FOUND_FALSE or else R2=TMP_FALSE then
                --
                --  Tmp_Result2 is  ABORTED or *_FALSE
                --
                if Tmp_Result2 = FOUND_FALSE or else Tmp_Result2 = TMP_FALSE then
                   --
                   -- Tmp_result was ABORTED or *TRUE but now that becomes unrelevant
                   --
                   Tmp_Result := R2;
                   if Tmp_Result2 = TMP_FALSE then
                      Tmp_Result := TMP_FALSE;
                   end if;
                   -- we should remove the FORM1 subcomputation and the FORM subcomputations but we cant
                   -- This may lead to problems in Explanations ...
                   Set_Status(Cref, Tmp_Result2);
                   Result :=  Tmp_Result2;
                   Iterator_Finalize (My_Iter);
                   return;
                   --
                elsif Tmp_Result2= ABORTED then
                   --
                   Tmp_Result := ABORTED; 
                end if;
             end if;
             --
           end if;
         end;
       end if;  -- if satisfies ...
     end;
     Iterator_Advance (My_Iter);
   end loop ;
   Iterator_Finalize (My_Iter);
   --
   Result := Tmp_Result;
   if Result=TMP_TRUE or Result=FOUND_TRUE then
      Check_Liveness(Fromstate);
   end if; 
   Set_Status(Cref,Result);
   return;
end Check_AUntil2;


procedure Check_Assertion (Form: UCTL_Types.Formula_Ref;
                  FromState: System_Configuration;
                  Context: Computations_Table := Empty_Computations_Table;
                  Cref: Out Computation_Step;
                  Result: Out Computation_Status;
                  N_Depth: Natural;
                  C_Depth: Natural;
                  Rec_Depth: out Natural) is
  --
  My_Status: Computation_Status;
--  Result: Computation_Status;
  --
   Tmp_Depth: Natural := C_Depth;
begin
   Rec_Depth := C_Depth;
-- Check_Computation (Form, Context, FromState, FromState, Cref,My_Status,Tmp_Depth);
--  if (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
--         (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
--     Result :=  My_Status;
--      return;
--  end if;
   --
   --  PRINT_ONCE
   --
   if Form.Pred /= null and then
      Form.Pred.Right_Ids /= null and then
     Form.Pred.Right_Ids(1).all = "PRINT_ONCE" then
     if Flags.NoExplanations then
        Check_Computation (Form, Context, 1, 1, Cref,My_Status,Tmp_Depth); 
     else
        Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     end if;
     Result := FOUND_FALSE;
     Set_Status (Cref, Result);
     PrintONCE_Message(FromState,Form.Pred.Right_Ids(2..Form.Pred.Right_Ids.all'Last));
     return;
   end if;
   --
  Check_Computation (Form, Context, FromState, FromState, Cref,My_Status,Tmp_Depth);
  if (My_Status /= NOT_YET_STARTED and Form.Env_Selector'Length =0) or
           (My_Status = ABORTED or My_Status=FOUND_TRUE or My_Status=FOUND_FALSE) then
       Result :=  My_Status;
        return;
  end if;
  --
  if Form.Pred /= null and then
     Form.Pred.Right_Ids /= null and then
     Form.Pred.Right_Ids(1) /= null and then
         Form.Pred.Right_Ids(1).all'Last >9 and then
         Form.Pred.Right_Ids(1)(1..9) = "DEPTH_GT_" then
     Cref :=1;
     declare
       S: String :=  Form.Pred.Right_Ids(1)(10 .. Form.Pred.Right_Ids(1).all'last);
       N: Natural := Natural'Value(s);
     begin
       if N_Depth  > N then
         Result := FOUND_TRUE;
     else
           Result := FOUND_FALSE;
     end if;
       return;
     exception
       when others => Put_line("DEPTH_GT_??????");
     end;
   elsif Form.Pred /= null and then
         Form.Pred.Right_Ids /= null and then
         Form.Pred.Right_Ids(1) /= null and then
         Form.Pred.Right_Ids(1).all'Last >9 and then
         Form.Pred.Right_Ids(1)(1..9) = "DEPTH_LT_" then
     Cref :=1;
     declare
       S: String :=  Form.Pred.Right_Ids(1)(10 .. Form.Pred.Right_Ids(1).all'last);
       N: Natural := Natural'Value(s);
     begin
       if N_Depth < N then
     Result :=  FOUND_TRUE;
       else
           Result := FOUND_FALSE;
       end if;
       return;
     exception
       when others => Put_line("DEPTH_LT_??????");
     end;
   else
     if Configuration_Satisfies (
             System_Configuration(FromState), 
             Form.Pred) then
     Result :=  FOUND_TRUE;
  else
     Result :=  FOUND_FALSE;
  end if;
  Set_Status(Cref,Result);   -- FOUND_FALSE / FOUND_TRUE
  end if;
end Check_Assertion;


procedure Check_True (Form: UCTL_Types.Formula_Ref;
                  FromState: System_Configuration;
                  Context: Computations_Table := Empty_Computations_Table;
                  Cref: Out Computation_Step;
                  Result: Out Computation_Status;
                  N_Depth: Natural;
                  C_Depth: Natural;
                  Rec_Depth: out Natural) is
  --------------------------------------------------
  --   COMPUTATION_STATUS  =  
  --    NOT_YET_STARTED, IN_PROGESS, ABORTED, FOUND_TRUE, TMP_TRUE, FOUND_FALSE, TMP_FALSE
  --------------------------------------------------
  Tmp_Result: Computation_Status;
  SubCref: Computation_Step;
  My_Status: Computation_Status;
  Truncated: Boolean := False;
  Tmp_Depth: Natural;
  Tmp_Depth2: Natural;
begin
  Tmp_Depth := C_Depth;
  Rec_Depth := C_Depth;
  Form.Depth := Form.Depth +1;   -- A CHE SERVE???!?!?!?!?!?!??!
  case Form.Kind is
    --
  when Assertion =>
     Check_Assertion (Form, FromState, Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
     --
  when Ffalse =>
     if Flags.NoExplanations then
        -- save memory
        Check_Computation (Form, Context, 1,1, Cref,My_Status,Tmp_Depth);
     else
        Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     end if;
     if My_Status = NOT_YET_STARTED then
       Set_Status(Cref, FOUND_FALSE);
     end if;
     Result := FOUND_FALSE;
     --
  when Ftrue => 
     if Flags.NoExplanations then
        -- save memory
        Check_Computation (Form, Context, 1,1, Cref,My_Status,Tmp_Depth);
     else
        Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     end if;
     if My_Status = NOT_YET_STARTED then
       Set_Status(Cref, FOUND_TRUE);
     end if;
     Result := FOUND_TRUE;
     --
  when Fnot =>
     --------------------------------------------------
     --  Form  =  (Fnot, NotRef)
     --------------------------------------------------
     Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     if My_Status = NOT_YET_STARTED or  else
           (My_Status = TMP_TRUE and Form.env_selector'Length/=0) or else
           (My_Status = TMP_FALSE and Form.env_selector'Length/=0) then
       -- 
       Check_True (Form.NotRef,FromState,Context, SubCref, Tmp_Result,N_Depth,C_Depth+1,Tmp_Depth);
       --
       Set_Subcomputation(Cref, SubCref, No_Evolution);
       if Tmp_Result = FOUND_TRUE then
          Result := FOUND_FALSE;
       elsif Tmp_Result = FOUND_FALSE then
         Result := FOUND_TRUE;
       elsif Tmp_Result = TMP_FALSE then
         Result := TMP_TRUE;
       elsif Tmp_Result = TMP_TRUE then
         Result := TMP_FALSE;
       elsif Tmp_Result=ABORTED then
         Result := ABORTED;  
       else
          raise UCTL_Error;
       end if;
       Set_Status(Cref,Result);
     else
       Result := My_Status;
     end if;
   --
  when Fmax | Fmin =>
     --------------------------------------------------
     --  Form  =  (Fmax, IDef, FDef) or (Fmin, IDef, FDef)
     -------------------------------------------------
     Check_Rec (Form, FromState, FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
       --
  when Fapply =>
     --------------------------------------------------
     --  Form  =  (Fapply, IDen, FullDef, Optargs)
     --------------------------------------------------
     --
     Check_Apply (Form, FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
     --
  when Foor =>
     --------------------------------------------------
     --  Form  =  (For, LeftRef, RightRef)
     --------------------------------------------------
     Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     if My_Status = NOT_YET_STARTED  or  else
           (My_Status = TMP_TRUE and Form.env_selector'Length/=0) or else
           (My_Status = TMP_FALSE and Form.env_selector'Length/=0) then
       --
       Check_True (Form.LeftRef,FromState,Context, SubCref, Tmp_Result,N_Depth,C_Depth+1,Tmp_Depth);
       Result := Tmp_Result;  --- when ABORTED and PRINT
       --
       if Tmp_Result = FOUND_TRUE or Tmp_Result= TMP_TRUE then
          Set_Subcomputation(Cref,SubCref, No_Evolution);
          Result := Tmp_Result;
          --
       elsif (Tmp_Result /= ABORTED)                     
              or else not UCTL_Types.Formula_contains_PRINT then 
        -- i.e. TMP_FALSE / FOUND_FALSE   or -- ABORTED but not necessarily sequential
         if Tmp_Result /= ABORTED then
           Add_Subcomputation(Cref,SubCref, No_Evolution);
         end if;
         --
         Check_True (Form.RightRef, FromState,Context, SubCref, Result,N_Depth,C_Depth+1,Tmp_Depth2);
         if Tmp_Depth2 < Tmp_Depth then Tmp_Depth := Tmp_Depth2; end if;
         --
         if Result = FOUND_TRUE or Result= TMP_TRUE then
          --
           Set_Subcomputation(Cref,SubCref, No_Evolution);
           --
         elsif Result = ABORTED or Tmp_Result = ABORTED then
            Result := ABORTED;
            --
         --elsif Tmp_Result=TMP_FALSE or Result=FOUND_FALSE then
         else
           Add_Subcomputation(Cref,SubCref, No_Evolution);
           if Tmp_Result=TMP_FALSE and Result=FOUND_FALSE then
             Result := TMP_FALSE;  
           end if;
         end if;
       end if;
       Set_Status(Cref,Result);
     else
       Result := My_Status;
     end if;
     --
  when Fand =>
     --------------------------------------------------
     --  Form  =  (Fand, LeftRef, RightRef)
     --------------------------------------------------
     Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     --
     if My_Status = NOT_YET_STARTED or  else
           (My_Status = TMP_TRUE and Form.env_selector'Length/=0) or else
           (My_Status = TMP_FALSE and Form.env_selector'Length/=0) then
       --
       Check_True (Form.LeftRef,FromState,Context, SubCref, Tmp_Result,N_Depth,C_Depth+1,Tmp_Depth);
       Result := Tmp_Result;  --- when ABORTED and PRINT
       --
       if Tmp_Result = FOUND_FALSE or Tmp_Result = TMP_FALSE then
           -- 
          Set_Subcomputation(Cref,SubCref, No_Evolution);
          Result := Tmp_Result;
       elsif (Tmp_Result /= ABORTED)                     
              or else not UCTL_Types.Formula_contains_PRINT then 
        -- i.e. TMP_FALSE / FOUND_FALSE   or -- ABORTED but not necessarily sequential
         if Tmp_Result /= ABORTED then
           Add_Subcomputation(Cref,SubCref, No_Evolution);
         end if;
         Check_True (Form.RightRef,FromState,Context, SubCref, Result,N_Depth,C_Depth+1,Tmp_Depth2);
         if Tmp_Depth2 < Tmp_Depth then Tmp_Depth := Tmp_Depth2; end if;
         if Result = FOUND_FALSE or Result = TMP_FALSE then
           Set_Subcomputation(Cref,SubCref, No_Evolution);
            --
         elsif Result = ABORTED or Tmp_Result = ABORTED then
            Result := ABORTED;
--         elsif Tmp_Result=TMP_TRUE or Result=FOUND_TRUE then  ?????
         else
             Add_Subcomputation(Cref,SubCref, No_Evolution);
             if Tmp_Result=TMP_TRUE and Result=FOUND_TRUE then
               Result := TMP_TRUE;
             end if;
         end if;
       end if;
       Set_Status(Cref,Result);
     else
       Result := My_Status;
     end if;
     --
  when Fimply =>
     --------------------------------------------------
     --  Form  =  (Fimply, LeftRef, RightRef)
     --------------------------------------------------
     Check_Computation (Form, Context, FromState,FromState, Cref,My_Status,Tmp_Depth);
     if My_Status  = NOT_YET_STARTED or  else
           (My_Status = TMP_TRUE and Form.env_selector'Length/=0) or else
           (My_Status = TMP_FALSE and Form.env_selector'Length/=0) then
       Check_True (Form.LeftRef,FromState,Context, SubCref, Tmp_Result,N_Depth,C_Depth+1,Tmp_Depth2);
       Result := Tmp_Result;  --- when ABORTED and PRINT
       --
       if Tmp_Depth2 < Tmp_Depth then Tmp_Depth := Tmp_Depth2; end if;
       if Tmp_Result = FOUND_FALSE then
           Set_Subcomputation(Cref,SubCref, No_Evolution);
           Result := FOUND_TRUE;
       elsif Tmp_Result = TMP_FALSE then
           Set_Subcomputation(Cref,SubCref, No_Evolution);
           Result := TMP_TRUE;
       elsif (Tmp_Result /= ABORTED)                     
              or else not UCTL_Types.Formula_contains_PRINT then 
        -- i.e. TMP_FALSE / FOUND_FALSE   or -- ABORTED but not necessarily sequential
          if Tmp_Result /= ABORTED  then
             Add_Subcomputation(Cref,SubCref, No_Evolution);
          end if;
          --  Tmp_Result is here   TMP_TRUE  or FOUND_TRUE
          Check_True (Form.RightRef, FromState,Context, SubCref,Result,N_Depth,C_Depth+1,Tmp_Depth);
          if Result = FOUND_TRUE or Result=TMP_TRUE  then
           Set_Subcomputation(Cref,SubCref, No_Evolution);
          elsif Result = ABORTED or Tmp_Result = ABORTED then
            Result := ABORTED;
--         elsif Tmp_Result=TMP_FALSE or Result=FOUND_FALSE then
         else
             Add_Subcomputation(Cref,SubCref, No_Evolution);
             if Tmp_Result=TMP_FALSE and Result=FOUND_FALSE then
                Result := TMP_FALSE;          
             end if;
          end if;
       end if;
       Set_Status(Cref,Result);
     else
       Result := My_Status;
     end if;
       --
  when Fangle =>
     Check_Angle (Form,FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
     --
  when FWangle =>
     Check_WAngle (Form,FromState,FromState,Context,Cref,Result,N_Depth,C_Depth+1,Tmp_Depth);
     --------------------------------------------------
     --  Form  =  (Fangle, Form.FormRef1, Form.ARef2, Form.FormRef2)
     --------------------------------------------------
     null;
     --
  when Fsquare =>
     Check_Square (Form,FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
     --
  when FWsquare =>
     Check_WSquare (Form,FromState,FromState,Context,Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
     --
  when Fexist | Fall =>
    --------------------------------------------------
    --  Form  =  ([Fexist,Fall], Form.PRef)
    --------------------------------------------------
    --
    case Form.Pref.Kind is 
       --
    when Eventually =>
      --------------------------------------------------
      --  PRef = (Eventually, Form.TForm)
      --------------------------------------------------
      if Form.Kind = Fexist then
        Check_EF (Form, FromState, FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
      else
        Check_AF (Form, FromState, FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
      end if;
      --
    when Always =>
      --------------------------------------------------
      --  PRef = (Eventually, Form.TForm)
      --------------------------------------------------
      if Form.Kind = Fexist then
        Check_EG (Form, FromState,FromState, Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      else
        Check_AG (Form, FromState,FromState, Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      end if;
      --
      --
    when Until1  =>
      --------------------------------------------------
      --  PRef = (Until1, Form.U1FormRef1,  Form.U1ARef  Form.U1FormRef2)
      --------------------------------------------------
      if Form.Kind = Fexist then
        Check_EUntil1 (Form, FromState,FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
      else 
        Check_AUntil1 (Form, FromState,FromState,Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      end if;
      --
    when Wuntil1 =>
      --------------------------------------------------
      --  PRef = (Until1, Form.U1FormRef1,  Form.U1ARef  Form.U1FormRef2)
      --------------------------------------------------
      if Form.Kind = Fexist then
        Check_EUntil1 (Form, FromState,FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
        -- ^*****  TBD *****
      else
        Check_AWUntil1 (Form, FromState,FromState,Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      end if;
      --
    when Until2  | Wuntil2 =>
      if Form.Kind = Fexist then
        Check_EUntil2 (Form, FromState,FromState,Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      else 
        Check_AUntil2 (Form, FromState,FromState,Context, Cref, Result,N_Depth,C_Depth+1,Tmp_Depth);
      end if;
      --
    when Act_Next =>
      --------------------------------------------------
      --  PRef = (Act_Next,Form.AARef, Form.AAVec, Form.AFormRef)
      --------------------------------------------------
      if Form.Kind = Fexist then
        Check_EX (Form, FromState,Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      else
        Check_AX (Form, FromState,Context, Cref,Result, N_Depth,C_Depth+1,Tmp_Depth);
      end if;
    end case;
  end case;
  --
  Form.Depth := Form.Depth - 1;    --- A CHE  SERVE ?!?!?!?!?!?!?
  --
  if Tmp_Depth < Rec_Depth then Rec_Depth := Tmp_Depth; end if;
  Result :=  Result;
      return;
end Check_True;

function Eval (Formula: UCTL_Types.Formula_Ref;
                 State: System_Configuration) return Computation_Status is
    Res: Computation_Status;
    Cref: Computation_Step;
    Tmp_Depth: Natural:=1;
begin
   -- 
   -- adjust Max_Computation_Depth for bounded model checking
   --
   Max_LTS_Depth := Default_LTS_Depth;
   --
    Requested_Counts := 0;
    All_Steps := 0;
    -- SEMPRE? o solo per product families e quant PRINT requested?
    Computations_DB.Initialize_DB;
    --
    if verbose_Eval then
      Put_Line ("-- Starting Evaluation with LTS Bound set to" &
           Integer'Image(Max_LTS_Depth));
    end if;
    --
    Check_True (Formula, State, Empty_Computations_Table, Cref, Res,1,1,Tmp_Depth);
    while (not Static_MAX_Depth) and Res = Aborted  loop
        if Max_LTS_Depth = 0 then
            Max_LTS_Depth := 1;
        else
          if Depth_Increment = 0 then
            Max_LTS_Depth := Max_LTS_Depth*2;
          else
            Max_LTS_Depth := Max_LTS_Depth + Depth_Increment;
          end if;
        end if;
        All_Steps := 0;
        Put_line ("-- Restarting the evaluation with LTS Bound set to" &
        Integer'Image(Max_LTS_Depth));
        Check_True (Formula, State,Empty_Computations_Table, Cref,Res,1,1,Tmp_Depth);
    end loop;
    --
    UCTL.FragmentsCOunt := Natural(Computations_DB.ComputationsSpace_Size);
    Print_COUNT_Info;
    if Res = TMP_TRUE then
        return FOUND_TRUE;
    elsif res = TMP_FALSE then
        return  FOUND_FALSE;
    else
        return Res;
    end if;
    --
exception
  when Event: others =>
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    Put_line (Current_Error, "Eval: unexpected error at step " &
            Int64'Image(All_Steps));
      raise;
end Eval;
-------------------------------------------------------------------------
-------------------------------------------------------------------------

  --------------------------------------
  function Are_Matching(Item1: String; Item2:String) return Boolean is
  begin
   if Item1'length /= Item2'Length then return False; end if;
   --
   if Item1= "True" and then Item2= "true" then return True; end if;
   if Item1= "true" and then Item2= "True" then return True; end if;
   if Item1= "False" and then Item2= "false" then return True; end if;
   if Item1= "false" and then Item2= "False" then return True; end if;
   --
   return Item1=Item2;
  end Are_Matching;

 
  -------------------------------------------------------------------------
  --  called when an Action has to be evaluted w.r.t. a possible evolution.
  --
  -- If the successfull evalution BINDS a formula parameter ($id), its corresponding Value
  --  is set in the  VarsNames/VarsValues/VarsCount variables AS E SIDE EFFECT.
  -- These variables are later acquired by the function EVOLUTION_BINDINGS.
  --
  -- Formula parameter CANNOT appear inside boolean operators , or more then ONCE.
  -------------------------------------------------------------------------
  function  Evolution_Satisfies
    (It: Evolutions_Iterator; Action_Formula: UCTL_Types.Action_Ref) return  Boolean is
   --
--    This_Evolution: COW_Evolution;
   Result: Boolean;
   --
 begin
   --
--   VarsCount :=0;
--
--   if It.Evolutions = null then
--     Put_Line(Current_Error, "Internal Error");
--     raise COW_Error;
--   end if;
--
--    This_Evolution := It.Evolutions(It.Index);
   --
   if Action_Formula = null then
     Result :=  Evolution_Satisfies_Tau(It);
     return Result;
   end if;
     --
     case Action_Formula.Kind is
     --
     when Atrue   =>
       Result :=  True;
       return Result;
     --
     when  Afalse   =>
       Result := False;
       return Result;
     --
     when Anot  =>
         --  we want  EX {a & ~ a} to be false !!!
       Result := Evolution_Satisfies(It, Action_Formula.Anot);
       Result := not Result;
        -- action parameters CANNOT appear inside negations !!!  check supposed static
       return Result;
     --
     when Aand =>
       Result := Evolution_Satisfies(It, Action_Formula.Aref1);
       if Result then
          Result := Evolution_Satisfies(It, Action_Formula.Aref2);
       end if;
       return Result;
     --
     when  Aor   =>
       Result := Evolution_Satisfies(It, Action_Formula.Aref1);
       if not Result then
          Result := Evolution_Satisfies(It, Action_Formula.Aref2);
       end if;
       return Result;
     --
     when Aid =>
         Evolution_Satisfies_With_Param(It,Action_Formula.AidPred,Result);
         return Result;
     --
     when Aas =>
         Evolution_Satisfies_With_Param(It,Action_Formula.AasPred,Result);
         return Result; 
     end case;
  end Evolution_Satisfies;
  
function Evolution_Bindings (It: Evolutions_Iterator;
      Action_Formula: UCTL_Types.Action_Ref) return VarBindings is
     NoBindings: VarBindings(0);
begin
    if Action_Formula= null then return NoBindings; end if;
    if Action_Formula.Kind= Aid then
      return Evolution_Bindings(It,Action_Formula.AidPred);
    elsif Action_Formula.Kind= Aas then
      return Evolution_Bindings(It,Action_Formula.AaSPred);
    else
      return NoBindings;
    end if;
end Evolution_Bindings;

  -----------------------------------------------------------------------------
  subtype Formula_Ref  is  Basic_Predicate_Ref;
  subtype Action_Ref is  Basic_Action_Ref;
  --
  VarsNames: String_Table(1..100);
  VarsValues: String_table(1..100);
  VarsCount: Natural :=0;
  BindCount: Natural :=0;               -- actual number of bindings
  CurBindings: BindTable(1..100);  -- maximun number of bindings
  CurrentBindings: String_Table(1..100);     -- tmp binding values
  -----------------------------------------------------------------------------

  ------------------------------------------------
  function Evolution_Satisfies_Tau (It: Evolutions_Iterator) return Boolean is
    TheLabels: String_Tables_Vector := Get_Abstract_Action_Labels(It);
  begin
    if  TheLabels'Length =0 then 
      return True;
    elsif TheLabels'Length =0 and then 
           TheLabels(1) /= null and then
            TheLabels(1)(1).all'Length >4 and then
             TheLabels(1)(1)(1..4) = "tau=" then
        return True;
    else
      return False;
    end if;
  end Evolution_Satisfies_Tau;


  -------------------------------------------------
  function Is_Number (Token: String) return Boolean is
  begin
    if Token(Token'First) /= '-' then
    for I in Token'Range loop
      if Token(I) not in '0'..'9' then
         return False;
      end if;
    end loop;
    return True;
    else
      for I in Token'First+1 .. Token'Last loop
      if Token(I) not in '0'..'9' then
         return False;
      end if;
    end loop;
    return True;
    end if;
  end Is_Number;

  -------------------------------------------------
  function IsLT (Item1: String; Item2:String) return Boolean is
  begin
     -- should be;
     if Is_Number(Item1) and Is_Number(item2) then
       return  Integer'Value(Item1) < Integer'Value(Item2);
     else
       return False;
     end if;
  end IsLT;

  -------------------------------------------------
  function IsLE (Item1: String; Item2:String) return Boolean is
  begin
     -- should be;
     if Is_Number(Item1) and Is_Number(item2) then
       return  Integer'Value(Item1) <= Integer'Value(Item2);
     else
       return False;
     end if;
  end IsLE;

  -------------------------------------------------
  function IsGT(Item1: String; Item2:String) return Boolean is
  begin
     -- should be;
     if Is_Number(Item1) and Is_Number(item2) then
       return  Integer'Value(Item1) > Integer'Value(Item2);
     else
       return False;
     end if;
  end IsGT;

  -------------------------------------------------
  function IsGE(Item1: String; Item2:String) return Boolean is
  begin
     -- should be;
     if Is_Number(Item1) and Is_Number(item2) then
       return  Integer'Value(Item1) >= Integer'Value(Item2);
     else
       return False;
     end if;
  end IsGE;


  ------------------------------------------------
  function Configuration_Satisfies
       (This_Conf: System_Configuration;
       Assertion: Basic_Predicate_Ref) return Boolean is
    Thislabels: String_Tables_Vector := Get_Abstract_State_Labels(This_Conf);
--    Result: Boolean;
   begin
    -- special case:   (id = id )  (num =  num)
    if Assertion.Left_Ids = null and then
       Assertion.Left_Index /= null and then
       Assertion.Op = EQ and then
       Assertion.Right_Ids = null and then
       Assertion.Right_Index /= null and then
       Assertion.RightOp = NOOP and then
       Assertion.LeftOp = NOOP then
      --
      return Are_Matching(Assertion.Left_Index.all,Assertion.Right_Index.all);
    end if;
    --
    -- special case:   (id /= id )  (num /=  num)
    if Assertion.Left_Ids = null and then
       Assertion.Left_Index /= null and then
       Assertion.Op = NE and then
       Assertion.Right_Ids = null and then
       Assertion.Right_Index /= null and then
       Assertion.RightOp = NOOP and then
       Assertion.LeftOp = NOOP then
      --
      return not Are_Matching(Assertion.Left_Index.all,Assertion.Right_Index.all);
    end if;
    --
    -- special case:   (id < id )  (num <  num)
    if Assertion.Left_Ids = null and then
       Assertion.Left_Index /= null and then
       Assertion.Op = LT and then
       Assertion.Right_Ids = null and then
       Assertion.Right_Index /= null and then
       Assertion.RightOp = NOOP and then
       Assertion.LeftOp = NOOP then
      --
      return isLT(Assertion.Left_Index.all,Assertion.Right_Index.all);
    end if;
    --
    -- special case:   (id > id )  (num >  num)
    if Assertion.Left_Ids = null and then
       Assertion.Left_Index /= null and then
       Assertion.Op = GT and then
       Assertion.Right_Ids = null and then
       Assertion.Right_Index /= null and then
       Assertion.RightOp = NOOP and then
       Assertion.LeftOp = NOOP then
      --
      return isGT(Assertion.Right_Index.all,Assertion.Left_Index.all);
    end if;
    --
    -- special case:   (id >= id )  (num >=  num)
    if Assertion.Left_Ids = null and then
       Assertion.Left_Index /= null and then
       Assertion.Op = GE and then
       Assertion.Right_Ids = null and then
       Assertion.Right_Index /= null and then
       Assertion.RightOp = NOOP and then
       Assertion.LeftOp = NOOP then
      -- 
      return isGE(Assertion.Right_Index.all,Assertion.Left_Index.all) or else
          (Assertion.Right_Index.all = Assertion.Left_Index.all);
    end if; 
    --
    -- special case:   (id <= id )  (num <=  num)
    if Assertion.Left_Ids = null and then
       Assertion.Left_Index /= null and then
       Assertion.Op = LE and then
       Assertion.Right_Ids = null and then
       Assertion.Right_Index /= null and then
       Assertion.RightOp = NOOP and then
       Assertion.LeftOp = NOOP then
      -- 
      return isLE(Assertion.Left_Index.all,Assertion.Right_Index.all) or else
         (Assertion.Right_Index.all = Assertion.Left_Index.all);
    end if; 
     --
    --
    -- accepting(arg, ... arg)
    --
--    for I in Thislabels'Range loop  -- look inside all model lables
--      if Thislabels(I) /= null and then 
--         Assertion.Right_Ids /= null and then
--         Assertion.Right_Ids.all'length = Thislabels(I).all'Length then
--       Result := True;
--       for J in Assertion.Right_Ids.all'Range loop   -- look inside assertion ids
--         if Assertion.Right_Ids(J).all /= "*" and then
--             not Are_Matching(Assertion.Right_Ids(J).all, Thislabels(I)(J).all) then
--            Result := False;
--            exit;
--         end if;
--       end loop;
--       if Result=True then return True; end if;
--      end if;
--    end loop;
--    return False;
    if Assertion.Right_Ids /= null or else
        Assertion.Right_Ids.all'Length=0 then
      return Configuration_Satisfies(This_Conf,Assertion.Right_Ids.all);
    else
       return True;
      end if;
    --
  end Configuration_Satisfies;


 function Evolution_Satisfies (It: Evolutions_Iterator;
    Action_Formula: Action_Ref) return Boolean is
    --
    Result: Boolean := False;
  begin
      VarsCount :=0;
      Evolution_Satisfies_with_Param(It, Action_Formula, Result);
      return Result;
  end Evolution_Satisfies;


  procedure  Evolution_Satisfies_with_Param
       (It: Evolutions_Iterator;
        Action_Formula: Basic_Action_Ref;
        Result: in out Boolean) is
    Thislabels: String_Tables_Vector := Get_Abstract_Action_Labels(It);
    Tmp_Result: Boolean;
  begin
    --
    if Action_Formula = null then
     Result := Evolution_Satisfies_Tau(It);
     return;
    end if;
    --
    case Action_Formula.Kind is
     when Aid =>
       --   "tau"
       if Action_Formula.Event /= null and then
            Action_Formula.Event.all = "tau" then
          Result := Evolution_Satisfies_Tau (It);
          return;
       end if;
       --
       --
       --  ACTION FORMULAS are of the form:
       --           request(interaction, arg, ...,  arg)
       --           response(interaction, arg, ...,  arg)
       --           dispatched(interaction,arg, .. arg)
       --           discarded(interaction)
       --           action
       --           action()
       --           action(args)
       --           *
       --           *()
       --              ...
       ---------------------------------------------------------------------
       --   COME DISTUNGUO NELLA ACTION FORMULA   "action" da "action() tramite Action_Formula.Params
       ----------------------------------------------------------------------
       for I in Thislabels'Range loop  -- look inside all model lables
         --if Action_Formula.Labels.all'length <= Thislabels(I).all'Length then
         Tmp_Result := False;
         if Action_Formula.Labels.all'length = Thislabels(I).all'Length or else
             Action_Formula.Params = null  then
           Tmp_Result := True;
           for J in Action_Formula.Labels.all'Range loop   -- look inside asertion ids
             if Action_Formula.Labels(J).all /= "*" and then
               Action_Formula.Labels(J)(1) /= '$' and then
               not Are_Matching(Action_Formula.Labels(J).all,Thislabels(I)(J).all) then
               Tmp_Result := False;
               exit;
             end if;
             if Action_Formula.Labels(J).all(1) = '$' then
               -- the parameter is a variable binder
               VarsCount := VarsCount+1;
               VarsNames(VarsCount) := Action_Formula.Labels(J);
               VarsValues(VarsCount) := Thislabels(I)(J);
             end if;
           end loop;
           if Tmp_Result=True then Result := True; return; end if;
         end if;
       end loop;
       Result := False;
       return;
      --
     when Aas =>
        --  originally we allowed ground formulas of the kind:  Obj.x' = Obj.x+1;
        --  these are no longer supported.
       Result := False;
       return;
    end case;
    --
  end Evolution_Satisfies_with_Param;


  function MatchID (TheLabels: String_Tables_Vector;
                 Action_Formula: UCTL_Types.Action_Ref) return  AllBindings is
       NoBindings: AllBindings(0,0);
       ThisAction: Basic_Action_Ref  renames Action_Formula.AidPred;
       Tmp_Result: Boolean;
       Result: Boolean;
       LocalCount: Natural;
   begin
         --
         --  ACTION FORMULAS are of the form:
         --           request(interaction, arg, ...,  arg)
         --           response(interaction, arg, ...,  arg)
         --           dispatched(interaction,$v1, .. $vn)
         --           discarded(interaction)
         --           foo(id1,$1,$2,$3)
         --              ...
         --   THISLABELS are of the form
         --      ((foo,id1,a1,a2),(bar,id2,b1,b2),(foo,id1,b1,b2))
         --
         --  for each abstract label we must check if it satisfies the given
         --     basic action formula labels, and if so, we must collect all the
         --     possibly generated bindings,
         --
         --  foo satisfies   foo(), foo(a1,a2)
         --  foo()  satisfies  foo,  foo(), NOT foo(a1)
         --  foo(a1)  satisfies  foo(a1),  NOT foo(a1,a2)
         --  foo(*)   satisfies  foo(a1), NOT foo(a1,a2)
         --  foo($1)  satisfies  foo(a1), NOT foo(a1,a2,a3)   creating binding $1 -> a1
         --  $1($2)   satisfioes  foo(a1)
         --  *(a1)    satisfies  must(a1) , may(a1)
         --
         -- just count the vars in action formula id
         VarsCount :=0;
         for J in ThisAction.Labels.all'Range loop 
            if ThisAction.Labels(J).all(1) = '$' then
               -- the parameter is a variable binder
               VarsCount := VarsCount+1;
               VarsNames(VarsCount) := ThisAction.Labels(J);
            end if;
         end loop;
         --
         Result := False;
         BindCount :=0;
         for I in Thelabels'Range loop  -- look inside all L2TS lables
           Tmp_Result := False;
           --  if ThisAction.Labels.all'length <= Thelabels(I).all'Length then
           if ThisAction.Labels.all'length = Thelabels(I).all'Length or else
                ThisAction.Params = null  then  
                 -- if the label has less parameters than the action, we skip the label
             Tmp_Result := True;
             LocalCount :=0;
             for J in ThisAction.Labels.all'Range loop   -- look inside asertion ids
               if ThisAction.Labels(J).all /= "*" and then
                  ThisAction.Labels(J)(1) /= '$' and then
                  not Are_Matching(ThisAction.Labels(J).all,Thelabels(I)(J).all) then
                  -- as we found a non matching parameter we skip the label
                 Tmp_Result := False;
                 exit;
               end if;
               if ThisAction.Labels(J).all(1) = '$' then
                  -- the parameter is a variable binder
                  LocalCount := LocalCount +1;
                  VarsValues(LocalCount) := Thelabels(I)(J);
               end if;
             end loop;
             if Tmp_Result=True then
                 if VarsCount >0 then
                    --  we should avoid duplicates at least empty ones!!!
                    BindCount := BindCount +1;
                    CurBindings(BindCount) := (VarsCount,VarsValues(1..VarsCount));
                 end if;
                 Result := True;
             end if;
           end if;
         end loop;
     declare
       Res: AllBindings(VarsCount,BindCount);
     begin
       Res.MatchOK := Result;
       Res.VarNames := VarsNames(1..VarsCount);
       Res.AllValues := CurBindings(1..BindCount);
       return Res;
     end;
  end MatchId;


  function Match (It: Evolutions_Iterator;
                 Action_Formula: UCTL_Types.Action_Ref) return  AllBindings is
    NoBindings: AllBindings(0,0);
 begin
   --
   if Action_Formula = null then
     NoBindings.MatchOK :=  Evolution_Satisfies_Tau(It);
     return NoBindings;
   end if;
     --
     case Action_Formula.Kind is
     --
     when Atrue   =>
       NoBindings.MatchOK :=  True;
       return NoBindings;
     --
     when  Afalse   =>
       NoBindings.MatchOK := False;
       return NoBindings;
     --
     when Anot  =>
         --  we want  EX {a & ~ a} to be false !!!
       NoBindings.MatchOK := Evolution_Satisfies(It, Action_Formula.Anot);
       NoBindings.MatchOK := not NoBindings.MatchOK;
        -- action parameters CANNOT appear inside negations !!!  check supposed static
       return NoBindings;
     --
     when Aand =>
       declare
--         B1: AllBindings := Match(Get_Abstract_Action_Labels(It).all,Action_Formula.Aref1);
         B1: AllBindings := Match(It,Action_Formula.Aref1);
       begin
         if B1.MatchOK then
           declare
--             B2: AllBindings := Match(Get_Abstract_Action_Labels(It).all,Action_Formula.Aref2);
             B2: AllBindings := Match(It,Action_Formula.Aref2);
           begin
             if B2.MatchOK then
               if B1.VarsCount=0 then
                 return B2;
               elsif B2.VarsCount=0 then
                 return B1;
               end if;
               --
               -- join the two independent  bindings
               --
               declare
                 B3: AllBindings(B1.VarsCount+B2.VarsCount, B1.BindCount * B2.BindCount);
               begin
                 B3.Varnames := B1.VarNames & B2.VarNames;
                 for i1 in 1.. B1.BindCount loop
                   for i2 in 1.. B2.BindCount loop
                     B3.AllValues((i1-1)*B2.BindCount +i2) := 
                       (B3.VarsCount, B1.AllValues(i1).VarValues & B2.AllValues(i2).VarValues);
                   end loop;
                 end loop;
                 B3.MatchOK := True;
                 return B3;
               end;
             else
               NoBindings.MatchOK := False;
               return NoBindings;
             end if;
           end;
         else
           NoBindings.MatchOK := False;
           return NoBindings;
         end if;
       end;

--       NoBindings.MatchOK := Evolution_Satisfies(It, Action_Formula.Aref1);
--       if NoBindings.MatchOK then
--          NoBindings.MatchOK := Evolution_Satisfies(It, Action_Formula.Aref2);
--       end if;
--       return NoBindings;
     --
     when  Aor   =>
       NoBindings.MatchOK := Evolution_Satisfies(It, Action_Formula.Aref1);
       if not NoBindings.MatchOK then
          NoBindings.MatchOK := Evolution_Satisfies(It, Action_Formula.Aref2);
       end if;
       return NoBindings;
     --
     when Aid =>
        return MatchID(Get_Abstract_Action_Labels(It).all,Action_Formula);
     --
     when Aas =>
         NoBindings.MatchOK := False;
         return NoBindings;
     end case;
     --
  end Match;


   function Evolution_Bindings (It: Evolutions_Iterator;
     Action_Formula: Basic_Action_Ref) return VarBindings is
     TheBindings: VarBindings(VarsCount);
   begin
    TheBindings.VarNames := VarsNames(1..VarsCount);  -- String_table
    TheBindings.VarValues := VarsValues(1..VarsCount);  -- String_Tables_Vector
    return TheBindings;
  end Evolution_Bindings;

end Classic;

---------------------------------
--
--  max Z:   ((EF Z) and false)
--
--   Quando valuto EF Z, in presenza di loopback Z vale TEMP_TRUE. 
--   La presenza di Z viene pero' notata dal check  Form.Env_Selector.all'Length =0 e 
--     TMP non viene trasformato in FOUND_
--    Idem quando completo la valutazione di EF Z, alla fine quando Owner=State il risultato da 
--     TMP_TRUE NON diventa FOUND_TRUE perche Form.Env_Selector.all'Length >0.
--
---------------------------------
--
--  A COSA SERVER OWNER ??
--
-- 1)  Alla fine di valutazioni ricorsive (EF) se WithOwner = FromState and then Form.Env_Selector.all'Length =0 
--     un risultato TMP_False viene consolidato in FOUND_FALSE. Questo attiva le ottimizzazioni.
--     La stessa cosa viene ora fatta in alcuni casi, anche se C_Depth <= Rec_Depth e 
--     Form.Env_Selector.all'Length =0,  condizione piu' generale della precedente.
--
-- 2) Supponiamo di aver valutato (Form,s1,Own1). Durante la valutazione di (Form,Own1,OWn1) di cui la
--    precedenete e' una subcomputatione, puo' capitare di dover rivalutare (Form,s1,Own1), riciclando il
--    valore TMP gia trovato. (Cio' non giustifica la necessita dei TMP!!)
--    Se invece (Form,Own1,OWn1)  e' gia stata completata, potremmo trovarci a dover rivalutare
--    (form,s1,Own2).  A questo punto se (Form,s1,Own1) esiste sempre ad ha un valore TMP_* e' importante NON
--    riciclarlo, ma iniziare una nuova DIFFERENTE valutazione.  Se invce tutte le precedenti (Form,s1,Own1) 
--    sono state consolidate in (Form,s1,s1) con valore FOUND_*  L'attributo Owner non e' piu' necessario.
--    (Questo per quanto riguarda le formula UCTL pure senza fixpoint, in cui Rec_Depth permette di consolidare
--    in modo efficente tutte le subcomputations).
--    In presenza di punti fissi, le rec_depth NON permette di consolidare sufficentemente presto le
--    computazioni prima che essere vengano re-iniziate dall'alto. In questo caso pro' c'e' ENV_SELECTOR che
--    permette di distinguere le varie subcomputations 
--    (in modo molto piu' fine di quanto faccia Owner=Env_Selector(1)).
--    QUINDI, UNA VOLTA CORRETTAMENTE OTTIMIZZATE LE COMPUTAZIONI RICORSIVE SENZA FIX, SI PUO' FARE A MENO
--    dell'attributo OWNER.
--  
-- NOTA 09-08-2015: Mi pare che la condizione Form.Env_Selector.all'Length =0 sia inutile.
--  Durante una valutation EF Una subcomputation Tmp_FALSE puo' sempre essere consolidata se Rec_Depth > C_Depth
--  Infatti Rec_Depth < C_Depth implica necessariamente sia l'esistenza di variabili di punto fisso sia 
--  l'esistenza di loop back. E la presenza di varibili di punto senza loop back non avrebbe portato ad un valore
--  TMP_FALSE. In altre parole, NON PUO' ESSERE che abbiamo Rec_Depth > C_Depth e Tmp_Result = TMP_... quando
--  la valutazione e' ricorsiva e SEQUENZIALE.  Nel caso di valutazione parallela si crea effettivamente un problema.
--
--  A COSA SERVE TMP_FALSE  FOUND_FALSE ??
--
--  1) Intuitivamente, distingue riultati NON definitivi (che dipendono da computazioni a piu' alto livello
--     ancora in corso) da risultati definitivi.  I risultati NON definitivi possono essere sbagliati, e quindi
--     o sono aggiustati appena quelli definitivi sono noti, o NON vengono riciclati.
--
-- 2)  I discendenti di una valutazione ricorsiva (e.g. EF FF) possono  essere Tmp_False o Found_False.
--     Nel primo caso, quando il risultato e' definitivo (C_Depth <= Rec_Depth and Env_Selector'Length=0) 
--     possono essere consolidati (Tmp_false  diventa Final Value - possibly TRUE, Owner diventa State).
--     Nel primo caso,  quando il risultato e' TMP_* ha bisogno di essere corretto appena noto quello definitivo.
--     Nel secondo caso (FF) sono immediatamente ottimizzati (OWner diventa State).
--     In caso di assenza di punti fissi (UCTL puro), TMP_* permette quando una valutazione diventa definitiva,
--     di indentificare  quelle specifiche subcomputations il cui valore deve essere modificato perche da Tmp_False
--     debbono diventare TRUE (o viceversa).  
--     Attualmente questa ottimizzazione NON viene fatta!!
--
--   A COSA SERVONO C_DEPTH  e REC_DEPTH ??
--
--   CDEPTH conta il livello di annidamento delle valutazioni (incrementato ogni volta che una subcomputazione
--   passa ad uno stato adiacente, quindi conta in effetti i "passi" fatti sul grafo del modello, quindi la massima
--   distanza raggiunta dallo stato iniziale).  Quandi C_Depth supera Max_Depth la computazione viene abortita.
--   C_Depth viene anche utilizzato per registrare la profondita della stack di valutazione quando viene
--   iniziata una computazione ricorsiva tipo EF (C_DEPTH viene salvato in CREF). 
--   Esso e' anche il valore iniziale di REC_DEPTH (potenzia√lmente rivisto al ribasso da subcompuations).
--   Quando una computazione ricorsiva incappa in un lookback, restituisce TMP_* ed il C_DEPTH del punto di
--   ritorno (minore del C_DEPTH attuale).  Cio' permette di comprendere quando il risultato della valutazione
--   e' definitivo (REC_DEPTH returned >= C_DEPTH  implica Result = FOUND*)
--   In effetti, osservando i REC_DEPTH, posso distinguere i valori definitivi FOUND_* delle subcomputations 
--   dai valori temporanei TMP_* (eventualmente da modificare). (Rec_depth di subcom <= C_DEPTH implica che
--   il valore e' un TMP_* eventualmente da modificare).  Cio' permetterebbe di eliminare la distinzione
--   from TMP_*  e FOUND_√*  e la necessita di consolidare da TMP_F a FOUND_F (con notevole incremento 
--   dell'efficenza), in caso di assenza di operatori di punto fisso.

--  STEPS:
-- AA )  Estendere l'uso di Rec_depth a tutti gli operatori ricorsivi UCTL
-- BB )  Elimiare Owner
-- CC )  Limitare l'uso di TMP_ solo al caso di rec e di apply.  (?????????)
--
--    PURO UCTL  VS   FIXPOINT:
--   Nelm caso di FixPoint espliciti la situazione e'  olto piu' complessa.
--
--  Consideriamo la formauula:   max Z: ((<> (Z or true)) and false)
-- valuto Z in s1  (in_progres)
-- <> step,
--  valuto (Z or true) in s2.    Trovo Z TMP_True e smetto!! (LAZY EVALUATION)
--  (<> (Z or True),[s1)] quiandi e' Tmp_True   (non valuto "or true")
--  ritorno in s1 per Z  e trovo risultato definitivo per Z = False
--  se consolido il risultato ..  vado ma mettere <>(Z or true) a FALSE, ma e' SBAGLIATO!!!
--  infatti consolidando dovrei mettere silo Z in s2 = FALSE!!!
--  Il valore TMP_* di <>()[s1] dovrebbe rimanere tale!!!(perche la valutazione non e'm dettop che sia 
--  stata fatta in modo completato)
--  (Per cercare le Z da consoloidare dovrei osservare ENV_SELECTOR, per capire quali subcomp cercare)

--
-- Check_Computation (Form, Context, WithOwner, FromState,Cref,My_Status,Tmp_Depth);
-- a)  Looks if <Form,Context,FromState> is in the DB
--       if NOT PRESENT  return CREF=new, saving STATUS=NOT_YET_STARTED, DEPTH=Tmp_Depth
--       if PRESENT, STATUS=ABORTED or TMP_* and This_Comp.Max_LTS_Depth < Max_LTS_Depth
--          behaves has NOT PRESENT restarting the computation
--          otherwise return CREF=foundCref, STATUS=foundStatus DEPTH=foundDepth
--       if PRESENT, STATUS=INPROGRESS  returns TMP_*, and returns saved DEPTH
--
-- Add_Subcomputation(Cref,SubCref,Evdata)
--    Adds SubCref as subcomputatiin ofCref, performed after evolution with Evdata
--
-- Set_Subcomputation(Cref,SubCref,Evdata)
--   Removes all previous Subcomputations, setting SubCref as unique Subcomoputation of Cref
--
-- Set_Status  (Cref,Status,Depth);
-- a)  Sets Status,Depth of Cref to the given Status.Depth 
-- b)  if Status=ABORTED clear all subcomputations (and realted Evdata)
-- c)  nel caso di formule UCTL pure (This_Computation.Form.Env_Selector.all'Length=0)
--     con risultato definitivo FOUND_* consolida il risultato a tutte le subcomputations 
--       della medesima formula, e status=TMP_*
--
--
------------------------------------------------------------------------------------
--     RIASSUNTO OTTIMIZZAZIONI:
--            chiusura loop        risultato definitivo  c_depth < rec_depth & env_selector'Length=0
--
-- EF           TMP_FALSE           consolidate RESULT to all TMP_* subcpomputations
-- EG           FOUND_TRUE          nothing to consolidate
-- <<>>         TMP_FALSE           consolidate RESULT to all TMP_* subcpomputations
-- EU1          TMP_FALSE           consolidate RESULT to all TMP_* subcpomputations
-- EU2          TMP_FALSE           consolidate RESULT to all TMP_* subcpomputations          
-- EW1          FOUND_TRUE          nothing to consolidate
-- EW2          FOUND_TRUE          nothing to consolidate
--
-- AF           FOUND_FALSE         nothing to consolidate
-- AG           TMP_TRUE            consolidate RESULT to all TMP_* subcpomputations
-- [[]]         TMP_TRUE            consolidate RESULT to all TMP_* subcpomputations 
-- AU1          FOUND_FALSE         nothing to consolidate
-- AU2          FOUND_FALSE         nothing to consolidate
-- AW1          TMP_TRUE            consolidate RESULT to all TMP_* subcpomputations
-- AW2          TMP_TRUE            consolidate RESULT to all TMP_* subcpomputations
--


-----------------------------------------------------------------------
--Class Classname is
--Behavior
-- s0 -> s1 {-/a}
-- s1 -> s2 {-/a}
-- s1 -> s3 {-/a}
-- s2 -> s1 {-/a}
-- s0 -> s2 {-/b}
--end Classname;
-----
--  Obj1: Classname
--
-- la formula  max Z: ((AX{a}Z) or <b>Z)  viene data scorrettamente VERA
--se pero riordiniamo le transizioni :
--
-- s0 -> s2 {-/b}
-- s0 -> s1 {-/a}
-- s1 -> s2 {-/a}
-- s1 -> s3 {-/a}
-- s2 -> s1 {-/a}
--
--  allora viene corramente valutata come Falsa!!!
--
-- Il problema e' che AX{a}Z viene valutata temporaneamente Vera in (2) mentre poi risulta falsa
-- Quando valuto <b> Z in (0)   vado a rivalutare  Z in (2) e quiandi AX{a}Z in (2) recurando
--  il vecchio valore Temporaneo Vero che per√≤ e' in realta'  falso!
--
--  E' qiundi essenziale che le sottoformule   Z, AX{a}Z, <b>Z    quando Tmp-True/TMp_False
--   vengano rivalutate  se il loro context non e' vuoto!!!  Oppure che il loro stato venga lasciato a INPROGRESS
-- se la foormula non e' FOUND_XX o  il contesto della formula NON e' vuoto!!!
---------------------------------------------------------------------------------



--------------------------------------------------------------------
--  BINDINGS MULTIPLY:  semantica di  <a((X)> ,  a[(X)]
--
-- 1)  Vogliamo che la semantica di [a(X)] F(X)  sia:
--      Per ogni transizione "t" possibile:
--       Per ogni label "l" della transizione:
--        se b = match(a(X),l)  allora poi vale F(b)
--
-- 2)  Poiche'  <a(X)> F(X) vogliamo che sia definito come
--       not [a(X)] not F(X)
--    cio' implica che la semantica di <a(X)>   sia:
--       Esiste una transizione "t"  per cui:
--         Esiste una lable "l"  per cui:
--           b=match(a(X),l) and F(b)
--
--    nota che in questo modo:
--       <a(X)> F(X)        e
--       <a(X)> not F(X)    possono essere entrambe vere.  (ma cio√' ee' OK)
--
-- 3)  Vogliamo che EX{a(X)} F(X)    sia esattamente:   <a(X)> F(X)
--
-- 4)  Vogliamo che la sematica di AX{a(X)} F(X)   sia la stessa di EX{a(X)} F(X)
--    nel caso esista una unica transizione uscente dallo stato iniziale
--
-- 5)  Volgiamo che EF {a(X)} F(X)  sia true se EX{a(X)} F(X)
--
-- 6)  Vogliamo che AF {a(X)} F(X)  sia true se AX{a(X)} F(X)
--
--   Quindi
--   la semantica delle path formule, indipendentemente dal quantificatore utilizzato,
--    e che Esiste una transitione, per cui esiste una label
--          tale che b= match(a(X),l)  per cui successivamente vale F(b)
--
--  7) Questa e' la semantica generica dell'Until doppio.
--------------------------------------------------------------------


-- The effect of evaluating a "formula" at a certain state is the return of
--   a Result and of a Cref (computation reference); moreover as a side effect,
--   a computation item can be added (or modified) in the database of perfomed computations.
--

-- A "formula" is actually constituted by a pair:  A Form (syntactic formula) and a
--   "Context". The Form is just the structuref form of the formula string image; the Context
--   identifies the semantics of the free variables appearing inside the Form, associating them
--   to the state in which their corresponding max/min fixpoint definition is being evaluated.
--

-- The Result returned by the evaluation of a "formula" in one "state" can be one of the values:
--     ABORTED, FOUND_TRUE, FOUND_FALSE, TMP_TRUE, TMP_FALSE.
--     During the evaluation itself the computation can assume other temporary values
--     like NOT_YET_STARTED and IN_PROGRESS.
-- Examples of usual evulation sequences of a computation Result are:
--    NOT_YET_STARTED -> IN_PROGRESS -> FOUND_FALSE -> TMP_FALSE -> FOUND_TRUE     (EF)
--    NOT_YET_STARTED -> IN_PROGRESS -> FOUND_FALSE -> TMP_FALSE -> FOUND_FALSE     (EF)
--    NOT_YET_STARTED -> IN_PROGRESS -> FOUND_FALSE              (EF)
--    NOT_YET_STARTED -> IN_PROGRESS -> FOUND_FALSE -> ABORTED   (EF)
--    FOUND_FALSE -> FOUND_TRUE     (EX)
-- Formulas without FIXPOINTS should always return FOUND_TRUE/FALSE when their evaluation is
-- completed.
--  TMP/TRUE_FALSE should also be associated with loops inside nested recursive subcomputations

--  A recursive computation remains IN_PROGRESS, until its evaluation is completed
--    (i.e. when all the needed subcomputions on next steps are computed).
--    Once completed its returned value can be FOUND_TRUE / FOUND_FALSE, in which case
--    its value is definitive and does not depend from still ongoing evaluations since there are
--    no backward loops in the recursion (I.e Rec_Dept==C_Dept), or it can be
--    TMP_TRUE / TMP FALSE, which means a backward loop has been found.
--    In this last case the returned  Rec_Depth will be < current N_Depth, in particolar it
--    will be the Rec_Depth of the most ancient node in the path referred by a backward link.
--



--
-- N_Depth:  Incremented each the the evaluation passes from one state to another
--           (NOT incremented when a subevaluation is started on the same state)
--           When it reaches the Max_LTS_Depth, the evaluation is aborted.

--
-- Owner:  For recursive computations, the state in which the computation is initially started.
--       When a subcomputation with a given Owner /State has a definitive result
--       (FOUND_TRUE / FOUND_FALSE) the owner of the computation will be set to the current state.
--       When looking is a result exists for  <form, state, owner>, we first look if a result exists
--       for <form, state, state>.    Computations of the kind <form,state,state> always have
--       a definitive value (FOUND_TRUE / FOUND_FALSE).
--       Only computations with a TMP value may have Owner /= State. These subcomputations
--       may be reevaluated several times, returning their TMP value.
--
--

--
-- FromState:  The current state in which the evalution is (re)started.
--             Each time N_Depth in incremented, FromState is set to This_Next
--             (the next state on which the evaluation proceedes)

--
-- Context: The table of active rec fix point computations in which we are nested
--            (for each rec_id, giving to it its semantics)
--

--
-- Cref:    OUT value. The identifier of the ongoing computation.
--          It can be a newly created value when a new computation isn started.
--          It can be an existing value when the computation was already started and saved.
--          used to change the value of the computation (and to save the computation structure)

--
-- Result:  OUT value. The result of the execution of the computation. And its temporary value.
--           (NOT_YET_STARTED, IN_PROGRESS, ABORTED, FOUND_TRUE, FOUND_FALSE, TMP_TRUE,TMP_FALSE)
--          An computation (fragment) remains IN_PROGRESS, until its evaluation is completed
--          (i.e. when all the needed subcomputions on next steps are computed).
--          Once completed its returned value can be FOUND_TRUE / FOUND_FALSE, in which case
--          its value is definitive and does not depend from still ongoing evaluations since there are
--          no backward loops in the recursion (I.e Rec_Dept==C_Dept), or it can be
--          TMP_TRUE / TMP FALSE, which means a backward loop of a fixpoint has been found.
--          (backword loops in U F G [[]] <<>>  are handled replacing TMP_TRUE with FOUND__ in all
--            subcomputations)
--          In this last case the returned  Rec_Depth will be < current C_Depth, in particolar it
--          will be the Rec_Depth of the most ancient node in the path referred by a backward link.
--
--
-- Rec_Depth:  used only in Check_EF / AF / EG / AG
--            Initialized by C_Depth (hence incremented at each recursion step).
--            Saved in the computation data when a new computation is started,
--            Retrieved from the saved data when a recursive loop in discovered.
--            In case of recursion loop it can get a smaller value than the original C_Depth
--               (depending from the state in which the loop was entered)

--------------------------------------------------------------
--  BASIC PRINCIPLES:
--
--   QUANDO UNO FORMULA DIVENTA FOUND_XXXX  TUTTE LE SUE SOTTOCOMPUTAZIONI (NECESSARIE
--     PER EXPLANATIONS)   SE TMP_YYY  DIVENTANO FOUND_YYYY.
--     IN QUESTO CASO NON DEVONO ESISTERE SUBCOMPUTATIONS CHE DA TMP_XXX DIVENTANO FOUND_YYY PERCHE'
--     SE COSI' FOSSE VORREBBE DIRE CHE ESSE SONO IRRILEVANTI AI FINI DEL RISULTATO E QUINDI CHE
--     AVREBBERO DOVUTE ESSERE RIMOSSE DA UNA SET_COMPUATATION. ?????????????????????????????????
--
--   Il REC_DEPTH restituito da una valutazione e' sempre <= il C_DEPTH che gli viene passato.
--   Se REC_DEPTH < C_DEPTH allora la formula ha variabili di punto fisso il cui valore PUO' essere instabile.
--   La DEPTH smemorizzata in una COMPUTATION e' la C_DEPTH del punto in cui la computationviene (RI)iniziata.
--------------------------------------------------------------



