separate(UCTL.UCTL_Logics)
package body NWO is
--GDB: uctl.uctl_logics.nwo.xxx

  -----------------------------------------------------------------------
  ---------------------- PACKAGE FRAGMENTS ------------------------------
  -----------------------------------------------------------------------
package Fragments is

  type Fragment;
  type Fragment_Ref is access Fragment'Class with Volatile;

  -----------------------------------------------------
  -- appena creato (da Check_Fragment) lo stato ' JustCreated.
  -- subito dopo lo stato diventa ACTIVATED e vieno messo nello stack degli available
  -- quando il suo stato diventa definitivo puo diventare Found_True o Found_False
  -----------------------------------------------------
  type FragStatus is (JustCreated, ACTIVATED, In_Progress, Found_True, Found_False)
       with Atomic;

  type FragList;
  type FragListRef is access FragList with Atomic;
  --
  type FragList is record
     FragRef: Fragment_Ref with Atomic;
     FragNext: FragListRef with Atomic;
     FragPrec: FragListRef with Atomic;
  end record;

  type FragTable is array(Positive range <>) of Fragment_Ref;
  type FragTableRef is access FragTable;
  type FragStatusTable is array(Positive range <>) of FragStatus;
  type FragStatusTableRef is access FragStatusTable;

  type FragKind is (Primitive, Negation, ParallelOR, SequentialOR,  ParallelAND, SequentialAND);
  
  type FragQueue;
  type FragQueueRef is access FragQueue with Atomic;
  --
  type FragQueue is record
     FragRef: Fragment_Ref with Atomic;
     FragParent: Fragment_Ref with Atomic;
     FragNext: FragQueueRef with Atomic;
  end record;
  ------------------------------------------------------------------------
  ----------------------- RECORD TYPE FRAGMENT ---------------------------
  ------------------------------------------------------------------------
  type Fragment (FromState: System_Configuration; 
			Form: Formula_Ref;
  			Kind: FragKind) is tagged limited record
    Lock: Lock_Ref := new Lock_Data with Volatile;
    XSuperFrags: FragListRef; --  with Volatile;
    XStatus: FragStatus := JustCreated; -- with Volatile; -- default initialization important
    XIncomplete: FragListRef;
--    XSupers: Integer :=0;            --  might temporarily become negative!
    XSubFrags: FragTableRef;         -- used to further kill orphans
    XSubData: FragStatusTableRef;    -- used to optimize evaluations and killing
  end record;
  ------------------------------------------------------------------------
  ----------------------- END RECORD TYPE FRAGMENT -----------------------
  ------------------------------------------------------------------------
    --
    function GetStatus(X: in out Fragment) return FragStatus;
    procedure SetStatus(X: in out Fragment;Status:FragStatus);
    --
    procedure SetIncomplete(X: in out Fragment;Item:FragListRef; OldItem: out FragListRef);
    procedure GetNextSub(X: in out Fragment;NextSub: out Fragment_Ref);
    procedure Link(X: in out Fragment;
                  Parent: Fragment_Ref; NewStatus: out FragStatus; MySelf: in out Fragment_Ref);
                   
    procedure Notify(X: in out Fragment;
                     SubFrag: in Fragment_Ref;
                     SubStatus: in FragStatus;
                     SuperItems: out FragListRef;
                     NewStatus: out FragStatus);
    pragma InLine(Notify);

  -----------------------------------------------------------------------
  ---------------------- END PACKAGE FRAGMENTS --------------------------
  -----------------------------------------------------------------------
  end Fragments;
  use Fragments;
  --

  type Key_Data is record
    FromState: System_Configuration;        -- KEY
    Form: Formula_Ref;
    Kind: FragKind;
  end record;
  --
  function Equivalent (K1: Key_Data; K2: Key_Data) return Boolean is
  begin
    if K1.FromState /= K2.FromState then  return False; end if;
    if K1.Kind /= K2.Kind then  return False; end if;
    if K1.Form=null and K2.Form /=null then return False; end if;
    if K1.Form/=null and K2.Form =null then return False; end if;
    return K1.Form.FImage.all = K2.Form.Fimage.all;
  end Equivalent;

  function Mk_key (KK: Key_Data) return Natural is
  begin
     return Progressive(KK.FromState);
  end Mk_Key;

  procedure Set_Progressive(N:positive; ER: Fragment_Ref) is
  begin
   null; -- not needed
  end;

  function Initial_Element(KK: Key_Data) return Fragment_Ref is
    EE: Fragment_Ref;
  begin
    EE := new Fragment(KK.FromState,KK.Form,KK.Kind);
    return EE;
  end Initial_Element;

  ---------------------------------------------------
  ---------------------------------------------------
  type Suspension_Object_Ref is access Suspension_Object;
  --================
  ALLwait: Suspension_Object_Ref with Volatile;
  AllEvalDone: Boolean := False;
  pragma Atomic(AllEvalDone);
  ALLResult: FragStatus := JustCreated with Atomic;
  --================
  task type Worker (id:Natural) is 
     PRAGMA STORAGE_SIZE( taskstacksize);
  end Worker;
  type Wref is access Worker;
  WR: Wref;
  ---------------------------------------------------
  ---------------------------------------------------

  -------------------------------------------------------
  -- the stack/queue  of available fragments for concurrent elaboration
  --
  LockAvailables: Lock_Ref := new Lock_Data;
  FragsAvailable: FragListRef with Volatile;   -- used as simple list (frags just created)
  ---------------------------------------------------
  PoppedFrags: Counter32 :=0 with Atomic;
  PushedFrags: Counter32 :=0 with Atomic;
  EDGES: Counter32 :=0 with Atomic;
  -----------------------------------------------------

  --------------------------------------------------
  ---------- AG COMPUTATION DATA -------------------
  type AGData is record
    AGLock: Lock_Ref := new Lock_Data;
    -- AGIncomplete is a boubly linkd list of AGPar fragments whose evaluation is suspended
    --  (i.e. whose elaboration has been compled with status=ACTIVATED,  and
    --   not yet Consolidate to a definitive result). 
    AGIncomplete: FragListRef with Volatile;  -- used as double list (frags elab started but not completed)
    AGDone: Boolean;
    -- The AG root fragment of the current AG evaluation
    AGCurrent: Fragment_Ref;
    -- AGResult:  NOT_YET_STARTED -> ACTIVATED -> FOUND_XXX
    --
    AGResult: Computation_Status := NOT_YET_STARTED with Atomic;
    -- AGCOUNT is the number of AG/SeqAND  fragments whose evaluation is still in progress or waiting.
    -- AGCOUNT is incremented each time an AG/Seq is Pushed, and decremened each time
    --  its elaboration (or consolidation) is completed (either with a suspension or 
    --   with a definitive result)
    AGCOUNT: Counter32 :=0 with Atomic;  
    -- We need also a queue of waiting fragments (new AG roots waiting a previous one to complete)
    -- when a root AG computation completed a new AGroot frag is taked from the AGQUEUE and **Pushed**.
    --  (or **Elaborated**)
    AGQUEUE: FragQueueRef;
    --  Notice:  FRAGMENTS DEPENDENT ON QUEUED FRAGS are susended and waiting for a Notify
  end record;
  ---------------------------------------------------
  -- AGTable(Form.AGIndex) contains the data of the ongoing AG evaluation
  AGTable: array (1..100) of AGData;
  ---------------------------------------------------


  -------------------------------------------------------------------
  -------------------------------------------------------------------
  package FragHashes is new My_Hash(
     Fragment_Ref, 
     Key_Data,
     Equivalent,
     Mk_key,
     Initial_Element,
     Set_Progressive);
  use FragHashes;
  -------------------------------------------------------------------
  -------------------------------------------------------------------

    ------------------------------------------------------------------------
    -- 
    -- E.G.  FRAG A requires  B or C, and both are already ACTIVATED. When B completes C is
    -- no longer needed. C moreover depends on D.
    -- Another fragment A2 may exist which still depends on C.   
    -- *** D is orphan is all its leafs supers are FOUND_XXX ***
    -- *** D is not orphan if all its *root* fragments are ACTIVATED (occhio ai loops nei Supers!)
    -- POTREMMO TENERE UN ORPHANS_COUNT! e quando sono un certo numero si inizia la orphans collection.
    -- Oppure potremmo evitare per quanto possibile di generare orfani facendo si che
    --  quandi frag A necessita frag B, se esso e' gia' ACTIVATED aspetto prima di procedere 
    -- i.e.  valutazione sequenziale!).
    --
    -- IN VALUTAZIONE:  Tutte le valutazioni sono sequenziali, tranne  le formule AG.
    --  (due sottoformule AG sono diverse anche se sintatticamente uguali).
    -- Nella valutazione parallela di formula "AG fi" , le varie valutazioni di "fi" (e gli 
    --  spawning di AG) avvengono in parallelo. 
    -- Gli spawning in parallelo però possono essere gestiti senza ricorrere alla eliminazione 
    -- degli orfani (??? O NO ???).  
    -- Quando una root AG fi in s1 produce un risultato FALSE e' comunque necessario abortire tutti gli orfani,
    -- della altre AG spawned, altrimenti nuove valutazioni root di AG fi in s2 incontrano frammenti
    -- apparentemente in via di elaborazione ma in realta' in via di abbandono (perche pushed ma
    -- da non elaborare più).
    -- Possiamo quindi avere da una parte una attivita' di reset in corso, e dall'altra una
    -- attività di Link in corso che si sovrappongono.
    -- Se fi non contiene altre AG  e viene valutata sequenzialmente.
    -- Se fi contiene altre AG come sottoformula allora puo' succedere che la medesima formula
    -- AG fi2  sia valutata in parallelo!!!  
    -- In questocaso  il completamento con successo avviene quando entrambe le fi completano con successo.
    -- La presenza di loop viene riconosciuto solo quando entrambe sono terminate.
    -- il fallimento di una NON deve interferire con la valutazione della seconda.
    --
    ------------------------------------------------------------------------
    ------------------------------- RIEPILOGO ------------------------------
    ------------------------------------------------------------------------
    -- Elab_fragment:
    -- a)  Verifica lostato di un Fragment: CheckElement 
    --      (gia' stato creato? gia valutato? evenutalmente creandolo se necessario)
    -- b) Links un root frag al nuovo fragment (LinksTo) ottenendo come info
    --      se un risultato e' gia noto (FOUND__XXX) o se la valutazione e' in corso
    --       ACTIVATED (i.e. fragment already Pushed)
    -- c) Se il risultato e' noto lo gestisce, altrimenti si sospende sul root frag
    --
    -- Nel frattempo un task:
    -- a) preleva unn frag dalla lista  (Pop)
    -- b) attiva Elab_Fragment su questo frag:
    --
    -- Elab_Fragment:
    --    b1) Inizializza il frag  (Initialize) creando la progress map 
    --          (CheckElement per ogni possibile subfrag)
    --    b2) Prende un subfrag JUSTCREATED e ne attiva il link (Link)
    --    b3) Ne elabora il risultato (FOUND_XXX  ACTIVATED) aggiornando il frag (Update)
    --    b4)    a secondo dal tipo di frammento e del risultato  ripete b2-b4 quanto necessario
    --
    --  Durante la esecuzione di Elab puo' arrivare una richiesta di Unlink o Demote:
    --   quindi il ciclo b2-b4 viene CANCELLATO se SuperFrags=0. 
    --   In ogni caso la richiesta di demotion viene PROPAGATA ai subfrags gia ACTIVATED.
    --  Il passo b1 viene comunque fatto (o forse no).
    --  Se dopo aver bloccato la propria attivita' a seguito di Unlink il frammento
    --    viene riattivato da un nuovo Link o promosso da un Promote, occorre 
    --     RIPRENDERE il ciclo b2-b4 (se necessario: i.e a di Seq/parallel e status SubFrags).
    --   Vemgono compunque PROMOSSi tutti i subfrags ACTIVATED. 
    --
    -- Dopo che Elab e' completato possono arrivare richieste di Unlink che se
    --  necessario portano alla demotion dei subfrags ACTIVATED.
    --
    -- Durante o dopo la esecuzione di Elab, possono arrivare consolidamenti si risultati
    --   dei subfrags, che vengono valutati evantualmente portando al consolidamento
    --   del risultato di questo frag o alla attivazione di nuovi links (nel caso di frag
    --   sequenziale un nuovo link viene fatto solo dopo che il precedente e' FOUND_XXX).
    --
    -- La attivazione di nuovi Links viene sospesa nel caso il frammento sia stato congelato
    --   a seuito di Demote o Unkink, e viene ripresa qualora il frammento venga 
    --   promosso o riattivato in seguito a promote e Link.
    --
    -- Questo meccanismo di Promotion/Demotion dei subfrags ACTIVATED NON va fatto
    --  nel caso di formule ricorsive paralle AG+ParalllelOR perche' darebbe luogo a loops
    --  nel caso di cicli nel modello. Viene invece fatto nel caso AG+SequentialAND
    -- 
    --  OCCORRE RSOLVERE IL CASO RICORSIVO AG PRIMA DI IMPLEMENTARE TUTTO QUESTO?
    -- o almeno avere idea di come risolvere?
    -- UN MODO POTREBBE ESSERE QUELLO DI VALUTARE LA AG SEQUENZIALMENTE?
    -- possiamo avere due valutazioni parallele di AG che vanno in deadlock ???
    --   form = e.g. "AG1 (AG2 P)", oppure  "(AG2 P) ORpar (AG2 P)"
    --   model= s1 -> s2, s1 -> s3, s2 -> s3, s3 ->s2
    -- FORSE NON E' VERO!!  perche' nel caso:  "(AG2 P) OR (AG2 P)" le due sottoformule
    --   possono essere considerate DIVERSE!!!!
    --  e nel caso  "AG1 (AG2 P)" se AG1 e' sequenziale NON attivo due AG2 in parallelo!!
    --
    --  MA IL PAREALLELISMO COSI SI RIDUCE A SOLO GLI OR PARALLELI???
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    -- PROMOTIOINS e DEMOTIONS,   LINK e UNLINK
    -- Link => Promotion*, Unlink -> Demoption*
    -- Link => Super++, e Promotion di tutti i SubFrags=ACTIVATED (non ricorsivi).
    -- Unlink => Super--, e Demotion di tutti i SubFrags=ACTIVATED (non ricorsivi, DA RIVEDERE).
    -- Se Unlink/Demotion porta a Super <= 0 vengono SOSPESE NUOVE ATTIVAZIONI
    -- Se Link/Promotion porta a Super >0 :
    --     se Kind=Sequential e NON esiste subFrag ACTIVATED LINK Next_Fragment
    --     se Kind=Paralled e ESISTE subFrag JUSTCRATED LINK all of them (restart cycle)
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    
  procedure AGForceIncompleteTrue (AGIndex: Natural);
  procedure Push_Fragment (N: Fragment_Ref);
  procedure Elab_Fragment(TheFrag: Fragment_Ref);
    
  function DefaultKind (Formula: Formula_ref) return FragKind is
    begin
      if Formula.Kind=Ftrue or else
          Formula.Kind=Ffalse or else
          Formula.Kind=Fapply then
        return Primitive;
        --
      elsif Formula.Kind=Fangle or else
          Formula.Kind=Foor then
        return ParallelOR;
        --
      elsif Formula.Kind=Fsquare or else
          Formula.Kind=Fand then
        return ParallelAND;
        --
      elsif Formula.Kind=Fnot then
        return Negation;
       --
      elsif (Formula.Kind=Fall and then Formula.PRef.Kind= Always) then
        return SequentialAND;
       --
      elsif Formula.Kind=Fall and then Formula.PRef.Kind= Eventually then
         return SequentialOR;
      else
         Put_line(Standard_error, "Missing Formula.Kind");
         raise Program_Error;
      end if;
  end DefaultKind;

  -----------------------------------------------------------------------
  ---------------------- PACKAGE BODY FRAGMENT --------------------------
  -----------------------------------------------------------------------
  package body Fragments is

    function GetStatus(X: in out Fragment) return FragStatus is begin return X.XStatus; end;
    procedure SetStatus(X: in out Fragment;Status:FragStatus) is begin X.XStatus := Status; end;
    --
    procedure SetIncomplete(X: in out Fragment;Item:FragListRef; OldItem: out FragListRef) is
    begin
      SeizeLock(X.Lock);
      OldItem := X.Xincomplete;
      if OldItem=null and then Item/=null then
        X.Xincomplete := Item;
      elsif OldItem /= null and then Item=null then
        X.Xincomplete := Item;
      end if;
      ReleaseLock(X.Lock);
     end SetIncomplete;
    
    -----------------------------------------------------------------
    -- If there still subfrags to be activated returns the first of them.
    -- If the status is already FOUND_XXX or all subs have beeen activated
    --  return null;
    -- CHIAMATA da ELAB_FRAGMENT (e indirettamente da Consolidate nel caso
    -- di frags sequenziali), resituisce un frag di cui fare una Link,
    --  il cui risulatato TRUE/FALSE verra comunicato via Notify
    -----------------------------------------------------------------
    procedure GetNextSub(X: in out Fragment;NextSub: out Fragment_Ref) is
     NextFound: Fragment_Ref;
    begin
      SeizeLock(X.Lock);
      if X.XStatus /= FOUND_TRUE and then 
          X.XStatus /= FOUND_FALSE then
        for I in X.XSubData.all'Range loop 
          if X.XSubData(I)= JUSTCREATED and then
              NextFound = null then
             -- find first sub to be activated
             X.XSubData(I) := ACTIVATED;
             NextFound :=  X.XSubFrags(I);
             --
          elsif X.XSubData(I)= JUSTCREATED and then
              X.XSubFrags(I) = NextFound then
            -- handle duplications
            X.XSubData(I) := ACTIVATED;
          end if;
        end loop;
        NextSub := NextFound;
      else
        -- se lo stato e' gia definitivo,
        -- O SE IL FRAG E' STATO SOSPESO A CAUSA DI DEMOTIONS
        NextSub := null;
      end if;
      ReleaseLock(X.Lock);
    end GetNextSub;

    ------------------------------------------------------------------------


	
    ------------------------------------------------------------------------
    -- Called by Link when a new fragment is activated
    -- Initialize is independent from the FragKind (i.e. if parallel or sequential)
    --  because it simply sets the structure of the potentially needed subfrags.
    -- If there are no needed subfrags just sets the status to FOUND_XXX depending
    -- on the kind of formula.
    ------------------------------------------------------------------------
    procedure Initialize(X: in out Fragment) is 
       Form: Formula_Ref renames X.Form;
       FromState: System_Configuration renames X.FromState;
       Kind: FragKind renames X.Kind;
       UNUSED: Boolean;
       NewKey: Key_Data;
    begin
      if Form =null or else 
         X.XSubFrags/= null or else
         X.XStatus = FOUND_TRUE or else
         X.XStatus= FOUND_FALSE then 
        return; 
      end if;
      if Form.Kind = Ftrue then
        X.XStatus := FOUND_TRUE;  -- small optimization to avoid push and eval
         --
      elsif Form.Kind = Ffalse then
        X.XStatus := FOUND_FALSE; -- small optimization to avoid push and eval
         --
      elsif Form.Kind= FApply  then
        declare
           My_Iter: Evolutions_Iterator;
           Id_String: String(1..Form.IDen.all'Length) := Form.IDen.all;
        begin
          if Id_String = "FINAL" then
            Iterator_Initialize (My_Iter,System_Configuration(FromState));
            if Has_System_Transition (My_Iter)  then
              X.XStatus := FOUND_FALSE;
            else
              X.XStatus := FOUND_TRUE;
            end if;
            Iterator_Finalize (My_Iter);
          else
            null; -- if Id_String = "COUNT"   "PRINT" then
          end if;
        end;
        --
      elsif Form.Kind= Fnot  then
        X.XSubFrags := new FragTable(1..1);
        X.XSubData := new FragStatusTable(1..1);
        NewKey := (FromState,Form.NotRef,DefaultKind(Form.NotRef));
        X.XSubFrags(1) := CheckElement(NewKey,UNUSED);        --  <<<-------  POTENTIALLY SUSPENSIVE!!!!
        X.XSubData(1) := JUSTCREATED; -- i.e. yet to be seen
        --
      elsif form.Kind= Foor or form.Kind= Fand then
        X.XSubFrags := new FragTable(1..2);
        X.XSubData := new FragStatusTable(1..2);
        X.XSubFrags(1) := CheckElement((FromState,Form.LeftRef,DefaultKind(Form.LeftRef)),UNUSED);
        X.XSubData(1) := JUSTCREATED; -- i.e. yet to be seen
        X.XSubFrags(2) := CheckElement((FromState,Form.RightRef,DefaultKind(Form.RightRef)),UNUSED);
        X.XSubData(2) := JUSTCREATED; -- i.e. yet to be seen
        --
      elsif Form.Kind=Fangle or Form.Kind=Fsquare then
        declare
          My_Iter: Evolutions_Iterator ;
          This_Next: System_Configuration;
          SubFrags: FragTable(1..100);
          SubData: FragStatusTable(1..100);
          SubsCount: Natural :=0;
        begin
          Iterator_Initialize (My_Iter, FromState);
          while Has_System_Transition (My_Iter) loop
            if Classic.Evolution_Satisfies (My_Iter, Form.ARef) then
              SubsCount := SubsCount+1;
              This_Next := Get_Target_Configuration (My_Iter);
              SubFrags(SubsCount) := CheckElement((This_Next,Form.FormRef,DefaultKind(Form.FormRef)),UNUSED);
              SubData(SubsCount) := JUSTCREATED;  -- i.e. unknown
            end if;
            Iterator_Advance (My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
          X.XSubFrags := new FragTable'(SubFrags(1..SubsCount));
          X.XSubdata := new FragStatusTable'(Subdata(1..SubsCount));
          if SubsCount=0 then 
            if Form.Kind=Fangle  then 
              X.XStatus := FOUND_FALSE;
            else
              X.XStatus := FOUND_FALSE;
            end if;
          end if;
        end;
      elsif Form.Kind= Fall and then Form.Pref.Kind=Always and then Kind=SequentialAND then
         -- FINALLY THE AG CASE   CASE 1
         --   Form = AG Form.PRef.Tform
        X.XSubFrags := new FragTable(1..2);
        X.XSubData := new FragStatusTable(1..2);
        X.XSubFrags(1) := CheckElement((FromState,Form.PRef.Tform,DefaultKind(Form.PRef.Tform)),UNUSED);
        X.XSubData(1) := JUSTCREATED; -- i.e. yet to be seen
        X.XSubFrags(2) := CheckElement((FromState,Form,ParallelAND),UNUSED);
        X.XSubData(2) := JUSTCREATED; -- i.e. yet to be seen
        --
      elsif Form.Kind= Fall and then Form.Pref.Kind=Always and then Kind=ParallelAND then
        -- CASE 2:    for all next:  AG(seqAND) 
        declare
          My_Iter: Evolutions_Iterator ;
          This_Next: System_Configuration;
          SubFrags: FragTable(1..100);
          SubData: FragStatusTable(1..100);
          SubsCount: Natural :=0;
        begin
          Iterator_Initialize (My_Iter, FromState);
          while Has_System_Transition (My_Iter) loop
            SubsCount := SubsCount+1;
            This_Next := Get_Target_Configuration (My_Iter);
            SubFrags(SubsCount) := CheckElement((This_Next,Form,SequentialAND),UNUSED);
            SubData(SubsCount) := JUSTCREATED;  -- i.e. unknown
            Iterator_Advance (My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
          X.XSubFrags := new FragTable'(SubFrags(1..SubsCount));
          X.XSubdata := new FragStatusTable'(Subdata(1..SubsCount));
          if SubsCount=0 then
            X.XStatus := FOUND_TRUE;
          end if;
        end;
      end if;
--      X.XSupers:=0; -- wil be incremented by Link
    end Initialize;
    ------------------------------------------------------------------------


    ------------------------------------------------------------------------
    -- Called by Consolidate or Eval when a dependence of a parent towards this fragment is Found.
    -- If the fragment was in Status JUSTCREATED it evolves intop ACTIVATED 
    --   meaning "Ready to be Pushed"
    -- A Link operation on a fragment is followed by an Update operation on the parent frag.
    -- A Link operation directly increases the Fragment Supers, and will indirectly 
    --  increase the Supers of all existing ACTIVATED subfragments (except AG forms).
    -----------------------------
    -- Form NON e' AG devo restituire tutti i subfrags ACTIVATED per poteri promuovere!!
    -- SE FACESSI LE PROMOTIONS DIRETTAMENTE QUI, POTREI AVERE DEADLOSKS IN CASO DI LOOPS NEL MODELLO.   
    -- E ANCHE NEL CASO   s1 -a-> s1
    -----------------------------
    -- Se Supers era <= 0 allora il frammento era congelato e devo scongelarlo.
    -- Cio' cio' avviene 
    --   a) promuovendo i suoi subfrags ACTIVATED (indirettamente)
    --   b) eventualmente facendo ripartire le valutazione dei sub JUSTCREATED (se Kind=Parallel)
    --       o se nessun subfrag e' ACTIVATED.
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    --  NON sto' elaborando MySELF! Dovo solo aggiungere il Parent ai SuperFrags
    -- e' notificargli se questa frammento e' nuovo e da pushes (Mysef/= null) o se un
    -- valore noto NewStatus=FOUND_XXX, o se la sua elaborazione e' gia' in corso NewStatus=ACTIVATED
    -- NON so nulla del valore dei subfrags, che verra' oreso in considerazione quando essi saranno linkati
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    procedure Link(X: in out Fragment;
           Parent: Fragment_Ref; NewStatus: out FragStatus; MySelf: in out Fragment_Ref)is
       Form: Formula_Ref renames X.Form;
       FromState: System_Configuration renames X.FromState;
       Kind: FragKind renames X.Kind;
      NewSuper: FragListRef;
      OldStatus: FragStatus;
    begin
      SeizeLock(X.Lock);
      X.Initialize;
      OldStatus := X.XStatus;
      if X.XStatus = JUSTCREATED or else 
          X.XStatus = ACTIVATED then
        NewSuper := new FragList;
        NewSuper.FragRef := Parent;
        NewSuper.FragNext := X.XSuperFrags;
        X.XSuperFrags := NewSuper;
--        X.XSupers := X.XSupers+1;
        if X.XStatus = JUSTCREATED then
          X.XStatus := ACTIVATED;
          -- leave MySelf untouched so that so that this frag can be pushed.
        else
          MySelf := null;
          -- notify that the fragment should not be pushed
        end if;
      else -- XSTATUS = FOUND_XXX
        MySelf := null;
      end if;
      NewStatus := X.XStatus;
      ReleaseLock(X.Lock);
      ---------------------------
      --  Il problema e:     [true] AG form,  dove [true]=parallelAND
      -- in questo caso vengono attivate diverse AG con diverse root e stessa formula,
      -- che INTERFERISCONO:    e.g. quando una termina FALSE il reset ammazza anche le seconda!!
      -- e che e' quindi meglio SEQUENZIALIZZARE!
      -- COME SI PUO' OTTENERE CIO?
      ---------------------------
      -- Nel caso di AG quando questo frag (Myself) viene linkato da un Parent,
      --  DOVREI PROCEDERE SOLO NEL CASO DI UNICA VALUTAZIONE IN CORSO DELLA FORMULA
      -- Potrei anche automaticamente Linkare questo frammento(Appena inizializzato) 
      --  al suo primo subfrag, e se questo gia' complato au suo secondo subfrag,
      -- restituendo in NewStatus il resultato dei tre passi (myself, sub1 e sub2)
      -- ed in MySelf il riferimento al sucessivo fragment da pushare.
      ---------------------------
    end Link;
    

    ------------------------------------------------------------------------
    -- Una Notify e' la consequenza di una GetNextSub e la sua successiva Link.
    -- Una Notify(TRUE/FALSE) arriva quando il subfrag completa e normalmente si
    --  riferisce ad un subfrag il cui status e' ACTIVATED. 
    -- Puo' anche riferisrsi un ad un subfrag gia' TRUE/FALSE in caso di doppioni.
    -- Notify puo' essere chiamata dalla Elaborate_Fragment che da una Consolidate.
    -- Una Notify(TRUE/FALSE) puo' anche arrivare con SubFrag=null, nel caso
    --  di loops fra frags indotti da formule ricorsive.
    -- Quando la Notify e' chiamata da Consolidate e il frammento e' Sequential,
    --  essa e' seguita da Eval_Fragment (i.e. getNextFrag + Link + Notify)
    ------------------------------------------------------------------------
    procedure Notify(X: in out Fragment;
                     SubFrag: in Fragment_Ref;
                     SubStatus: in FragStatus;
                     SuperItems: out FragListRef;
                     NewStatus: out FragStatus) is
       Form: Formula_Ref renames X.Form;
       FromState: System_Configuration renames X.FromState;
       Kind: FragKind renames X.Kind;
      FDone: Natural :=0;
      TDone: Natural :=0;
      Updated: Boolean := False;
    begin
      SeizeLock(X.Lock);
      -- eventuali arrivi di notify quando lo stato e' definitivo vengono ignorati
      -- e.g. nel caso ParallelOR puo' arrivare una seconda Notify(True) o Notify(FALSE)
      -- dopo che una prima Notify(TRUE) ha gia dato il risultato definitivo.
      if X.XStatus = FOUND_TRUE or else X.XStatus = FOUND_FALSE then
         NewStatus := X.XStatus;
         SuperItems := null;
         ReleaseLock(X.Lock);
         return;
      end if;
      -- Caso speciale AG: il completamento di una AG che e' parte di un loop
      -- In questo caso non c'e' Subfrag specifico che fa la notify
      if SubFrag = null and then 
          (SubStatus = FOUND_TRUE or SubStatus = FOUND_FALSE)  then
         X.XStatus := SubStatus;
         NewStatus := X.XStatus;
         SuperItems := X.XSuperFrags;
         ReleaseLock(X.Lock);
         return;
         -- maybe we could set to Substatus all the recursive still ACTIVATED subsfrags 
         -- (maybe for explanations)
      end if;
      --
      -- ANALIZZA TUTTI I SUBFRAGS PER TROVARE QUELLI A CUI SI RIFERISCE,
      -- CONTANDO NEL FRATTEMPO QUELLI TRUE E FALSE e MEMORIZZANDO
      -- IL PRIMO JUSTCREATED INCONTRATO
      --
      for I in reverse X.XSubFrags.all'Range loop
         -- notice: all matching subfrags are updated
         if X.XSubData(I) = ACTIVATED and then (X.XSubFrags.all(I)= SubFrag) then
           X.XSubData(I) := SubStatus;
         else
           -- SubData=ACTIVATED but SubFrag not matching
           null;
         end if;
         -- notice X.SubData(I) may have been changed by previous stmt
         if X.XSubData(I) = FOUND_TRUE then
            TDone := TDone+1;
         elsif X.XSubData(I) = FOUND_FALSE then
            FDone := FDone+1;
         end if;
      end loop;
      --
      if Kind = Negation and then SubStatus = FOUND_TRUE then
        X.XStatus := FOUND_FALSE;
        NewStatus := FOUND_FALSE;
        SuperItems := X.XSuperFrags;
        --
      elsif Kind = Negation and then SubStatus = FOUND_FALSE then
        X.XStatus := FOUND_TRUE;
        NewStatus := FOUND_TRUE;
        SuperItems := X.XSuperFrags;
        --
      elsif Kind = ParallelOR and then SubStatus = FOUND_TRUE then
        X.XStatus := FOUND_TRUE;
        NewStatus := FOUND_TRUE;
        SuperItems := X.XSuperFrags;
        --
      elsif Kind = ParallelOR and then SubStatus = FOUND_FALSE then
        if FDone = X.XSubFrags.all'Length then
          X.XStatus := FOUND_FALSE;
          NewStatus := FOUND_FALSE;
          SuperItems := X.XSuperFrags;
        else
          NewStatus := ACTIVATED; --(XStatus)
          SuperItems := null;
        end if;
        --
      elsif Kind = ParallelAND and then SubStatus = FOUND_FALSE then
        X.XStatus := FOUND_FALSE;
        NewStatus := FOUND_FALSE;
        SuperItems := X.XSuperFrags;
        --
      elsif Kind = ParallelAND and then SubStatus = FOUND_TRUE then
        if TDone = X.XSubFrags.all'Length then
          X.XStatus := FOUND_TRUE;
          NewStatus := FOUND_TRUE;
          SuperItems := X.XSuperFrags;
        else
          NewStatus := ACTIVATED; --(XStatus)
          SuperItems := null;
        end if;
        --
      elsif Kind = SequentialAND and then SubStatus = FOUND_FALSE then
        X.XStatus := FOUND_FALSE;
        NewStatus := FOUND_FALSE;
        SuperItems := X.XSuperFrags;
        --
      elsif Kind = SequentialAND and then SubStatus = FOUND_TRUE then
        if TDone = X.XSubFrags.all'Length then
          X.XStatus := FOUND_TRUE;
          NewStatus := FOUND_TRUE;
          SuperItems := X.XSuperFrags;
        else
          NewStatus := ACTIVATED; --(XStatus)
          SuperItems := null;
        end if;
        --
      else
       NewStatus := X.XStatus;
       SuperItems := null;
      end if;
      ReleaseLock(X.Lock);
    end Notify;
    
  end Fragments;
  -----------------------------------------------------------------------
  ---------------------- END BODY FRAGMENTS -----------------------------
  -----------------------------------------------------------------------


  procedure Free is new Ada.Unchecked_Deallocation(FragList,FragListref);
  procedure Free is new Ada.Unchecked_Deallocation(Fragment'Class,Fragment_Ref);
  --
  ---------- AG COMPUTATION DATA ------------------------
  --
  procedure Consolidate(TheFrag: Fragment_Ref;
                         SubFrag: Fragment_Ref;
                         SubStatus: FragStatus);


  ---------------------------------------
  -- TO BE DONE:   SKIP PARENTS WITH SUPERS=0
  ---------------------------------------
  -- Called by the Workers taks in their main loop consisting in popping a new
  --  fragment and elaboratong it.
  ---------------------------------------
  -- If there are more active workers than available frags just loop and dely.
  ---------------------------------------
 function Pop_Fragment return Fragment_Ref is
    TopFrag: Fragment_Ref;
    NF: FragListRef;
  begin
    SeizeLock(LockAvailables);
    while (not AllEvalDone) loop
      --if Debug then Put("POP "); end if;
      if FragsAvailable /= null then
         NF := FragsAvailable;
         FragsAvailable := FragsAvailable.FragNext;
         -- RIMOSSO:   NF.FragRef.SetStatus(In_Progress);  A CHE SERVE??  
         -- if Debug then
         --  Put_line("POPPED " &
         --     NF.FragRef.Form.Fimage.all & " (" &
         --     Integer'Image(Progressive(NF.FragRef.Fromstate)) & ")");
         -- end if;
         exit;
      else
        ReleaseLock(LockAvailables);
        delay 0.01;  
        SeizeLock(LockAvailables);
     end if;
    end loop;
    ReleaseLock(LockAvailables);
    if AllEvalDone then
       return null;
    else
      TopFrag := NF.FragRef;
      Free (NF);  -- free the list item container
      Increment32(PoppedFrags'Address);
      if PoppedFrags mod 20_000 = 0 then
         Put_Line("frags: " & Counter32'Image(PoppedFrags) &
            " (" & Counter32'Image(PushedFrags - PoppedFrags) & " )" );
      end if;
      return TopFrag;
    end if;
  end Pop_Fragment;
  ---------------------------------------

  ---------------------------------------
  -- Called after a Link operation inside Elab_Fragment, and
  --  Indirectly inside a Consolidate in the case Sequential Frags
  ---------------------------------------
  procedure Push_Fragment (N: Fragment_Ref) is
     NF : FragListRef;
  begin
     SeizeLock(LockAvailables);
     NF := new FragList;
     NF.FragNext := FragsAvailable;
     NF.FragRef := N;
     if Debug then
       Put_line("  Pushing: " & N.Form.Fimage.all);
     end if;
     FragsAvailable := NF;
     ReleaseLock(LockAvailables);
     Increment32(PushedFrags'Address);
     --if Flags.Debug then Put_line("PUSHED: " &
     --    N.Form.Fimage.all & " (" &  -- if FORM /= null!!
     --    Integer'Image(Progressive(N.FromState)) & ")");
     --end if;
     -- if AddDone reset fragment to JustCreated.
  end Push_Fragment;
  ---------------------------------------
  
  
  ---------------------------------------
  procedure Insert_Incomplete(Frag: Fragment_Ref) is
    Item: FragListRef := new FragList;
    OldItem: FragListRef;
  begin
    Frag.SetIncomplete(Item,OldItem); 
    if OldItem=null then
      SeizeLock(AGTable(Frag.Form.AGIndex).AGLock);
      -- inizializza nuova head della lista
      Item.FragRef := Frag;
      Item.FragNext := AGTable(Frag.Form.AGIndex).AGIncomplete;
      -- aggancia la head alla vecchia AGIncomplete
      if AGTable(Frag.Form.AGIndex).AGIncomplete /= null then
         AGTable(Frag.Form.AGIndex).AGIncomplete.FragPrec := Item;
      end if;
      -- aggiorna AGIncomplete
      AGTable(Frag.Form.AGIndex).AGIncomplete := Item;
      ReleaseLock(AGTable(Frag.Form.AGIndex).AGLock);
    end if;
    --
    -- if AddDone reset fragment to JustCreated. 
       null;
  end Insert_Incomplete;

  ---------------------------------------
  procedure Remove_Incomplete (Frag: Fragment_Ref) is
     Item: FragListRef;
  begin
    Frag.SetIncomplete(null,Item);
    if Item /= null then
      SeizeLock(AGTable(Frag.Form.AGIndex).AGLock);
      if Item.FragNext /= null then
         Item.FragNext.FragPrec := Item.FragPrec;
      end if;
      if Item.FragPrec /= null then
         Item.FragPrec.FragNext := Item.FragNext;
      else
         AGTable(Frag.Form.AGIndex).AGIncomplete := Item.FragNext;
      end if;
      ReleaseLock(AGTable(Frag.Form.AGIndex).AGLock);
    end if;
    --  ??? Free(Item);
    -- if AddDone reset fragment to JustCreated.
    null;
  end Remove_Incomplete;

  

  procedure AGRoot_Complete (AGIndex: Natural) is
    NextSub: Fragment_Ref;
    ToBePushed: Fragment_Ref;
    ItsParent: Fragment_Ref;
    SubStatus: FragStatus;
    NEWSTATUS: FragStatus;
    SUPERFRAGS: FragListRef;
    Resumed: Boolean := False;
    AGInfo: AGData renames AGTable(AGIndex);
  begin
    if Flags.Debug then
       Put_line(Standard_Error, "  AGRoot_Complete: " &
          AGInfo.AGCurrent.Form.Fimage.all & 
          "(s" & Integer'Image(Integer(AGInfo.AGCurrent.FromState)) & ")" );
    end if;
    SeizeLock(AGTable(AGIndex).AGLock);
    if AGInfo.AGQUEUE /= null then
      while AGInfo.AGQUEUE /= null loop
        NextSub :=  AGInfo.AGQUEUE.FragRef;
        ItsParent :=  AGInfo.AGQUEUE.FragParent;
        --- maybe Free AGTable(AGIndex).AGQUEUE
        AGInfo.AGQUEUE := AGInfo.AGQUEUE.FragNext;
        AGInfo.AGCOUNT :=1;
        AGInfo.AGResult := IN_PROGRESS;
        AGInfo.AGCurrent := NextSub;
        ------------------------------------
        -- Devo Prima fare il LINKING !!!!!!
        -- e il Notify!!!!!
        ToBePushed := NextSub;
        NextSub.Link(ItsParent,SUBSTATUS,TOBEPUSHED);
        if SubStatus=FOUND_TRUE or else
            SubStatus=FOUND_FALSE then
          ItsParent.Notify(NextSub,SubStatus,SUPERFRAGS,NEWSTATUS);
          while SuperFrags /= null loop
            Consolidate(SuperFrags.FragRef,ItsParent,NewStatus);
            SuperFrags := SuperFrags.FragNext;
          end loop;
        else
          if ToBePushed /= null then
            Push_Fragment(ToBePushed);
            if Flags.Debug then
              Put_line(Standard_Error, "AGRoot_Resumed: " &
                 AGInfo.AGCurrent.Form.Fimage.all & 
                  "(s" & Integer'Image(Integer(AGInfo.AGCurrent.FromState)) & ")" );
            end if;
            exit;
          end if;
        end if;
        ------------------------------------
      end loop;
    else
      AGInfo.AGResult := NOT_YET_STARTED;
      AGInfo.AGCOUNT :=0;
      AGInfo.AGCurrent := null;
    end if;
    ReleaseLock(AGInfo.AGLock);
  end AGRoot_Complete;

  -------------------------------------
  -------------------------------------
  procedure AGRoot_Start (TheFrag: in out Fragment_Ref;
                          TheParent: in Fragment_Ref) is
  begin 
    -- ASSERT (TheFrag.Form.Kind = Fall and then
    --       TheFrag.Form.Pref.Kind = Always and then
    --       TheFrag.Kind = SequentialAND
        --
        --    *** NextSub *** is a  ROOT  AG/Seq
        --
        SeizeLock(AGTable(TheFrag.Form.AGIndex).AGLock);
        if AGTable(TheFrag.Form.AGIndex).AGResult = NOT_YET_STARTED then
          -- or maybe  AGCurrent = null !!!!!!!!!
          --
          --  inizio una prima elaborazione di questo tipo di AG
          --
          AGTable(TheFrag.Form.AGIndex).AGResult := IN_PROGRESS;
          AGTable(TheFrag.Form.AGIndex).AGCurrent := TheFrag;
          AGTable(TheFrag.Form.AGIndex).AGCOUNT := 1;
          AGTable(TheFrag.Form.AGIndex).AGIncomplete := null;
          ReleaseLock(AGTable(TheFrag.Form.AGIndex).AGLock);
          if Flags.Debug then
             Put_line(Standard_Error, "AGRoot_Started: " &
                TheFrag.Form.Fimage.all &
                "(s" & Integer'Image(Integer(TheFrag.FromState)) & ")" );
          end if;
          --
        else
          --
          --  NON posso iniziare questa root evaluation di una AG
          --  quindi la incodo in AGQUEUE
          --*****************************s
          -- Ma in che stato lascio il frammento?
          -- In che stato lascio l'immagine del subfrag qui??
          -- DOVREI SETTARGLI L'IMMAGINE DELLO STATO A ACTIVATED ????
          --  (come minimo per evitare di ritrovarmelo fra i piede con una successiva GetTheFrag)
          -- Quando viene prelevato dalla coda come si fa a ricreare il LINK al Frag attuale
          --   come SuperFrag?
          -- POTREI qui mettergli l'immagine dello stato a ACTIVATED anche se NON lo e'.
          --        lasciargli il suo stato a JUSTCREATED ( o quelllo che e', magari ACTIVATED)
          --   valutare se mattere il Super come info aggiuntiva alla AGqueue (con link fatto poi)
          --  o se aggiungere il link adesso. (cosa succede in caso di reset?)
          -- Da capire se il caso in cui il TheFrag sia gia' FOUND_TRUE/FALSE il risultato possa
          --  essere riciclato oppurre no. Se lo riciclo rischio di far partire una AG covered??
          -- probabilmente no. Quindi vale la pena riciclare.
          --******************************
          declare
            QAG: FragQueueRef := new FragQueue;
          begin
            QAG.FragRef := TheFrag;
            QAG.FragParent := TheParent;
            QAG.FragNext :=  AGTable(TheFrag.Form.AGIndex).AGQUEUE;
            AGTable(TheFrag.Form.AGIndex).AGQUEUE := QAG;
            ReleaseLock(AGTable(TheFrag.Form.AGIndex).AGLock);
            if Flags.Debug then
               Put_line(Standard_Error, "AGRoot_Queued: " &
                  TheFrag.Form.Fimage.all &
                  "(s" & Integer'Image(Integer(TheFrag.FromState)) & ")" );
            end if;
            TheFrag := null;
          end;
        end if;
  end AGRoot_Start;

  ------------------------------------------------
  -- Consolidate viene chiamata quando quando lo stato di un fragment passa
  -- da  ACTIVATED a FOUND_TRUE/FOUND_FALSE causa di un SubFrag che a sua
  --  volta era diventato FOUND_TRUE/FOUND_FALSE
  ------------------------------------------------
  procedure Consolidate(TheFrag: Fragment_Ref; 
                         SubFrag: Fragment_Ref;
                         SubStatus: FragStatus) is
    SuperItems: FragListRef;
    NewStatus: FragStatus;
    NEWCOUNT:Counter32;
  begin
    if TheFrag.Form = null then
      -- this is the Root fragment
      ALLResult := SubStatus;
      Set_True(ALLWait.all);
      return;
    end if;
    if Debug then
      Put_Line("  Consolidate: Sub is " & FragStatus'Image(SubStatus) & " "  & TheFrag.Form.Fimage.all & " (" &
                Integer'Image(Progressive(TheFrag.FromState)) & ")" );
    end if;
    -- notify the definitive result to the fragment, and if necessary Consolidate the
    --  result to all its SuperFrags
    --
    TheFrag.Notify(SubFrag,SubStatus,SUPERITEMS,NEWSTATUS);
    if NEWSTATUS = FOUND_TRUE or NEWSTATUS=FOUND_FALSE then
      -- If the frag being Consolidated is an AG/Par fragment, remove it from
      --   the IncompleteFrags List
      if TheFrag.Form.Kind=Fall and then
         TheFrag.Form.Pref.Kind=Always and then
         TheFrag.Kind= ParallelAND and then
         SUPERITEMS /= null then
        Remove_Incomplete(TheFrag);
        -- Notice: Consolidation can occur multiple times! the check on SUPEITEMS
        --  guaranteed that removal is done only once.
      end if;
      --
      -- SuperItems will be null if the status of the subfrag was already definitive
      while SuperItems /= null loop
        if Debug then Put("  Consolidate:"); end if; 
        Consolidate(SuperItems.FragRef,TheFrag,NewStatus);
        SuperItems := SuperItems.FragNext;
      end loop;
      --
      if TheFrag.Form.Kind=Fall and then TheFrag.Form.Pref.Kind=Always and then
          TheFrag.Kind= SequentialAND and then
          NEWSTATUS=FOUND_FALSE and then
          (SubFrag.Kind /= ParallelAND or else 
            SubFrag.Fromstate /= TheFrag.FromState or else
            SubFrag.Form.Fimage.all /= TheFrag.Form.Fimage.all) then
        --
        --  TheFrag= AGSeq/Case A) FALSE
        --
        -- NOTICE: This particulat kind of Consolidation can appear at most ONCE
        --  (i.e. AG/Seq has only two subfrags, and only the first is not a AG/Par)
        --
        AGTable(TheFrag.Form.AGIndex).AGresult := FOUND_FALSE;
        --
        -- i.e. [NextSub.SubData(2)= JUSTCREATED] !!
        -- I.E. NEWSTATUS IS RELATED TO THE FIRST (FF) ALTERNATIVE of the SequentialAND
        --   if during the evaluation of AG FF, the evaluation of FF returns FALSE
        --   the valuation if AG FF is abandoned with FALSE value
        NEWCOUNT := Decrement32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
        --   actually, since tht whole AG is consolidated as FALSE, and all leftover 
        --   activities demoted, we could avoid this check and update.
        --   Decrementing the AGCOUNT we still allow loops to become TRUE
        --
        if NEWCOUNT=0 then
           AGForceIncompleteTrue(TheFrag.Form.AGIndex);
           -- Having already Consolidated the FOUND_FALSE, the remaining incomplete
           -- frags areclosed loop elements  (SURE!??!?!)
           -- NEWCOUNT=0 when all frags have been pushed and elaborated. 
           -- if the valuation of AG has been INTERRUPTED by the discovering of the
           --  FOUND_FALSE value, we may have incomplete frags which are not loop elems.
           -- THEREFORE: either we DO NOT Interrupt, or we do RESET incomplete not loop
           --  elems.  (i.e. frag which decides to abort, consolidates the reset )
           --
--           if AGTable(TheFrag.Form.AGIndex).AGCurrent = TheFrag then
             AGRoot_Complete(TheFrag.Form.AGIndex);
--           end if;
        end if;
      --
      elsif TheFrag.Form.Kind=Fall and then TheFrag.Form.Pref.Kind=Always and then
            TheFrag.Kind= SequentialAND and then
            AGTable(TheFrag.Form.AGIndex).AGCurrent = TheFrag and then
            NEWSTATUS=FOUND_TRUE then
        --
        --  TheFrag = AGSeq Case B)  TRUE
        --
          --
          --  ASSERT (AGTable(TheFrag.Form.AGIndex).AGCOUNT =0)
          --  ASSERT (AGTable(TheFrag.Form.AGIndex).AGIncomplete =null)
          --
          AGRoot_Complete(TheFrag.Form.AGIndex);
          -- Se AGSeq diventa TRUE allora tute i subfrag sono necessariamente finiti
          --  e AGCOunt e' appena diventato =0  (VERO??!?)
          -- Posso e Devo notificate la fine della AG
          --
    --  elsif AGTable(TheFrag.Form.AGIndex).AGCurrent = TheFrag and then
    --       NEWSTATUS=FOUND_FALSE and then
    --       (AGTable(TheFrag.Form.AGIndex).AGCOUNT =0)  then
    --  --
    --  --  TheFrag = AGSeq Case B)  FALSE
    --  --
    --  -- QUESTO CASO NON CI DEVE ESSERE PERCHE GIA GESTITO ALTROVE
    --    --
    --    AGForceIncompleteTrue(TheFrag.Form.AGIndex);
    --    AGRoot_Complete(TheFrag.Form.AGIndex);
    --    -- 
    --    end if; 
      end if;
      --
    elsif NEWSTATUS=ACTIVATED and then TheFrag.Kind= SequentialAND then
      if Debug then
        Put("  Consolidate: ");
      end if;
      Elab_Fragment(TheFrag); 
    end if;
  end Consolidate;
  -------------------------------------------------

----------------------------------------
----------------------------------------
--   AGCOUNT:   incrementatato ad ogni push di AGSeq/AGPar
--   DECREMENTATO: Da AGSeq quando Phi e' *immediatamente* False
--   DECREMENTATO: Da AGSeq quando AGpar e' stato Linked (Sia TRUE/FALSE/ACTIVATED)
--   DECREMENTATO: Da AGPar quando tutti subfrag sono stati Linked
----------------------------------------
----------------------------------------
--   AGRESULT: diventa FOUND_TRUE quando AGCOUNT=0 e AGRESULT=INPROGRESS
--   AGRESULT: diventa FOUND_TRUE quando AGRESULT=INPROGRESS e AGCURRENT diventa FOUND_TRUE
--   AGRESULT: diventa FOUND_FALSE quando AGSeq/Phi (case A) diventa FOUND_FALSE
----------------------------------------
----------------------------------------
--   AGRoot_Complete: chiamata quando AGCOUNT diventa = 0 
----------------------------------------
----------------------------------------


----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
--                     ELAB_FRAGMENT                               --
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------

 procedure Elab_AGSeq(TheFrag: Fragment_Ref) is
    ToBePushed: Fragment_Ref;
    NextSub: Fragment_Ref;
    NewStatus: FragStatus := ACTIVATED;
    SubStatus: FragStatus;
    SuperFrags: FragListRef;
    NEWCOUNT: Counter32;
  begin
    ------------------------------
    -- NOTICE: The AGCounter related to this evaluation has already been incremented
    --   by the worker task which has pushed it.
    -- THIS PROCEDURE IS CALLED:
    --   A) When a worker task POPs a fragment
    --   B) When the PHI part of AG PHI consolidates to TRUE
    ------------------------------
    if Flags.Debug then
       Put_Line(Standard_Error, "ElabAG/Seq: "&
           TheFrag.Form.Fimage.all & " (s=" &
           Integer'Image(Progressive(TheFrag.FromState)) & ") AGCOUNT=" 
           & Counter32'Image(AGTable(TheFrag.Form.AGIndex).AGCOUNT));
    end if;
    --
    TheFrag.GetNextSub(NextSub);
    --
    --------------------  AG FALSE COMPLETION -------------------
    --
--    FOW NOW WE DO NOT INTERRUPT FALSE AG EVALUATIONS
--    if NextSub /= null and then 
--        AGTable(TheFrag.Form.AGIndex).AGResult = FOUND_FALSE then
--       --  Similar to the case A) FOUND FALSE 
--       --  only that we reset, not notify False
--       NEWCOUNT := Decrement32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
--       return;
--    end if;
    --
    if (NextSub.Form.Kind/=Fall or else NextSub.Form.Pref.Kind/=Always or else
          NextSub.Kind/=ParallelAND) then
      --
      -------------  WE ARE IN CASE A) ----------------------
      --
      if (NextSub.Form.Kind = Fall and then NextSub.Form.Pref.Kind = Always and then
            NextSub.Kind = SequentialAND) then
        --
        --        special case: "AG AG PHI"
        --
        AGRoot_Start(NextSub,TheFrag);
        if NextSub= null then
           return;
        end if;
      end if;
      --
      ToBePushed := NextSub;  --  THIS is the PHI part of AG PHI
      ---------------------------------------------
      NextSub.Link(TheFrag,SUBSTATUS,ToBePushed);
      ---------------------------------------------
      --
      if SubStatus = FOUND_FALSE then
        -----------------------------------------------------------
        TheFrag.Notify(NextSub,SubStatus, SUPERFRAGS, NEWSTATUS);
        ---------------------------------------------------------
        -- NewStatus is Necessarily FOUND_FALSE and SuperFrags /= null
        -- SuperFrags check is actually redundant
        while SuperFrags /= null loop
          Consolidate(SuperFrags.FragRef,TheFrag,NewStatus);
          SuperFrags := SuperFrags.FragNext;
        end loop;
        NextSub := null;
        --
        AGTable(TheFrag.Form.AGIndex).AGResult := FOUND_FALSE;
        NEWCOUNT := Decrement32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
        --
        if NEWCOUNT=0 then
          -- Dopo avre consolitato a FALSE tutti i superfrags, se NEWCOUNT=0 vuol dire
          -- che non esistono AG ancora in corso. Qualle sospese se non sono diventate false
          -- sono loop chiusi.
          AGForceIncompleteTrue(TheFrag.Form.AGIndex);
          --
          -- Either DO NOT ABORT AG, OR RESET ABORTED FRAGS, WE DO NOT ABORT
          --
--          if AGTable(TheFrag.Form.AGIndex).AGCurrent = TheFrag then
            AGRoot_Complete(TheFrag.Form.AGIndex);
--          end if;
        end if;
        --
      elsif SubStatus = FOUND_TRUE then
        -----------------------------------------------------------
        TheFrag.Notify(NextSub,SubStatus, SUPERFRAGS, NEWSTATUS);
        ---------------------------------------------------------
        -- we know that NEWSTATUS=ACTIVATED and SUPERFRAGS=null
        --  because we have another nextsub to take into account
        ------------------------------ 
        TheFrag.GetNextSub(NextSub);
        ------------------------------
      else
        -- SubStatus = ACTIVATED 
        -- ***********************************************
        -- WE SUPPOSE for now NOT TO HAVE   "AG AG PHI" Formulas ...
        -- ***********************************************
        -- otherwise we should handle the case of new root AG
        --
        if ToBePushed /= null then
          ------------------------------------------
          Push_Fragment(ToBePushed);
          ------------------------------------------
        end if;
        --
        NextSub := null;
      end if;
    end if;  -- end CASE a)

    -- we are here in case B) or case A) with first sub=TRUE
    -- in both cases NextSub is the AG/Par component
    -- Notice: in case A) with sub=False or ACTIVATED NextSub has been set to null.
    --
    if NextSub /= null and then
        NextSub.Form.Kind = Fall and then NextSub.Form.Pref.Kind = Always and then
        NextSub.Kind = ParallelAND then
      --
      -------------- WE ARE IN CASE B) ------------------
      -- WE ALWAYS PUSH THE AGPar frag, even if the state is FINAL
      --
      ToBePushed := NextSub;  --  THIS is the PHI part of AG PHI
      ---------------------------------------------
      NextSub.Link(TheFrag,SUBSTATUS,ToBePushed);
      ---------------------------------------------
      --
      if SubStatus = FOUND_TRUE or SubStatus = FOUND_FALSE then
        -----------------------------------------------------------
        TheFrag.Notify(NextSub,SubStatus, SUPERFRAGS, NEWSTATUS);
        -----------------------------------------------------------
        if SubStatus = FOUND_FALSE then
           AGTable(TheFrag.Form.AGIndex).AGresult := FOUND_FALSE;
        end if;
        -- we know that:  NewStatus = FOUND_TRUE or NewStatus = FOUND_FALSE then
        -- and that:  SuperFrags /= null then
        while SuperFrags /= null loop
          Consolidate(SuperFrags.FragRef,TheFrag,NewStatus);
          SuperFrags := SuperFrags.FragNext;
        end loop;
        --
        -- IF Frag=Root handle completion
        --
      else
        -- SubStatus = ACTIVATED 
        if ToBePushed /= null then
          -- 
          ------------------------------------------
          Increment32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
          Push_Fragment(ToBePushed);
          ------------------------------------------
          -- NOTICE: a final Notify will set this frag to TRUE/FALSE and 
          --  consolidate the result
        end if;
      end if;
      --
      NEWCOUNT := Decrement32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
      if AGTable(TheFrag.Form.AGIndex).AGresult = IN_PROGRESS and then
           NEWCOUNT=0 then 
        AGTable(TheFrag.Form.AGIndex).AGresult := FOUND_TRUE;
        AGForceIncompleteTrue(TheFrag.Form.AGIndex);
        -- Notice SubStatus can be any value TRUE/FALSE/ACTIVATED
      end if;
      --
      if AGTable(TheFrag.Form.AGIndex).AGCurrent = TheFrag and then
          NEWSTATUS=FOUND_FALSE and then
          (NEWCOUNT =0)  then
        AGRoot_Complete(TheFrag.Form.AGIndex);
        --se NEWCOUNT > 0 AGRoot_Complete  Dovra' essere fatta da qualche altro frag!!
      end if;

    end if;  -- end CASE B)
    --
    if Flags.Debug then
       Put_Line("END ElabAG/Seq: "&
           TheFrag.Form.Fimage.all & " (s=" &
           Integer'Image(Progressive(TheFrag.FromState)) & ") AGCOUNT=" 
           & Counter32'Image(AGTable(TheFrag.Form.AGIndex).AGCOUNT));
    end if;
  end Elab_AGSeq;

 procedure Elab_AGPar(TheFrag: Fragment_Ref) is
    ToBePushed: Fragment_Ref;
    NextSub: Fragment_Ref;
    NewStatus: FragStatus := ACTIVATED;
    SubStatus: FragStatus;
    SuperFrags: FragListRef;
    NEWCOUNT: Counter32;
  begin
    --
    if Flags.Debug then
       Put_Line(Standard_Error, "ElabAG/Par: "&
           TheFrag.Form.Fimage.all & " (s=" &
           Integer'Image(Progressive(TheFrag.FromState)) & ") AGCOUNT=" 
           & Counter32'Image(AGTable(TheFrag.Form.AGIndex).AGCOUNT));
    end if;
    --
    TheFrag.GetNextSub(NextSub);
    while NextSub /= null loop

      if Flags.Debug then
       Put_Line(Standard_Error,
           Integer'Image(Progressive(TheFrag.FromState)) & " ->" &
           Integer'Image(Progressive(NextSub.FromState)) );
      end if;
      ToBePushed := NextSub;
      ---------------------------------------------
      NextSub.Link(TheFrag,SUBSTATUS,ToBePushed);
      ---------------------------------------------
      --
      if SubStatus = FOUND_TRUE or SubStatus = FOUND_FALSE then
        -- Notify will return in NEWSTATUS the consequences of the immediate
        --  subevaluation. If the subelaboration was critical for establishing
        --  the final result of the evaluation of the frag then
        --  the definitive result is obtained in NEWSTATUS and the list of Super
        --  is obtained in SUPERFRAGS.
        --
        -----------------------------------------------------------
        TheFrag.Notify(NextSub,SubStatus, SUPERFRAGS, NEWSTATUS);
        -----------------------------------------------------------
        --
        -- If NEWSTATUS= FOUND_XXX and SUPERFRAGS is not null then
        --  we need to propagate the result.
        -- Notice: if the TheFrag was already TRUE/FALSE, Superfrags is null
        --
        if (NewStatus = FOUND_TRUE or NewStatus = FOUND_FALSE) and then
            SuperFrags /= null then
          while SuperFrags /= null loop
            Consolidate(SuperFrags.FragRef,TheFrag,NewStatus);
            SuperFrags := SuperFrags.FragNext;
          end loop;
          --
          exit;   --   <<------------------  !!!!!!!!!!
        else
          -- both in the case of sequential or parallel fragments, as long as we find
          -- subfrags with imediate final value (though non decisive or ?redundant?)
          -- we continue the analisys of subfrags.
          null;
        end if;
        --
    --elsif SubStatus = ACTIVATED  then if ToBePushed /= null then
    --   test non necessario perche Substatus non puo' essere JUSTCREATED dopo una link.
      elsif ToBePushed /= null then
          ------------------------------------------
          Increment32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
          Push_Fragment(ToBePushed);
          ------------------------------------------
      end if;
      --
      TheFrag.GetNextSub(NextSub);
      --
    end loop;
    --
    -- If TheFrag is an AG/Par fragment which does not have a definitve result, and which
    -- therefore has to wait for some consolidation, insert it the the IncompleteFrags list.
    -- (It will be removed if and when eventually will be Consolidated TRUE or FALSE).
    --
    if (NewStatus = ACTIVATED) then
      ---------
      Insert_Incomplete(TheFrag);
      ---------
    end if;
    --
    --  Notice:  Since we do not ABORT AG, we might have NewStatus=FOUND_TRUE and
    --      AGREsult already=FOUND_FALSE
    --
    NEWCOUNT := Decrement32(AGTable(TheFrag.Form.AGIndex).AGCOUNT'Address);
    --
    if TheFrag.XStatus=ACTIVATED and then 
        NEWCOUNT=0 then
      -- ?? not necessary!!!  NEWSTATUS = FOUND_TRUE then
      -- when FOUND_TRUE, this last evaluation is complete, but there are loops elsewere
      AGForceIncompleteTrue(TheFrag.Form.AGIndex);
    end if;
    --
    if AGTable(TheFrag.Form.AGIndex).AGCurrent = TheFrag and then
        AGTable(TheFrag.Form.AGIndex).AGResult = FOUND_FALSE and then
        -- same as TheFrag.XStatus = FOUND_FALSE
        (NEWCOUNT =0)  then
      AGRoot_Complete(TheFrag.Form.AGIndex);
      -- se ho attivato 3 subfragsAG, ed il primo restituisce FALSE non faccio la Root_Complete
      --  ma aspetto la terza Consolidate (che non arrivera?).
      --se NEWCOUNT > 0 AGRoot_Complete  Dovra' essere fatta da qualche altro frag!!
    end if;
    --
    if Flags.Debug then
       Put_Line("END ElabAG/Par: "&
           TheFrag.Form.Fimage.all & " (s=" &
           Integer'Image(Progressive(TheFrag.FromState)) & ") AGCOUNT=" 
           & Counter32'Image(AGTable(TheFrag.Form.AGIndex).AGCOUNT));
    end if;
  end Elab_AGPar;

----------------------------------------------------------------------
--  CHIAMATA DAL TASK WORKER DOPO UN POP (I.e. quando un fragment viene
--  elaborato per la prima volta) e potenzialmente CHIAMATA da CONSOLIDATE nel
--  caso di Elborazioni SEQUENZIALI, quando un sottoframmento termina ed
--  un nuovo sottoframmento deve essere attivato.
----------------------------------------------------------------------
--  Le ELAB_FRAGMENT sono eseguite SEQUENZIALMENTE PER OGNI SINGOLO FRAGMENT
--  Nel caso Parallelo le Consolidate (che possono sovrapporsi) NON rchiamano 
--    la ELAB_FRAGMENT.
--  Nel caso Sequenziale le Consolidate possono richiamare la ELAB_FRAGMENT, ma
--    cio' non interferisce con la Elab_Fragment iniziale eventualemente ancora
--    in corso (la quale ha appena fatto una LINK e sta terminando senza fare alcuna Notify)
--  Nel caso sequenziale inoltre non possono avvenire due Consolidate in parallelo
--    perche' solo un subfragment alla volta viene attivato.
--
--  Le ELAB_FRAGMENT POSSONO AVVENIRE IN PARALLELO CON ALTRE REGISTRAZIONI DI 
--    SUPERFRAG (i.e. Link), o con richieste di Promotions o Demotions.
--
--  Il cambiamento di Stato NON puo' avvenire in modo concorrente
--    FINTANTO che questa elaborazione non attiva nuove subcomputations
--    (tramite PUSH) o si registra come Super in computazioni gia' create.
--    (Non e' chiaro cosa succede in caso do demotion).
--  Nel caso di AG un FRAGMENT può passare da ACTIVATED a JUSTCREATED in caso di RESET.
--  (RESET = full Demotion)?
----------------------------------------------------------------------
-- NOTA SU EVAL: NON  e' piu' vero che le consolidations possono arrivare solo
-- dopo la prima Link, possono arrivare anche da vite precedenti. ??
-----------------------------------------------------------------------
  procedure Elab_Fragment(TheFrag: Fragment_Ref) is
    ToBePushed: Fragment_Ref;
    NextSub: Fragment_Ref;
    NewStatus: FragStatus := ACTIVATED;
    SubStatus: FragStatus;
    SuperFrags: FragListRef;
--    NEWCOUNT: Counter32;
  begin
    if TheFrag.Form.Kind=Fall and then TheFrag.Form.Pref.Kind=Always then
      if TheFrag.Kind = SequentialAND then
        Elab_AGSeq(TheFrag);
      else
        Elab_AGPar(TheFrag);
      end if;
      return;
    end if;
    --
    if Flags.Debug then
       Put_Line(Standard_Error, "Elab: "&
           TheFrag.Form.Fimage.all & " (" &
           Integer'Image(Progressive(TheFrag.FromState)) & ")");
    end if;
    --
    TheFrag.GetNextSub(NextSub);
    --
    -- Normally NextSub CANNOT be null for Frags not yet elaborated, because
    --  they are pushed only if they do have subcomputations to be done, but
    --  until we link the frag to the subs the status of the subs remains JUSTCREATED.
    -- HOWEVER if we have a DEMOTION of the frag (Supers <=0) before its evaluation 
    --  we no longer have (at the current time) frags to activate.
    -- Notice: If NextSub /= null its imagestatus in the frag has already been
    --  marked as ACTIVATED.
    --
    while NextSub /= null loop
      -- Link either returns (in SUBSTATUS) an immediate defintive value, or
      --  returns an already existing definitive or ACTIVATED value, or
      --  returns a ACTIVATED value for a newly ACTIVATED and pushed subfrag.
      -- If ACTIVATED is returned, nothis needs to be done in the frag
      --
      ----**********************************************
      --  SE NEXTSUB E' AG/Seq (quindi una root AG)
      --  FACCIO LA LINK STANDARD  SOLO SE NESSUNA AG SIMILE E' IN CORSO.
      --  ALTRIMENTI FACCIO UNA ENQUEUE
      ----**********************************************
      --
      -- Se il Subfrag e' una formula AG/Seq e il frag non e' un AG/Par
      --   (i.e. la formulamdin frag e nexsub e' la stessa), questa e' una root.
      -- Controllo se una computazione AG con la medesima Form.Fimage e' gia' in corso
      -- e se io sono una root .. a seconda dei casi o continua normalmente
      -- o incoda la rootAG nella sua AGQueue SENZA fare la push!!;
      --
      if NextSub.Form.Kind = Fall and then 
           NextSub.Form.Pref.Kind = Always and then
           NextSub.Kind = SequentialAND and then
           TheFrag.Form.Fimage.all /= NextSub.Form.Fimage.all then
        --
        --  NextSub NON E' una chiamata ricorsiva (TheFrag.Form /= NextSub.Form)
        --  *** NextSub *** is a  ROOT  AG/Seq
        AGRoot_Start(NextSub,TheFrag);
      end if;
        --
      if NextSub /= null then
        --
        --   NextSub is a Normal Fragment
        --
        ToBePushed := NextSub;
        ---------------------------------------------
        NextSub.Link(TheFrag,SUBSTATUS,ToBePushed);
        ---------------------------------------------
        --
        --if ToBePushed /= null and then ToBePushed.Form.Kind=Fall and then
        --     ToBePushed.Form.Pref.Kind=Always then
        --  --  ASSERT Kind=   AG/Seq  
        --  ------
        --  Increment32(AGTable(ToBePushed.Form.AGIndex).AGCOUNT'Address);
        --  ------
        --end if;
        --
        if SubStatus = FOUND_TRUE or SubStatus = FOUND_FALSE then
          -- Notify will return in NEWSTATUS the consequences of the immediate 
          --  subevaluation. If the subelaboration was critical for establishing
          --  the final result of the evaluation of the frag then
          --  the definitive result is obtained in NEWSTATUS and the list of Super
          --  is obtained in SUPERFRAGS.
          --
          -----------------------------------------------------------
          TheFrag.Notify(NextSub,SubStatus, SUPERFRAGS, NEWSTATUS);
          -----------------------------------------------------------
          --
          -- If NEWSTATUS= FOUND_XXX and SUPERFRAGS is not null then 
          --  we need to propagate the result.
          -- Notice: if the TheFrag was already TRUE/FALSE, Superfrags is null
          --
          if (NewStatus = FOUND_TRUE or NewStatus = FOUND_FALSE) and then
              SuperFrags /= null then
            while SuperFrags /= null loop
              Consolidate(SuperFrags.FragRef,TheFrag,NewStatus);
              SuperFrags := SuperFrags.FragNext;
            end loop;
            exit;   --   <<------------------  !!!!!!!!!!
          else
            -- both in the case of sequential or parallel fragments, as long as we find
            -- subfrags with imediate final value (though non decisive or ?redundant?) 
            -- we continue the analisys of subfrags.
            null;
          end if;
          --
        elsif ToBePushed /= null then 
            -- We do not Notify just ACTIVATED subfrags, since they are already considered such
            -- In case of fragment of a sequential kind we must stop here!
            --
            ------------------------------------------
            Push_Fragment(ToBePushed); 
            ------------------------------------------
          if TheFrag.Kind= SequentialOR or else TheFrag.Kind=SequentialAND then
             exit;
          end if;
        end if;  -- substatus of NextSub
      end if;  -- root or nor root?
      --
      TheFrag.GetNextSub(NextSub);
      --
    end loop;
    --
    if Flags.Debug then
       Put_Line("Elab END: "&
           TheFrag.Form.Fimage.all & " (" &
           Integer'Image(Progressive(TheFrag.FromState)) & ")");
    end if;
  end Elab_Fragment;

----------------------------------------------------------------------
-- 1) form=agSeq and nextsub/= AG/par and subst=false agcount--
-- 2)                nextsub=AG/par and substatus=true or false  agcount --
-- 3)                nextsub=AG/par anf subst =act+inprogr   agcount--

-- 4   form=agpar      alla fine AG--
--se frag =agpar and subst =activated .. agcount===, infatti dopo la consolidare
--vieni richiamata per il nuovo ciclo co nextsu=AAG/par

  -------------------------------------------------
  --  Called by WORKER TASK
  -------------------------------------------------
  procedure ForceTRUE (TheFrag: Fragment_Ref) is
    SuperFrags: FragListRef;
    NewStatus: FragStatus;
  begin
    if Flags.Debug then
       Put_Line("  ForceTRUE: "&
           TheFrag.Form.Fimage.all & " (" &
           Integer'Image(Progressive(TheFrag.FromState)) & ")");
    end if;
    TheFrag.Notify(null,FOUND_TRUE,SuperFrags,NewStatus);
    while SuperFrags /= null loop
        Consolidate(SuperFrags.FragRef,TheFrag,NewStatus);
        SuperFrags := SuperFrags.FragNext;
    end loop;
    -- solo le AG/Par incomplete vengonoforzate a TRUE, il resto poi viene
    -- gestito normalmente dalle Consolidate.
    Put_Line("  END ForceTRUE");
  end ForceTRUE;
  
  
 ------------------------------------------------------------
 -- AGForceIncompleteTrue viene chiamata quando AGCOUNT diventa = 0 senza che la formula
 --   AG sia stata invalidata.  In tale caso viene presa per Valida e tutti i frag 
 --   in AGIncomplete (corrispondenti nodi di loop) vengono direttamente forzati a 
 --   diventare FOUND_TRUE, ed indirettamnte tramite Consolidate il risultato viene propagato
 --   ai tutti i SuperFrags.
 -- Alla fine di tutto cio' il o il recerd AGDATA viene resettato,
 --   o viene aggiustato per una nuova AG prelevata dalla AGQUEUE (se c'e')
 ------------------------------------------------------------
 procedure AGForceIncompleteTrue(AGIndex: Natural) is
 begin
   --  From now on Consolidate will not handle AGData management
   while AGTable(AGIndex).AGIncomplete /= null loop
      ForceTRUE(AGTable(AGIndex).AGIncomplete.FragRef);
      --if Flags.Debug then
      --Put_line("ForcedTRUE (loop element):" &
      --Integer'Image(Progressive(AGTable(AGIndex).AGIncomplete.FragRef.FromState)));
      --end if;
      Remove_Incomplete(AGTable(AGIndex).AGIncomplete.FragRef);
   end loop;
   ---
 end AGForceIncompleteTrue;
 


----------------------------------------------------------------------
  task body Worker is
    TheFrag: Fragment_Ref;
  begin
    if Debug then
      Put_line ("worker started");
    end if;
    while not AllEvalDone loop
      TheFrag := Pop_Fragment;
      -- Il task si sospende nella Pop_Fragment fino a quando:
      --    AGResult= In_Progress and FragsAvailable /= null : (TheFrag è null, AGCOUNT >0)
      --    FragsAvailable =null , AGResult = In_Progress and AGCOUNT =0: (TheFrag=null)
      --    AGResult = Found_False and AGCOUNT =0 : (TheFrag=null)
      --
      if  TheFrag /= null then
        --  if Debug then Put_Line("#" & Natural'Image(Id)); end if;
        Elab_Fragment(TheFrag);
--      else
--        -- computation completed ??????
--        delay 0.1;
      end if;
    end loop;  -- task cycle on all available fragments
    if Debug then
      Put_line ("worker ended");
    end if;
  exception
  when Event: others =>
    Put_line (Current_Error,
       "Error in Worker task");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end Worker;
----------------------------------------------------------------------

  ---------------------------------

  function Eval (Formula: UCTL_Types.Formula_Ref;
                 State: System_Configuration) return Computation_Status is
    Res: Computation_Status;
    Tmp_Depth: Natural:=1;
  begin
    --
    Requested_Counts := 0;
    All_Steps := 0;
      --
      -- SPAWN THREAD POOL
      --
      Put_line(" Starting evaluation of " &Formula.fimage.all &
               " with " & Integer'Image(Flags.Cores) & " cores ...");
      -----------------------------------
      AllEvalDOne := False;
      if WR = null then
        WR := new Worker(1);
        if Flags.Cores >1  then
          for K in 2..Flags.Cores loop
            WR := new Worker(K);
          end loop;
        end if;
      end if;
      -- Prepara il suspension object su cui attendere il risultato
      --
      ALLWait := new Suspension_Object;
      Set_False(ALLWait.all);
       --
      -- start the evaluation
      Max_LTS_Depth := 1_000_000_000;
      -- 
      declare
        This_Fragment: Fragment_Ref;
        Root_Fragment: Fragment_Ref;
        Unused: Boolean; 
        Next_Status: FragStatus;
        ToBePushed: Fragment_Ref;
      begin
        Root_Fragment  := new Fragment(State,null,Primitive);
        --Root_Fragment.Initialize(FromState,null);
        This_Fragment := CheckElement((State,Formula,DefaultKind(Formula)),Unused);
        ToBePushed := This_Fragment;
        This_Fragment.Link(Root_Fragment,NEXT_STATUS,ToBePushed);
        if Next_Status = FOUND_TRUE then
          -- COMPUTAZIONE GIA FATTA DA PRECEDENTI EVAL:
          Res := FOUND_TRUE;
        elsif Next_Status = FOUND_FALSE then
          -- COMPUTAZIONE GIA FATTA DA PRECEDENTI EVAL:
         Res := FOUND_FALSE;
        else  
          -- Next_Status = ACTIVATED
          -- NUOVA COMPUTAZIONE O COMPUTAZIONE IN  CORSO
          -- SE LA COMPUTAZIONE E' IN CORSO DA PRECEDENTE EVAL INIZIATA MA NON FINITA
          -- 
          -- se le Top_level formula e' una AG occurre inizializzare la corretta componente 
          --   di AGDATA o se ne puo' fare a meno? 
          if ToBePushed /= null then 
            if This_Fragment.Form.Kind = Fall and then 
              This_Fragment.Form.Pref.Kind = Always and then
              This_Fragment.Kind = SequentialAND  then
              --
              AGRoot_Start(ToBePushed,This_Fragment);
              --
            end if;
            -- ASSERT ToBePushed /= null
            ------------------------------------
            Push_Fragment(This_Fragment); 
            ------------------------------------
          end if;
        end if;
      end;
      --
      if Res /= FOUND_TRUE and Res /= FOUND_FALSE then
        --  i.e.  IN_PROGRESS (PARALLEL)
        -- FORM DEVE ESSERE UNA FORMULA PARALLELIZZATA GIA AL TOP LEVEL!!
        Suspend_Until_True(ALLWait.all); -- when resumed in AGResult we have the result
        if ALLResult = FOUND_TRUE then
          Res := FOUND_TRUE;
        elsif ALLResult = FOUND_FALSE then
          Res := FOUND_FALSE;
        else
          Put_line("Eval Error: TRUE or FALSE EXPECTED from FRAGMENTS");
          Res := FOUND_TRUE;
        end if;
      end if;
      --
      -- free ALLWait
      AllEvalDone := True;
      WR := null;
      --
      UCTL.FragmentsCount := Integer(PushedFrags);
      PushedFrags :=0;
--      Put_line("TOTAL ACTIVATED FRAGS= " & Counter32'Image(PushedFrags));
      if Res = TMP_TRUE then
          return FOUND_TRUE;
      elsif res = TMP_FALSE then
          return  FOUND_FALSE;
      else
          return Res;
      end if;
  exception
    when Event: others =>
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
      Put_line (Current_Error, "Eval: unexpected error at step " &
              Int64'Image(All_Steps));
        raise;
  end Eval;
  
begin
  null;
end NWO;

