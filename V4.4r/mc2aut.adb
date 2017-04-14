with Bool_Dyn_Store;
with Dyn_Store;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;
with Configurations; 
with Flags; use Flags;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Assertions; use Ada.Assertions;
-- --------------------------------------------------------------------------
-- dato un modello, genera:
-- * un automa "tracemodel.aut" in formato .aut, dove eventuali state labels
--   sono codificate nelle edge labels, senza "tau" transition, ma con "#loop" e #final"
-- * un automa "flatmodel.aut" in formato .aut, dove eventuali state labels
--   sono codificate nelle edge labels, dove "[labelss] {} [labels]"  diventa "tau"
-- --------------------------------------------------------------------------
procedure MC2aut is
------------------------------------------------
---   struttura  filae model.aut
--  des (initialnode,edgescount,nodescount)      -- follow edgescount lines
--  (srcnode,edgelabel,tgtnode)
--  (0,"[{src(1),src(2)}][{}]{edge(1)}",1)
--  (1,"{edge(2)}",2)         -- src state labels empty, tgt state labels empty or to be defined
--  (2,"[{src(3)}]#final",3)  -- edge labels not omitted if empty 
--  (1,tau,1)                 -- tau propagates src state labels to tgt state labels with empty edge labels
----------------------------------------------
-- --------------------------------------------------------------------------
--   filename   namo of file containing the source model
--
--   -debug  Debug Mode, prints stats into
--
--   -c   Just Count 
--
--   -m<N> uses N cores for state-space generation
--
--   -d<N>  truncates the graph at depth <Depth> introducendo transizioni #truncated
--
--   -t   elimina le transizioni tau introducendo se necessario transizioni #loop
-- --------------------------------------------------------------------------
--
--   -f   product families display  (handling of may-must transitions) (??)
-- --------------------------------------------------------------------------

  ----------------------------------------------------------------------------
  Remove_Tau_Transitions: Boolean := False;   -- option "-t"      
  --
  Transitions_Count: Natural :=0;  -- to bbuild unique node names
  ----------------------------------------------------------------------------
  

-- ---------------------------------------------------------------------------
  States_Limit: Integer := 16_000_000;         -- option "-s"
  Depth_Limit: Integer := 15_000_000;            -- option "-d"
  SetOutput: Boolean := False;                -- option "-o"
  OutputFileName: access String;
  Use_Ground_Labels: Boolean := True;         -- option "-g"
  Use_Abstract_Labels: Boolean := False;      -- option "-a"
  Model_FileName: access String;
-- ---------------------------------------------------------------------------
  Model_OK : Boolean := False;
  Argc: Natural;
  Just_Count: Boolean :=False;
  Aborted: Boolean := False;

  LastWidth: Natural := 0;
  WDepth: Natural :=0;
  WCount: Natural :=0; -- used to set the statenums of the weak automaton
  Wedges: Natural :=0; -- used to set the edgesnums of the weak automaton
  ----------------------------------------------------------------------------
  package MyConfigurations is new Configurations;
  use MyConfigurations;
  use MyConfigurations.Kernel;
  ----------------------------------------------------------------------------
  type System_Configuration_Table is array (Positive range <>) of System_Configuration;
  type System_Configuration_Table_Ref is access System_Configuration_Table;
----  --Empty_System_Configuration_Table: System_Configuration_Table_Ref := new System_Configuration_Table(1..0);
  ----------------------------------------------------------------------------
  Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref;
  ---------------------------------------------------------------------------

  ---------------------------------------------------------------------------
   type System_Configuration_Info is record
     AbstractStateLabels: String_Ref;
     BackwardTAUS: System_Configuration_Table_Ref := new System_Configuration_Table(1..0);
     ForwardCONFS:  System_Configuration_Table_Ref := new System_Configuration_Table(1..0);
     ForwardLABELS: String_Table_Ref := new String_Table(1..0);
     HasLoop: Boolean := False;
     HasFinal: Boolean := False;
     ElaborationStarted: Boolean := False;
     ElaborationEnded: Boolean := False;
     BookedNode: Integer := -1;
     PrintedNode: Boolean := False;
   end record;
   
   function Null_Info return System_Configuration_Info is
      Tmp: System_Configuration_Info;
   begin
      return Tmp;
   end Null_Info;
   
   MaxBreadth: Natural := 1_000_000;  -- size of the two System_Configuration_Tables used)
   Slot_Size: Natural := Flags.Vectors_Max;   -- in flags.ads  == 4096
   type Seen_Table is array (Positive Range 1..Slot_Size) of Boolean;
   type Seen_Table_Ref is access Seen_Table;
   SeenStates: Int64 :=0;
   Previous_Count: Natural :=0;   -- used to avoid message repetition
   -- Transitions_Count: Int64 :=0;
   NeededStates: Natural := 0;
   NeededEdges: natural :=0;
   truncated: boolean := False;
   
   CurTable: Natural :=1;
   T1: access System_Configuration_Table := new  System_Configuration_Table(1..MaxBreadth);
   I1: Natural :=0;
   T2: access System_Configuration_Table := new System_Configuration_Table(1..MaxBreadth);
   I2: Natural :=0;
   ------------------------------------------------------------------------------
   ALLDONE: Boolean := False with Atomic;
   FLATOUT: File_Type;
   WEAKOUT: File_Type;
   --------------------------------------------------------------------
   WSTEPS: Natural :=0;   -- used both to print the minimization progress
   PSTEPS: Natural :=0;
   --------------------------------------------------------------------
   
   
   procedure Free is new  Ada.Unchecked_Deallocation (System_Configuration_Table,System_Configuration_Table_Ref);
  ---------------------------------------------------------------------------
   package SCI_DB is new Dyn_Store (System_Configuration_Info, Null_Info);
  ---------------------------------------------------------------------------
  
  
   -- uno stato viene considerato "Booked" quando viene aggiusto alla lista dei successivi
   -- nodi da explorare tramite Add_New_Configuration.
   -- Per poter diventare "Seen", bisogna prima diventare "Booked"!
   --
   procedure MarkBooked(N: Positive) is
      SCI: System_Configuration_Info;
   begin
      WCount := WCount+1;          --  counts range 1..statescount
      SCI := SCI_DB.Retrieve(N);
      SCI.BookedNode := WCount-1;  -- nums range 0..statescount-1
      SCI_DB.Store(SCI,N);
   end MarkBooked;

   function Already_Booked(N: Positive) return Boolean is
   begin
      return SCI_DB.Retrieve(N).BookedNode >=0;
   end Already_Booked;

   procedure Add_New_Configuration (Conf: System_Configuration);

   function Mk_Edge_Label(It: Evolutions_Iterator) return String is
      ActionLabels: String := Abstract_Action_Labels(It);
   begin
      return ActionLabels;
   end Mk_Edge_Label;

 
   procedure PRINT_EDGES(Current: System_Configuration;
                           This_SCI: System_Configuration_Info);


   procedure MERGE_INFO(This_Conf: System_Configuration;
                        Next_Conf: System_Configuration;
                        Changed: in out Boolean);
			      
   procedure ADD_BACKWARDTAU(Current: System_Configuration;
                              Next_SCI: in out System_Configuration_Info);

   procedure ADD_EVOLUTION_SORTED (This_Conf: System_Configuration;
                          This_Target: System_Configuration;
                          This_Edge_Label: String_Ref;
                          changed: in out Boolean);
			  
   procedure BACKPROPAGATE_INFO(This_Conf: System_Configuration);


  procedure Print_Header is
  begin
    Put_Line(WEAKOUT, "des (0,                 ");
  end;
  
  --------------------------------------------------------------
  --  Perche servono la targetlabels e non sono sufficenti le
  --  sourcelabels? 
  ----------------------------------------------------------------
  --  (srcnode,edgelabel,tgtnode)
  --  (0,"[{src(1),src(2)}][{}]{edge(1)}",1)
  ----------------------------------------------------------------
  procedure Print_Truncated (num: Natural; sourcelabels: String);
  procedure Print_Loop (num: Natural; sourcelabels: String);
  procedure Print_Final (num: Natural;  sourcelabels: String);

  ----------------------------------------------------------
  --  transform i charatteri '"'  e '"'  in "\""  e "\"" in modo che non
  --  interferiscano con i costrutti dot.
  ----------------------------------------------------------
  function Adjust (Str: String) return String is
    Old: String := Str;
    Count: Natural :=0;
  begin
    for I in Str'Range loop
     if Str(I)='"' and then (I = Str'First or else Str(I-1) /= '\') then
       Count := Count+1; 
     end if;
    end loop;
    if Count =0 then return Str; end if;
    declare
      Changed: String(1..Str'Length+Count);
      Index: Natural :=1;
    begin
    for I in Str'Range loop
     if Str(I)='"' and then (I = Str'First or else Str(I-1) /= '\') then
        Changed(Index..Index+1) := "\""";
        Index := Index+2;
     else
         Changed(Index) := Str(I);
         Index := Index+1;
     end if;
    end loop;
    return Changed;
    end;
  end Adjust;

     ----------------------------------------------------------------------------
   --   Nota sulle trilabels:
   --    (1[p1]) -a-> (2[p2])
   --    (1[p1]) -a-> (3[p3]) Dobbiamo distinguerle!!! quindi bisogna codificare gli edges
   --       come  (1) -"[p1]a[p2]"-> (2)    e (1) -"[p1]a[p3]"->  (3)
   --  se non si fa cosi' dopo le ltsconvert minimization ci riptroviamo con un lts:
   --    (t1) -"[p1]a"-> (t2)  ,  (tx) -"[p2].."-> (...) ,  (tx) -"[p3].."-> (...)
   -- che non siamo più in gradi di visualizzare correttamente come L2TS per la doppia 
   -- state lable presente su (tx)
   --
   --   Nota sulle state labels:
   --   ????????????????  da capire meglio ???????????????
   --  Se ci sono state labels e ci sono azioni TAU (non c'e' l'opzione -t)
   --  e' necessario aggiungere ad ogni nodo (che ha transizioni tau) una transizione
   --   esplicita  ((sourcelabels))#statelabels in modo da essere in grado di ricostruire
   --  la struttura corretta (beautified) dopo una eventuale minimizzazione strong.
   --  Per semplicita la transizione #statelabels viene aggiunta atutti i nodi da Print_Dot_Node
   ----------------------------------------------------------------------------
   --  (0,"[{src(1),src(2)}][{}]{edge(1)}",1)
   ----------------------------------------------------------------------------
   procedure Print_Edge (sourcenum: Natural; 
                         sourcelabels: String;
                         targetlabels: String; 
                         actionlabels: String; 
                         targetnum: Natural) is
      SRC: String := Trim(Integer'Image(sourcenum),Left);
      TGT: String := Trim(Integer'Image(targetnum),Left);
   begin
     --   source   -[..]{..}[..]-> target
     --   source   -[..]{..}-> target
     --   source   -{..}[..]-> target
     --   source   -{..}-> target
     --   source   -"tau"-> target
     --
         if sourcelabels = targetlabels and then actionlabels ="{}" then
           -- draw the TAU edge and the  #statelabels
           Put_line(WEAKOUT, "(" & SRC & 
               ",""tau""," & TGT & ")" );
           --
         elsif (sourcelabels /= "" and sourcelabels /= "{}") and 
             (targetlabels /= "" and targetlabels /= "{}") then
           -- draw trilabelled transition label
           Put_line(WEAKOUT, "(" & SRC & ",""" &
               "[" & sourcelabels & "]" & 
               Adjust(actionlabels)  & "[" & targetlabels & "]" &
               """," & TGT & ")" );
           --
         elsif (sourcelabels /= "" and sourcelabels /= "{}") and 
             (targetlabels = "" or targetlabels = "{}") then
           -- draw bilabelled transition label
           Put_line(WEAKOUT, "(" & SRC & ",""" &
               "[" & sourcelabels & "]" & 
               Adjust(actionlabels)  & 
               """," & TGT & ")" );
           --
         elsif (sourcelabels = "" or sourcelabels = "{}") and 
             (targetlabels /= "" and targetlabels /= "{}") then
           -- draw bilabelled transition label
           Put_line(WEAKOUT, "(" & SRC & ",""" &
               Adjust(actionlabels)  & "[" & targetlabels & "]" &
               """," & TGT & ")" );
           --
         else  -- sourcelabels=""  targetlabels=""  
           -- draw monolabelled transition label
           Put_line(WEAKOUT, "(" & SRC & ",""" &
               Adjust(actionlabels)  & 
               """," & TGT & ")" );
         end if;
   end Print_Edge;
   
   -----------------------------
   --  PROBLEM .. DO WE NEED AN ADDITIONAL NODE??? ... 
   --   CAN WE MAKE A LOOP??    CAN USE A COMMON  FINAL SHARED NODE??
   procedure Print_Final (num: Natural; sourcelabels: String) is
      NODE: String := Trim(Integer'Image(num),Left);
   begin
     if Remove_Tau_Transitions then
        --  case of old dot generation when removing taus before ltsmin minimizations
       if sourcelabels /= "" and sourcelabels /= "{}" then
          Put_line(WEAKOUT, "(" & NODE & ",""" &
               "[" & sourcelabels & "]" & 
               "#final"  & 
               """," & NODE & ")" );
       else
           Put_line(WEAKOUT, "(" & NODE & ",""" &
               "#final"  & 
               """," & NODE & ")" );
       end if;
     end if;
   end Print_Final;


   procedure Print_Loop (num: Natural; sourcelabels: String) is
      NODE: String := Trim(Integer'Image(num),Left);
   begin
     if Remove_Tau_Transitions then
        --  case of old dot generation when removing taus before ltsmin minimizations
       if sourcelabels /= "" then
          Put_line(WEAKOUT, "(" & NODE & ",""" &
               "[" & sourcelabels & "]" & 
               "#loop"  & 
               """," & NODE & ")" );
       else
           Put_line(WEAKOUT, "(" & NODE & ",""" &
               "#loop"  & 
               """," & NODE & ")" );
       end if;
     end if;
   end Print_Loop;
   
   
   procedure Print_Truncated (num: Natural; sourcelabels: String) is
      NODE: String := Trim(Integer'Image(num),Left);
   begin
     if Remove_Tau_Transitions then
        --  case of old dot generation when removing taus before ltsmin minimizations
       if sourcelabels /= "" and then sourcelabels /= "{}" then
          Put_line(WEAKOUT, "(" & NODE & ",""" &
               "[" & sourcelabels & "]" & 
               "#truncated"  & 
               """," & NODE & ")" );
       else
           Put_line(WEAKOUT, "(" & NODE & ",""" &
               "#truncated"  & 
               """," & NODE & ")" );
       end if;
     end if;
   end Print_Truncated;
   
  ----------------------------------------------------------------------------
   -- Utilizzata quando vogliamo rimuovere tutte le transizioni tau dal grafo.
   -- Chiamata con uno stato iniziale di attivazione (Initial), per ogni weak next transition
   --   non tau generiamo un edge che lo descrive (Print_Edge), e se il target di 
   --   questi edges non sono ancora stati descritti, li inserisco nel vettore
   --   degli stati da analizzare (Add_New_Configuration).
   --
   -- Valutazione top-down pura, con grande stack e task ad hoc.
   -- Ogni nodo raggiunto con transizioni tau viene analizzato, ed i suoi 
   --   "non tau next weak successors" sono salvati nello SCI_DB associato al nodo.
   -- Questo e' necessario per evitare di rianalizzare i medesimi nodi intermedi più volte.
   --  Nell SCI_DB dei nodi in corso di eleborazione vengono salvati il vettore delle 
   --   next weak non teu transitions, se dal nodo sono possibili tau loops, e se dal nodo
   --   si può raggiungere via tau uno stato finale.
   -- Inoltre e' necessario salvare nello SCI_DB i nodi "super" da cui si raggiunge il nodo con una 
   --    transizione tau. Questo perchè, in presenza di tau loops, quando viene scoprte una nuova
   --  non tau weak transition, questa transizione va aggiunta a tutti i super nodes (in modo ricorsivo)
   --  (in particolare a quelli la cui elaborazione e' gia terminata). Idem per le info su
   --    tau loops e tau final.
   --  E.g.  s1 -> s2,  s2 -> s1,  s1 -> s3final, s1 -a-> s4
   --     quando analizzo s3 e s4 s2 l'analisi di s2 e' gia' completata, ma devo propagargli le info.
   --
   -- Inparticolare, quando sono sul nodo N,  guardo tutte le transizioni possibili.
   -- Se ho una transizione {tau} ed incontro un nodo gia visto e completamente analizzato, 
   --  non continuo la ricorsione su di esso, ma recupero le sue info dallo SCI_DB.
   -- Se ho una transizione {tau} ed incontro un nodo gia visto ma analizzato parzialmente, 
   --  non continuo la ricorsione su di esso, ma mi registro nella lista dei suoi super.
   -- Se ho transizione {tau} ed il nodo target non e' mai stato visto prima,
   --    continuo ricorsivamente dal nodo targat e poi aggiungerò
   --    alle evoluzioni del nodo tutte le evoluzioni erditate ricorsivamente da esso.
   -- Dopo aver analizzato tutte le transizioni uscenti,
   --   se esistono, salvo le evoluzioni sul descrittore del nodo,
   --    e le propago ricorsivamente (se aggiungono qualcosa di nuovo)a tutti i supernodi.
   -- Se ho una transizione {a},  aggiungo {a} alle evoluzioni del nodo,
   --
   -- Quando tutta l'esplorazione di Last_Weak_Explore e' finita, se i target delle non tau transitions 
   --  non sono gia' stati elaborati e non sono già stato aggiunti (booked) li aggiungo
   --  ai prossimi stati da analizzare.
   --
   -- struttura dagli SCI__DB Elements:
   --
   --  AbstractStateLabels: String_Ref;
   --  BackwardTAUS: System_Configuration_Table_Ref := new System_Configuration_Table(1..0);
   --  ForwardCONFS:  System_Configuration_Table_Ref := new System_Configuration_Table(1..0);
   --  ForwardLABELS: String_Table_Ref := new String_Table(1..0);
   --  HasLoop: Boolean := False;
   --  HasFinal: Boolean := False;
   --  ElaborationStarted: Boolean := False;  -- ongoing Last_Weak_Explore on node
   --  ElaborationEnded: Boolean := False;    -- Last_Weak_Explore completed on node
   --  BookedNode: Integer  := -1;  -- Last_Weak_Explore completed,
   --                                -- node reached with non-tau, added in vector
   --  BISOGNA FARE MOLTA ATTENZIONE ALLA GESTIONE DELLE SCI_DB.  In fatti i vettori
   -- BackwardTAUS  ForwardCONFS  ForwardLABELS vengono estesi continuamente, facendo
   --  la free delle versioni precedenti. Se si accede ad una versione gia' obsoleta
   -- si ha una esecuzione erronea!
   --------------------------------------------------------------------
   procedure Last_Weak_Explore (Initial,Current: System_Configuration) is
     --
     This_Iter: Evolutions_Iterator;
     This_Target: System_Configuration;
     To, From: Natural;
     This_SCI: System_Configuration_Info;
     Next_SCI: System_Configuration_Info;
     This_Edge_Label: String_Ref;
     changed: Boolean := False;
   begin
     From := Progressive(Current);
     This_SCI := SCI_DB.Retrieve(From);
     if This_SCI.ElaborationStarted then -- and also ElaborationEnded (implicitly)
       if Initial=Current then 
         -- the booked node has been aready fully elaborated we just have to print its edges
         NeededStates := NeededStates +1;
         PRINT_EDGES(Current,This_SCI);
         This_SCI.PrintedNode := True;
         SCI_DB.Store(This_SCI,From);
       end if;
       return;
     end if;
     
     if truncated then 
       Print_Truncated(This_SCI.Bookednode,"");
       return;
     end if;
     -- Last_Weak_Explore VIENE CHIAMATA, RICORSIVAMAMENTE, SU NODI NON ANCORA ANALIZZATI.
     -- Viene anche chiamata a partire dalla lista delle configurazioni da analizzare.
     -- Puo' succedere che la elaborazione di una configurazione elabori anche altre configurazioni
     --  (raggiunte tramite tau) presenti nella lista delle configurazioni da analizzare.
     -- In tal caso quando le analizzerò stamperò le transizioni uscenti gia' trovate.
     --
     This_SCI.ElaborationStarted := True;
     This_SCI.AbstractStateLabels :=
          new String'("{" & Display_AbstractStateLabels(Get_Abstract_State_Labels(Current)) & "}");
     SCI_DB.Store(This_SCI,From);
     --
     -- verbose when necessary (i.e. when we are writing to a file, or just counting)
     if Just_Count or (OutputFileName=null or else  OutputFileName'Length >0) then
       -- WSTEPS conta il numero di volte che Last_Weak_Explore viene chiamata
       --  (contatore di attivita' che coincide con il numero di nodi del L2TS sorgente)
       WSTEPS := WSTEPS+1;
       if (PSTEPS +1000 < WSTEPS and then WSTEPS < 20_001 ) or else
         (PSTEPS +2000 < WSTEPS and then WSTEPS < 40_001 ) or else
           (PSTEPS +5000 < WSTEPS and then WSTEPS < 100_001 ) or else
           (PSTEPS +10_000 < WSTEPS and then WSTEPS < 200_001 ) or else
           (PSTEPS +20_000 < WSTEPS and then WSTEPS > 200_001 ) then
         Set_Output(Standard_Output);
         MyConfigurations.Kernel.Print_StatesSpace_Stats;
         if Is_Open(WEAKOUT) then Set_Output(WEAKOUT); end if;
         PSTEPS := WSTEPS;
       end if;
     end if;
     -- if just starting the analysys of a new state record it.
     --if Initial=Current then
     --   MarkPrinted(From);
     --   NeededStates := NeededStates +1;
     --end if;
     --
     -- INIZIAMO AD ANALIZZARE LE EVOLUZIONI
     --
     Iterator_Initialize (This_Iter, Current);
     --
     -- This new node has no transitions. Just remember there is a final state
     --  SHOULD THIS NODE BY PRINTED IN SOME WAY (if Initial = Current)?
     if not Has_System_Transition (This_Iter) then
        This_SCI.HasFinal := True;
       SCI_DB.Store(This_SCI,From);   --- TDAdded
     end if;
     --
     while Has_System_Transition (This_Iter) loop
       This_Edge_Label := new String'(Mk_Edge_Label(This_Iter));
       This_Target := Get_Target_Configuration (This_Iter);
       To := Progressive(This_Target);
       Next_SCI := SCI_DB.Retrieve(To);
       --
       if Next_SCI.AbstractStateLabels = null then
           Next_SCI.AbstractStateLabels :=
             new String'("{" & Display_AbstractStateLabels(
               Get_Abstract_State_Labels(Get_Target_Configuration(This_Iter))) & "}");
          SCI_DB.Store(Next_SCI,To);
       end if;
       --
       if Remove_Tau_Transitions and then
           This_SCI.AbstractStateLabels.all = Next_SCI.AbstractStateLabels.all and then
            This_Edge_Label.all = "{}" then
         ------------------------------------------------
         -- this is a TAU transition (xx)(xx){}
         ------------------------------------------------
         if Next_SCI.ElaborationEnded then
            -- if To is a fully elaborated node then recycle its info inside the current info
            MERGE_INFO(Current,This_Target,CHANGED);
            --
            Next_SCI := SCI_DB.Retrieve(To);
            ADD_BACKWARDTAU(Current,Next_SCI); -- beware of self loops
            SCI_DB.Store(Next_SCI,To);
         elsif Next_SCI.ElaborationStarted then
            -- this is  loop backmay to the node itself
            This_SCI := SCI_DB.Retrieve(From);
            This_SCI.HasLoop := True;
            SCI_DB.Store(This_SCI,From);
            --
            MERGE_INFO(Current,This_Target,CHANGED);
            --
            Next_SCI := SCI_DB.Retrieve(To);
            ADD_BACKWARDTAU(Current,Next_SCI); -- beware of self loops
            SCI_DB.Store(Next_SCI,To);
         else
           -- this is new node.
           -- notice recursion can update This_SCI in terms of backwardTAUS,Hasloop
           -- ********
           Last_Weak_Explore (Initial,This_Target);
           -- ********
           MERGE_INFO(Current,This_Target,CHANGED); 
           --
           Next_SCI := SCI_DB.Retrieve(To);
           ADD_BACKWARDTAU(Current,Next_SCI); -- beware of self loops
           SCI_DB.Store(Next_SCI,To);
         end if;
         --
       else
         ------------------------------------------------
         -- this is a NON-TAU transition 
         --  evolution is (xx)(yy){}    == NONTAU
         --  evolution is (..)(..){zzz} == NONTAU
         ------------------------------------------------
         --
         ADD_EVOLUTION_SORTED (Current,This_Target,This_Edge_Label, CHANGED);  
         --
         -- invece di continuare ricorsivamente mette il target nella lista di cose da fare
         Next_SCI := SCI_DB.Retrieve(To);
         if To /= From and Next_SCI.BookedNode = -1 then
           WCount := WCount+1;
           Next_SCI.BookedNode := WCount-1;
           SCI_DB.Store(Next_SCI,To);
           Add_New_Configuration(This_Target); 
           -- Se il nodo e' gia fully elaborated potrei stamparlo qui (se non gia' printed)
           -- senza aggiungerlo alla lista dei nodi da elaborare, ma in questo modo le stampe
           -- avverrebbero in parte depth-first, il che non e' gradevole (anche se pù efficente).
           -- Non e' chiaro se al formato aut o aut tale ordinamento creerebbe problemi.
           -- E.g. 1--2, 2-a->3, 1-->4, 4-b->2,  genererebbe: 2-a->3, 1-a->3, 1-b->2
           -- (e lo stato iniziale sarebbe la src dell'aultima transizione)
         end if;
       end if;
       --
       Iterator_Advance(This_Iter);
     end loop;
     --
     Iterator_Finalize(This_Iter);
     --
     ---------------
     -- Quando ho finito di esplorare una configurazione in SCI_DB ci sono
     --  tutte le info sulle sue possibili evoluzioni.
     --  HasLoop, HasFinal, ForwardConfs/Forwardlabels. E queste non possono piu' cambiare.
     -- dopo aver propagato le info ai supernodes, la lista BackwardsTAU può quindi essere rimossa.
     --
     -- save node info
     This_SCI := SCI_DB.Retrieve(From);
     This_SCI.ElaborationEnded := True;
     SCI_DB.Store(This_SCI,From);
     BACKPROPAGATE_INFO(Current);
     if Initial=Current then
       PRINT_EDGES(Current,This_SCI); 
     end if;
     return;
   end Last_Weak_Explore;

   -------------------------------------------------------------------
   -- called to print the nonTAU node transitions, when the closure of its tau 
   --  evolutions has been traversed.
   -------------------------------------------------------------------
   procedure PRINT_EDGES(Current: System_Configuration; 
                           This_SCI: System_Configuration_Info) is
     Next_SCI:System_Configuration_Info;
   begin
     if Just_Count then return; end if;
     for I in This_SCI.ForwardCONFS.all'Range loop
       Transitions_Count := Transitions_Count +1;
       Next_SCI := SCI_DB.Retrieve(Progressive(This_SCI.ForwardCONFS(I)));
       Print_Edge(This_SCI.BookedNode,
                     This_SCI.AbstractStateLabels.all,
                     Next_SCI.AbstractStateLabels.all, -- needed for "tau"
                     This_SCI.ForwardLabels(I).all,
                     Next_SCI.BookedNode);
       NeededEdges :=  NeededEdges +1;
       Wedges := Wedges+1;
     end loop;
     if This_SCI.HasFinal then
       Wedges := Wedges+1;
       Print_Final(This_SCI.BookedNode, This_SCI.AbstractStateLabels.all);
     end if;
     if This_SCI.HasLoop then
       Wedges := Wedges+1;
       Print_Loop(This_SCI.BookedNode, This_SCI.AbstractStateLabels.all);
     end if;
   end PRINT_EDGES;


   ---------------------------------------------------------------
   -- called when we find a non TAU evolution 
   -- to be added to the current ForwardCONFS/ForwardLABELS
   ---------------------------------------------------------------
   procedure ADD_EVOLUTION_SORTED (
                          This_Conf: System_Configuration;
                          This_Target: System_Configuration;
                          This_Edge_Label: String_Ref;
                          changed: in out Boolean) is
     Insert_Point: Integer;
     MoreCONFS: System_Configuration_Table_Ref;
     MoreLabels: String_Table_Ref;
     This_SCI: System_Configuration_Info;
   begin
     Insert_Point := 1;
     This_SCI := SCI_DB.Retrieve(Progressive(This_Conf));
     -- individua l'indice (positivo) a cui inserire il nuovo elemento
     -- Se la coppia (This_Conf,This_Edge_Label) e' gia presente mette Insert_Point==0
     -- il vettore viene conservato in modo crescente
     --
     for I in This_SCI.ForwardCONFS.all'Range loop
       Insert_Point :=I;
       if This_SCI.ForwardCONFS(I) = This_Target then 
         if This_SCI.ForwardLABELS(I).all = This_Edge_Label.all then
           Insert_Point := 0;
           exit;
         else
           if This_Target < This_SCI.ForwardCONFS(I)  or else
              (This_Target = This_SCI.ForwardCONFS(I)  and
               This_Edge_Label.all < This_SCI.ForwardLABELS(I).all) then
                -- we keep the current index I as Insert Point
             exit;
           end if;
         end if;
       end if;
     end loop;
     --
     if Insert_Point > 0 then
        changed := True;
        -- da aggiungere all'interno del vettore alla posizione Insert_Point
        MoreCONFS :=
          new System_Configuration_Table'(
               This_SCI.ForwardCONFS(1..Insert_Point-1)
               & This_Target  
               & This_SCI.ForwardCONFS(Insert_Point..This_SCI.ForwardCONFS.all'Last));
        MoreLABELS :=
          new String_Table'(
               This_SCI.ForwardLABELS(1..Insert_Point-1) 
               & This_Edge_Label
               & This_SCI.ForwardLABELS(Insert_Point..This_SCI.ForwardLABELS.all'Last));
        Free(This_SCI.ForwardCONFS);
        Free(This_SCI.ForwardLABELS);
        This_SCI.ForwardCONFS := MoreCONFS;
        This_SCI.ForwardLABELS := MoreLABELS;        
     end if;
     SCI_DB.Store(This_SCI,Progressive(This_Conf));
   end ADD_EVOLUTION_SORTED;


   -- called when a fully elaborated Next_Conf is encountered.
   -- the Next_Conf evolution must me merged with the current ones.
   -- Next_Conf cannot be == This_Conf)
    procedure MERGE_INFO(This_Conf: System_Configuration;
                        Next_Conf: System_Configuration;
                        changed: in out Boolean) is
     This_SCI: System_Configuration_Info;
     Next_SCI: System_Configuration_Info;
   begin
     if This_Conf = Next_Conf then
        return;
     end if;
     Next_SCI := SCI_DB.Retrieve(Progressive(Next_Conf));
     for K in Next_SCI.ForwardCONFS.all 'Range loop
        -- add all the targets of Next_Conf to This_Conf
        ADD_EVOLUTION_SORTED (
             This_Conf, Next_SCI.ForwardCONFS(K), Next_SCI.ForwardLABELS(K),CHANGED);
     end loop;
     This_SCI := SCI_DB.Retrieve(Progressive(This_Conf));
     if Next_SCI.HasFinal and This_SCI.HasFinal= False then 
         CHANGED := True;
         This_SCI.HasFinal := True; 
     end if;
     if Next_SCI.HasLoop  and This_SCI.HasLoop= False then 
         CHANGED := True; 
         This_SCI.HasLoop := True; 
     end if;
     SCI_DB.Store(This_SCI,Progressive(This_Conf));
   end MERGE_INFO;

   -- called when the elaboration of a node completes.
   -- the data collected on the node must be propagated back to the calling TAUs
   --   AND SO ON, RECURSIVELY!!!!  UNTIL NO MORE BACKWARD TAUS OR NO CHANGED INFO.
   procedure BACKPROPAGATE_INFO(This_Conf: System_Configuration) is
     This_SCI: System_Configuration_Info;
     changed: Boolean;
   begin
     This_SCI := SCI_DB.Retrieve(Progressive(This_Conf));
     for I in This_SCI.BackwardTAUS.all'Range loop
        if This_Conf /= This_SCI.BackwardTAUS(I) then
           changed := False;
           MERGE_INFO(This_SCI.BackwardTAUS(I),This_Conf,CHANGED);
           if changed then
             BACKPROPAGATE_INFO(This_SCI.BackwardTAUS(I));
           end if;
        end if;
     end loop;
   end BACKPROPAGATE_INFO;

   procedure ADD_BACKWARDTAU(Current: System_Configuration;
                              Next_SCI: in out System_Configuration_Info) is
     Found: Boolean:= False;
     MoreBackwardTAUS: System_Configuration_Table_Ref;
   begin
     for I in Next_SCI.BackwardTAUS.all'Range loop
        if Next_SCI.BackwardTAUS(I) = Current then
           Found := True;
        end if;
     end loop;
     if not Found then
        MoreBackwardTAUS :=
          new System_Configuration_Table'(Next_SCI.BackwardTAUS.all & Current);
        Free(Next_SCI.BackwardTAUS);
        Next_SCI.BackwardTAUS := MoreBackwardTAUS;
     end if;
   end ADD_BACKWARDTAU;


 ----------------------- PARALLLEL MODEL GENERATION -------------------

    task type ModWorker(I: Natural; Max: Natural) is
    end;
    
    task body ModWorker is
      NextItem: Natural;
      This_Iter: Evolutions_Iterator;
    begin
      NextItem := I;
      while not ALLDONE loop
        if MyConfigurations.Kernel.StatesSpace_Size >= NextItem+Max then
          Iterator_Initialize (This_Iter,System_Configuration(NextItem));
          Iterator_Finalize(This_Iter);
          NextItem := NextItem + Max;
        else
          delay 0.00001;
        end if;
      end loop;
    exception
      when Event: others =>
        Put_Line (Current_Error, Exception_Name(Event));
        Put_Line (Current_Error, Exception_Message(Event));
        Put_line(Current_Error, "++++++++  ModWorker dying ... ++++++++++++++");
    end ModWorker;

    type  Wref is access ModWorker;
   ---------------------------------------------------------
   --  costrusce in modo breadth-first il modello tab delle tracce (rimuovendo le azioni tau)
   --  e troncando il modello appena viene superato il numero massimo di nodi indicati 
   --  (States_Limit)
   ---------------------------------------------------------
   procedure BreadthFirstExplore  is
     Has_Final, Has_Loop: Boolean := False;
     Depth : Natural := 0;
     WW: Wref;
   begin
     if not Just_Count then
       Create(WEAKOUT,Out_File,"weakmodel.aut");
       Create(FLATOUT,Out_File,"flatmodel.aut");
       Print_Header;
     end if;
     --  WTF!!!!!   WTF!!!!!
     if (Flags.ModelCores >=1 and Flags.ModelCores <=16) then
        ALLDONE := False;
        for K in 1..Flags.ModelCores loop
           WW := new ModWorker(K,Flags.ModelCores);
        end loop;
     end if;
     T1(1) := Initial_Configuration;
     I1 := 1;
     MarkBooked(1);
     --
     while I1 /= 0 loop
       LastWidth := I1;
       CurTable := 2;
       I2 := 0;  -- prepare T2 for getting next round of items
       Depth := Depth+1;
       WDepth := Depth;
       if Depth > Depth_Limit then
         truncated := True;
       end if;
       for I in 1..I1 loop
         Has_Final := False;
         Has_Loop := False;
         Last_Weak_Explore(T1(I),T1(I));
       end loop;
       --
       if I2 = 0 then exit; end if;
       --
       --   Check to be done on States COunt , not NeededStates
       if NeededStates > States_Limit then
           truncated := True;
       end if;
       --
       LastWidth := I2;
       CurTable := 1;
       I1 := 0;  -- prepare T1 for getting next round of items
       Depth := Depth+1;
       WDepth := Depth;
       if Depth > Depth_Limit then
         truncated := True;
       end if;
       for I in 1..I2 loop
         Has_Final := False;
         Has_Loop := False;
         Last_Weak_Explore(T2(I),T2(I));
       end loop;
       --
       if I1 = 0 then exit; end if;
       --
       if NeededStates > States_Limit then
           truncated := true;
       end if;
       --
       if Just_Count or (OutputFileName=null or else  OutputFileName'Length >0) then
         SeenStates := MyConfigurations.Kernel.StatesSpace_Size; -- numero di stati generati finora
         if (Previous_Count +1000 < SeenStates and then SeenStates < 20001 ) or else
            (Previous_Count +2000 < SeenStates and then SeenStates < 40001 ) or else
            (Previous_Count +5000 < SeenStates and then SeenStates < 100001 ) or else
            (Previous_Count +10000 < SeenStates and then SeenStates < 200001 ) or else
            (Previous_Count +20000 < SeenStates and then SeenStates < 400001 ) or else
            (Previous_Count +50000 < SeenStates and then SeenStates > 400000 ) then
            Set_Output(Standard_Output);
            MyConfigurations.Kernel.Print_StatesSpace_Stats;
            Put_Line ("Depth = " & Integer'Image(Depth) &
                      " Width = " & Integer'Image(LastWidth) &
                      " Needed = " & Integer'Image(NeededStates));
           if Is_Open(WEAKOUT) then Set_Output(WEAKOUT); end if;
           Previous_Count := SeenStates;
         end if;
       end if;
       --
     end loop;
     --
     ALLDONE := True;
     if not Just_Count then
       Close(FLATOUT);
       Close(WEAKOUT);
       declare
         package MyDir is new Ada.Direct_IO(Character);
         FF: MyDir.File_Type;
         Header: String := 
          "des (0," & Trim(Integer'Image(WEdges),Left) & ","& Trim(Integer'Image(WCount),Left) & ")" ; 
       begin
         MyDir.Open(FF, MyDir.INOUT_File,"weakmodel.aut");
         for I in Header'Range loop
           MyDir.Write(FF, Header(I), MyDir.Positive_Count(I));
         end loop;
         MyDir.Close(FF);
       end;
     end if;
     --
   end BreadthFirstExplore;

   ---------------------------------------------------------
   -- if Depth mod2=1 adds Conf in T2
   -- else adds Conf in T1
   ---------------------------------------------------------
   procedure Add_New_Configuration (Conf: System_Configuration) is
   begin
      -- if Already_Seen(Progressive(Conf)) then return; end if;
      --
      --  CHECK ALREADY DONE IN LWAST_WEAK_EXPLORE
      --if Already_Booked(Progressive(Conf)) then return; end if;
      --MarkBooked(Progressive(Conf));
      --
      if CurTable =1 then
         -- adds Conf in T1
         I1 := I1 +1;
         if I1 not in T1'Range then
           Put_Line(Current_Error, "Ooops: System_Configuration_Table too small ..." & Integer'Image(I1));
           raise Program_Error;
         end if;
         T1(I1) := Conf;
      else
         -- adds Conf in T2
         I2 := I2 +1;
         if I2 not in T2'Range then
           Put_Line(Current_Error, "Ooops: System_Configuration_Table too small ..." & Integer'Image(I2));
           raise Program_Error;
         end if;
         T2(I2) := Conf;
      end if;
   end Add_New_Configuration;
   ---------------------------------------------------------

   task type worker is
      PRAGMA STORAGE_SIZE( taskstacksize);
   end  worker;

   task body worker is
   begin
      Load_Model(Model_FileName.all);
      BreadthFirstExplore;

      if Just_Count then
       Put_Line (Standard_Output, "-------------------------------------------");
       Put_Line (Standard_Output, "The system has" & Int64'Image(MyConfigurations.Kernel.StatesSpace_Size) & " states" );
       Put_Line (Standard_Output, "and " & Int64'Image(Transitions_Count) & " transitions" );
       Put_Line (Standard_Output, "-------------------------------------------");
      end if;
   exception
    when Event: others =>
      Put_Line (Current_Error, "Runtime Error in mc2aut worker!");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
   end worker;

begin
  Argc := 0;
  -- in the case of FMC, slways show tau#channel(val) in place of just "tau"
  -- unless the aut output is going to be minimized (-l) by ltsconvert
  loop
    Argc := Argc +1;
    if Argc > Ada.Command_Line.Argument_Count  then
       exit;
    end if;
    declare
      Input_Arg : String := Ada.Command_Line.Argument (Argc);
    begin
      if Input_Arg'Length >2 and then
            Input_Arg(1) = '-'  and then
            Input_Arg(2) = 'm'  and then
            Input_Arg(3) in '1'..'9' then
        Flags.ModelCores := Integer'Value(Input_Arg(3..Input_Arg'Last));
        Flags.ThreadSafe := True;
      elsif Input_Arg = "-debug" then
         Debug := True;
      elsif Input_Arg = "-f" then
         Product_Families := True;
      elsif Input_Arg = "-t" then
         -- if selected "() () {}" become tau  (and optimized away)
         -- "tau" transitions are fully removed.
         Remove_Tau_Transitions := True;  
      elsif Input_Arg'Length > 2 and then
             Input_Arg(1)='-' and then 
             Input_Arg(2)='s' and then 
             Input_Arg(3) in '0'..'9' then
        States_Limit := Integer'Value(Input_Arg(3..Input_Arg'Length));
      elsif Input_Arg'Length > 2 and then
             Input_Arg(1)='-' and then 
             Input_Arg(2)='d' and then 
             Input_Arg(3) in '0'..'9' then
        Depth_Limit := Integer'Value(Input_Arg(3..Input_Arg'Length));
      elsif Input_Arg'Length > 2 and then
             Input_Arg(1)='-' and then
             Input_Arg(2)='s' and then
             Input_Arg(3) in '0'..'9' then
        States_Limit := Integer'Value(Input_Arg(3..Input_Arg'Length));
      elsif SetOutput then
         OutputFileName := new String'(Input_Arg);
         SetOutput := False; 
      elsif Input_Arg(1) /='-' then
         Model_FileName := new String'(Input_Arg);
      else
        null; -- ignore meaningless parameters
      end if;
    end;
  end loop;   -- cycle on arguments 
  if not Use_Ground_Labels then 
    MemorySaving := True;
  else
    MemorySaving := False;
  end if;

  if Ada.Command_Line.Argument_Count = 0 or else
    Model_FileName = null then
    Put_line(Current_Error, "mc2aut: ERROR incorrect parameters ...");
    Put_line(Current_Error, " usage: mc2aut [options] [inputmodelfile] ");
    Put_line(Current_Error, " options: "); 
    Put_line(Current_Error, "   -sN  (N= number  limit output graph size to N nodes)");
    Put_line(Current_Error, "   -dN  (N= number  limit output graph depth to N )");
    Put_line(Current_Error, "   -t (remove tau transitions, and generted only tracemodel.aut)");
    Put_line(Current_Error, "   -f (handle may/must transition of product families)");
    Put_line(Current_Error, " examples: ");
    Put_line(Current_Error, " mc2aut -d50 model.umc");
    Put_line(Current_Error, " mc2aut -t model.umnc (preparation for trace mininization)");
    --
    --  FORSE HA PIU' SENSO LIMITARE LA DIMENSIONE DEL GRAFO IN INPUT !?!?!?
    --   E.g. per modelli divergenti .... OPPURE ENTRAMBI ....
    -- LIMITE IN OUTOPUT  OK per limitare la visualizzazione (quindi per DOT files?) ...
    -- LIMITE IN INPUT OK per divergenza o modelli troppo grossi ...
   return;
  end if;
  declare
    WW: Worker;
  begin
    null;
  end;
    --
end MC2aut;
