with Dyn_Store;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line;
with Configurations; 
with Flags; use Flags;
with DotLib; use DotLib;
with Ada.Exceptions; use Ada.Exceptions;
procedure MC2dot is
-- --------------------------------------------------------------------------
--   -debug  Debug Mode, prints stats into
--   -c  Just Count 
--
--   -d <N>  truncates the graph at depth <Depth> introducendo transizioni #truncated
--
--   -g   use le ground labels come edge labels
--        INCOMPARTIBILE CON -a  (o no???)
--
--   -a   usa le abstract labels come edge labels
--        INCOMPARTIBILE CON -g  (o no???)
--
--   -t   elimina le transizioni tau introducendo se ncessario transizioni #loop
-- --------------------------------------------------------------------------
--   -l   mette le labels degli stati sulle lables degli edges (per usare minimizzatori su lts)
--        aggiunge #final  agli stati finali,  (se non c'e' -r) aggiunge trasforma {} in tau
--        USATO PER GENERARE DESCRIZIONI DA MINIMIZZAZRE CON LTSCONVERT
--
--   -b   lascia le labels degli stati sugli stati,
--        con -r trasforma #loop, con -l  trasforma #final, con -d trasforma #truncated
--        se le edge labels codificano state labels le ripulisce.
--        USATO PER GENERARE DESCRIZIONI DA VISUALIZZRE DIRETTAMENTE
--
--   -f   product families display  (handling of may-must transitions)
-- --------------------------------------------------------------------------
--  declared inside DotLib
--
--  Remove_Tau_Transitions: Boolean := False;   -- option "-t"
--  Encode_State_Labels: Boolean := False;      -- option "-l"
--  Beautify: Boolean := False;                 -- option "-b"
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
  States_Limit: Integer := 50_000_000;         -- option "-s"
  Depth_Limit: Integer := 50_000_000;            -- option "-d"
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
  ----------------------------------------------------------------------------
  package MyConfigurations is new Configurations;
  use MyConfigurations;
  use MyConfigurations.Kernel;
  ----------------------------------------------------------------------------
  type System_Configuration_Table is array (Positive range <>) of System_Configuration;
  type System_Configuration_Table_Ref is access System_Configuration_Table;
----  --Empty_System_Configuration_Table: System_Configuration_Table_Ref := new System_Configuration_Table(1..0);
  procedure Free is new  Ada.Unchecked_Deallocation (System_Configuration_Table,System_Configuration_Table_Ref);
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
   end record;
   function Null_Info return System_Configuration_Info is
      Tmp: System_Configuration_Info;
   begin
      return Tmp;
   end Null_Info;
  ---------------------------------------------------------------------------
   package SCI_DB is new Dyn_Store (System_Configuration_Info, Null_Info);
  ---------------------------------------------------------------------------



--   States: natural :=0;
--   Edges: natural :=0;
--   Transitions_Max: Natural := 1_000_000;
   ------------------------------------------------------------------------------
   MaxBreadth: Natural := 1_000_000;     -- size of the two System_Configuration_Tables used)
   Slot_Size: Natural := Flags.Vectors_Max;   -- in flags.ads  == 4096
   type Seen_Table is array (Positive Range 1..Slot_Size) of Boolean;
   type Seen_Table_Ref is access Seen_Table;
   Seen: array(1..Slot_Size) of Seen_Table_Ref;   -- total 4096 * 4096 elems
   Booked: array(1..Slot_Size) of Seen_Table_Ref;   -- total 4096 * 4096 elems
   SeenStates: Int64 :=0;
   ------------------------------------------------------------------------------
    DOTOUT: File_Type;

   --  uno stato viene considerato "seen" quando viene iniziata a partire da esso
   --  una valutazione di Weak/New Explore_Configration (depth=0,confs'Length=1)
   --
   procedure MarkSeen(N: Positive) is
      Slot: Positive := ((N-1) / Slot_Size) +1;
      Index: Positive := ((N-1) mod Slot_Size) +1;
   begin
      if Seen(Slot) = null then
         Seen(Slot) := new Seen_Table;
         Seen(Slot).all := (others => False);
      end if;
      Seen(Slot)(Index) := True;
   end MarkSeen;

   function Already_Seen(N: Positive) return Boolean is
     Slot: Positive:= ((N-1) / Slot_Size) +1;
     Index: Positive := ((N-1) mod Slot_Size) +1;
   begin
      if N > Flags.Max_Elements then
         Put_Line(Standard_Error,"mc2dot: Model generation aborted for slot_size limit");
         Aborted := True;
      end if;
      if Seen(Slot) = null then
         Seen(Slot) := new Seen_Table;
         Seen(Slot).all := (others => False);
         return False;
      else
         return Seen(Slot)(Index);
      end if;
   end Already_Seen;

   -- uno stato viene considerato "Booked" quando viene aggiusto alla lista dei successivi
   -- nodi da explorare tramite Add_New_Configuration.
   -- Per poter diventare "Seen", bisogna prima diventare "Booked"!
   --
   procedure MarkBooked(N: Positive) is
      Slot: Positive := ((N-1) / Slot_Size) +1;
      Index: Positive := ((N-1) mod Slot_Size) +1;
   begin
      if Booked(Slot) = null then
         Booked(Slot) := new Seen_Table;
         Booked(Slot).all := (others => False);
      end if;
      Booked(Slot)(Index) := True;
   end MarkBooked;

   function Already_Booked(N: Positive) return Boolean is
     Slot: Positive:= ((N-1) / Slot_Size) +1;
     Index: Positive := ((N-1) mod Slot_Size) +1;
   begin
      if N > Flags.Max_Elements then
         Put_Line(Standard_Error,"mc2dot: Model generation aborted for slot_size limit");
         Aborted := True;
      end if;
      if Booked(Slot) = null then
         Booked(Slot) := new Seen_Table;
         Booked(Slot).all := (others => False);
         return False;
      else
         return Booked(Slot)(Index);
      end if;
   end Already_Booked;


   procedure Add_New_Configuration (Conf: System_Configuration);

   Previuos_Count: Natural :=0;   -- used to avoid message repetition

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------

   ----------------------------------------------
   --  inseriscr "\n"  in alcun punto per forzare la visuluazzazione
   --- della label su piu righe (usato per le azioni UML)
   ----------------------------------------------
   function Dot_Splitted(Source: String) return String is
     N: Natural :=0;
   begin
     if not Beautify then
        return Source;
     end if;
     for I in Source'Range loop
       if Source(I) = ';' then
          N := N+1;
       end if;
       if Source'Last >= I+3 and then
           Source(I..I+3) = "then" then
          N := N+1;
       end if;
       if Source'Last >= I+3 and then
           Source(I..I+3) = "else" then
          N := N+1;
       end if;
     end loop;
     N := Source'Length + 2*N;
     declare
      Res: String(1..N);
      I: Natural :=0;
     begin
       for J in Source'Range loop
         if Source(J) = ';' then
            I := I+1;
            Res(I..I+2) := ";\n" ;
            I := I+2;
         elsif Source'Last >= J+3 and then
           Source(J..J+3) = "then" and then
           J >2 and then Res(I) /= 'n' then
           I := I+1;
           Res(I..I+1):= "\n";
           I := I+1;
         elsif Source'Last >= J+3 and then
           Source(J..J+3) = "else" and then
           J >2 and then Res(I) /= 'n' then
           I := I+1;
           Res(I..I+1):= "\n";
           I := I+1;
         else
            I := I+1;
            Res(I) := Source(J);
         end if;
       end loop;
       return Res;
     end;
   end Dot_Splitted;

   function Dot_Joined (Sources: String_Table) return String is
   begin
     if Sources'Length = 0 then
       return "";
     elsif Sources'Length = 1 or else
           (Sources'Length =2 and Sources(Sources'First+1).all = "") then
          return Dot_Splitted(Sources(Sources'First).all);
     else 
        return Dot_Splitted(Sources(Sources'First).all) & "\n " &
           Dot_Joined(Sources(Sources'First+1 .. Sources'Last));
     end if;
   end Dot_Joined;

   function Dot_Ground_Label (It: Evolutions_Iterator) return String is
     Ground_Labels: String_Table := Get_Ground_Action_Labels(It);
   begin
        return "{" & Dot_Joined(Ground_Labels) & "}";
   end Dot_Ground_Label;

   function Mk_Edge_Label(It: Evolutions_Iterator) return String is
   begin
     if Use_Ground_Labels and not Use_Abstract_Labels then 
       declare
         Ground_Labels: String := Dot_Ground_Label(It);
       begin
         return Ground_Labels;
       end;
     elsif Use_Abstract_Labels and not Use_Ground_Labels then
       declare
         ActionLabels: String := Abstract_Action_Labels(It);
       begin
         return ActionLabels;
       end;
     else
       declare
         Ground_Labels: String := Dot_Ground_Label(It);
         ActionLabels: String := Abstract_Action_Labels(It);
       begin
         return Ground_Labels & ActionLabels;
       end;  
     end if;
   end Mk_Edge_Label;


-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
   Transitions_Count: Int64 :=0;
   NeededStates: Natural :=0;
   NeededEdges: natural :=0;
  ----------------------------------------------------------------------------
  truncated: boolean := False;
  ----------------------------------------------------------------------------

   -----------------------------------------------------------------
   --  data una confs  (in realta un vettore  (1..1 => conf) )
   --  la marca come "Seen",
   --  osserva gli archi uscenti,
   --  stampa i dati sul nodo source e sugli edges uscenti
   --  chiama Add_New_Configuration per tutti i taget 
   --     (che vengono  aggiunti solo se non Booked e non Seen)
   -----------------------------------------------------------------
   procedure New_Explore_Configuration (
          Confs: System_Configuration_Table;
          Depth: Natural) is
     --
     CURR_ITEM: Natural;
     This_Iter: Evolutions_Iterator;
     SourceLabels: String := "{" & Display_AbstractStateLabels(Get_Abstract_State_Labels(Confs(1))) & "}";

   begin
     --
     CURR_ITEM := Progressive(Confs(Confs'Last));
     --
     -- exit if state already analysed
     if Already_Seen(Progressive(Confs(1))) then
         return;
     end if;
     --
     if Confs'Length=1 then
        MarkSeen(Progressive(Confs(1)));
        NeededStates := NeededStates +1;
     end if;
     --
     if truncated and not Just_Count then
       Print_Dot_Truncated_Node(NickName(Confs(1), "C"),SourceLabels);
       return;
     end if;
     --
     -- INIZIAMO AD ANALIZZARE LE EVOLUZIONI
     --
     Iterator_Initialize (This_Iter, Confs(Confs'Last));
     --
     if not Just_Count then
       if Has_System_Transition(This_Iter) then
         Print_Dot_Node(NickName(Confs(1), "C"), SourceLabels);
       else 
         Print_Dot_Final(NickName(Confs(1), "C"), SourceLabels);
       end if;
     end if;
     --
     while Has_System_Transition (This_Iter) loop
       declare
         This_Target: System_Configuration := Get_Target_Configuration (This_Iter);
         TargetLabels: String :=
           "{" & Display_AbstractStateLabels(Get_Abstract_State_Labels(This_Target)) & "}";
         EdgeLabels: String := Mk_Edge_Label(This_Iter);
       begin
         Transitions_Count := Transitions_Count+1;
         if not Just_Count then
           Print_Dot_Edge(NickName(Confs(1),"C"), 
                        SourceLabels, TargetLabels, EdgeLabels,
                        NickName(This_Target, "C"));
         end if;
         Add_New_Configuration(This_Target);
       end;
       Iterator_Advance(This_Iter);
     end loop;
     Iterator_Finalize(This_Iter);
     --
   end New_Explore_Configuration;

  

   procedure PRINT_DOT_EDGES(Current: System_Configuration;
                           This_SCI: System_Configuration_Info);

   procedure MERGE_INFO(This_Conf: System_Configuration;
                              Next_Conf: System_Configuration);
			      
   procedure MERGE_EVOLUTIONS_SORTED(This_Conf: System_Configuration;
                              Next_Conf: System_Configuration);

   procedure ADD_BACKWARDTAU(Current: System_Configuration;
                              Next_SCI: in out System_Configuration_Info);

   procedure ADD_EVOLUTION_SORTED (This_Conf: System_Configuration;
                          This_Target: System_Configuration;
                          This_Edge_Label: String_Ref);
			  
   procedure BACKPROPAGATE_INFO(This_Conf: System_Configuration);

   --------------------------------------------------------------------
   WSTEPS: Natural :=0;   -- used both to print the minimization progress
   PSTEPS: Natural :=0;
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- Valutazione top-down pura, con grande stack e task ad hoc.
   --
   -- Sono sul nodo N,  guardo tutte le transizioni possibili.
   --  Se ho una transizione {a}  aggiungo {a} alle evoluzioni del nodo,
   --  Se ho transizione {tau} ed il nodo target non e' mai stato visto prima,
   --    continuo ricorsivamento dal nodo targate e aggiungo
   --    alle evoluzioni del nodo tutte le evoluzioni erditate ricorsivamente da esso.
   -- Se ho una transizione {tau} ed incontro un nodo gia visto, non continuo la ricorsione su
   --    di esso, ma recupero e aggiungo le sue evoluzioni se ci sono.
   -- Dopo aver analizzato una transizione, se il target e' stato raggiunto con {tau} 
   --    aggiungo il nodo N come super del target.
   -- 
   -- Dopo aver analizzato tutte le transizioni uscenti,
   --  se esistono, salvo le evoluzioneìi {a,b,c} sul descrittore del nodo,
   --  e le propago ricorsivamente a tutti i supernodi noti di N.
   --------------------------------------------------------------------
   procedure Last_Weak_Explore (Initial,Current: System_Configuration) is
     --
     This_Iter: Evolutions_Iterator;
     This_Target: System_Configuration;
     To, From: Natural;
     This_SCI: System_Configuration_Info;
     Next_SCI: System_Configuration_Info;
     This_Edge_Label: String_Ref;
   begin
     From := Progressive(Current);
     This_SCI := SCI_DB.Retrieve(From);
     if not This_SCI.ElaborationStarted then
       This_SCI.ElaborationStarted := True;
       This_SCI.AbstractStateLabels :=
          new String'("{" & Display_AbstractStateLabels(Get_Abstract_State_Labels(Current)) & "}");
       SCI_DB.Store(This_SCI,From);
       -- verbose when necessary
       if Just_Count or (OutputFileName=null or else  OutputFileName'Length >0) then
         -- SeenStates := MyConfigurations.Kernel.StatesSpace_Size; -- numero di stati generati finora
         WSTEPS := WSTEPS+1;
         if (PSTEPS +1000 < WSTEPS and then WSTEPS < 20_001 ) or else
           (PSTEPS +2000 < WSTEPS and then WSTEPS < 40_001 ) or else
           (PSTEPS +5000 < WSTEPS and then WSTEPS < 100_001 ) or else
           (PSTEPS +10_000 < WSTEPS and then WSTEPS < 200_001 ) or else
           (PSTEPS +20_000 < WSTEPS and then WSTEPS > 200_001 ) then
           Set_Output(Standard_Output);
           MyConfigurations.Kernel.Print_StatesSpace_Stats;
           if Is_Open(DOTOUT) then Set_Output(DOTOUT); end if;
           PSTEPS := WSTEPS;
         end if;
       end if;
     end if;
     -- if just starting the analysys of a new state record it.
     if Initial=Current then
        MarkSeen(From);
        NeededStates := NeededStates +1;
     end if;
     --
     -- INIZIAMO AD ANALIZZARE LE EVOLUZIONI
     --
     Iterator_Initialize (This_Iter, Current);
     --
     -- This new node has no transitions. Just remember there is a final state
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
       if This_SCI.AbstractStateLabels.all = Next_SCI.AbstractStateLabels.all and then
             This_Edge_Label.all = "{}" then
         ------------------------------------------------
         -- this is a TAU transition (xx)(xx){}
         ------------------------------------------------
         if Next_SCI.ElaborationEnded then
            -- if To is a seen node, recycle its info
            MERGE_INFO(Current,This_Target) ;
            This_SCI := SCI_DB.Retrieve(From);
         elsif Next_SCI.ElaborationStarted then
            -- this is  loop back
            This_SCI.HasLoop := True;
            Next_SCI.HasLoop := True;
            SCI_DB.Store(Next_SCI,To); -- Ci vogliono tuti e due?? non si perde la ricorsione indietro?
            SCI_DB.Store(This_SCI,From); 
         else
           -- this is new node.
           -- notice recursion can update This_SCI in terms of backwardTAUS,Hasloop
           SCI_DB.Store(This_SCI,From);
           -- ********
           Last_Weak_Explore (Initial,This_Target);
           -- ********
           This_SCI := SCI_DB.Retrieve(From);
           Next_SCI := SCI_DB.Retrieve(To);
           -- NON  e INUTILE a causa della BACKPROPAGATE_INFO chiamata 
           --    all fine del Last_Weak_Explore da This_Target
           -- perche il backward tau non e' ancora stato settato
           MERGE_INFO(Current,This_Target); 
         end if;
         --
         ADD_BACKWARDTAU(Current,Next_SCI);
         SCI_DB.Store(Next_SCI,To);
         This_SCI := SCI_DB.Retrieve(From);   -- From could be == To
         --
       else
         ------------------------------------------------
         -- this is a NON-TAU transition 
         --  evolution is (xx)(yy){}    == NONTAU
         --  evolution is (..)(..){zzz} == NONTAU
         ------------------------------------------------
         ADD_EVOLUTION_SORTED (Current,This_Target,This_Edge_Label);
         This_SCI := SCI_DB.Retrieve(From);
         --
         -- invece di continuare ricorsivamente mette il target nella lista di cose da fare
         Add_New_Configuration(This_Target);
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
     -- dopo aver propagato le info ai supernodes, la lista BackwardsTAU può quandi essere rimossa.
     --
     -- save node info
     This_SCI.ElaborationEnded := True;
     SCI_DB.Store(This_SCI,From);
     if Initial=Current then
        PRINT_DOT_EDGES(Current,This_SCI);
     end if;
     BACKPROPAGATE_INFO(Current);
     return;
   end Last_Weak_Explore;

   -------------------------------------------------------------------
   -- called to print the nonTAU node transitions, when the closure of its tau 
   --  evolutions has been traversed.
   -------------------------------------------------------------------
   procedure PRINT_DOT_EDGES(Current: System_Configuration; 
                           This_SCI: System_Configuration_Info) is
     Next_SCI:System_Configuration_Info;
   begin
     if Just_Count then return; end if;
     for I in This_SCI.ForwardCONFS.all'Range loop
       Transitions_Count := Transitions_Count +1;
       Next_SCI := SCI_DB.Retrieve(Progressive(This_SCI.ForwardCONFS(I)));
       Print_Dot_Edge(NickName(Current,"W"),
                     This_SCI.AbstractStateLabels.all,
                     Next_SCI.AbstractStateLabels.all, -- needed for "tau"
                     This_SCI.ForwardLabels(I).all,
                     NickName(This_SCI.ForwardCONFS(I),"W"));
       NeededEdges :=  NeededEdges +1;
     end loop;
     if This_SCI.HasFinal then
       Print_Dot_Final(NickName(Current,"W"), This_SCI.AbstractStateLabels.all);
     end if;
     if This_SCI.HasLoop then
       Print_Dot_Loop(NickName(Current,"W"), This_SCI.AbstractStateLabels.all);
     end if;
   end PRINT_DOT_EDGES;

   ---------------------------------------------------------------
   -- called when we find a non TAU evolution 
   -- to be added to the current ForwardCONFS/ForwardLABELS
   ---------------------------------------------------------------

   procedure ADD_EVOLUTION_SORTED (This_Conf: System_Configuration;
                          This_Target: System_Configuration;
                          This_Edge_Label: String_Ref) is
     Insert_Point: Integer;
     MoreCONFS: System_Configuration_Table_Ref;
     MoreLabels: String_Table_Ref;
     This_SCI: System_Configuration_Info;
   begin
     Insert_Point := -1;
     This_SCI := SCI_DB.Retrieve(Progressive(This_Conf));
     for I in This_SCI.ForwardCONFS.all'Range loop
        if This_SCI.ForwardCONFS(I) = This_Target and then
             This_SCI.ForwardLABELS(I).all = This_Edge_Label.all then
           Insert_Point := 0;
           exit;
        end if;
        if Progressive(This_SCI.ForwardCONFS(I)) > Progressive(This_Target) then
           Insert_Point :=I;
           exit;
        end if;
        if I = This_SCI.ForwardCONFS.all'Last then
           Insert_Point :=I+1;
        end if;
     end loop;
     if Insert_Point =-1 then
         -- da aggiungere a vettore vuoto 
        MoreCONFS := new System_Configuration_Table'(1 => This_Target);
        MoreLABELS := new String_Table'(1 => This_Edge_Label);
        Free(This_SCI.ForwardCONFS);
        Free(This_SCI.ForwardLABELS);
        This_SCI.ForwardCONFS := MoreCONFS;
        This_SCI.ForwardLABELS := MoreLABELS;
        --
     elsif Insert_Point = 0 then
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
        --
     else
        -- da aggiungere all fine (posizione Insert_Point+1)
        MoreCONFS :=
          new System_Configuration_Table'(This_SCI.ForwardCONFS.all & This_Target);
        MoreLABELS :=
          new String_Table'(This_SCI.ForwardLABELS.all & This_Edge_Label);
        Free(This_SCI.ForwardCONFS);
        Free(This_SCI.ForwardLABELS);
        This_SCI.ForwardCONFS := MoreCONFS;
        This_SCI.ForwardLABELS := MoreLABELS;        
        --
     end if;
     SCI_DB.Store(This_SCI,Progressive(This_Conf));
   end ADD_EVOLUTION_SORTED;

   
   procedure MERGE_EVOLUTIONS_SORTED(This_Conf: System_Configuration;
                              Next_Conf: System_Configuration) is
     This_SCI: System_Configuration_Info := SCI_DB.Retrieve(Progressive(This_Conf));
     Next_SCI: System_Configuration_Info := SCI_DB.Retrieve(Progressive(Next_Conf));
     NextFWC: System_Configuration_Table := Next_SCI.ForwardCONFS.all;
     NextFWL: String_Table := Next_SCI.ForwardLABELS.all;
     Found: Boolean:= False;
     MoreCONFS: System_Configuration_Table_Ref;
     MoreLabels: String_Table_Ref;
     FinalCONFS: System_Configuration_Table(1..This_SCI.ForwardCONFS.all'Length +
                                            Next_SCI.ForwardCONFS.all'Length);
     FinalLABELS: String_Table(1..This_SCI.ForwardLABELS.all'Length +
                                            Next_SCI.ForwardLABELS.all'Length);
     ThisCON: Integer;
     NextCON: Integer;
     Findex: Integer;
     Leftover: Integer;
   begin
     if This_Conf = Next_Conf then
        return;
     end if;
     if Next_SCI.ForwardCONFS.all'Length=0 then
        return;
     end if;
     if This_SCI.ForwardCONFS.all'Length=0 then
         MoreCONFS := new System_Configuration_Table'(NextFWC);
         MoreLABELS := new String_Table'(NextFWL);
         Free(This_SCI.ForwardCONFS);
         Free(This_SCI.ForwardLABELS);
         This_SCI.ForwardCONFS := MoreCONFS;
         This_SCI.ForwardLABELS := MoreLABELS;
         SCI_DB.Store(This_SCI,Progressive(This_Conf));
        return;
     end if;
     ---
     ThisCON :=1;
     NextCON :=1;
     Findex :=1;
     while true loop
       if This_SCI.ForwardCONFS(ThisCON) = NextFWC(NextCON) and then
           This_SCI.ForwardLABELS(ThisCON).all = NextFWL(NextCON).all then
          -- element already present, push it and skip Final, This and Next indexes 
          FinalCONFS(Findex) := NextFWC(NextCON);
          FinalLABELS(Findex) := NextFWL(NextCON);
          Findex := Findex+1;
          NextCON := NextCON+1;
          ThisCON := ThisCON+1;
       else
          -- we must push only one of the two items
          if Progressive(This_SCI.ForwardCONFS(ThisCON)) < Progressive(NextFWC(NextCON)) or else
            (This_SCI.ForwardCONFS(ThisCON) = NextFWC(NextCON)
              and then This_SCI.ForwardLABELS(ThisCON).all < NextFWL(NextCON).all) then
            FinalCONFS(Findex) := This_SCI.ForwardCONFS(ThisCON);
            FinalLABELS(Findex) := This_SCI.ForwardLABELS(ThisCON);
            Findex := Findex+1;
            ThisCON := ThisCON+1;
          else
            FinalCONFS(Findex) := NextFWC(NextCON);
            FinalLABELS(Findex) := NextFWL(NextCON);
            Findex := Findex+1;
            NextCON := NextCON+1;
          end if;
       end if;
       --
       if NextCON > NextFWC'Last then
          -- push remaining This_SCI items
          Leftover := This_SCI.ForwardCONFS.all'Last - ThisCON +1;
          FinalCONFS(Findex ..Findex+Leftover-1) := 
              This_SCI.ForwardCONFS(ThisCON..ThisCON+Leftover-1);
          FinalLABELS(Findex ..Findex+Leftover-1) := 
              This_SCI.ForwardLABELS(ThisCON..ThisCON+Leftover-1);
          Findex := Findex+Leftover -1;
          exit;
       elsif ThisCON > This_SCI.ForwardCONFS.all'Last then
          -- push remaining Next_SCI items
          Leftover := NextFWC'Last - NextCON +1;
          FinalCONFS(Findex ..Findex+Leftover-1) :=
              NextFWC(NextCON..NextCON+Leftover-1);
          FinalLABELS(Findex ..Findex+Leftover-1) :=
              NextFWL(NextCON..NextCON+Leftover-1);
          Findex := Findex+Leftover -1;
          exit;     
       end if;
     end loop; 
     if Findex > This_SCI.ForwardCONFS.all'Last then
       Free(This_SCI.ForwardCONFS);
       Free(This_SCI.ForwardLABELS);
       This_SCI.ForwardCONFS := new System_Configuration_Table'(FinalCONFS(1..Findex));
       This_SCI.ForwardLABELS := new String_Table'(FinalLABELS(1..Findex));
       SCI_DB.Store(This_SCI,Progressive(This_Conf));
     end if;
   end MERGE_EVOLUTIONS_SORTED;

   
   procedure MERGE_INFO(This_Conf: System_Configuration;
                              Next_Conf: System_Configuration) is
     This_SCI: System_Configuration_Info;
     Next_SCI: System_Configuration_Info;
   begin
     if This_Conf = Next_Conf then
        return;
     end if;
     MERGE_EVOLUTIONS_SORTED(This_Conf,Next_Conf);
     This_SCI := SCI_DB.Retrieve(Progressive(This_Conf));
     Next_SCI := SCI_DB.Retrieve(Progressive(Next_Conf));
     if Next_SCI.HasFinal then This_SCI.HasFinal := True; end if;
     if Next_SCI.HasLoop then This_SCI.HasLoop := True; end if;
     SCI_DB.Store(This_SCI,Progressive(This_Conf));
   end MERGE_INFO;

   procedure BACKPROPAGATE_INFO(This_Conf: System_Configuration) is
     This_SCI: System_Configuration_Info;
     SuperChanged: Boolean;
     Super_SCI: System_Configuration_Info;
     Prevsize: Integer;
   begin
     This_SCI := SCI_DB.Retrieve(Progressive(This_Conf));
     for I in This_SCI.BackwardTAUS.all'Range loop
        Super_SCI:=  SCI_DB.Retrieve(Progressive(This_SCI.BackwardTAUS(I)));
        Prevsize := Super_SCI.ForwardCONFS.all'Length;
        MERGE_EVOLUTIONS_SORTED(This_SCI.BackwardTAUS(I),This_Conf);
        Super_SCI:=  SCI_DB.Retrieve(Progressive(This_SCI.BackwardTAUS(I)));
        if Prevsize < Super_SCI.ForwardCONFS.all'Length then
           SuperChanged := True;
        end if;
        if Super_SCI.HasLOOP = False and then This_SCI.HasLOOP then
           Super_SCI.HasLOOP := True;
           SuperChanged := True;
        end if;
        if Super_SCI.HasFinal = False and then This_SCI.HasFinal then
           Super_SCI.HasFinal := True;
           SuperChanged := True;
        end if;
        --
        if SuperChanged then
           SCI_DB.Store(Super_SCI,Progressive(This_SCI.BackwardTAUS(I)));
           -- if not Super_SCI.This_SCI.ElaborationEnded we ned not propagate the updates
           -- because that willbe done later by another BACKPROPAGATE_INFO called
           --  by the Super node itself at the end of its Last_Weak_Explore
           if This_SCI.ElaborationEnded then
             BACKPROPAGATE_INFO(This_SCI.BackwardTAUS(I));
           end if;
        end if;
     end loop; -- for all This_SCI.BackwardTAUS
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

   -------------------------------------------------------------------
   CONFS: access System_Configuration_Table := new System_Configuration_Table(1..10_000);
   --  Confs e' lo stack utilizzato per analizzare le transizioni weak che partono da Confs(1).
   -------------------------------------------------------------------


   CurTable: Natural :=1;
   T1: access System_Configuration_Table := new  System_Configuration_Table(1..MaxBreadth);
   I1: Natural :=0;

   T2: access System_Configuration_Table := new System_Configuration_Table(1..MaxBreadth);
   I2: Natural :=0;

   ---------------------------------------------------------
   --  costrusce in modo breadth-first il modello tab delle tracce (rimuovendo le azioni tau)
   --  e troncando il modello appena viene superato il numero massimo di nodi indicati 
   --  (States_Limit)
   ---------------------------------------------------------
   procedure BreadthFirstExplore  is
     Has_Final, Has_Loop: Boolean := False;
     Depth : Natural := 0;
   begin
      if not Just_Count then
        if Remove_Tau_Transitions then
          Print_Dot_Header(NickName(Initial_Configuration, "W"));
        else
          Print_Dot_Header(NickName(Initial_Configuration, "C"));
        end if;
      end if;
     T1(1) := Initial_Configuration;
     I1 := 1;
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
         if Remove_Tau_Transitions then
           Has_Final := False;
           Has_Loop := False;
           CONFS(1) := T1(I);
           --  Weak_Explore_Configuration((1=>T1(I)),0,Has_Final,Has_Loop,I);
           Last_Weak_Explore(T1(I),T1(I));
         else
           New_Explore_Configuration((1=>T1(I)),0);
         end if;
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
         if Remove_Tau_Transitions then
           Has_Final := False;
           Has_Loop := False;
           CONFS(1) := T2(I);
           Last_Weak_Explore(T2(I),T2(I));
         else
           New_Explore_Configuration((1=>T2(I)),0);
         end if;
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
         if (Previuos_Count +1000 < SeenStates and then SeenStates < 20001 ) or else
            (Previuos_Count +2000 < SeenStates and then SeenStates < 40001 ) or else
            (Previuos_Count +5000 < SeenStates and then SeenStates < 100001 ) or else
            (Previuos_Count +10000 < SeenStates and then SeenStates < 200001 ) or else
            (Previuos_Count +20000 < SeenStates and then SeenStates < 400001 ) or else
            (Previuos_Count +50000 < SeenStates and then SeenStates > 400000 ) then
            Set_Output(Standard_Output);
            MyConfigurations.Kernel.Print_StatesSpace_Stats;
            Put_Line ("Depth = " & Integer'Image(Depth) &
                      " Width = " & Integer'Image(LastWidth) &
                      " Needed = " & Integer'Image(NeededStates));
           if Is_Open(DOTOUT) then Set_Output(DOTOUT); end if;
           Previuos_Count := SeenStates;
         end if;
       end if;
       --
     end loop;
     --
     if not Just_Count then
       Print_Dot_Footer;
     end if;
     --
   end BreadthFirstExplore;

   ---------------------------------------------------------
   -- if Depth mod2=1 adds Conf in T2
   -- else adds Conf in T1
   ---------------------------------------------------------
   procedure Add_New_Configuration (Conf: System_Configuration) is
   begin
      if Already_Seen(Progressive(Conf)) then return; end if;
      if Already_Booked(Progressive(Conf)) then return; end if;
      MarkBooked(Progressive(Conf));
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
      if OutputFileName /= null and then OutputFileName.all'Length >0 then
        declare
        begin
          Create(DOTOUT,Out_File,OutputFileName.all);
          Set_Output(DOTOUT);
          BreadthFirstExplore;
          Set_Output(Standard_Output);
          Close(DOTOUT);
          end;
      else
        BreadthFirstExplore;
      end if;
      --
      if Just_Count then
       Put_Line (Standard_Output, "-------------------------------------------");
       Put_Line (Standard_Output, "The system has" & Int64'Image(MyConfigurations.Kernel.StatesSpace_Size) & " states" );
       Put_Line (Standard_Output, "and " & Int64'Image(Transitions_Count) & " transitions" );
       Put_Line (Standard_Output, "-------------------------------------------");
      end if;
   exception
    when Event: others =>
      Put_Line (Current_Error, "Runtime Error in mc2dot worker!");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
   end worker;

begin
  Argc := 0;
  Beautify := False;
  Encode_State_Labels := False;
  -- in the case of FMC, slways show tau#channel(val) in place of just "tau"
  -- unless the dot output is going to be minimized (-l) by ltsconvert
  Expanded_Tau := True;
  loop
    Argc := Argc +1;
    if Argc > Ada.Command_Line.Argument_Count  then
       exit;
    end if;
    declare
      Input_Arg : String := Ada.Command_Line.Argument (Argc);
    begin
      if Input_Arg = "-c" then
           Just_Count := True;
      elsif Input_Arg = "-debug" then
         Debug := True;
      elsif Input_Arg = "-b" then
         Beautify := True;
--         Encode_State_Labels := False;
      elsif Input_Arg = "-a" then
         Use_Abstract_Labels := True;
         Use_Ground_Labels := False;
      elsif Input_Arg = "-g" then
         Use_Ground_Labels := True;
      elsif Input_Arg = "-f" then
         Product_Families := True;
      elsif Input_Arg = "-t" then
         -- if selected "() () {}" become tau  (and optimized away)
         -- "tau" transitions are fully removed.
         Remove_Tau_Transitions := True;  
      elsif Input_Arg = "-l" then
         Encode_State_Labels := True;
         Expanded_Tau := False;
--         Beautify := False;
      elsif Input_Arg = "-o" then
         SetOutput := True;
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
    Put_line(Current_Error, "mc2dot: ERROR incorrect parameters ...");
    Put_line(Current_Error, " usage: mc2dot [options] [inputmodelfile] ");
    Put_line(Current_Error, " options: "); 
    Put_line(Current_Error, "   -o outputfilename ");
    Put_line(Current_Error, "   -sN  (N= number  limit graph size to N nodes)");
    Put_line(Current_Error, "   -dN  (N= number  limit graph depth to N )");
    Put_line(Current_Error, "   -l (encode state labels inside edge labels)");
    Put_line(Current_Error, "   -t (remove tau transitions)");
    Put_line(Current_Error, "   -b (beautify #loop, #truncated, #final) - default");
    Put_line(Current_Error, "   -a (use abstract labels as edge labels)");
    Put_line(Current_Error, "   -g (use ground labels as edges lables - default)");
    Put_line(Current_Error, "   -f (handle may/must transition of product families)");
    Put_line(Current_Error, "   -c (just count the states, do not produce dot file)");
    Put_line(Current_Error, " examples: ");
    Put_line(Current_Error, " mc2dot -d50 model.umc -o model.dot");
    Put_line(Current_Error, " mc2dot -g -b    (visualization of concrete graph)");
    Put_line(Current_Error, " mc2dot -a       (visualization of rough abstract graph)");
    Put_line(Current_Error, " mc2dot -a -l -t (preparation for trace mininization)");
    Put_line(Current_Error, " mc2dot -a -l    (preparation for branching mininization)");

    return;
  end if;
  
  declare
    WW: Worker;
  begin
    null;
  end;
    --
end MC2dot;
