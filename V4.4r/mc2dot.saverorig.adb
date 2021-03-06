with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line;
with Configurations; 
with Flags; use Flags;
with DotLib; use DotLib;
procedure MC2dot is
-- --------------------------------------------------------------------------
--   -debug  Debiug Mode, prints stats into
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
  States_Limit: Integer := 2_000_000;         -- option "-s"
  Depth_Limit: Integer := 500_000;            -- option "-d"
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
  type System_Configuration_Table is array (Natural range <>) of System_Configuration;

  ----------------------------------------------------------------------------
  Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref;
  ---------------------------------------------------------------------------

--   States: natural :=0;
--   Edges: natural :=0;
--   Transitions_Max: Natural := 1_000_000;
   ------------------------------------------------------------------------------
   MaxBreadth: Natural := 300000;     -- size of the two System_Configuration_Tables used)
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
     SourceLabels:  String :=
       "{" & Display_AbstractStateLabels(Get_Abstract_State_Labels(Confs(1))) & "}";

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

   ---------------------------------------------------------------------------------
   type Traversed_Table is array (Positive Range 1..Slot_Size) of Natural;
   type Traversed_Table_Ref is access Traversed_Table;
   Traversed: array(1..Slot_Size) of Traversed_Table_Ref;
   ---------------------------------------------------------------------------------

   ------------------------------------------------------------------------
   --  Durante l'analisi delle evoluzioni tau partenti da un nodo "from"
   --   vengono segnati "traversed(from)" tutti i nodi intermedi osservati
   -- (che spraranno)
   ------------------------------------------------------------------------
   procedure MarkTraversed(From: natural; N: Positive) is
      Slot: Positive := ((N-1) / Slot_Size) +1;
      Index: Positive := ((N-1) mod Slot_Size) +1;
   begin
      if Traversed(Slot) = null then
         Traversed(Slot) := new Traversed_Table;
         Traversed(Slot).all := (others => 0);
      end if;
      Traversed(Slot)(Index) := From;
   end MarkTraversed;

   ------------------------------------------------------------------------
   --  returns  Traversed(N)=From
   ------------------------------------------------------------------------
   function Already_Traversed(From: Natural; N: Positive) return Boolean is
     Slot: Positive:= ((N-1) / Slot_Size) +1;
     Index: Positive := ((N-1) mod Slot_Size) +1;
   begin
      if Traversed(Slot) = null then
         Traversed(Slot) := new Traversed_Table;
         Traversed(Slot).all := (others => 0);
         return False;
      else
         return Traversed(Slot)(Index) = From;
      end if;
   end Already_Traversed;


   -------------------------------------------------------------------
   -- chiamata inizialmente come 
   --   Weak_Explore_Configuration((1=>T1(I)),0,FinalFalse,LoopFalse,I) 
   --  e successivamente in modo rocorsivo come 
   --   Weak_Explore_Configuration(Confs&This_Target, Depth+1,Has_Final,Has_Loop,Index);
   --   se la configurazione currente Conf(ulima) e' segnata Traversed, si ritorna subito.
   -- segna la configurazione corrente (l'ultima delle vettore delle conf) come "Traversed"
   -- se Conf'Length=1 stampa la descrizione del nodo corrente = Conf(1)
   -- Poi vengono analizzate le evoluzione dello stato corrente (l'ultimo del vettore):
   --  se si trova una transizione NON_TAU  il nodo target viene aggiunto con Add_New_Configuration
   --   e viene stampato un edge  dal nodo Conf(1) al nodo Conf(ultimo)
   -- se si trova una transizione tau che porta ad uno stato target 
   --    si guarda se il target e' un loop back ad uno dei nodi Conf(1..ultimo)) e se si si setta Has_Loop
   --    se il target non e' un loop back si chiama di nuovo
   --    Weak_Explore_Configuration(Confs & This_Target, Depth+1, Has_Final, Has_Loop,Index)
   -------------------------------------------------------------------
   -- IL PROBLEMA DI QUESTA RICORSIONE E' l'ELEVATO DELLO STACK
   -- PROVIAMO AD USARE UNN VETTORE CONDIVISO
   CONFS: access System_Configuration_Table := new System_Configuration_Table(1..10_000);
   WSTEPS: Natural :=0;
   PSTEPS: Natural :=0;
   procedure Weak_Explore_Configuration (
--          Confs: System_Configuration_Table;
          Ultimo: Natural; --- NEW
          Depth: Natural;
          Has_Final: in out Boolean;
          Has_Loop: in out Boolean;
          Index: Natural) is
     --
     CURR_ITEM: Natural;
     This_Iter: Evolutions_Iterator;
     This_Target: System_Configuration;
     To, From: Natural;
     SourceLabels:  String :=
       "{" & Display_AbstractStateLabels(Get_Abstract_State_Labels(Confs(1))) & "}";
   begin
     --
     From := Progressive(Confs(1));
--     CURR_ITEM := Progressive(Confs(Confs'Last));
     CURR_ITEM := Progressive(Confs(Ultimo));
     --
     -- exit if state already analysed
--     if (Confs'Length=1 and then Already_Seen(From)) or Aborted then
     if (Ultimo=1 and then Already_Seen(From)) or Aborted then
         return;
     end if;
     --
--     if Confs'Length=1 then
     if Ultimo=1 then
        MarkSeen(From);
        NeededStates := NeededStates +1;
     end if;
     --
     if truncated  and not Just_Count then
       Print_Dot_Truncated_Node(NickName(Confs(1), "W"),SourceLabels);
       return;
     end if;
     --
--     if (Confs'Length>1 and then
     if (Ultimo > 1 and then
          Already_Traversed(From,CURR_ITEM)) or Aborted then
         return;
     end if;
     --
--     if Confs'Length>1 then
     if Ultimo > 1 then
        MarkTraversed(From,CURR_ITEM);
     end if;
     --
       if Just_Count or (OutputFileName=null or else  OutputFileName'Length >0) then
         -- SeenStates := MyConfigurations.Kernel.StatesSpace_Size; -- numero di stati generati finora
         WSTEPS := WSTEPS+1;
         if (PSTEPS +1000 < WSTEPS and then WSTEPS < 20_001 ) or else
            (PSTEPS +2000 < WSTEPS and then WSTEPS < 40_001 ) or else
            (PSTEPS +5000 < WSTEPS and then WSTEPS < 100_001 ) or else
            (PSTEPS +10_000 < WSTEPS and then WSTEPS < 200_001 ) or else
            (PSTEPS +20_000 < WSTEPS and then WSTEPS > 400_001 ) then
            Set_Output(Standard_Output);
            MyConfigurations.Kernel.Print_StatesSpace_Stats;
            Put_Line ("Depth = " & Integer'Image(WDepth) &
                      " Width = " & Integer'Image(Index) & "/" & Integer'Image(LastWidth));
           if Is_Open(DOTOUT) then Set_Output(DOTOUT); end if;
           PSTEPS := WSTEPS;
         end if;
       end if;
     --
--     if Confs'Length=1 and not Just_Count then
     if Ultimo =1 and not Just_Count then
       Print_Dot_Node(NickName(Confs(1), "W"), SourceLabels);
     end if;
     --
     -- INIZIAMO AD ANALIZZARE LE EVOLUZIONI
     --
--     Iterator_Initialize (This_Iter, Confs(Confs'Last));
     Iterator_Initialize (This_Iter, Confs(Ultimo));
     --
     if not Has_System_Transition (This_Iter) then
        Has_Final := True;
     end if;
     --
     while Has_System_Transition (This_Iter) loop
       --
       This_Target := Get_Target_Configuration (This_Iter);
       --
       declare
         TargetLabels: String :=
            "{" & Display_AbstractStateLabels(
               Get_Abstract_State_Labels(Get_Target_Configuration(This_Iter))) & "}";
         ActionLabels: String := Mk_Edge_Label(This_Iter);
         This_Final:Boolean;
         This_Loop:Boolean;
       begin
         --
         if SourceLabels = TargetLabels and then ActionLabels = "{}" then
           --
            -- evolution is (xx)(xx){}   == TAU
           --
           To := Progressive(Get_Target_Configuration(This_Iter));
           -- check for loops
           This_Loop := False;
--           for I in Confs'Range loop
           for I in 1..Ultimo loop
             if To = Progressive(Confs(I)) then
                This_Loop := True;
                Has_Loop := True;
                exit;
             end if;
           end loop;
           --
           if not This_Loop then
             -- IL PROBLEMA DI QUESTA RICORSIONE E' l'ELEVATO DELLO STACK
--             Weak_Explore_Configuration(Confs & This_Target, Depth+1, Has_Final, Has_Loop,Index);
             Confs(Ultimo+1) := This_Target;
             Weak_Explore_Configuration(Ultimo+1, Depth+1, Has_Final, Has_Loop,Index);
           end if;
           --
         else
           --
           --  evolution is (xx)(yy){}    == NONTAU
           --  evolution is (..)(..){zzz} == NONTAU
           --
           Transitions_Count := Transitions_Count +1;
           if not Just_Count then
             Print_Dot_Edge(NickName(Confs(1),"W"), 
                          SourceLabels, TargetLabels, -- needed for "tau"
                          ActionLabels,
                          NickName(This_Target, "W"));
           end if;
           NeededEdges :=  NeededEdges +1;
           This_Final := False;
           This_Loop := False;
           --
           -- invece di continuare ricorsivamente mette target nella lista di cose da fare
           --
           Add_New_Configuration(This_Target);
           --
           --
         end if;
       end;
       --
       Iterator_Advance(This_Iter);
     end loop;
     Iterator_Finalize(This_Iter);
     --
--     if (not Just_Count) and Confs'Length=1 and Has_Final then
     if (not Just_Count) and Ultimo=1 and Has_Final then
        -- add #final transition
        Print_Dot_Final(NickName(Confs(1),"W"), SourceLabels);
     end if;
--     if (not Just_Count) and Confs'Length=1 and Has_Loop then
     if (not Just_Count) and Ultimo =1 and Has_Loop then
        -- add #loop transition
        Print_Dot_Loop(NickName(Confs(1),"W"),SourceLabels);
     end if;
     --
   end Weak_Explore_Configuration;


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
--           Weak_Explore_Configuration((1=>T1(I)),0,Has_Final,Has_Loop,I);
           CONFS(1) := T1(I);
           Weak_Explore_Configuration(1,0,Has_Final,Has_Loop,I);
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
--           Weak_Explore_Configuration((1=>T2(I)),0,Has_Final,Has_Loop,I);
           CONFS(1) := T2(I);
           Weak_Explore_Configuration(1,0,Has_Final,Has_Loop,I);
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
                      " Width = " & Integer'Image(LastWidth));
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
    --
    Load_Model(Model_FileName.all);
    --
    if OutputFileName /= null and then OutputFileName.all'Length >0 then
      declare
--        DOTOUT: File_Type;
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
    --
  end MC2dot;
