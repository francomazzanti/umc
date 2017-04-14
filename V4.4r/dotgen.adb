with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line;
--with Configurations; 
with Flags; use Flags;
with DotLib; use DotLib;
package body DOTgen is
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
--  MODEV INTO DOTGEN package spec
--  RootState: Integer :=1;
--  States_Limit: Integer := 2_000_000;         -- option "-s"
--  Depth_Limit: Integer := 500_000;            -- option "-d"
--  SetOutput: Boolean := False;                -- option "-o"
--  OutputFileName: access String;
--  Use_Ground_Labels: Boolean := True;         -- option "-g"
--  Use_Abstract_Labels: Boolean := False;      -- option "-a"
--  Model_FileName: access String;
--  ---
--  Model_OK : Boolean := False;
--  Argc: Natural;
--  Just_Count: Boolean :=False;
--  Aborted: Boolean := False;

--  ----------------------------------------------------------------------------
--  package MyConfigurations is new Configurations;
   use MyConfigurations;
   use MyConfigurations.Kernel;
   ----------------------------------------------------------------------------
   type System_Configuration_Table is array (Natural range <>) of System_Configuration;
 
   ----------------------------------------------------------------------------
   Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref;
   ---------------------------------------------------------------------------
 
   ------------------------------------------------------------------------------
   MaxBreadth: Natural := 100000;     -- size of the two System_Configuration_Tables used)
   Slot_Size: Natural := Vectors_Max;   -- in flags.ads  == 2048
   type Seen_Table is array (Positive Range 1..Slot_Size) of Boolean;
   type Seen_Table_Ref is access Seen_Table;
   Seen: array(1..Slot_Size) of Seen_Table_Ref;   -- total 2048 * 2048 elems
   SeenStates: Int64 :=0;
   ------------------------------------------------------------------------------
   DOTOUT: File_Type;

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
      if Seen(Slot) = null then
         Seen(Slot) := new Seen_Table;
         Seen(Slot).all := (others => False);
         return False;
      else
         return Seen(Slot)(Index);
      end if;
   end Already_Seen;

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


   procedure Weak_Explore_Configuration (
          Confs: System_Configuration_Table;
          Depth: Natural;
          Has_Final: in out Boolean;
          Has_Loop: in out Boolean) is
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
     CURR_ITEM := Progressive(Confs(Confs'Last));
     --
     -- exit if state already analysed
     if (Confs'Length=1 and then Already_Seen(From)) or Aborted then
         return;
     end if;
     --
     if Confs'Length=1 then
        MarkSeen(From);
        NeededStates := NeededStates +1;
     end if;
     --
     if truncated  and not Just_Count then
       Print_Dot_Truncated_Node(NickName(Confs(1), "W"),SourceLabels);
       return;
     end if;
     --
     if (Confs'Length>1 and then
          Already_Traversed(From,CURR_ITEM)) or Aborted then
         return;
     end if;
     --
     if Confs'Length>1 then
        MarkTraversed(From,CURR_ITEM);
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
            Set_Output(DOTOUT);
           Previuos_Count := SeenStates;
         end if;
       end if;
     --
     if Confs'Length=1 and not Just_Count then
       Print_Dot_Node(NickName(Confs(1), "W"), SourceLabels);
     end if;
     --
     -- INIZIAMO AD ANALIZZARE LE EVOLUZIONI
     --
     Iterator_Initialize (This_Iter, Confs(Confs'Last));
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
           for I in Confs'Range loop
             if To = Progressive(Confs(I)) then
                This_Loop := True;
                Has_Loop := True;
                exit;
             end if;
           end loop;
           --
           if not This_Loop then
             Weak_Explore_Configuration(Confs & This_Target, Depth+1, Has_Final, Has_Loop);
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
     if (not Just_Count) and Confs'Length=1 and Has_Final then
        -- add #final transition
        Print_Dot_Final(NickName(Confs(1),"W"), SourceLabels);
     end if;
     if (not Just_Count) and Confs'Length=1 and Has_Loop then
        -- add #loop transition
        Print_Dot_Loop(NickName(Confs(1),"W"),SourceLabels);
     end if;
     --
   end Weak_Explore_Configuration;


   CurTable: Natural :=1;
   T1: System_Configuration_Table(1..MaxBreadth);
   I1: Natural :=0;

   T2: System_Configuration_Table(1..MaxBreadth);
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
     T1(1) := Initial_Configuration;
     T1(1) := System_Configuration(RootState);
      if not Just_Count then
        if Remove_Tau_Transitions then
          Print_Dot_Header(NickName(T1(1), "W"));
        else
          Print_Dot_Header(NickName(T1(1), "C"));
        end if;
      end if;
     I1 := 1;
     --
     while I1 /= 0 loop
       CurTable := 2;
       I2 := 0;  -- prepare T2 for getting next round of items
       Depth := Depth+1;
       if Depth > Depth_Limit then
         truncated := True;
       end if;
       for I in 1..I1 loop
         if Remove_Tau_Transitions then
           Has_Final := False;
           Has_Loop := False;
           Weak_Explore_Configuration((1=>T1(I)),0,Has_Final,Has_Loop);
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
       CurTable := 1;
       I1 := 0;  -- prepare T1 for getting next round of items
       Depth := Depth+1;
       if Depth > Depth_Limit then
         truncated := True;
       end if;
       for I in 1..I2 loop
         if Remove_Tau_Transitions then
           Has_Final := False;
           Has_Loop := False;
           Weak_Explore_Configuration((1=>T2(I)),0,Has_Final,Has_Loop);
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
            Set_Output(DOTOUT);
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
    --
    if OutputFileName /= null and then OutputFileName.all'Length >0 then
      declare
      begin
        Create(DOTOUT,Out_File,OutputFileName.all);
        Set_Output(DOTOUT);
        BreadthFirstExplore;
        Set_Output(Standard_Output);
        Put_Line(DOTOUT, "//done");
        Close(DOTOUT);
      end;
    else
      BreadthFirstExplore;
    end if;
    --
    if Just_Count then
     Put_Line (Standard_Output, "-------------------------------------------");
     Put_Line (Standard_Output, 
            "The system has" & Int64'Image(MyConfigurations.Kernel.StatesSpace_Size) & " states" );
     Put_Line (Standard_Output, "and " & Int64'Image(Transitions_Count) & " transitions" );
     Put_Line (Standard_Output, "-------------------------------------------");
    end if;
    --
  end MC2dot;

end DOTgen;
