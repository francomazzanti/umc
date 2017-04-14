separate (MC)
procedure DFLEXplore is

  type ITref is access all Evolutions_Iterator;

  States_Limit: Integer := 16_000_000;         -- option "-s"
  NeededStates: Integer := 0; 
  Aborted: Boolean := False;
  type System_Configuration_Table is array (Positive range <>) of System_Configuration;
  type System_Configuration_Table_Ref is access System_Configuration_Table;

   Slot_Size: Natural := Flags.Vectors_Max;   -- in flags.ads  == 4096
   type Seen_Table is array (Positive Range 1..Slot_Size) of Boolean;
   type Seen_Table_Ref is access Seen_Table;
   Seen: array(1..Slot_Size) of Seen_Table_Ref;   -- total 4096 * 4096 elems
   SeenStates: Int64 :=0;

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

   procedure Tail_Scan(This_Iter: ITref);

   -- THIS VERSION OF DEPTH FIRST GENERATION USES TAIL_SCAN
   -- IN ORDER TO TRAVERSE THE GRAPH IN THE OPPOSITE (DEPTH_FIRST)
   -- WAY WITH RESPECT THE ORDER USED BY THE CLASSICAL EVALUATION
   -- In this way we reduce the clashes on the same nodes
   --
   procedure DFLExplore (Conf: System_Configuration) is
     This_Iter: aliased Evolutions_Iterator;
   begin
     if ALLDONE then return; end if;
     if NeededStates > States_Limit then return; end if;
     if Already_Seen(Progressive(Conf)) then return; end if;
     MarkSeen(Progressive(Conf));
     NeededStates := NeededStates +1;
     --
     Iterator_Initialize (This_Iter,Conf);
     Tail_Scan(This_Iter'Unchecked_Access); 
     Iterator_Finalize(This_Iter);
     --
   end DFLExplore;

   procedure Tail_Scan(This_Iter: ITref) is
       This_Target: System_Configuration;
   begin
     if Has_System_Transition (This_Iter.all)  then
       This_Target := Get_Target_Configuration (This_Iter.all);
       Iterator_Advance (This_Iter.all);
       Tail_Scan(This_Iter);
       DFLExplore(This_Target);
     end if;
   end Tail_Scan;

begin
     DFLExplore(Initial_Configuration);
end DFLEXplore;
