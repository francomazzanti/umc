separate (MC)
procedure BFEXplore is

  States_Limit: Integer := 16_000_000;         -- option "-s"
  NeededStates: Integer := 0; 
  Aborted: Boolean := False;
  LastWidth: Natural := 0;
  WDepth: Natural :=0;

  type System_Configuration_Table is array (Positive range <>) of System_Configuration;
  type System_Configuration_Table_Ref is access System_Configuration_Table;

  Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref;

   MaxBreadth: Natural := 1_000_000;  
   Slot_Size: Natural := Flags.Vectors_Max;   -- in flags.ads  == 4096
   type Seen_Table is array (Positive Range 1..Slot_Size) of Boolean;
   type Seen_Table_Ref is access Seen_Table;
   Seen: array(1..Slot_Size) of Seen_Table_Ref;   -- total 4096 * 4096 elems
   Booked: array(1..Slot_Size) of Seen_Table_Ref;   -- total 4096 * 4096 elems
   SeenStates: Int64 :=0;

   CurTable: Natural :=1;
   T1: access System_Configuration_Table := new  System_Configuration_Table(1..MaxBreadth);
   I1: Natural :=0;

   T2: access System_Configuration_Table := new System_Configuration_Table(1..MaxBreadth);
   I2: Natural :=0;

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

   procedure Add_New_Configuration (Conf: System_Configuration) is
   begin
      if Already_Seen(Progressive(Conf)) then return; end if;
      if Already_Booked(Progressive(Conf)) then return; end if;
      MarkBooked(Progressive(Conf));
      --
      if CurTable =1 then
         -- adds Conf in T1
         I1 := I1 +1;
         if I1 in T1'Range then
           T1(I1) := Conf;
         end if;
      else
         -- adds Conf in T2
         I2 := I2 +1;
         if I2 in T2'Range then
           T2(I2) := Conf;
         end if;
      end if;
   end Add_New_Configuration;

   procedure Explore_Configuration (
          Conf: System_Configuration) is
     This_Iter: Evolutions_Iterator;
   begin
     if Already_Seen(Progressive(Conf)) then return; end if;
     MarkSeen(Progressive(Conf));
     NeededStates := NeededStates +1;
     --
     Iterator_Initialize (This_Iter,Conf);
     while Has_System_Transition (This_Iter) loop
       declare
         This_Target: System_Configuration := Get_Target_Configuration (This_Iter);
       begin
         Add_New_Configuration(This_Target);
       end;
       Iterator_Advance(This_Iter);
     end loop;
     Iterator_Finalize(This_Iter);
     --
   end Explore_Configuration;

begin
     T1(1) := Initial_Configuration;
     I1 := 1;
     --
     while I1 /= 0 loop
       LastWidth := I1;
       CurTable := 2;
       I2 := 0;  -- prepare T2 for getting next round of items
       for I in 1..I1 loop
         if ALLDONE then return; end if;
         Explore_Configuration(T1(I));
       end loop;
       if I2 = 0 then exit; end if;
       if NeededStates > States_Limit then return; end if;
       --
       LastWidth := I2;
       CurTable := 1;
       I1 := 0;  -- prepare T1 for getting next round of items
       for I in 1..I2 loop
         if ALLDONE then return; end if;
         Explore_Configuration(T2(I));
       end loop;
       if I1 = 0 then exit; end if;
       if NeededStates > States_Limit then return; end if;
       --
     end loop;
end BFEXplore;
