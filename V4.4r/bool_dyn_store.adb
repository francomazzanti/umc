--with AllSemaphores; use AllSemaphores;
with Ada.Unchecked_Deallocation;
package body Bool_Dyn_Store is
 ---------------------------------------------------------
 -- The structure is orginized as a dynamically growable vector
--   whose elements are Vectors_Max items fo 32bits words.
--  Each bit of the 32bits word stores the status of an element
-- whose index is:
--   (vector_index-1) * (Vector_Max-1) * 32
--    + (Vector_Max-1) * 32
--    + Bit_index
 ---------------------------------------------------------
 --  packed vector of booleans
 type  Bools32 is array (Positive range 1..32) of Boolean;
 for Bools32'Component_Size use 1;
 
 Empty32: constant Bools32 := (others => False);
 SlotChunk: constant Natural := 32*Vectors_Max;  -- 4096*32
 Initial_Chunks: constant Natural := 4096;   -- 4096*4096*32 elements 
 --
 type States_Vector is array (Positive range 1..Vectors_Max) of Bools32;
 pragma Volatile_Components(States_Vector);
 type States_Vector_Ref is access States_Vector;
 pragma Volatile(States_Vector_Ref);
 type State_Vectors_Table is array (Positive range <>) of States_Vector_Ref;
 pragma Volatile_Components(State_Vectors_Table);
 type State_Vectors_Table_Ref is access State_Vectors_Table;
 pragma Volatile(State_Vectors_Table_Ref);
 --
 procedure Free is new Ada.Unchecked_Deallocation(States_Vector,States_Vector_Ref);

 All_Configurations: State_Vectors_Table_Ref :=
     new State_Vectors_Table(1..Initial_Chunks);
 pragma Volatile(All_Configurations);
 --
-------------------------------------
  --
  procedure Initialize_DB is
  begin
    if All_Configurations  /= null then
      -- Free all structures
      for I in All_Configurations.all'Range loop
        if All_Configurations(I) /= null then
          Free(All_Configurations(I));
        end if;
      end loop;
    end if;
  end Initialize_DB;
  --
 procedure Store (Item: Boolean; NickNum: Integer) is
     Vector_Index: Positive;
     Position_Index: Positive;
     Bit_Index: Positive;
  begin
    Vector_Index :=
        Positive (((abs(NickNum)-1) / Integer(SlotChunk)) +1) ;
    Position_Index :=
        Positive (((abs(NickNum)-1) mod Integer(SlotChunk))/32 +1);
    Bit_Index :=
        Positive (((abs(NickNum)-1) mod 32) +1);
    --
--        AllSemaphores.Seize(Vector_Index);
    --
    -- if necessary expand the matrix
    --
    if Vector_Index > All_Configurations.all'Length  then
      All_Configurations :=
           new State_Vectors_Table'( All_Configurations.all &
			    (1..1000+Vector_Index-All_Configurations.all'Length => null));
    end if;
    --
    -- if necessary, allocate e new column
    --
    if All_Configurations(Vector_Index) = null then
       All_Configurations(Vector_Index) := new States_Vector'(others => Empty32);
    end if;
    --
    All_Configurations(Vector_Index)(Position_Index)(Bit_Index) := Item;
--    AllSemaphores.Release(Vector_Index);
    --
  end Store;
  --
  --
  function Retrieve (NickNum: Integer) return Boolean is
    Vector_Index: Positive;
    Position_Index: Positive;
    Bit_Index: Positive;
    Tmp:  States_Vector_Ref;
  begin
    --  added to avoid runtime errors
    if NickNum <=0 then
     return False;
    end if;
    --
    Vector_Index :=
        Positive (((abs(NickNum)-1) / Integer(SlotChunk)) +1) ; 
    Position_Index :=
        Positive (((abs(NickNum)-1) mod Integer(SlotChunk))/32 +1);
    Bit_Index :=
        Positive (((abs(NickNum)-1) mod 32) +1);
    --
--    AllSemaphores.Seize(Vector_Index);
    --
    if Vector_Index <= All_Configurations.all'Length then
      Tmp := All_Configurations(Vector_Index);
      if Tmp = null then
--        AllSemaphores.Release(Vector_Index);
        return False;
      else
--        AllSemaphores.Release(Vector_Index);
        return Tmp(Position_Index)(Bit_Index);
      end if;
    else
--        AllSemaphores.Release(Vector_Index);
      return False;
    end if;
  end Retrieve;
  --
end Bool_Dyn_Store;

