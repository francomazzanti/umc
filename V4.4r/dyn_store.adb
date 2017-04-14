with Flags;  use Flags;
with My_Atomic_Counters; use My_Atomic_Counters;
with Ada.Text_IO; use ADa.Text_IO;
with Ada.Unchecked_Deallocation;
package body Dyn_Store is

  Vectors_Max: constant Positive := 4096;
  Table_Max: constant Positive := 4096 * 4096;

  Locks: array (1..Vectors_Max) of Lock_ref :=(others=> new Lock_Data);

  procedure Seize(K: Positive) is
  begin
     SeizeLock(Locks(((K-1) mod Vectors_Max)+1));
  end Seize;
  pragma Inline(Seize);

  procedure Release(K: Positive) is
  begin
    ReleaseLock(Locks(((K-1) mod Vectors_Max)+1));
  end Release;
  pragma Inline(Release);


 --
 -- Nota: il fatto di utlizzare un access type ha in vantaggio di poter
 --  accettare come elements anche tipi unconstrained, il vantaggio di
 -- poter agevolmente distinguere elementi undefined da elementi defined.
--
--  BEWARE: MEMORY ALLOCATED INISIDE ELEMENT ITEMS IS NEVER FREED 
--  WE SHOULD ADD a FREE FUNCTION ARGUMEMT
--  MEMORY ACCESS NOT PROTECTED BY CONCURRENT OPERATION
--
--    generic
--    type Element is private;
--    with function Default_Initial return Element;
--    package Def_Dyn_Store is
--      Vectors_Max : Natural := 4096;   --  slots of 4096 elements 
--      function Retrieve (NickNum: Integer) return Element;
--      procedure Store (Item: Element; NickNum: Integer);
--      procedure ReInitialize_DB;
--    end Def_Dyn_Store;
--
-- All_Configurations:   |--|--|--|     Root_Array
--                        |
--                       |--|--|--|--|--|--|--|--|..  4096 Vectors_Table
--                        |
--                       |--|--|--|--|--|--|--|--|..  4096 Elements_Vector
 type Element_Ref is access Element;
 pragma Volatile(Element_Ref);
 type Elements_Vector is
      array (Positive range 1..Vectors_Max) of Element_Ref;
 pragma Volatile_Components(Elements_Vector);
 type Elements_Vector_Ref  is access Elements_Vector
   with Atomic;
-- pragma Volatile(Elements_Vector_Ref);
 --
 type Vectors_Table is
      array (Positive range 1..Vectors_Max) of Elements_Vector_Ref;
 pragma Volatile_Components(Vectors_Table);
 type Vectors_Table_Ref is access Vectors_Table;
 pragma Volatile(Vectors_Table_Ref);
 --
 type Root_Array is array (Positive range <>) of Vectors_Table_Ref;
 pragma Volatile_Components(Root_Array);
 type Root_Ref is access Root_Array; 
 Pragma Volatile(Root_Ref);

 All_Configurations: Root_Ref;
 pragma Volatile(All_Configurations);
 --
 procedure Free is new Ada.Unchecked_Deallocation (Element, Element_Ref);
 procedure Free is new
       Ada.Unchecked_Deallocation (Elements_Vector, Elements_Vector_Ref);
 procedure Free is new
       Ada.Unchecked_Deallocation (Vectors_Table, Vectors_Table_Ref);
 procedure Free is new
       Ada.Unchecked_Deallocation (Root_Array, Root_Ref);

  procedure ReInitialize_DB is
  begin
    if All_Configurations  /= null then
      -- Free all element vectors
      for I in All_Configurations.all'Range loop
        if All_Configurations(I) /= null then
          for J in All_Configurations(I).all'Range loop
             -- maybe call user defined free of All_Configurations(I)(J)(K), for all K
             Free (All_Configurations(I)(J));
          end loop;
          Free(All_Configurations(I));
        end if;
      end loop;
      Free(All_Configurations);
    end if;
    All_Configurations := new Root_Array(1..256);
    All_Configurations.all := (others => new Vectors_Table);
  end ReInitialize_DB;
  --
 procedure Store (Item: Element; NickNum: Positive) is
     Root_Index : Positive;
     Vector_Index: Positive;
     Position_Index: Positive;
     Oldref: Element_Ref;
     Newref: Element_Ref;
  begin
    Root_Index :=
        ((abs(NickNum)-1) / Integer(Table_Max)) +1;
    Position_Index :=
        (((abs(NickNum)-1) mod Integer(Table_Max)) ) / Vectors_Max +1;
    Vector_Index :=
        ((abs(NickNum)-1) mod Integer(Vectors_Max)) +1;
    --
    Newref := new Element'(Item);
    if LocalThreadSafe and ThreadSafe then
      Seize(Vector_Index);
    end if;
    if All_Configurations(Root_Index)(Vector_Index) = null then
       All_Configurations(Root_Index)(Vector_Index) := new Elements_Vector;
    end if;
    Oldref := All_Configurations(Root_Index)(Vector_Index)(Position_Index);
    All_Configurations(Root_index)(Vector_Index)(Position_Index) := Newref;
    if LocalThreadSafe and ThreadSafe then
      Release(Vector_Index);
    end if;
    --
    -- if necessary, free the previous element
    --
    if OldRef /= null then
       Free(OldRef);
    end if;
  end Store;
  --
  --
  function Retrieve (NickNum: Positive) return Element is
    Root_Index : Positive;
    Vector_Index: Positive;
    Position_Index: Positive;
    Tmp:  Elements_Vector_Ref;
  begin
    --  added to avoid runtime errors
    if NickNum <=0 then
     return Default_Initial;
    end if;
    --
    Root_Index :=
        Positive (((abs(NickNum)-1) / Integer(Table_Max)) +1) ;
    Position_Index :=
        (((abs(NickNum)-1) mod Integer(Table_Max)) ) / Vectors_Max +1;
    Vector_Index :=
        Positive (((abs(NickNum)-1) mod Integer(Vectors_Max)) +1);
    --
    if Root_Index > All_Configurations.all'Length  then
       return Default_Initial;
    end if;
    --
    if LocalThreadSafe and ThreadSafe then
      Seize(Vector_Index);
      Tmp := All_Configurations(Root_Index)(Vector_Index);
      if Tmp = null then
        Release(Vector_Index);
        return Default_Initial;
      elsif Tmp(Position_Index) = null then
        Release(Vector_Index);
        return Default_Initial;
      else
        declare
          Res: Element := Tmp(Position_Index).all;
        begin
          Release(Vector_Index);
          return Res;
        end;
      end if;
    else
        Tmp := All_Configurations(Root_Index)(Vector_Index);
        if Tmp = null then
          return Default_Initial;
        elsif Tmp(Position_Index) = null then
          return Default_Initial;
        else
          return Tmp(Position_Index).all;
        end if;
    end if;
    --
  end Retrieve;
  --
begin
    All_Configurations := new Root_Array(1..256);
    All_Configurations.all := (others => new Vectors_Table);
end Dyn_Store;

