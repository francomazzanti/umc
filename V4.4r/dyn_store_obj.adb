with My_Atomic_Counters; use My_Atomic_Counters;
--with SYSTEM; use System;
--with System.Atomic_Primitives; use System.Atomic_Primitives;
with Ada.Text_IO; use ADa.Text_IO;
with Ada.Unchecked_Deallocation;
package body Dyn_Store_Obj is

--    generic
--    type Element is private;
--    type Element_Ref is access Element;
--    ThreadSafe: Boolean := True;
--    Vectors_Max: Positive := 4096;
--    Table_Max: Positive := 4096 * 4096;
--    package Dyn_Store_Obj is
--      function Retrieve (NickNum: Positive) return Element_Ref;
--        procedure Store (Item: Element_Ref; NickNum: Positive);
--      procedure ReInitialize_DB(Deep: Boolean := True);
--    end Dyn_Store_Obj;


  Locks: array (1..Vectors_Max) of Lock_Ref := (others=> new Lock_Data);

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
-- All_Configurations:   |--|--|--|     Root_Array
--                        |
--                       |--|--|--|--|--|--|--|--|..  4096 Vectors_Table
--                        |
--                       |--|--|--|--|--|--|--|--|..  4096 Elements_Vector

 type Elements_Vector is
      array (Positive range 1..Vectors_Max) of Element_Ref
        with Atomic_Components;
-- pragma Volatile_Components(Elements_Vector);
 type Elements_Vector_Ref  is access Elements_Vector;
 pragma Volatile(Elements_Vector_Ref);
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
 procedure Free is new
       Ada.Unchecked_Deallocation (Elements_Vector, Elements_Vector_Ref);
 procedure Free is new
       Ada.Unchecked_Deallocation (Vectors_Table, Vectors_Table_Ref);
 procedure Free is new
       Ada.Unchecked_Deallocation (Root_Array, Root_Ref);
 procedure Free is new
       Ada.Unchecked_Deallocation (Element, Element_Ref);

  procedure ReInitialize_DB(Deep: Boolean := True) is
  begin
    if All_Configurations  /= null then
      -- Free all element vectors
      for I in All_Configurations.all'Range loop
        if All_Configurations(I) /= null then
          for J in All_Configurations(I).all'Range loop
             -- maybe call user defined free of All_Configurations(I)(J)(K), for all K
             if Deep then
              for K in All_Configurations(I)(J).all'Range loop
                if All_Configurations(I)(J)(K) /= null then
                   Free(All_Configurations(I)(J)(K));
                end if;
              end loop;
             end if;
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
 procedure Store (Item: Element_Ref; NickNum: Positive) is
     Root_Index : Positive;
     Vector_Index: Positive;
     Position_Index: Positive;
     Newref: Element_Ref;
  begin
    Root_Index :=
        ((abs(NickNum)-1) / Integer(Table_Max)) +1;
    Position_Index :=
        (((abs(NickNum)-1) mod Integer(Table_Max)) ) / Vectors_Max +1;
    Vector_Index :=
        ((abs(NickNum)-1) mod Integer(Vectors_Max)) +1;
    --
    Newref := Item;
    if ThreadSafe then
      Seize(Vector_Index);
    end if;
    if All_Configurations(Root_Index)(Vector_Index) = null then
       All_Configurations(Root_Index)(Vector_Index) := new Elements_Vector;
    end if;
    All_Configurations(Root_index)(Vector_Index)(Position_Index) := Newref;
    if ThreadSafe then
      Release(Vector_Index);
    end if;
  end Store;
  --
  --
  function Retrieve (NickNum: Positive) return Element_Ref is
    Root_Index : Positive;
    Vector_Index: Positive;
    Position_Index: Positive;
    Tmp:  Elements_Vector_Ref;
  begin
    --  added to avoid runtime errors
    if NickNum <=0 then
     return null;
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
       return null;
    end if;
    --
    if ThreadSafe then
      Seize(Vector_Index);
      Tmp := All_Configurations(Root_Index)(Vector_Index);
      if Tmp = null then
        Release(Vector_Index);
        return null;
      elsif Tmp(Position_Index) = null then
        Release(Vector_Index);
        return null;
      else
        Release(Vector_Index);
        return Tmp(Position_Index);
      end if;
    else
        Tmp := All_Configurations(Root_Index)(Vector_Index);
        if Tmp = null then
          return null;
        elsif Tmp(Position_Index) = null then
          return null;
        else
          return Tmp(Position_Index);
        end if;
    end if;
    --
  end Retrieve;
  --
begin
    All_Configurations := new Root_Array(1..256);
    All_Configurations.all := (others => new Vectors_Table);
end Dyn_Store_Obj;

