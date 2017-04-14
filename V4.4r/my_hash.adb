with My_Atomic_Counters; use My_Atomic_Counters;
with Dyn_Store_Obj;
package body My_Hash is

  type Collision_Item;
  type Collision_Item_Ref is access Collision_Item 
     with Volatile;
  type Collision_Item is record
    Next: Collision_Item_Ref;
    TheItemData: Key_Data;
    TheItem: Element_Ref;
  end record
     with Volatile;

  Vectors_Max: constant Positive := 4096;
  Count: Natural :=0 with Atomic;
  Locks: Locks_Table(1..Vectors_Max) := (others => new Lock_Data);
  --
  package Collisions is new Dyn_Store_Obj(Collision_Item,Collision_Item_Ref);
  --
  function Current_Count return Natural  is
  begin
     return Count;
  end Current_Count;

 function CheckElement (KK: Key_Data; JUST_ADDED: out Boolean) return Element_Ref is
    Key: Natural :=0;
    LockKey: Natural :=0;
    Tmp: Collision_Item_Ref;
    Orig: Collision_Item_Ref;
    Found: Boolean;
    N: Element_Ref;
    NewCI: Collision_Item_Ref;
    ProgressNum: Natural;
  begin
    Key := Mk_Key(KK);
    LockKey := (Key-1) mod Vectors_Max +1;
    --
    if Thread_safe then
       SeizeLock(Locks(LockKey));
    end if;
    --
    -- Tmp e' una lista di frammenti associata alla chiave key.
    Tmp := Collisions.Retrieve(Key);
    Orig := Tmp;
    Found := False;
    while Tmp /= null and not Found loop
       -- per ogni indice nella lista, guardo se il corripondente elemento e' uguale a quello
       --  che sto' cercando di inserire.
       if not Matching(Tmp.TheItemData, KK) then
          Tmp := Tmp.Next;
       else
          Found := True;
          N := Tmp.TheItem;
          JUST_ADDED := False;
          exit;
       end if;
    end loop;
    --
    -- se la lista degli indici e' vuota aggiungi questo elemento nello store
    -- ed il suo puntatore nella lista
    if not Found then
      NewCI := new Collision_Item;
      NewCI.TheItem := Initial_Element(KK);
      NewCI.TheItemData := KK;
      NewCI.Next :=  Orig;
      N := NewCI.TheItem;
      Collisions.Store(NewCI,Key);
      ProgressNum := Natural(Increment32(Count'Address));
      Set_Progressive(ProgressNum,N);
      JUST_ADDED := True;
    end if;
    -- 
    if Thread_Safe then
      ReleaseLock(locks(LockKey));
    end if;
    -- 
    return N;
  end CheckElement;

begin
  null;
end;
