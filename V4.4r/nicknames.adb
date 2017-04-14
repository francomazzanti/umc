with My_Atomic_Counters; use My_Atomic_Counters;
with SYSTEM; use System;
--with System.Atomic_Primitives; use System.Atomic_Primitives;
with Ada.Unchecked_Deallocation;
with Dyn_Store;
with Ada.text_IO; use Ada.Text_IO;
package body NickNames is

  Vectors_Max: constant Positive := 4096;
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

  type HashK_Item;
  type HashK_Item_Ref is access HashK_Item;
  pragma Atomic(HashK_Item_Ref);

  type HashK_Item is record
    Next: HashK_Item_Ref;
    NickNum: Integer;
  end record;
  pragma Volatile(HashK_Item);

  Null_HashK_Item: HashK_Item_Ref :=null;
  function Null_Item return  HashK_Item_Ref is
  begin
    return null;
  end Null_Item;
  --
  procedure Free is new Ada.Unchecked_Deallocation (HashK_Item, HashK_Item_Ref);
  --
  Configurations_Count: Integer := 0;  -- max 2**31 -1
  Atomic_CC: Counter32 :=0;   -- mod 2**32
  pragma Atomic(Atomic_CC);
  --
  ----------------------------------------------------------------------------
  --  Nicknames fa uso di 2 vettori:
  -- Elements_Store che contiene gli elementi effettivi memorizzati nell'archivio,
  -- Hash_Table che contiene liste di indici di elementi con medesima chiave
  ----------------------------------------------------------------------------
  package Elements_Store is new Dyn_Store(Element, Default_Initial);
  package Hash_Table is new Dyn_Store (HashK_Item_Ref,Null_Item);
  ----------------------------------------------------------------------------
  --
  -------------------------------------
  procedure ReInitialize_DB is
  begin
    Configurations_Count := 0;
    Atomic_CC :=0; 
    Hash_Table.ReInitialize_DB;
    Elements_Store.ReInitialize_DB;
  end ReInitialize_DB;

  procedure Debug_DB is
    V1: Integer := 0;
    V2: Integer :=0;
  begin
    null;
  end Debug_DB;
   --
   --  returns the current count of elements in the DB.
   --
  function Current_Count return Integer is
  begin
    return Integer(Current_Value32(Atomic_CC'Address));
  end Current_Count;


  -----------------------------------------------------------------
  --  restituisce un nicknum unico per questo Elemento, in base
  --   all'ordinale di inserzione nel DB.
  --  Se l'elemento era gia' nel DB, resituisce il suo nicknum originale
  --  Se l'elemento non era nel DB, ce lo aggiunge, generando un nuovo
  --    nicknum e restituendolo col valore negativo per indicare che e'
  --    stato appena creato. (questa informazione puo' essere utile per
  --    inizializzare altre strutture)
  -----------------------------------------------------------------
  function CheckNum (This: Element) return Integer is
    Key: Natural :=0;
    Tmp: HashK_Item_Ref;
    Orig: HashK_Item_Ref;
    Found: Boolean;
    Depth: Natural :=0;
    N: Integer;
   CC: Integer;
  begin
    --
    --  cerca se questa configurazione e' gia' stata incontrata.
    --  SE AVVENGONO IN PARALLELO DUE CHECK CON LA MEDESIMA KEY ???
    --  entrambi possono trovare l'elemento NOT_FOUND  in tal case poi ripeteranno il check
    --   dope aver acquisito il lock
    --
    Key := Mk_Key(This);  -- Positive
    -- Tmp e' una lista di elementi associata alla chiave key.
    Tmp := Hash_Table.Retrieve(Key);
    Orig := Tmp;
    Found := False;
    while Tmp /= null and not Found loop
       -- per ogni indice nella lista, guardo se il corripondente elemento e' uguale a quello 
       --  che sto' cercando di inserire.
       if This /=  Elements_Store.Retrieve(Tmp.NickNum) then
          Tmp := Tmp.Next;
          Depth := Depth +1;
       else
          Found := True;
       end if;
    end loop;
    --
    -- se la lista degli indici e' vuota aggiungi questo elemento nello store ed
    --  il suo indice nella lista
    --
    if not Found then
       if ThreadSafe then
         -- ATOMIC START FOR KEY
         --
         Seize(key);
         --
         -- repeat the check in case of concurrent access (now it is safe!)
         Orig := Hash_Table.Retrieve(Key);
         Tmp := Orig;
         Found := False;
         while Tmp /= null and not Found loop
            -- per ogni indice nella lista, guardo se il corripondente elemento e' uguale a quello
            --  che sto' cercando di inserire.
            if This /=  Elements_Store.Retrieve(Tmp.NickNum) then
               Tmp := Tmp.Next;
            else
               Found := True;
            end if;
         end loop;
         --
         if not Found then
           CC := Integer(Increment32(Atomic_CC'Address));
           Elements_Store.Store (This,CC);
           --
           -- la lista era vuota, o tutti gli indice denotavano elementi diversi da questo,
           --  quindi aggiungo un nuovo elemento in testa alla lista esistente
           Tmp := new HashK_Item;
           Tmp.NickNum  := CC;
           Tmp.Next :=  Orig;
           Hash_Table.Store(Tmp,Key);
           if Depth > max_key_conflicts then
               max_key_conflicts := Depth; -- dovrebbe essere atomico ....
           end if;
         end if;
         --
         Release(key);
         --
         -- ATOMIC END
       else
         Configurations_Count := Configurations_Count+1;
         Atomic_CC :=  Counter32(Configurations_Count);
         CC:= Configurations_Count;
         Elements_Store.Store (This,CC);
         --
         -- la lista era vuota, o tutti gli indice denotavano elementi diversi da questo,
         --  quindi aggiungo un nuovo elemento in testa alla lista esistente
         Tmp := new HashK_Item;
         Tmp.NickNum  := CC;
         Tmp.Next :=  Orig;
         Hash_Table.Store(Tmp,Key);
         if Depth > max_key_conflicts then
             max_key_conflicts := Depth; -- dovrebbe essere atomico ....
         end if;
       end if;
    end if;
    N := Tmp.NickNum;
    if Found then
       return N;
    else 
       return - N;
    end if;
  end CheckNum;


  -----------------------------------------------------------------
  --  Se l'elemento era gia' nel DB, resituisce il suo nicknum originale
  --  altrimenti restituisce 0 ( e non avvengono inserzioni)
  -----------------------------------------------------------------
  -- non e' ncessario sincronizzarsi sulla key. 
  -- La sincronizzazzione delle receive e' sufficente
  -----------------------------------------------------------------
  function JustCheckNum (This: Element) return Integer is
    Key: Positive;
    Tmp: HashK_Item_Ref;
    Found: Boolean;
    N: Integer;
  begin
    --
    --  cerca se questa configurazione e' gia' stata incontrata.
    --
    Key := Mk_Key(This);
    Tmp := Hash_Table.Retrieve(Key);
    Found := False;
    while Tmp /= null and not Found loop
       if This /=  Elements_Store.Retrieve(Tmp.NickNum) then
          Tmp := Tmp.Next;
       else
          Found := True;
       end if;
    end loop;
    --
    if Found then
       N := Tmp.NickNum;
       return N;
    else
       return  0;
    end if;
  end JustCheckNum;

  function NickNum (This: Element) return Integer is
    Pos: Integer;
  begin
    Pos := CheckNum(This);
    if Pos > 0 then
      return Pos;
    else
      return -Pos;
    end if;
  end NickNum;

  function NickName (This: Element; Prefix: String := "#") return  String is
    Pos: Integer := NickNum(This);
     Static: String := Integer'Image(Pos);
   begin
      return Prefix & Static(2..Static'Length);
  end NickName;

  -----------------------------------------------------------------
  -- non e' ncessario sincronizzarsi sulla key.
  -- La sincronizzazione delle receive e' sufficente
  -----------------------------------------------------------------
  function Retrieve (NickNum: Positive) return Element is
  begin
     declare
       E: Element := Elements_Store.Retrieve(abs(NickNum));
     begin
       return E;
     end;
  end Retrieve;

  -----------------------------------------------------------------
  -- non e' ncessario sincronizzarsi sulla key.
  -- La sincronizzazione delle store e' sufficente
  -----------------------------------------------------------------
  procedure Store (This: Element; NickNum: Positive) is
  begin
     Elements_Store.Store(This,abs(NickNum));
  end Store;

  -----------------------------------------------------------------
  -- non e' ncessario sincronizzarsi sulla key.
  -- La sincronizzazione delle store e' sufficente
  -----------------------------------------------------------------
  function Store (This: Element) return Integer is
     tmp: Positive;
  begin
     tmp := NickNum(This);
     Elements_Store.Store(This,abs(tmp));
     return tmp;
  end Store;
  
   function Get_Key_Conflicts (Key: Positive) return Int_Table is
    Tmp: HashK_Item_Ref :=Hash_Table.Retrieve(Key);
    Count: Natural :=0;
    keys: Int_Table(1..1000);
   begin
      while Tmp /= null loop
         Count := Count+1;
         keys(Count) :=  Tmp.NickNum;
         Tmp := Tmp.Next;
      end loop;
      return keys(1..Count);
   end Get_Key_Conflicts;
   
begin
   Configurations_Count := 0;
   Atomic_CC :=0;
end NickNames;
