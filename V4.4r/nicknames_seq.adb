with SYSTEM; use System;
with Ada.Unchecked_Deallocation;
with Dyn_Store;
with Ada.text_IO; use Ada.Text_IO;
package body NickName_Seq is
  Vectors_Max: constant Positive := 4096;

  type Cache_Item;
  type Cache_Item_Ref is access Cache_Item;
  -- pragma Atomic(Cache_Item_Ref);
  -- pragma Volatile(Cache_Item_Ref);
  type Cache_Item is record
    Next: Cache_Item_Ref;
    NickNum: Integer;
  end record;
  -- pragma Volatile(Cache_Item);

  Null_Cache_Item: Cache_Item_Ref :=null;
  function Null_Item return  Cache_Item_Ref is
  begin
    return null;
  end Null_Item;
  --
  procedure Free is new Ada.Unchecked_Deallocation (Cache_Item, Cache_Item_Ref);
  --
  Configurations_Count: Integer := 0;  -- max 2**31 -1
  --
  ----------------------------------------------------------------------------
  --  Nicknames fa uso di 2 vettori:
  -- Elements_Store che contiene gli elementi effettivi memorizzati nell'archivio,
  -- Hash_Table che contiene liste di indici di elementi con medesima chiave
  ----------------------------------------------------------------------------
  package Elements_Store is new Dyn_Store_Seq(Element, Default_Initial);
  package Hash_Table is new Dyn_Store_Seq (Cache_Item_Ref,Null_Item);
  --
  -------------------------------------
  procedure ReInitialize_DB is
  begin
    Configurations_Count := 0;
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
    return Configurations_Count;
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
    Tmp: Cache_Item_Ref;
    Orig: Cache_Item_Ref;
    Found: Boolean;
    Depth: Natural :=0;
    N: Integer;
   CC: Integer;
  begin
    --
    --  cerca se questa configurazione e' gia' stata incontrata.
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
         Configurations_Count := Configurations_Count+1;
         CC:= Configurations_Count;
         Elements_Store.Store (This,CC);
         --
         -- la lista era vuota, o tutti gli indice denotavano elementi diversi da questo,
         --  quindi aggiungo un nuovo elemento in testa alla lista esistente
         Tmp := new Cache_Item;
         Tmp.NickNum  := CC;
         Tmp.Next :=  Orig;
         Hash_Table.Store(Tmp,Key);
         if Depth > max_key_conflicts then
             max_key_conflicts := Depth; 
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
    Tmp: Cache_Item_Ref;
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

begin
   Configurations_Count := 0;
end NickNames_Seq;
