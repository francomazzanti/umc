    generic
    type Element is private;
    type Element_Ref is access Element;
    ThreadSafe: Boolean := True;
    Vectors_Max: Positive := 4096;
    Table_Max: Positive := 4096 * 4096;
    package Dyn_Store_Obj is
      --
      -- Given a nicknum, retrieves the corresponding full description
      --
      function Retrieve (NickNum: Positive) return Element_Ref;
      --
      -- Given a nicknum, store the given item at the given position
      --  it the nicknum is not specified, it is stored at a new position,
      --  returning it.
      --
        procedure Store (Item: Element_Ref; NickNum: Positive);
      --
      -- inizializza le strutture del DB
      --
      procedure ReInitialize_DB(Deep: Boolean := True);
      --
    end Dyn_Store_Obj;

