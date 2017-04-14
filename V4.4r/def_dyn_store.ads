    generic
    type Element is private;
    with function Default_Initial return Element;
    package Def_Dyn_Store is
      Vectors_Max : Natural := 4096;   --  slots of 4096 elements
      --
      -- Given a nicknum, retrieves the corresponding full description
      --
      function Retrieve (NickNum: Integer) return Element;
      --
      -- Given a nicknum, store the given item at the given position
      --  it the nicknum is not specified, it is stored at a new position,
      --  returning it.
      --
        procedure Store (Item: Element; NickNum: Integer);
      --
      -- inizializza le strutture del DB
      --
      procedure Initialize_DB;
      --
    end Def_Dyn_Store;

