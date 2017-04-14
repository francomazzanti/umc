    generic
    type Element (<>) is private;
    with function Default_Initial return Element;
    LocalThreadSafe: Boolean := True;
    package Dyn_Store is
      --
      -- Given a nicknum, retrieves the corresponding full description
      --
      function Retrieve (NickNum: Positive) return Element;
      --
      -- Given a nicknum, store the given item at the given position
      --  it the nicknum is not specified, it is stored at a new position,
      --  returning it.
      --
        procedure Store (Item: Element; NickNum: Positive);
      --
      -- inizializza le strutture del DB
      --
      procedure ReInitialize_DB;
      --
    end Dyn_Store;

