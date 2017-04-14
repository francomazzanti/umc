    generic
    package Bool_Dyn_Store is
      --  memory structured in N blocks (as much as needed) of 4096 elements 
      Vectors_Max : Natural := 4096; 
      --
      -- Given a nicknum, retrieves the corresponding full description
      --
      function Retrieve (NickNum: Integer) return Boolean;
      --
      -- Given a nicknum, store the given item at the given position
      --  it the nicknum is not specified, it is stored at a new position,
      --  returning it.
      --
        procedure Store (Item: Boolean; NickNum: Integer);
      --
      -- inizializza le strutture del DB
      --
      procedure Initialize_DB;
      --
    end Bool_Dyn_Store;

