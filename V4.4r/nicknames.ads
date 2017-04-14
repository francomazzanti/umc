    with Flags; use Flags;
    generic
    type Element (<>) is private;
    with function Default_Initial return Element;
    with function Mk_Key (
           This_Elem: Element;
           Cache_Max: Natural := Key_Module) return Positive;
    with function "=" (Left: Element; Right: Element) return Boolean;
    UNUSEDThreadSafe: Boolean := True;
    package NickNames is
      --
      --  returns the current count of elements in the DB.
      --
      function Current_Count return Integer;
      --
      --  inserts a configuration in the DB, if not already there,
      --  and returns a unique positive number (or string) which identifies it.
      --
      function NickNum (This: Element) return Integer;
      function NickName (This: Element; Prefix: String := "#") return String;
      --
      --  inserts a configuration in the DB, if not already there,
      --  and returns a unique number which identifies it.
      --  If the item has was not present in the DB, the returned number
      --  is a negative one.

      function JustCheckNum (This: Element) return Integer;
      --
      --  if the given configuration is in the DB returns the positive
      --  number which identifies it, otherwise returns 0.
      --
      function CheckNum (This: Element) return Integer;
      --
      --  if the given configuration is in the DB returns the positive
      --  number which identifies it, otherwise adds the computations in the DB
      --  and returns the negative of its value;
      --
--      function Already_Seen (This: Element) return Integer;
      --
      -- basic storage operations
        --
      function Retrieve (NickNum: Positive) return Element;
      procedure Store (This: Element; NickNum: Positive);
      function Store (This: Element) return Integer;
      --
      -- inizializza le strutture del DB
        --
      procedure ReInitialize_DB;
      --
      function Get_Key_Conflicts (Key: Positive) return Int_Table;
      --
      Max_key_Conflicts: Natural :=0;
      pragma Volatile(Max_key_Conflicts);
    end NickNames;
