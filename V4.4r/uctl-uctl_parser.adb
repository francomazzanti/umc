--with UCTL_Types; use UCTL_Types;
--with UCTL_Utilities;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
separate(UCTL)
package body UCTL_Parser is
  use Ada.Text_IO;
  use Ada.Exceptions;
  use Ada.Characters.Latin_1;
  use Ada.Characters.Handling;
  -- use Global_Env;
  use UCTL_Types;
  --use Basic_Properties;
  
  ------------------------------------------------------------
  ------------------------------------------------------------
  --
  --    TOKENS MANAGEMENT AND GENERIC PARSING ROUTINES
  --
  ------------------------------------------------------------
  ------------------------------------------------------------
  subtype Action is UCTL_Types.Action;

  type Token_Kind is (Num, Id, Str, Special);  --  special num id   12a

  type Token is record
     Item: String_Ref;
     Line: Natural;
     Line_Ref: String_Ref;
     Column: Natural;
     Kind: Token_Kind;
  end record;

  type Tokens_Table is  array (Positive range <>) of Token;
  type Tokens_Table_Ref is access Tokens_Table;

  --------------------------------------------------------------------------
  Tokens: Tokens_Table_Ref := new Tokens_Table(1..0);
  Current_Token: Natural :=1;
  ------  BASIC PARSING IMPLEMENTATION VARIABLES ----------------------------
  Interactive: Boolean := False;
  F: File_Type;
  Line_Length: Natural := 0;
  Input_Line:String(1..Max_Line_Length);
  Current_Position: Positive :=1;
  Line_Number: Natural :=0;
  Max_Token_Size : Natural := 200;
  Last_Line: String_Ref;
  
    MoreFormulas:Boolean;
  ---------------------------------------------------------------------------

  -- actually unused: planned to check that state predicates really exist
  Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref; 

  procedure Read_Tokens;
  procedure Give_Error (Msg : in String);
  procedure Skip_Token (Item: String := "");

---------------------------------------------------------------------------
  function Current_Item return String is
  begin
    return Tokens(Current_Token).Item.all;
  end Current_Item;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  function Next_Item return String is
  begin
    if Current_Token >= Tokens.all'Length then
     return "";
    else
     return Tokens(Current_Token+1).Item.all;
    end if;
  end Next_Item;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  function Next_Next_Item return String is
  begin
    if Current_Token+2 > Tokens.all'Length then
     return "";
    else
     return Tokens(Current_Token+2).Item.all;
    end if;
  end Next_Next_Item;

  function Next_Next_Next_Item return String is
  begin
    if Current_Token+3 > Tokens.all'Length then
     return "";
    else
     return Tokens(Current_Token+3).Item.all;
    end if;
  end Next_Next_Next_Item;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  procedure Skip_Token (Item: String := "") is
    begin
    if Current_Token = 0 then
      Give_Error (" File Empty ??? ");
      raise Parsing_Error;
    elsif Current_Token = Tokens.all'Length then
        Give_Error ("Unexpected End of File");
        raise Parsing_Error;
    elsif Item'Length = 0 then
      Current_Token := Current_Token+1;
    elsif Tokens(Current_Token).Item.all = Item then
        Current_Token := Current_Token+1;
    else
      Give_Error ("Failure while looking for token " &  Item);
      raise Parsing_Error;
    end if;
  end Skip_Token;
---------------------------------------------------------------------------



---------------------------------------------------------------------------
  procedure Give_Error (Msg : in String) is
    Where:Token;
  begin
    if Current_Token in Tokens.all'Range then
      Where := Tokens(Current_Token);
      Put_Line (Current_Error,
                 "### Error found at line " & Natural'Image(Where.Line) &
                 ", column " & Natural'Image(Where.Column) & " ###" );
      if Where.Line_Ref /= null then
        Put_Line(Current_Error, "> " & Where.Line_Ref.all);
        Put(Current_Error, "  ");
        for I in 1..Where.Column-1 loop
          Put(Current_Error, " ");
        end loop;
        Put_line(Current_Error, "^");
      end if;
    end if;
    Put_line(Current_Error, Msg);
  end Give_Error;
---------------------------------------------------------------------------



  --------------------------------------------------------
  -- deletes the comment part from the current line
  --------------------------------------------------------
  procedure Remove_Comments is
    Inside_String_Literal: Boolean := False;
  begin
    if Line_Length >=2 and then
      (Input_Line(1..2) = "--" or Input_Line(1..2) = "//") then
      Line_Length :=0;
      return;
    end if;
    for I in 1..Line_Length loop
      if Input_Line(I)= '"' then
         Inside_String_Literal := not Inside_String_Literal;
      end if;
      if (Input_Line(I) = ' ' or else Input_line(I) = HT) and then
          not Inside_String_Literal and then
          I+1 < Line_Length  and then
          ((Input_Line(I+1) = '-' and then Input_Line(I+2) = '-') or
           (Input_Line(I+1) = '/' and then Input_Line(I+2) = '/')) then
        Line_Length := I-1;
        exit;                     --  comment found: line is truncated here
      end if;
    end loop;
  end Remove_Comments;
---------------------------------------------------------------------------



---------------------------------------------------------------------------
-- Legge dal file F una nuova riga di testo, 
--   la mette in Input_line(1..Line_Length), e ne rimuove i commenti
--   Se la riga non contiene dati utili (vuota o solo blanks o solo commenti)
--   continua a leggere righe dal file F.
-- Quando non si sono piu' righe disponili, Line_Length viene messa a 0.
---------------------------------------------------------------------------
  procedure Read_New_Line is
  begin
    Line_Length := 0;
    while not End_of_File(F) loop
      if  Line_Length = 0 then
        Get_Line(F, Input_Line, Line_Length);   -- skip empty  and comment lines
        Last_Line := new String'(Input_Line(1..Line_Length));
        Current_Position := 1;
        Line_Number := Line_Number +1;
        Remove_Comments;
      else
        exit;
      end if;
    end loop;
  end Read_New_Line;
---------------------------------------------------------------------------


  -----------------------------------------------------------------
  -- reads the input file until a non-white char is found
  --  when EOF, sets Line_Length to 0
  -----------------------------------------------------------------
  procedure Skip_Spaces is
  begin
    --
    if not Interactive and then
        Line_Length = 0 and then not End_Of_File(F) then
      Read_New_Line;
    end if;
    --
    -- cycle reading until a non empty is found
    loop
      -- find first next non space char
      for I in Current_Position .. Line_Length loop
        if Input_line(I) /= ' ' and Input_line(I) /= HT then
          Current_Position := I;
          return;                         -- found not blank char
        end if;
      end loop;
      --  empty line or completely white rest of line
      if Interactive or else End_Of_File(F) then
         Line_Length :=0;
         return;
      else 
        Read_New_Line;
      end if;
    end loop;
  end Skip_Spaces;
---------------------------------------------------------------------------




  --------------------------------------------------------------------
  -- extracts the next token from the stream
  -- returns a token with empty string item  when EOF;
  --
  -- TOKENS are either:
  -- 1)   numbers      0..9     0..9
  -- 2)   ids          a..z  A..Z  0..9 _ '
  -- 3)   strings      "any(<>)"
  -- 4)   specials long:  )->  -(  :=  ++ --  ->  >=  <= ==  !=  && ||
  -- 5)    specials short: ~ ( ) , ; = : - [ ] /  $ & * + > < | .  ^ # % ! ?  %
  --
  --     "// " is a comment separator
  --     "--"  is comment separator only if
  --         a)  appears at the beginnning of the line or
  --         b)  is preceeded by a white space
  --   N.B.  Tokens are buffered by Read_Tokens
  --------------------------------------------------------------------
  -- Chiamata da Read_Tokens.
  -- Lavora sulla variabile Input_Line(1..Line_Length) estraendo il
  --   token successivo a partire dalla posizione Current_Position.
  -- Se Line_Length=0 , cioe' non si sono piu' righe disponibili,
  --  restituisce un token con Item="".
  -- Idem se Interactive e Current_Position > Line_Length.
  -- Quando Current_Position arriva a fine riga, se non Interactive,
  --   viene letto dal file F una nuova riga.
  --------------------------------------------------------------------
  function Get_Token return Token is
    This_Token: Token;
    StringBuffer: String(1..Max_Token_Size);
    Item_Size: Natural :=0;
  begin
    Skip_Spaces;
    --
    -- if empty file, Current_Position =1  and
    --
    This_Token.Line :=  Line_Number;
    This_Token.Line_Ref := Last_Line;
    This_Token.Column :=  Current_Position;
    --
    -- if file empty return null final token
    --
    if Line_Length = 0  then
       This_Token.Item := new String'("");
       return  This_Token;   --  Item= null;
    end if;
    --
    -- if line is exhausted, read next line  or return final token
    --
    if Current_Position > Line_Length then
      if Interactive then
         This_Token.Item := new String'("");
         return  This_Token;   --  Item= null;
      else
         Read_New_Line;
         return Get_Token;
      end if;
    end if;
    --
    -- find the TOKEN KIND
    --
    if Input_line(Current_Position) in '0'..'9' then
      This_Token.Kind := Num;
      --
    elsif Input_line(Current_Position) in 'a'..'z' or else
--          Input_line(Current_Position) = '#'  or else
          Input_line(Current_Position) = '$'  or else
          Input_line(Current_Position) = '%'  or else
--          Input_line(Current_Position) = '?'  or else
--          Input_line(Current_Position) = '!'  or else
          Input_line(Current_Position) = '_'  or else
          Input_line(Current_Position) in 'A'..'Z' then
          --
      This_Token.Kind := Id;
    elsif Input_line(Current_Position) = '"' then
      This_Token.Kind := Str;
    else
      This_Token.Kind := Special;
    end if;
    --
    -- get the token item
    --  Ids are   "aa ab ia a1 a_2as3a
    --
    for I in Current_Position .. Line_Length loop
      case This_Token.Kind is
      --
      when Id =>
        --
        Item_Size := Item_Size +1;
        StringBuffer(Item_Size) := Input_line(Current_Position);
        Current_Position := Current_Position +1;
        --  '$'  and '%'  can happear only as forst char of the idenifier
        -- this is the last char of the item
        if Current_Position > Line_Length or else
           (Input_line(Current_Position) not in 'a'..'z' and then
           Input_line(Current_Position) not in 'A'..'Z' and then
           Input_line(Current_Position) not in '0'..'9' and then
--           Input_line(Current_Position) /= '?'  and then
--           Input_line(Current_Position) /= '!'  and then
--           Input_line(Current_Position) /= '#'  and then
           Input_line(Current_Position) /= '_')  then
           --  "-" and "*"  only as singletons
           exit;
        end if;
        --
      when Num =>
        Item_Size := Item_Size +1;
        StringBuffer(Item_Size) := Input_line(Current_Position);
        Current_Position := Current_Position +1;
        -- this is the last char of the item
        if Current_Position > Line_Length or else
           Input_line(Current_Position) not in '0'..'9' then
           if Input_line(Current_Position) in 'a'..'z' or else
              Input_line(Current_Position) in 'A'..'Z' then
              This_Token.Kind := Id;
           else
             exit;
           end if;
        end if;
        -- 
      when Str =>
          Item_Size := Item_Size +1;
          StringBuffer(Item_Size) := Input_line(Current_Position);
          Current_Position := Current_Position +1;
          -- this is the last char of the item
        if Item_Size > 1 and then Input_line(Current_Position) = '"' then
          Item_Size := Item_Size +1;
          StringBuffer(Item_Size) := Input_line(Current_Position);
          Current_Position := Current_Position +1;
          exit;
        elsif Current_Position > Line_Length then
          Give_Error ("Unterminated String Literal");
          raise Parsing_Error;
        end if;
        --
      when Special =>
      --
      --   specials long:  )->  -(  :=  ++ --  ->  >=  <= ==  !=  && || /  /= => ..  << >>  [[  ]]
      --   specials short: ~ ( ) , ; = : - [ ] /  $ & * + > < | .  ^ # % ! ? { } 
      --
        Item_Size := Item_Size +1;
        StringBuffer(Item_Size) := Input_line(Current_Position);
        -- this is the last char of the item
        if Current_Position+2 <=  Line_Length and then
           Input_Line(Current_Position..Current_Position+2)= ")->" then
           Item_Size := Item_Size+2;
           StringBuffer(2..3) := "->";
           Current_Position := Current_Position +3;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "-(" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '(';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= ".." then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '.';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "<<" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '<';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= ">>" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '>';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "[[" and then
             (Current_Position = Input_Line'First or else 
                (Input_Line(Current_Position-1) /= 'A' and then 
                Input_Line(Current_Position-1)/= 'E')) then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '[';
           Current_Position := Current_Position +2;
           exit;
           -- actually   "E [[true] true {true} U true]"   still fails ...
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "]]" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := ']';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "=>" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '>';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "->" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '>';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= ":=" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '=';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "++" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '+';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "--" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '-';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "->" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '>';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= ">=" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '=';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "<=" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '=';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "==" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '=';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "!=" then
           Item_Size := Item_Size+1;
           StringBuffer(1) := '/';
           StringBuffer(2) := '=';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "/=" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '=';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "&&" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '&';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "||" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '|';
           Current_Position := Current_Position +2;
           exit;
        else   --- special  one char token
               -- do not check for unexpected chars
           Current_Position := Current_Position +1;
           exit;
        end if;
      end case;
    end loop;
    --
    -- Do some adjustments of the kind for letteral operators
    --
    if StringBuffer(1..Item_Size) = "and" then
      This_Token.Kind := Special;
      This_Token.Item := new String'("&");
    elsif StringBuffer(1..Item_Size) = "or" then
      This_Token.Kind := Special;
      This_Token.Item := new String'("|"); 
    elsif StringBuffer(1..Item_Size) = "not" then
      This_Token.Kind := Special;
      This_Token.Item := new String'("~");
    elsif StringBuffer(1..Item_Size) = "implies" then
      This_Token.Kind := Special;
      This_Token.Item := new String'("->");
    else
      This_Token.Item := new String'(StringBuffer(1..Item_Size));
    end if;
    return This_Token;
    --
  end Get_Token;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Se Interactive = true  estrae tutti i tokens dalla input_line
--   trovandi in Input_line(1..Line_length)  aggiungendoli in coda al vettore Tokens.
-- La procedura Get_Token chiamata, quando arriva alla fine della riga,
-- Se Interactive o EndOfFile  restituisce unn token finale con item="",
--   altrimenti (non Interactive and not EOF) legge una nuova riga da F e continua
--   la estrazione di tokens.
---------------------------------------------------------------------------
procedure  Read_Tokens is
  Buffer: Tokens_Table(1..1000);
  Count: Natural := 0;
  Tmp: Token;
  Tmp2: Token;
begin
  Tmp:= Get_Token;
  if Tmp.Item.all'Length =4 and then
     (Tmp.Item(1) = 'E' or else Tmp.Item(1) = 'A') and then
     (Tmp.Item(2) = 'F' or else Tmp.Item(2) = 'G') and then
     (Tmp.Item(3) = 'E' or else Tmp.Item(3) = 'A') and then
     (Tmp.Item(4) = 'F' or else Tmp.Item(4) = 'G') then
     Tmp2 := Tmp;
     Tmp2.Item := new String(1..2);
     Tmp2.Item(1..2) := Tmp.Item(3..4);
     Tmp.Item := new String'(Tmp.Item(1..2));
      Count := Count +1;
      Buffer(Count) := Tmp;
      Count := Count +1;
      Buffer(Count) := Tmp2;
  else 
    Count := Count +1;
    Buffer(Count) := Tmp;
  end if;
  while Tmp.Item.all'Length /= 0  loop
    Tmp:= Get_Token;
    if Tmp.Item.all'Length =4 and then
       (Tmp.Item(1) = 'E' or else Tmp.Item(1) = 'A') and then
       (Tmp.Item(2) = 'F' or else Tmp.Item(2) = 'G') and then
       (Tmp.Item(3) = 'E' or else Tmp.Item(3) = 'A') and then
       (Tmp.Item(4) = 'F' or else Tmp.Item(4) = 'G') then
       Tmp2 := Tmp;
       Tmp2.Item := new String(1..2);
       Tmp2.Item(1..2) := Tmp.Item(3..4);
       Tmp.Item := new String'(Tmp.Item(1..2));
        Count := Count +1;
        Buffer(Count) := Tmp;
        Count := Count +1;
        Buffer(Count) := Tmp2;
    else
      Count := Count +1;
      Buffer(Count) := Tmp;
    end if;
    if Count =1000 then
      Tokens := new Tokens_Table'(Tokens.all & Buffer);
      Count :=0;
    end if;
  end loop;
  Tokens := new Tokens_Table'(Tokens.all & Buffer(1..Count));
  Current_Token := 1;
end Read_Tokens;
---------------------------------------------------------------------------



---------------------------------------------------------------------------
--                    SUMMARY OF PARSING BUILTIN
--
--  type Token_Kind is (Num, Id, Str, Special);  
--  
--  type Token is record 
--     Item: String_Ref;
--     Line: Natural; 
--     Line_Ref: String_Ref;
--     Column: Natural;
--     Kind: Token_Kind;
--  end record;
--
--  type Tokens_Table is  array (natural range <>) of Token;
--  type Tokens_Table_Ref is access Tokens_Table;
--
--  Tokens: Tokens_Table_Ref := new Tokens_Table(1..0);
--  Current_Token: Natural :=0;
--    
--  procedure Read_Tokens; 
--  procedure Give_Error (Msg : in String);
--  procedure Skip_Token (Item: String := "");
--  function Current_Item return String; 
--  function Next_Item return String; 
--  function Next_Next_Item return String; 
--  function Next_Next_Next_Item return String; 
---------------------------------------------------------------------------



---------------------------------------------------------------------------
--  UCTL SPECIFIC PARSING RUOTINES
---------------------------------------------------------------------------
---------------------------------------------------------------------------
--
-- procedure Parse (Tab_File_Name: String);
-- procedure Parse_From_String (Input_Line: String);
--

  function Parse_Identifier return String_Ref;
  function Parse_Formula (Env: String_Table;
                          Context_Size: Natural;
                          Is_Unary:Boolean := False;
                          Is_Top: Boolean := False) return Formula_Ref;
  function Parse_Path (Env: String_Table;
                       Context_Size: Natural)return Path_Ref;
  function Parse_Action (Tau_Allowed: Boolean;
                         Var_Allowed: Boolean;
                         Is_Negation:Boolean) return Action_Ref;



---------------------------------------------------------------------------
  --  NOTICE "'"  is allowed as identifier suffix
  --  NOTICE "*"  is allowed as identifier
  --  NOTICE "$"  is allowed as identifier prefix
  --  NOTICE "~"  is allowed as identifier prefix
  --  NOTICE "!"  is allowed as identifier suffix / prefix
  --  NOTICE "?"  is allowed as identifier suffix / prefix
--????   --  NOTICE  "#" is allowed once inside an identifier  as suffix or join
---------------------------------------------------------------------------
  function Parse_Identifier return String_Ref is
    Prefix: String_Ref := Null_String;
  begin
    if Tokens(Current_Token).Kind=Id or else
         Tokens(Current_Token).Kind=Num or else
         Current_Item= "*"  or else
         Current_Item(1)= '$'  or else
         Current_Item(1)= '%'  or else
         Tokens(Current_Token).Kind=Str then
      Prefix := Tokens(Current_Token).Item;
      Skip_Token;
      return Prefix;
    else
      Give_Error ("identifier expected");
      raise Parsing_Error;
    end if;
  end Parse_Identifier;
---------------------------------------------------------------------------


-- =============  NWO Parallel NonIntercative  AG Stuff ================
  AGTABLE: String_Table(1..100);
  AGCOUNT: Natural :=0;

  function Get_AGIndex(This_Formula: Formula_Ref) return Natural is
  begin
    for I in 1..AGCOUNT loop
       if AGTABLE(I).all = This_Formula.Fimage.all then
         return I;
       end if;
    end loop;
    AGCOUNT := AGCOUNT+1;
    AGTABLE(AGCOUNT) := This_Formula.Fimage;
    return AGCOUNT;
  end Get_AGIndex;

  procedure Set_AGIndex(This_Formula: Formula_Ref) is
  begin
    case This_Formula.Kind is
      when FNot => 
         Set_AGIndex(This_Formula.NotRef);
      when Fand | Foor ! Fimply =>
         Set_AGIndex(This_Formula.LeftRef);
         Set_AGIndex(This_Formula.RightRef);
      when Fangle | Fsquare =>
         Set_AGIndex(This_Formula.FormRef);
      when Fexist | Fall =>
         case This_Formula.PREf.Kind is
           when Eventually | Always =>
             This_Formula.AGIndex := Get_AGIndex(This_Formula);
             Set_AGIndex(This_Formula.PREf.TForm);
           when others => null;
         end case;
      when others => null;
    end case;
  end Set_AGIndex;
-- ====================================================================


---------------------------------------------------------------------------
  procedure Parse (Tab_File_Name: String) is
    FileName: String(1..Tab_File_Name'Length) := Tab_File_Name;
  begin 
    Formula_contains_PRINT := False;
    Interactive := False;
    --
    -- check existence and readability of file, otherwise exits
    --
    begin
      Open(F, In_File,FileName);
      -- legge la prima riga contenente un token
      --  (saltando righe vuote o con commenti)
      -- se non ci sono righe valide  Line_length=0;
      Read_New_Line;
      if Line_Length=0 then
         Give_Error ("The uctl file """ & FileName & """ is EMPTY ...");
         raise Parsing_Error;
      end if;
      -- estrai i tokens da questa riga, e dalle successive fino ad EOF
      Tokens := new Tokens_Table(1..0);
      Current_Token :=1;
      Read_Tokens;
      --
      Close(F);
    exception
      when End_Error =>
         Close(F);
         -- Non dovrebbe mai succedere ....
         Give_Error ("The uctl file """ & FileName & """ is EMPTY ...");
         raise Parsing_Error;
      when Name_Error =>
          Give_Error( "Unexpected Name Error for file """ & FileName & """");
         raise;
      when others =>
         Give_Error( "Unexpected Parsing Error");
         Close(F);
         raise;
    end;
    --
    -- effettua il parsing a partire dal vettore Tokens.
    AGTABLE := (others => null);
    AGCOUNT := 0;
    All_Formulas := new Form_Table(1..0);
    All_Prints := new Bool_Table(1..0);
    MoreFormulas := True;
    while  MoreFormulas = True loop
        This_Formula := Parse_Formula(Empty_String_Table,0,False,True);
        UCTL_Utilities.Prepare_Formula (This_Formula);
        Set_AGIndex(This_Formula);
        All_Formulas := new Form_Table'(All_Formulas.all & This_formula);
        All_Prints := new Bool_Table'(All_Prints.all & UCTL_TYPES.Formula_contains_PRINT);
        if Debug then
           Put_line("PARSED FORMULA: " & This_Formula.fimage.all);
        end if;
       if This_Formula.Free_Vars.all'Length /= 0 then
          Give_Error(" the formula " & This_Formula.fimage.all  & " contains undefined names....");
          raise UCTL_Error;   
       end if;
       UCTL_Utilities.Check_Params (This_Formula);
       --  MONOTONICITY CHECK
       if not UCTL_Utilities.Is_Monotone(This_Formula) then
         Give_Error("UCTL_Parser ERROR: the formula " & This_Formula.fimage.all  & "in not monotone");
         raise Non_Monotone;
       end if;
       if Current_Item =";" then
          Skip_Token(";");
       else 
          MoreFormulas := False;
          if Current_Item /= "" then
              Give_Error("UCTL_Parser ERROR: unexpected text ( " & Current_Item &" ) at the end of a formula.");
          end if;
       end if;
    end loop;
    --
    if All_Formulas.all'Length =0 then
       Give_Error("The formula file is empty ??");
       raise Parsing_Error;
    end if;
    This_Formula := All_Formulas(1);
    UCTL_TYPES.Formula_contains_PRINT := All_Prints(1);
    --
    declare
    begin
      declare
         SS: String := Current_Item;
      begin
         if SS'length >0 then
           Give_Error("Unxpected data (""" & SS & """) found after the end of the formula.");
           raise Parsing_Error;
         end if;
      end;
    exception
       when End_Error => null;
    end;
    --
    exception
      when End_Error => 
        Give_Error ("Unexpected end of file (there is a missing ""true"" ? ");
        Close(F);
        raise Parsing_Error;
      when Parsing_Error =>
        Close(F);
        raise;
      when others =>
        Close(F);
        raise;      
  end Parse;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  procedure Parse_From_String (Input_Line: String) is
  begin
    Formula_contains_PRINT := False;
    Interactive := True;
    Line_Number := 1;
    Current_Position := 1;
    Line_Length := Input_Line'Length;
    UCTL_Parser.Input_Line(1..Line_Length) := Input_Line;
    --
    -- estrae i token dalla input_line.
    Tokens := new Tokens_Table(1..0); 
    Current_Token :=0;
    Read_Tokens;
    --
    All_Formulas := new Form_Table(1..0);
    All_Prints := new Bool_Table(1..0);
    MoreFormulas := True;
    while MoreFormulas = True loop
      This_Formula := Parse_Formula(Empty_String_Table,0,False,True);
      -- effettua il parsing a partire dal vettore Tokens.
      --
        UCTL_Utilities.Prepare_Formula (This_Formula);
        Set_AGIndex(This_Formula);
        All_Formulas := new Form_Table'(All_Formulas.all & This_formula);
        All_Prints := new Bool_Table'(All_Prints.all & Formula_contains_PRINT);
        if Debug then
           Put_line("PARSED FORMULA: " & This_Formula.fimage.all);
        end if;
       if This_Formula.Free_Vars.all'Length /= 0 then
          Give_Error(" the formula " & This_Formula.fimage.all  & " contains undefined names....");
          raise UCTL_Error;
       end if;
       UCTL_Utilities.Check_Params (This_Formula);
       --  MONOTONICITY CHECK
       if not UCTL_Utilities.Is_Monotone(This_Formula) then
         Give_Error("UCTL_Parser ERROR: the formula " & This_Formula.fimage.all  & "in not monotone");
         raise Non_Monotone;
       end if;
       if Current_Item =";" then
          Skip_Token(";");
       else 
          MoreFormulas := False;
          if Current_Item /= "" then
              Give_Error("UCTL_Parser ERROR: unexpected text ( " & Current_Item &" ) at the end of a formula.");
          end if;
       end if;
    end loop;
    --
    if All_Formulas.all'Length =0 then
       Give_Error("The formula file is empty ??");
       raise Parsing_Error;
    end if;
    This_Formula := All_Formulas(1);
    UCTL_TYPES.Formula_contains_PRINT := All_Prints(1);
    --
     declare
     begin
       declare
          SS: String := Current_Item;
       begin
          if SS'length >0 then
            Give_Error("Unxpected data (""" & SS & """) found after the end of the formula.");
            raise Parsing_Error;
          end if;
       end;
     exception
        when End_Error => null;
     end;
  exception
      when End_Error =>
        Give_Error ("Unexpected end of line (there is a missing ""true"" ? ");
        raise Parsing_Error;
  end Parse_From_String;
  ---------------------------------------------------------------------------


---------------------------------------------------------------------------
  function Is_Abstract_StatePredicate (This: String) return Boolean is
  begin
     return True;
--    for I in Abstract_Predicates.all'Range loop
--      if This=Abstract_Predicates(I).all then return True; end if;
--    end loop;
--    return False;
  end Is_Abstract_StatePredicate;
---------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  --  the general structure of an assertion  is:
  --   id.id.id LEFTOP id.id.id  OP id.id.id RIGHTOP id.id.id
  -------------------------------------------------------------------------
  --     ---  ABSTRACT ASSERTIONS SYNTAX ----------
  --    %var = val,    %var > num,   id /= id !!!!   
  --      id /= id     id must be an Object name (token or active)
  --    Pred,  Pred(args)
  --    PRINT_ONCE(args)
  ---------------------------------------------------------------------------
  function Parse_Abstract_Assertion return Formula_Ref is
    Result: Formula_Ref := new Formula(Assertion);
    Prefix: Basic_Predicate_Ref := new Basic_Predicate;
    Ident: String_Ref;
    Ids: String_Table_Ref := new String_Table(1..0);
  begin
    Result.Pred := Prefix; 
    --
    --    %id /= id
    --    id /= id
    --
    if Current_Item = "-" and then Next_Item(1) in '1'..'9' then
       Skip_Token ("-");
       Ident := new String'("-" & Parse_Identifier.all);   --QQ
    else
       Ident :=  Parse_Identifier;
    end if;
    --
    if Current_Item = "/=" then
       Prefix.Op := NE;
       --Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token("/=");
       if Current_Item /= "-" then
           Ident :=  Parse_Identifier;
           if Ident(1) = '$' then
              Give_Error ("Variable not allowed in the context (" &
                Ident.all &  ") (use % instead of $)");
              raise Parsing_Error;
            end if;
       else
           Skip_Token ("-");
           Ident := new String'("-" & Parse_Identifier.all);   --QQ
       end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
    --    id >= id
    --
    if  Current_Item = ">=" then
       Prefix.Op := GE;
       --Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token(">=");
       if Current_Item /= "-" then
         Ident :=  Parse_Identifier;
         if Ident(1) = '$' then
           Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
           raise Parsing_Error;
          end if;
       else
         Skip_Token ("-");
         Ident := new String'("-" & Parse_Identifier.all);   --QQ
       end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
    --    id <= id
    --
    if Current_Item = "<=" then
       Prefix.Op := LE;
       -- Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token("<=");
      if Current_Item /= "-" then
        Ident :=  Parse_Identifier;
        if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
      else
        Skip_Token ("-");
        Ident := new String'("-" & Parse_Identifier.all);   --QQ
      end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
    --    id < id
    --
    if Current_Item = "<" then
       Prefix.Op := LT;
       --Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token("<");
      if Current_Item /= "-" then
        Ident :=  Parse_Identifier;
        if Ident(1) = '$' then
           Give_Error ("Variable not allowed in the context (" &
               Ident.all &  ") (use % instead of $)");
           raise Parsing_Error;
         end if;
      else
        Skip_Token ("-");
        Ident := new String'("-" & Parse_Identifier.all);   --QQ
      end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
    --   id > id
    --
    if Current_Item = ">" then
       Prefix.Op := GT;
       --Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token(">");
      if Current_Item /= "-" then
        Ident :=  Parse_Identifier;
        if Ident(1) = '$' then
           Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
           raise Parsing_Error;
         end if;
      else
        Skip_Token ("-");
        Ident := new String'("-" & Parse_Identifier.all);   --QQ
      end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
    --    id = id
    --
    if Current_Item = "=" then
       Prefix.Op := EQ;
       --Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token("=");
      if Current_Item /= "-" then
         Ident :=  Parse_Identifier;
         if Ident(1) = '$' then
           Give_Error ("Variable not allowed in the context (" &
              Ident.all &  ") (use % instead of $)");
           raise Parsing_Error;
         end if;
      else
       Skip_Token ("-");
       Ident := new String'("-" & Parse_Identifier.all);   --QQ
      end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
      --
      --  getting main label      label(p1,p2,..)
      --
      -- OOPs  so far  Is_Abstract_StatePredicate  ALWAYS returns True ...
      --
      if not Is_Abstract_StatePredicate(Ident.all) then
         Give_Error("Undefined Abstract State Predicate """ &  Current_Item & """");
         raise Parsing_Error;
      end if;

     if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
     end if;
     if Ident(1) in 'a'..'z' or else
        Ident(1) in 'A'..'Z' then
        --Ident := Parse_Identifier;
        Ids := new String_Table'(1..1 => Ident);
        if To_Upper(Ident.all) = "PRINT_ONCE" then
             Formula_contains_PRINT := True;
        end if;
      --
      -- getting the last labels
      --
      if  Current_Item = "(" then
        Skip_Token ("(");
        while Current_Item /= ")" loop
        --------------  parse abstract literal expression --------
        if Current_Item /= "," then
          -- parse abstract literal expression into a String
          if Tokens(Current_Token).Item(1) = '$' -- and then (not Var_Allowed) then
          then
            Give_Error ("Variable not allowed in the context");
            raise Parsing_Error;
          end if;
          if Tokens(Current_Token).Item.all /= "[" then
            Ids :=
             new String_Table'(Ids.all & Tokens(Current_Token).Item);
            Skip_Token;
          else
            declare
              ThisLiteral: String(1..100);
              ThisIndex: Natural :=0;
            begin
              --  merge "[" "a" "," "b" "]" info   "[a,b,c]"   disallowing $a, %a, * inside
              while Tokens(Current_Token).Item.all /= "]" loop
                if Tokens(Current_Token).Item(1) = '$' or else
                   Tokens(Current_Token).Item(1) = '%' or else
                   Tokens(Current_Token).Item(1) = '*' then
                  Give_Error("Variables and wildchars not allowed in this context");
                  raise Parsing_Error;
                end if;
                if Tokens(Current_Token).Item.all="true" then
                    Tokens(Current_Token).Item(1) := 'T';
                end if;
                if Tokens(Current_Token).Item.all="false" then
                    Tokens(Current_Token).Item(1) := 'F';
                end if;
                ThisLiteral(ThisIndex+1.. ThisIndex+Tokens(Current_Token).Item.all'length) :=
                  Tokens(Current_Token).Item.all;
                ThisIndex := ThisIndex+Tokens(Current_Token).Item.all'length;
                Skip_Token;  -- "[",  "aaa", ","
                if Current_Item="" then
                    Give_Error ("Missing ""]"" ...");
                    raise Parsing_Error;
                end if;
              end loop;
              ThisLiteral(ThisIndex+1..ThisIndex+1) := "]";
              ThisIndex := ThisIndex+1;
              Skip_Token;  -- "]"
              Ids :=
                new String_Table'(Ids.all &
                  new String'(ThisLiteral(1..ThisIndex)));
            end;
          end if;
        else
          Skip_Token(",");
        end if;
        if Current_Item="" then
            Give_Error ("Missing "")"" ...");
            raise Parsing_Error;
        end if;
        end loop;
        Skip_Token (")");
      end if;
    end if;
    --
    if Ids.all 'Length =0 then
        Give_Error(" Empty Assertion?!?!?");
        raise Parsing_Error;
    end if;
    --
    Prefix.Right_Ids := Ids;
    return  Result;
    --
  end Parse_Abstract_Assertion;

  ---------------------------------------------------------------------------
  --  the general structure of an assertion  is:
  --   id.id.id LEFTOP id.id.id  OP id.id.id RIGHTOP id.id.id
  ---------------------------------------------------------------------------
  --     ---   CONCRETE COWS ASSERTION SYNTAX
  --     for COWS the possible structures are:
  --
  --       OP = EQ,   leftids =<v1>,  rightids=<v2>
  --     or
  --       OP=NOOP,   leftids= <term,part,op,mode,arg1,arg2,..>
  --     or
  --       OP=NOOP    leftis=<term>, righitids = <mainlabel,label1,label2,label3...>
  --      
  --     term:partner.op!<n,m>   GROUND
  --     partner.operation?<x,y> GROUND
  --     op?<n,m> label          GROUND
  --     op?                     GROUND
  --
  ---------------------------------------------------------------------------
  function COWS_Parse_Assertion  return Formula_Ref is
    Result: Formula_Ref := new Formula(Assertion);
    Prefix: Basic_Predicate_Ref := new Basic_Predicate;
    Ident: String_Ref;
    Ids: String_Table_Ref := new String_Table(1..0);
  begin 
    Result.Pred := Prefix;
    --  
    --    %id = id
    --
    if Current_Item(1) = '%' and then Next_Item = "=" then
       Prefix.Op := EQ;
       Ident :=  Parse_Identifier;
       if Ident(1) = '$' then 
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if; 
       Prefix.Left_Index := Ident;
       Skip_Token("=");
       Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
    --    %id > id
    --
    if Current_Item(1) = '%' and then Next_Item = ">" then
       Prefix.Op := GT;
       Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
       Prefix.Left_Index := Ident;
       Skip_Token("=");
       Ident :=  Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
       Prefix.Right_Index := Ident;
       Return Result;
    end if;
    --
      --  getting term
      --
      if Next_Item = ":"  then
        Ident := Parse_Identifier;
        if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
        Ids := new String_Table'(1..1 => Ident);
        Skip_Token(":");
      else 
        Ids := new String_Table'(1..1 => null);
      end if;
      if Tokens(Current_Token).Kind /= Id and then
        (Next_Item /= "." and then
         Next_Item /= "?" and then
         Next_Item /= "!" and then
         Next_Item /= "(" )  then
         -- assertion ends here 
        Prefix. Left_Ids := Ids;
        return Result;
      end if;
      --
      -- getting partner
      --
      if Next_Item = "." then
        Ident := Parse_Identifier;
        if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
        Ids := new String_Table'(Ids.all & Ident);
        Skip_Token(".");
      end if;
      --
      if Ids.all'Length >1 and then    -- GROUND   Term: part.
          (Tokens(Current_Token).Kind /= Id or else
            ( Next_Item /= "?" and then
              Next_Item /= "!" )) then
         Give_Error(" Operation and modality expected!");
         raise Parsing_Error;
      end if;
      --
      -- getting operation, modes and params
      --
      if Next_Item = "!" or else Next_Item = "?" then
        if Ids.all'Length = 1 then
           Ids := new String_Table'(Ids.all & null);
        end if;
        Ident := Parse_Identifier;
        if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
        Ids := new String_Table'(Ids.all & Ident);
        Ids := new String_Table'(Ids.all & Tokens(Current_Token).Item); -- "!" or  "?" 
        --
        Skip_Token;
        --
        -- getting params
        --
        if Current_Item="<" then
          Skip_Token ("<");
          while Current_item /= ">"  and Current_item /= ">>" loop
          if Current_item /= "-" then
            Ident := Parse_Identifier;
            if Ident(1) = '$' then
               Give_Error ("Variable not allowed in the context");
               raise Parsing_Error;
            end if;
          else
            Skip_Token;
            Ident := new String'("-" & Parse_Identifier.all);
            end if;
            Ids := new String_Table'(Ids.all & Ident);
            if Current_Item ="," then
              Skip_Token(",");
            end if;
          end loop;
          if Current_Item=">" then
            Skip_Token(">");
          else
            -- change the incorrectly parsed ">>" token into the ">" ">" sequence.
            Tokens(Current_Token).Item := new String'(">");
          end if;
        end if;
      end if;
      --
      if Ids.all'Length > 1 then
         -- GROUND assertion ends here
         Prefix. Left_Ids := Ids;
         return Result; 
      end if;
      --
      if Ids.all'Length = 1 and then
         Tokens(Current_Token).Kind /= Id then
         Give_Error("Incomplete assertion ... ");
         raise Parsing_Error;
      end if;
      --
      -- store the term:  prefix of abstract predicate
      --
      if Ids.all'Length = 1 then
        Prefix. Left_Ids := Ids;
        Ids := new String_Table(1..0);
      end if;
      --
      --  getting main label      label(p1,p2,..)
      --
     if Tokens(Current_Token).Kind = Id then
    if Current_item /= "-" then
       Ident := Parse_Identifier;
       if Ident(1) = '$' then
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if;
    else
        Skip_Token;
        Ident := new String'("-" & Parse_Identifier.all);
        end if;
        Ids := new String_Table'(Ids.all & Ident);
     end if;
     --
     -- getting the last labels
     --
     if  Current_Item = "(" then
        Skip_Token ("(");
        while Current_Item /= ")" loop
          -------------------------
        if Current_Item /= "," then
          -- parse abstract literal expression into a String
          if Tokens(Current_Token).Item(1) = '$' -- and then (not Var_Allowed) then
          then
            Give_Error ("Variable not allowed in the context");
            raise Parsing_Error;
          end if;
          if Tokens(Current_Token).Item.all /= "[" then
            Ids :=
             new String_Table'(Ids.all & Tokens(Current_Token).Item);
            Skip_Token;
          else
            declare
              ThisLiteral: String(1..100);
              ThisIndex: Natural :=0;
            begin
              --  merge "[" "a" "," "b" "]" info   "[a,b,c]"   disallowing $a, %a, * inside
              while Tokens(Current_Token).Item.all /= "]" loop
                if Tokens(Current_Token).Item(1) = '$' or else
                   Tokens(Current_Token).Item(1) = '%' or else
                   Tokens(Current_Token).Item(1) = '*' then
                  Give_Error("Variables and wildchars not allowed in this context");
                  raise Parsing_Error;
                end if;
                if Tokens(Current_Token).Item.all="true" then
                    Tokens(Current_Token).Item(1) := 'T';
                end if;
                if Tokens(Current_Token).Item.all="false" then
                    Tokens(Current_Token).Item(1) := 'F';
                end if;
                ThisLiteral(ThisIndex+1.. ThisIndex+Tokens(Current_Token).Item.all'length) :=
                  Tokens(Current_Token).Item.all;
                ThisIndex := ThisIndex+Tokens(Current_Token).Item.all'length;
                Skip_Token;  -- "[",  "aaa", ","
                if Current_Item="" then
                    Give_Error ("Missing ""]"" ...");
                    raise Parsing_Error;
                end if;
              end loop;
              ThisLiteral(ThisIndex+1..ThisIndex+1) := "]";
              ThisIndex := ThisIndex+1;
              Skip_Token;  -- "]"
              Ids :=
                new String_Table'(Ids.all &
                  new String'(ThisLiteral(1..ThisIndex)));
            end;
          end if;
        else
          Skip_Token(",");
        end if;
        if Current_Item="" then
            Give_Error ("Missing "")"" ...");
            raise Parsing_Error;
        end if;
--        -------------------------
--        -- parse the predicate abstract literal expression
--        Ident := Parse_Identifier;
--       if Ident(1) = '$' then
--          Give_Error ("Variable not allowed in the context");
--          raise Parsing_Error;
--        end if;
--        Ids := new String_Table'(Ids.all & Ident);
--        if Current_Item ="," then
--          Skip_Token(",");
--        end if;
--        -------------
        end loop;
        Skip_Token (")");
     end if;
     -- 
     if Ids.all 'Length =0 then
        Give_Error(" Empty Assertion?!?!?");
        raise Parsing_Error;
     end if;
     --
     Prefix.Right_Ids := Ids;
     return  Result;
    --
  end COWS_Parse_Assertion;
---------------------------------------------------------------------------


 -----------------------------------------------------------------
  --  assertion is:
  --   obj.x.y.z = num
  --   obj.x.y  > obj.z.w
  --   obj.x.y  < obj.z.w
  --         aggiungere  GE, LE, NE
  --     aggiungere   ids =  ids + ids
  --     aggiungere   ids =  ids - ids
  --     aggiungere  obj'.ids.ids
  --
  --    altri casi:
  --       obj.x[0] > 2
  --       obj.x =  [onj,on2.on3]
  --       objw.u > nn[2]
  --       obj1.x.length > nj2.x.length
  --
  --    obj.x[1]   parsed come:   ["obj","x","1"]
--???  --     x=  [obj1,obj2,obj3]  parsed come ["x"] "EQ"  ["#", "obj1","obj2","obj3"]
--???  --     x=  []  parsed come ["x"]  "EQ"  ["#"]
  --
  --   IsActive(obj,top.s1)
  -----------------------------------------------------------------
 function UML_Parse_Assertion return Formula_Ref is
    Result: Formula_Ref := new Formula(Assertion);
    Prefix : Basic_Predicate_Ref := new Basic_Predicate;
    Ident: String_Ref;
  begin
    Result.Pred := Prefix;
    --  
    --    %id = id
    --
    if Current_Item(1) = '%' and then Next_Item = "=" then
       Prefix.Op := EQ;
       Ident :=  Parse_Identifier;
       if Ident(1) = '$' then 
          Give_Error ("Variable not allowed in the context");
          raise Parsing_Error;
        end if; 
       Prefix.Left_Index := Ident;
       Skip_Token("=");
       if Current_Item /= "-" then
         Ident :=  Parse_Identifier;
         if Ident(1) = '$' then
            Give_Error ("Variable not allowed in the context");
            raise Parsing_Error;
          end if;
         Prefix.Right_Index := Ident;
       else
         Ident := new String'("-" & Parse_Identifier.all);
         Prefix.Right_Index := Ident;
       end if;
       Return Result;
    end if;
    --
    Prefix.Left_Ids := new String_Table'(1..1 => Parse_Identifier);
    if Prefix.Left_ids(1).all(1) = '$' then
        Give_Error ("Variable not allowed in the context");
        raise Parsing_Error;
     end if;
    --
    --  if Abstractions {} are defined, the only allowed assertion is:
    --     accepting_request (interaction, args, args)
    --
        if Current_Item = "(" then
          Skip_Token ("(");
       else
          Give_Error("Error:  synatx is accepting_request(interaction,args,args,..)");
          raise Parsing_Error;
       end if;
       while Current_Item /= ")" loop
         Ident := Parse_Identifier;
         if Ident(1) = '$' then
            Give_Error ("Variable not allowed in the context");
            raise Parsing_Error;
         end if;
         Prefix.Left_Ids := new String_Table'(Prefix.Left_Ids.all & Ident);
         if Input_line(Current_Position) = ',' then
           Skip_Token (",");
         end if;
       end loop;
       Skip_Token (")");
       return Result;

  end UML_Parse_Assertion;
  -----------------------------------------------------------------
  -----------------------------------------------------------------




---------------------------------------------------------------------------
  function Parse_Assertion  return Formula_Ref is
  begin
    return Parse_Abstract_Assertion;
--    if Current_Flavour = COWS and then not Use_Abstractions then 
--          return COWS_Parse_Assertion;
--    elsif Current_Flavour = UML and not Use_Abstractions then
--          return UML_Parse_Assertion;
--    elsif Use_Abstractions then
--        return Parse_Abstract_Assertion;
--    else
--        return null;
--    end if;
  end Parse_Assertion;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  function Is_fixpoint (This: String; Env : String_Table) return Boolean is
  begin
    for I in Env'Range loop
      if This=Env(I).all then return True; end if;
    end loop;
    return False;
  end Is_fixpoint;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  function Parse_Formula (Env: String_Table;
                          Context_Size: Natural; 
                          Is_Unary: Boolean := False;
                          Is_Top: Boolean := False) return Formula_Ref is
    Form1: Formula_Ref;
    Act1: Action_Ref;
    Token, Ident : String_Ref;
    Prefix: Formula_Ref;
  begin
    --
    if Current_Item = "" then
        return True_Formula; -- will be used only for tail completing current formula
    -- 
    --    ~ sform
    -- 
    elsif  Current_Item = "~" then
       Skip_Token ("~");
        Prefix := 
          new Formula'(Fnot,null,0,0,null,null,null,Parse_Formula(Env,Context_Size,True));
       --
    elsif To_Lower(Current_Item) ="not" then
       Skip_Token;
        Prefix :=
          new Formula'(Fnot,null,0,0,null,null,null,Parse_Formula(Env,Context_Size,True));
    -- 
    -- ( sform )
    -- 
    elsif Current_Item = "(" then
       Skip_Token ("(");
       Form1 := Parse_Formula(Env,Context_Size);
       Skip_Token (")");
       Prefix :=  Form1;
    -- 
    -- < aform > sform
    -- 
    elsif Current_Item= "<" then 
      Skip_Token ("<");
      if Current_Item /= ">" then
        Act1 := Parse_Action (True, True, False);
      else
        Act1 := True_Action; -- new Action'(Kind => Atrue);
      end if;
      Skip_Token (">");
--      if Product_Families and Current_Item = "#" then 
      if Current_Item = "#" then 
        declare
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
          newact: Action_Ref := new Action'(Aand, Act1,notmay);
        begin
          mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
         Act1 := newact; 
        end; 
      end if;
      if Current_Item = "#" then
        Skip_Token("#");
      end if;
      Form1 := Parse_Formula(Env,Context_Size,True);
      if Form1 = null then Form1 := True_Formula; end if;
      Prefix :=  new Formula'(Fangle,null,0,0,null,null,null,Act1, null, Form1); 
    --
    -- << aform >> sform
    --
    elsif Current_Item = "<<" then 
      Skip_Token ("<<");
      Act1 := Parse_Action (False,True,False);
      Skip_Token (">>");
      Form1 := Parse_Formula(Env,Context_Size,True);
      if Form1 = null then Form1 := True_Formula; end if;
      Prefix :=  new Formula'(FWangle,null,0,0,null,null,null,Act1, null, Form1);
    -- 
    -- [ aform ] sform
    -- 
    elsif Current_Item= "[" then
       Skip_Token ("[");
      if Current_Item /= "]" then
        Act1 := Parse_Action (True,True,False);
      else
        Act1 := True_Action; -- new Action'(Kind => Atrue);
      end if;
       Skip_Token ("]");
--      if Product_Families and Current_Item = "#" then 
      if Current_Item = "#" then 
        declare
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
          newact: Action_Ref := new Action'(Aand, Act1,notmay);
        begin
         mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
         Act1 := newact;
        end;
      end if;
      if Current_Item = "#" then
        Skip_Token("#");
      end if;
       Form1 := Parse_Formula(Env,Context_Size,True);
       Prefix :=  
        new Formula'(Fsquare,null,0,0,null,null,null,Act1, null, Form1);
    --
    -- [[ aform ]] sform
    --
    elsif Current_Item= "[[" then
       Skip_Token ("[[");
       Act1 := Parse_Action (False,True,False);
       if Act1= null then
         Give_Error( "Missing action expression ?!");
         raise Parsing_Error;
       end if;
       Skip_Token ("]]");
       Form1 := Parse_Formula(Env,Context_Size,True);
      if Form1 = null then Form1 := True_Formula; end if;
       Prefix :=
        new Formula'(FWsquare,null,0,0,null,null,null,Act1, null, Form1);
    -- 
    --    (  
    --
    elsif Current_Item = "{" then
      -- if not explicit formula is given use "true" as default
      Prefix :=  True_Formula;
    -- 
    --    ]  
    --
    elsif Current_Item = "]" then
      -- if not explicit formula is given use "true" as default
      Prefix :=  True_Formula;
    -- 
    --    {  -- error !
    --
    elsif Current_Item = "{" then
      -- if not explicit formula is given use "true" as default
      Prefix :=  True_Formula;
    --    Give_Error ("Unexpected ""{"" ( maybe there is a missing EX or AX ?"); 
    --     raise Parsing_Error;
    --
    --
    -- true,  false, assert(..),  max Z , min Y ,  A, E, AF, EG,  AX, Z, Y 
    --
    -- 
    --  true
    -- 
    elsif To_Lower(Current_Item) = "true" then
       Skip_Token;
       Prefix :=  True_Formula;
       -- new Formula'(Ftrue,null,0,0,null,null,null);
    -- 
    -- false
    -- 
    elsif To_Lower(Current_Item) = "false" then
        Skip_Token;
        Prefix :=  False_Formula;  --  new Formula'(Ffalse,null,0,0,null,null,null);
    --   
    -- ASSERT
    -- 
    elsif To_Upper(Current_Item) = "ASSERT" then
        Skip_Token;
        Skip_Token ("(");
        Prefix := Parse_Assertion;
        Skip_Token (")");
    -- 
    -- max IDENT : sform
    -- 
    elsif To_Lower(Current_Item) = "max" then
        Skip_Token;
        Ident := Tokens(Current_Token).Item;
        Skip_Token;
        Skip_Token(":");
        Form1 := Parse_Formula(Env & Ident, Context_Size+1,True);
        Prefix :=  new Formula'(Fmax,null,0,0,null,null,null,Ident,Form1,Context_Size);
    --   
    -- min IDENT : sform
    -- 
    elsif To_Lower(Current_Item) = "min" then
        Skip_Token;
        Ident := Tokens(Current_Token).Item;
        Skip_Token;
        Skip_Token(":");
        Form1 := Parse_Formula(Env & Ident, Context_Size+1,True);
        Prefix := new Formula'(Fmin,null,0,0,null,null,null,Ident,Form1,Context_Size);
    --   
    -- A pform
    -- 
    elsif Current_Item = "A" then
        Skip_Token;
        -- 
        Prefix :=  
          new Formula'(Fall,null,0,0,null,null,null,Parse_Path(Env,Context_Size));
--        if Prefix.PRef.Kind = Until2 then
--           Prefix.PRef.YorF2.RightRef := Prefix;
--        end if;
    -- 
    -- E pform
    -- 
    elsif Current_Item = "E" then
        Skip_Token;
        Prefix :=  
           new Formula'(Fexist,null,0,0,null,null,null,Parse_Path(Env,Context_Size));
    -- 
    -- AF sform
    -- 
    elsif Current_Item = "AF" and then
          Next_Item /= "#" and then
          Next_Item /= "{" then
        Skip_Token("AF");
        Form1 := Parse_Formula(Env,Context_Size,True);
        if Form1 = null then Form1 := True_Formula; end if;
        Prefix :=  new Formula'(Fall,null,0,0,null,null,null,
                     new Path'(Eventually,Form1));
    --
    -- AF# sform
    --
    elsif Current_Item = "AF" and then
          Next_Item = "#" and then
          Next_Next_Item /= "{" then
          Skip_Token ("AF");
          Skip_Token ("#");
--       if Product_Families then
         declare
            Form1 : Formula_Ref := Parse_Formula(Env, Context_Size,True);
            mayact: Action_Ref := new Action(Aid);
            notmay: Action_Ref := new Action'(Anot, mayact);
         begin
           mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
           Prefix := new Formula'(Fall,null,0,0,null,null,null,
                       new Path'(Until1, True_Formula, notmay,Form1));
         end;
--       else
--        -- just ignore the "#"
--        Prefix :=  new Formula'(Fall,null,0,0,null,null,null,
--                     new Path'(Eventually,Parse_Formula(Env,Context_Size,True)));
--       end if;

    --
    -- AF {...} sform
    --
    elsif Current_Item = "AF" and then
          Next_Item = "{" then
        Skip_Token ("AF");
        SkiP_Token ("{");
        Prefix :=  new Formula'(Fall,null,0,0,null,null,null, 
               new Path' (Until2, True_Formula, True_Action,null,null));
        Prefix.Pref.U2ARef2 := Parse_Action (True,True,False);
         Skip_Token ("}");
         Form1 := Parse_Formula(Env,Context_Size,True);
         if Form1 = null then Form1 := True_Formula; end if;
         Prefix.Pref.U2FormRef2 := Form1;
    --
    -- AF# {} sform
    --
    elsif Current_Item = "AF" and then
          Next_Item = "#" and then
--          Product_Families and then
          Next_Next_Item = "{"then
        Skip_Token ("AF");
        Skip_Token ("#");
        Skip_Token ("{");
        --  Prefix :=  new Formula'(Fexist,null,0,0,null,null,null,
        --               new Path'(Eventually,Parse_Formula(Env,Context_Size,True)));
--     if Product_Families then
       declare
          Act2: Action_Ref := Parse_Action (True,True,False);
          Form2 : Formula_Ref;
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
          newact: Action_Ref := new Action'(Aand, Act2,notmay);
        begin
          Skip_Token ("}");
          Form2 := Parse_Formula(Env, Context_Size,True);
          mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
          Prefix := new Formula'(Fall,null,0,0,null,null,null,
                     new Path'(Until2, True_Formula, notmay,newact,Form2));
        end;
--      else
--        -- just ignore the "#"
--        Prefix :=  new Formula'(Fall,null,0,0,null,null,null,
--               new Path' (Until2, True_Formula, True_Action,null,null,null,null));
--        Prefix.Pref.U2ARef2 := Parse_Action (True,True,False);
--         Skip_Token ("}");
--         Prefix.Pref.U2FormRef2 := Parse_Formula(Env,Context_Size,True);
--      end if;
    -- 
    -- EF sform
    -- 
    elsif Current_Item = "EF" and then
          Next_Item /= "#" and then
          Next_Item /= "{" then
        Skip_Token ("EF");
        Prefix :=  new Formula'(Fexist,null,0,0,null,null,null,
                       new Path'(Eventually,Parse_Formula(Env,Context_Size,True)));
    --
    -- EF {...} sform
    --
    elsif Current_Item = "EF" and then
         Next_Item = "{" then
         Skip_Token ("EF");
         Skip_Token ("{");
         Prefix :=  new Formula'(Fexist,null,0,0,null,null,null, 
               new Path' (Until2, True_Formula, True_Action,null,null));
         Prefix.Pref.U2ARef2 := Parse_Action (True,True,False);
         Skip_Token ("}");
         Form1 := Parse_Formula(Env,Context_Size,True);
         if Form1 = null then Form1 := True_Formula; end if;
         Prefix.Pref.U2FormRef2 := Form1;
    --
    -- EF# sform
    --
    elsif Current_Item = "EF" and then
          Next_Item = "#" and then
          Next_Next_Item /= "{"then
        Skip_Token ("EF");
        Skip_Token ("#");
        --  Prefix :=  new Formula'(Fexist,null,0,0,null,null,null,
        --               new Path'(Eventually,Parse_Formula(Env,Context_Size,True)));
--     if Product_Families then
       declare
          Form1 : Formula_Ref := Parse_Formula(Env, Context_Size,True);
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
       begin
         mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
         Prefix := new Formula'(Fexist,null,0,0,null,null,null,
                     new Path'(Until1, True_Formula, notmay,Form1));
       end;
--     else
--       -- ignore the #
--        Prefix :=  new Formula'(Fexist,null,0,0,null,null,null,
--                       new Path'(Eventually,Parse_Formula(Env,Context_Size,True)));
--     end if;
    --
    -- EF# {} sform
    --
    elsif Current_Item = "EF" and then
          Next_Item = "#" and then
          Next_Next_Item = "{"then
        Skip_Token ("EF");
        Skip_Token ("#");
        Skip_Token ("{");
        --  Prefix :=  new Formula'(Fexist,null,0,0,null,null,null,
        --               new Path'(Eventually,Parse_Formula(Env,Context_Size,True)));
--     if Product_Families then
       declare
          Act2: Action_Ref := Parse_Action (True,True,False);
          Form2 : Formula_Ref;
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
          newact: Action_Ref := new Action'(Aand, Act2,notmay);
        begin
          Skip_Token ("}");
          Form2 := Parse_Formula(Env, Context_Size,True);
          mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
          Prefix := new Formula'(Fexist,null,0,0,null,null,null,
                     new Path'(Until2, True_Formula, notmay,newact,Form2));
        end;
--      else
--        -- ignore the #
--         Prefix.Pref.U2ARef2 := Parse_Action (True,True,False);
--         Skip_Token ("}");
--         Prefix.Pref.U2FormRef2 := Parse_Formula(Env,Context_Size,True);
--      end if;
    -- 
    -- AG sform
    -- 
    elsif Current_Item = "AG" then
         Skip_Token;
        Prefix :=  new Formula'(Fall,null,0,0,null,null,null,
                         new Path'(Always,Parse_Formula(Env,Context_Size,True)));
    --
    -- AG {...} sform ????
    --
    -- 
    -- EG sform
    -- 
    elsif Current_Item = "EG" then
         Skip_Token;
          Prefix :=  new Formula'(Fexist,null,0,0,null,null,null,
                        new Path'(Always,Parse_Formula(Env,Context_Size,True)));
    -- 
    -- AT sform
    -- 
      elsif Current_Item = "AT" then
         Skip_Token;
        Prefix :=  
          new Formula'(Fall,null,0,0,null,null,null,
               new Path'(Act_Next,null,Parse_Formula(Env,Context_Size,True)));
    -- 
    -- ET sform
    -- 
    elsif Current_Item = "ET" then
         Skip_Token;
        Prefix :=  
          new Formula'(Fexist,null,0,0,null,null,null,
              new Path'(Act_Next,null, Parse_Formula(Env,Context_Size,True)));
      -- 
      -- AX  { aform } sform
    -- 
    elsif Current_Item = "AX" and then
          Next_Item /= "#" then
         Skip_Token("AX");
         if Current_Item = "{" then
           Skip_Token ("{");
           Act1:= Parse_Action (True,True,False);
           Skip_Token ("}");
         else
            Act1 := True_Action; --  new Action'(kind =>Atrue);
         end if;
         Form1 := Parse_Formula(Env,Context_Size,True);
        Prefix :=  
         new Formula'(Fall,null,0,0,null,null,null,new Path'(Act_Next,Act1,Form1));
      --
      -- AX# { aform } sform
      --
    elsif Current_Item = "AX" and then
          Next_Item = "#" then
--          Next_Item = "#" and then
--          Product_Families then
         Skip_Token("AX");
         Skip_Token("#");
         if Current_Item = "{" then
           Skip_Token ("{");
           Act1:= Parse_Action (True,True,False);
           Skip_Token ("}");
         else
            Act1 := True_Action; --  new Action'(kind =>Atrue);
         end if;
--       if Product_Families then
         declare
           mayact: Action_Ref := new Action(Aid);
           notmay: Action_Ref := new Action'(Anot, mayact);
           newact: Action_Ref := new Action'(Aand, Act1,notmay);
         begin
           mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
           Act1 := newact;
         end;
--       end if;
         Form1 := Parse_Formula(Env,Context_Size,True);
         Prefix :=
          new Formula'(Fall,null,0,0,null,null,null,new Path'(Act_Next,Act1,Form1));
    -- 
    -- EX  { aform } sform
    -- 
    elsif Current_Item = "EX" and then
         Next_Item /= "#" then
         Skip_Token;
         if Current_Item = "{" then
           Skip_Token ("{");
           Act1:= Parse_Action (True,True,False);
           Skip_Token ("}");
         else
           Act1 := True_Action; --  new Action'(kind =>Atrue);
         end if;
         Form1 := Parse_Formula(Env,Context_Size,True);
         if Form1 = null then Form1 := True_Formula; end if;
        Prefix :=  
          new Formula'(Fexist,null,0,0,null,null,null,new Path'(Act_Next,Act1,Form1));
      --
      -- EX# { aform } sform
      --
    elsif Current_Item = "EX" and then
          Next_Item = "#" then
--          Next_Item = "#" and then
--          Product_Families then
         Skip_Token("EX");
         Skip_Token("#");
         if Current_Item = "{" then
           Skip_Token ("{");
           Act1:= Parse_Action (True,True,False);
           Skip_Token ("}");
         else
            Act1 := True_Action; --  new Action'(kind =>Atrue);
         end if;
--       if Product_Families then
         declare
           mayact: Action_Ref := new Action(Aid);
           notmay: Action_Ref := new Action'(Anot, mayact);
           newact: Action_Ref := new Action'(Aand, Act1,notmay);
         begin
           mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
           Act1 := newact;
         end;
--        end if;
         Form1 := Parse_Formula(Env,Context_Size,True);
         if Form1 = null then Form1 := True_Formula; end if;
         Prefix :=
          new Formula'(Fexist,null,0,0,null,null,null,new Path'(Act_Next,Act1,Form1));
    -- 
    -- IDENT
    --
    elsif To_Upper(Current_Item)="FINAL" then
      Skip_Token;
      Prefix :=  
          new Formula'(Fapply,null,0,0,null,null,null,new String'("FINAL"), null, null);
    elsif To_Upper(Current_Item)="COUNT" then
      Skip_Token;
      Formula_contains_COUNT := True;
      Prefix :=  
          new Formula'(Fapply,null,0,0,null,null,null,new String'("COUNT"), null, null);
    elsif To_Upper(Current_Item)="PRINT" then
      Formula_contains_PRINT := True;
      Skip_Token;
      if Current_Item /= "(" then
        Prefix :=  
          new Formula'(Fapply,null,0,0,null,null,null,new String'("PRINT"), null, null);
      else
        declare
          Optargs: String_Table_Ref := Empty_String_Table_Ref;
        begin
          Skip_Token ("(");
          while Current_Item /= ")" loop
            if Current_Item /= "," then
              Optargs := new String_Table'(Optargs.all & Tokens(Current_Token).Item);
              Skip_Token;
            else
              Skip_Token(",");
            end if;
            if Current_Item="" then
                Give_Error ("Missing "")"" ...");
                raise Parsing_Error;
            end if;
          end loop;
          Prefix :=  
            new Formula'(Fapply,null,0,0,null,null,null,new String'("PRINT"), null, Optargs);
          Skip_Token (")");
        end;
      end if;
    elsif Is_Fixpoint (Current_Item, Env) then
      Token :=  Tokens(Current_Token).Item;
      Skip_Token;
      Prefix :=  new Formula'(Fapply,null,0,0,null,null,null,Token, null, null);
    --
    --  IMPLICIT true
    --
    elsif Current_Item = "" then 
       -- when no explicit formula is present use "true" as default           
       Prefix :=  True_Formula;
    --
    -- ASSERTION   (%var = id)  (val1 > val2)  ...
    -- ASSERTION   Pred   Pred(val,val)
    -- ASSERTION   PRINT_ONCE(val,val)
    --
    else
        Prefix := Parse_Assertion;
    end if;
    --
    if Prefix = null then
       -- if no explit formula is given use "true" as default
       Prefix :=  True_Formula;
       --  Give_Error(" Formula expected but not found.");
       -- raise Parsing_Error;
    end if;
    --
    if Current_Item = "" then
       return Prefix;
    end if;
    --
    --  continue with a binary formula operator  (if any) 
    --   or maybe the final '.'
    -- 
    --  sform & sform
    -- 
    if  Current_Item = "&"  and not Is_Unary then
       Skip_Token ("&");
        Form1 := Parse_Formula(Env,Context_Size);
        return new Formula'(Fand,null,0,0,null,null,null,Prefix, Form1);
        --
    elsif To_Lower(Current_Item) = "and" and then
           not Is_Unary then
       Skip_Token;
        Form1 := Parse_Formula(Env,Context_Size);
        return new Formula'(Fand,null,0,0,null,null,null,Prefix, Form1);
    -- 
    -- sform | sform
    -- 
    elsif Current_Item = "|" and not Is_Unary then
       Skip_Token ("|");
        Form1 := Parse_Formula(Env,Context_Size);
        return new Formula'(Foor,null,0,0,null,null,null,Prefix, Form1);
        --
    elsif To_Lower(Current_Item) = "or"  and then 
          not Is_Unary then
       Skip_Token;
        Form1 := Parse_Formula(Env,Context_Size);
        return new Formula'(Foor,null,0,0,null,null,null,Prefix, Form1);
    -- 
    -- sform -> sform
    -- 
    elsif Current_Item = "->" and not Is_Unary then
       Skip_Token ("->");
        Form1 := Parse_Formula(Env,Context_Size);
        return new Formula'(Fimply,null,0,0,null,null,null,Prefix, Form1);
        --
    elsif  To_Lower(Current_Item) = "implies" and then
          not Is_Unary then
       Skip_Token;
        Form1 := Parse_Formula(Env,Context_Size);
        return new Formula'(Fimply,null,0,0,null,null,null,Prefix, Form1);
    -- 
--    elsif Is_Top then
--      Give_Error(" Unexpected Garbage at end of formula");
--      raise Parsing_Error;
    else
        return Prefix;
    end if;
  end Parse_Formula;
---------------------------------------------------------------------------


---------------------------------------------------------------------------
  function Parse_Path(Env: String_Table; Context_Size:Natural)  return Path_Ref is
    Form1, Form2: Formula_Ref;
    Act1, Act2: Action_Ref;
    IsWeak: Boolean;
  begin
    --
    -- F {} sform
    -- 
    if Current_Item  = "F" and then
       Next_Item = "{" then
       Skip_Token ("F");
       Skip_Token ("{");
       Act2 := Parse_Action (True,True,False);
       Skip_Token ("}");
       Form2 := Parse_Formula(Env,Context_Size,True);
       return new Path'(Until2, True_Formula, True_Action,Act2,Form2);
    --
    -- F sform
    --
    elsif Current_Item  = "F" and then
       Next_Item /= "#"  and then
       Next_Item /= "{"  then
       Skip_Token ("F");
       Form1 := Parse_Formula(Env, Context_Size,True);
       return new Path'(Eventually,Form1);
    --
    -- F# {} sform
    --
    elsif Current_Item  = "F" and then
       Next_Item = "#" and then
       Next_Next_Item = "{" then
       Skip_Token ("F");
       Skip_Token ("#");
       Skip_Token ("{");
       Act2 := Parse_Action (True,True,False);
       Skip_Token ("}");
       Form2 := Parse_Formula(Env,Context_Size,True);
--      if Product_Families then
        declare
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
          newact: Action_Ref := new Action'(Aand, Act2,notmay);
        begin
          mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
          Act2 := newact;
          return new Path'(Until2, True_Formula, notmay,Act2,Form2);
        end;
--      else
--       return new Path'(Until2, True_Formula, True_Action,null,Act2,null,Form2);
--      end if;
    --
    -- F#  sform
    --
    elsif  Current_Item  = "F" and then
       Next_Item = "#"  and then
       Next_Next_Item /= "{"  then
       Skip_Token ("F");
       Skip_Token ("#");
       Form1 := Parse_Formula(Env, Context_Size,True);
--      if Product_Families then
       declare
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
       begin
         mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
         return new Path'(Until1, True_Formula, notmay,Form1);
       end;
--      else
--       return new Path'(Eventually,Form1);
--      end if;
    -- 
    -- G sform
    -- 
    elsif Current_Item = "G" then
       Skip_Token ("G");
       Form1 := Parse_Formula(Env, Context_Size,True);
       return new Path'(Always,Form1);
    -- 
    -- T sform
    -- 
    elsif Current_Item = "T" then
       Skip_Token ("T");
       Form1 := Parse_Formula(Env, Context_Size,True);
       return new Path'(Act_Next,null,Form1);
    -- 
    -- X { aform } sform
    -- 
    elsif Current_Item = "X" and then
          Next_Item /= "#" then
       Skip_Token ("X");
       if Current_Item  = "{" then
         Skip_Token ("{");
         Act1:= Parse_Action (True,True,False);
         Skip_Token ("}");
       else
         Act1 := True_Action; --   new Action'(kind =>Atrue);
       end if;
       Form1 := Parse_Formula(Env, Context_Size,True);
       return new Path'(Act_Next, Act1, Form1);
    --
    -- X# {aform } sform
    --
    elsif Current_Item = "X" and then
         Next_Item = "#" then
       Skip_Token ("X");
       Skip_Token ("#");
       if Current_Item  = "{" then
         Skip_Token ("{");
         Act1:= Parse_Action (True,True,False);
         Skip_Token ("}");
       else
         Act1 := True_Action; --   new Action'(kind =>Atrue);
       end if;
--      if Product_Families then
        declare
          mayact: Action_Ref := new Action(Aid);
          notmay: Action_Ref := new Action'(Anot, mayact);
          newact: Action_Ref := new Action'(Aand, Act1,notmay);
        begin
          mayact.AidPred.Labels := new String_Table'(1..1 => new String'("may"));
          Act1 := newact;
        end;
--      end if;
       Form1 := Parse_Formula(Env, Context_Size,True);
       return new Path'(Act_Next, Act1, Form1);
    -- 
    -- [ sform { aform } U { aform } sform ]
    -- [ sform { aform } U sform ]
    -- [ sform { aform } W { aform } sform ]
    -- [ sform { aform } W sform ]
    -- 
    elsif Current_Item = "[" then
       Skip_Token ("[");
       Form1 := Parse_Formula(Env, Context_Size);
       if Current_Item = "{" then
         Skip_Token ("{");
         if ACTL_Compatibility then
           Act1:= Parse_Action (False,False,False);
         else
           Act1:= Parse_Action (True,False,False);
         end if;
         Skip_Token ("}");
       else
         Act1 := True_Action; --  new Action'(kind =>Atrue);
       end if;
       if Current_Item = "U" then
         Skip_Token ("U");
         IsWeak := False;
       else 
         Skip_Token ("W");
         IsWeak := True;
       end if;
       if Current_Item = "{" then
         Skip_Token ("{");
         Act2 := Parse_Action (True,True,False);
         Skip_Token ("}");
         Form2 := Parse_Formula(Env, Context_Size);
         Skip_Token ("]");
         if IsWeak then
           return new Path'(Wuntil2, Form1, Act1,Act2,Form2);
         else
           return new Path'(Until2, Form1, Act1,Act2,Form2);
         end if;
       else
         Form2 := Parse_Formula(Env, Context_Size);
         Skip_Token ("]");
         if IsWeak then
           return new Path'(Wuntil1, Form1, Act1, Form2);
         else
           return new Path'(Until1, Form1, Act1, Form2);
         end if;
       end if;
    else
      Give_Error ("Unexpected characters inside path formula");
      raise Parsing_Error;
    end if;
    --
  end Parse_Path;
  ---------------------------------------------------------------------------


  ---------------------------------------------------------------------------
  function Is_Tau (Action: String) return Boolean is
  begin
     if Action = "tau"  or
        Action  = "TAU"  then
       return True;
     elsif Action'Length >3 and then
          ( Action(Action'First..Action'First+3) = "tau("
            or Action(Action'First..Action'First+3) = "TAU(" ) then
        return True;
     else
        return False;
     end if;
  end Is_Tau;
  ---------------------------------------------------------------------------



  ---------------------------------------------------------------------------
  --   source:   abstract action 
  --
  --   request (interaction, args)
  --   request ()  -- explicitly without args
  --   response (interaction, args)
  --   cancel (interaction, args)
  --   done
  --
  --   args only can hold $vars
  --
  ---------------------------------------------------------------------------
  function Parse_Abstract_Action (Var_Allowed: Boolean) return Action_Ref is
    Result: Action_Ref;
    Prefix: Basic_Action_Ref;
  begin
    Result := new Action(Aid);
    Prefix := Result.AidPred;
    --
    -- get Source  (vars never allowed here)
    --
    if  Next_Item = ":" then
      Prefix.Source := Tokens(Current_Token).Item;
      if Prefix.Source(1) = '$' then
        Give_Error ("Variable not allowed in the context");
        raise Parsing_Error;
      end if;
      Skip_Token;
      Skip_Token(":");
    end if;
    --
    -- ABSTRACT ACTION prefix
    --
    if Current_Item ="}" or else    -- end of formula
       Current_Item =")"  or else  -- end of action
       Current_Item =">" or else    -- end of formula
       Current_Item =">>" or else    -- end of formula
       Current_Item ="" or else    -- end of formula
       Current_Item ="&"  or else    -- boolop
       Current_Item ="|"  or else    -- boolop
       Current_Item ="and"  or else    -- boolop
       Current_Item ="or"  or else    -- boolop
       Current_Item ="]" or else    -- end of formula
       Current_Item ="]]" then   -- end of formula
      return Result;
    end if;
    --
    -- initial main label
    --
    -- specific FMC case  (mail labels possibly:  "!aa"  or "?bb")
    --
    if Current_Item= "!" then
      Skip_Token;
      Prefix.Labels :=
        new String_Table'(1..1 =>
          new String'("!" & Tokens(Current_Token).Item.all));
      Skip_Token;
    elsif Current_Item= "?" then
      Skip_Token;
      Prefix.Labels :=
        new String_Table'(1..1 =>
          new String'("?" & Tokens(Current_Token).Item.all));
      Skip_Token;
      --
    elsif (Current_Item(1) in 'a'..'z' or else
        Current_Item(1) in '0'..'9' or else
        Current_Item(1) = '$' or else
        Current_Item(1) = '*' or else
        Current_Item(1) = '%' or else
        Current_Item(1) in 'A'..'Z')  and then
       Current_Item /="{" and then       --  begin of abstract params list
       Current_Item /="(" then       --  begin of abstract params list
--      if Tokens(Current_Token).Item(1) = '$' then
--        Give_Error ("Variable not allowed in the context");
--        raise Parsing_Error;
--      end if;
      Prefix.Labels :=
        new String_Table'(1..1 => Tokens(Current_Token).Item);
      --  Maybe we should check, analogously to abstract state predicates that the
      --    abstract label actually exists.
      Skip_Token;
    else
      Give_Error("Error: main abstract label expected after ground action");
      raise parsing_Error;
    end if;
    --
    if Current_Item = "(" then
      Skip_Token ("(");
      while Current_Item /= ")" loop
        if Current_Item /= "," then
          -- parse abstract literal expression into a String
          if Tokens(Current_Token).Item(1) = '$' and then
             (not Var_Allowed) then
            Give_Error ("Variable not allowed in the context");
            raise Parsing_Error;
          end if;
          if Tokens(Current_Token).Item.all /= "[" then 
            Prefix.Labels :=
             new String_Table'(Prefix.Labels.all & Tokens(Current_Token).Item);
            Skip_Token;
          else
            declare
              ThisLiteral: String(1..100);
              ThisIndex: Natural :=0;
            begin
              --  merge "[" "a" "," "b" "]" info   "[a,b,c]"   disallowing $a, %a, * inside 
              while Tokens(Current_Token).Item.all /= "]" loop
                if Tokens(Current_Token).Item(1) = '$' or else
                   Tokens(Current_Token).Item(1) = '%' or else
                   Tokens(Current_Token).Item(1) = '*' then
                  Give_Error("Variables and wildchars not allowed in this context");
                  raise Parsing_Error;
                end if;
                if Tokens(Current_Token).Item.all="true" then
                    Tokens(Current_Token).Item(1) := 'T';
                end if;
                if Tokens(Current_Token).Item.all="false" then
                    Tokens(Current_Token).Item(1) := 'F';
                end if;
                ThisLiteral(ThisIndex+1.. ThisIndex+Tokens(Current_Token).Item.all'length) := 
                  Tokens(Current_Token).Item.all; 
                ThisIndex := ThisIndex+Tokens(Current_Token).Item.all'length;
                Skip_Token;  -- "[",  "aaa", ","
                if Current_Item="" then
                    Give_Error ("Missing ""]"" ...");
                    raise Parsing_Error;
                end if;
              end loop;
              ThisLiteral(ThisIndex+1..ThisIndex+1) := "]";
              ThisIndex := ThisIndex+1;
              Skip_Token;  -- "]"
              Prefix.Labels :=
                new String_Table'(Prefix.Labels.all &
                  new String'(ThisLiteral(1..ThisIndex)));
            end;
          end if;
        else
          Skip_Token(",");
        end if;
        if Current_Item="" then
            Give_Error ("Missing "")"" ...");
            raise Parsing_Error;
        end if;
      end loop;
      Prefix.Params := new String_Table(1..Prefix.Labels.all'Length-1);
      Prefix.Params.all := Prefix.Labels(2..Prefix.Labels.all'Length);
      Skip_Token (")");
    end if;
    --
    return  Result;
  end Parse_Abstract_Action;
  ---------------------------------------------------------------------------


  function Parse_Action (Tau_Allowed: Boolean;  
                         Var_Allowed: Boolean;
                         Is_Negation: Boolean)  return Action_Ref is
    Act1, Act2: Action_Ref;
    Prefix: Action_Ref;
  begin
    --
    --  ( aform )
    -- 
    if Current_Item= "(" then
       Skip_Token ("(");
       Prefix := Parse_Action (Tau_Allowed,False,False);
-- tau allowed
--       if Prefix = null then
--         Give_Error("Missing action expression ?!");
--         raise Parsing_Error;
--       end if;
       Skip_Token (")");
    --
    --  ~ aform  /  not aform
    -- 
    elsif Current_Item = "~" or else
        Current_Item = "not"  then
       Skip_Token;
       Act1:= Parse_Action (Tau_Allowed,False,True);
-- tau allowed
--       if Act1 = null then
--         Give_Error ( "Missing action expression ?!");
--         raise Parsing_Error;
--       end if;
       Prefix := new Action'(Anot, Act1);
    --
    -- true / false / source:target.event(params) / obj'.var' = var + num 
    --
    --
    elsif not Tau_Allowed and Is_Tau(Current_Item)  then
        Give_Error ( "tau not allowed in this context");
        raise Parsing_Error;
      --
      --  true
      --
    elsif Current_Item = "true" or else
         Current_Item = "True" or else
         Current_Item = "TRUE" then
         Skip_Token;
         Prefix := True_Action; -- new Action'(Kind => Atrue);
      --
      --  false
      --
    elsif Current_Item = "false" or else
         Current_Item = "False" or else
         Current_Item = "FALSE" then
         Skip_Token;
         Prefix := False_Action; -- new Action'(Kind => Afalse);
      --
      --  tau
      --
    elsif Is_Tau(Current_Item)  then
         Skip_Token;
         Prefix := null;
      --
    elsif Current_Item = "}" or else
         Current_Item = ">" or else
         Current_Item = ">>" or else
         Current_Item = "]]" or else
         Current_Item = "]" then
          ---  Prefix is null;  cannot continue ...
         return null;
      --
    else
      Prefix := Parse_Abstract_Action(Var_Allowed);
    end if;    

    --
    --  aform & aform   
    --
    if (Current_Item = "&"  or else
       Current_item = "and") and not Is_Negation then
       --   not act1 and act2  ==>   (not act1) and act2
       Skip_Token;
--30-06-2016
--       Act2:= Parse_Action (Tau_Allowed,False,False);
       Act2:= Parse_Action (Tau_Allowed,Var_Allowed,False);
-- tau allowed
--       if Act2 = null then
--         Give_Error( "Missing action expression ?!");
--         raise Parsing_Error;
--       end if;
       return new Action'(Aand, Prefix, Act2);
    --
    --  aform | aform
    --
    elsif (Current_Item  = "|" or else
          Current_Item = "or") and not Is_Negation then
       --   not act1 or act2  ==>   (not act1) or act2
       Skip_Token;
       Act2:= Parse_Action (Tau_Allowed,False,False);
-- tau alllowed
--       if Act2 = null then
--         Give_Error("Missing action expression ?!");
--         raise Parsing_Error;
--       end if;
       return new Action'(Aor, Prefix, Act2);
       --
    else
      --
      --  current char is '}'  or ']' or '>' or garbage in case or errors
      --  Prefix can be null if no action was present  <> [] {}
      if Current_Item = "}" or else
         Current_Item = ">" or else
         Current_Item = "&" or else
         Current_Item = "and" or else
         Current_Item = "|" or else
         Current_Item = "or" or else
         Current_Item = ")" or else
         Current_Item = ">>" or else
         Current_Item = "]]" or else
         Current_Item = "]" then
         return Prefix;
      else
        Give_Error (" Error in action predicate");
        raise Parsing_Error;
      end if;
    end if;
    --
  end Parse_Action;
---------------------------------------------------------------------------

begin
  null;
end UCTL_Parser;
