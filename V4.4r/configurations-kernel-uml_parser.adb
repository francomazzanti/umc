with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
separate (configurations.Kernel)
package body UML_Parser is
------------------   SPEC -----------------
--  procedure Parse (Uml_File_Name: String);
--  --
--  Max_Line_Length: Positive := 10000;
--  Max_Empty_lines: Natural := 500;
--  Max_Comment_lines: Natural := 500;
--  Max_Components: Positive := 500;
--  Max_Synchronizations: Positive := 10000;
------------------------------------------------
use UML_Types;
--
--
--
--    TOKENS MANAGEMENT AND GENERIC PARSING ROUTINES
--  

  type Token_Kind is (Num, Id, Str, Special);  --  special num id   12a

  type Token is record
     Item: String_Ref;
     Line: Natural;
     Line_Ref: String_Ref;
     Column: Natural;
     Kind: Token_Kind;
  end record;

  type Comment_Element is record
     Item: String_Ref;
     Line: Natural;
     Column: Natural;
     AfterToken: Natural;
  end record;

  type Tokens_Table is  array (natural range <>) of Token;
  type Tokens_Table_Ref is access Tokens_Table;

  type Comments_Table is  array (natural range <>) of Comment_Element;
  type Comments_Table_Ref is access Comments_Table;

--------------------------------------------------------------------
  F: File_Type;
  Fincluded: File_Type;
  Main_File_Name: String_Ref;
  Included_File_Name: String_Ref;
  Current_File_Name: String_Ref;
  Parsing_Failed: Boolean;
  Parsing_Errors: Natural;
--------------------------------------------------------------------
--  data to be saved / restore in case of multiple file parsing
  Tokens: Tokens_Table_Ref := new Tokens_Table(1..0);  -- dynamically extended by Read_Tokens
  Current_Token: Natural :=0;
  Current_Position: Positive;
  Line_Number: Natural :=0;
  Last_Line: String_Ref;
  Input_line: String(1..Max_Line_Length);
  Previous: Character := '^';
  Line_Length: Natural := 0;
  --------------
  All_Comments: Comments_Table_Ref := new Comments_Table(1..0);
  All_Comments_Buffer: Comments_Table(1..1000);
  All_Comments_Count: Natural :=0;
-- repository for Main data saved while parsing included files
--  data to be saved / restore in case of multiple file parsing
  MainTokens: Tokens_Table_Ref;
  MainCurrent_Token: Natural :=0;
  MainCurrent_Position: Positive;
  MainLine_Number: Natural :=0;
  MainLast_Line: String_Ref;
  MainInput_line: String(1..Max_Line_Length);
  MainPrevious: Character := '^';
  MainLine_Length: Natural := 0;
--  MainAll_Comments: Comments_Table_Ref := new Comments_Table(1..0);
--  MainAll_Comments_Buffer: Comments_Table(1..1000);
--  MainAll_Comments_Count: Natural :=0;
--------------------------------------------------------------------
  --  more implementation vars
  Max_Token_Size : Natural := 200;
--------------------------------------------------------------------

  pragma SHARED (Current_Token);
  pragma VOLATILE (Current_Token);

  procedure Read_Tokens(F:File_Type);
  procedure Give_Error (Msg : in String);
  procedure Skip_Token (Item: String := "");


  function CleanControls (From: String) return String is
    res: String := From;
    Count: Natural :=0;
  begin
    for I in From'Range loop
      if not Is_Control(From(I))then
         Count := Count+1;
         res(Count) := From(I);
      end if;
    end loop;
    return res(1..Count);
  end CleanControls;

  function Current_Item return String is
  begin
    return Tokens(Current_Token).Item.all;
  end Current_Item;

  function Next_Item return String is
  begin
    if Current_Token + 1 > Tokens.all'Length then
     return "";
    else
     return Tokens(Current_Token+1).Item.all;
    end if;
  end Next_Item;

  function Next_Next_Item return String is
  begin
    if Current_Token+2 > Tokens.all'Length then
     return "";
    else
     return Tokens(Current_Token+2).Item.all;
    end if;
  end Next_Next_Item;

---------------------------------------------------------------------------
--  IMPLEMENTATION OF TOKENS MANAGENT AND GENERIC PARSING 

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

  procedure Give_Error (Msg : in String) is
    Where:Token;
  begin
    Parsing_Errors := Parsing_Errors+1;
    Parsing_Failed := True;
    if Current_Token in Tokens.all'Range then
      Where := Tokens(Current_Token);
    end if;
    New_Line(Current_Error);
    Put_Line (Current_Error,
               Current_File_Name.all & ": " &
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
    Put_line(Current_Error, Msg);
  end Give_Error;
  
  --------------------------------------------------------
  -- deletes the comment part from the current line
  --------------------------------------------------------
  procedure Remove_Comments is
    Inside_String_Literal: Boolean := False; 
    Thecomment: Comment_Element;
  begin
    if Line_Length >=2 and then
      (Input_Line(1..2) = "--" or Input_Line(1..2) = "//") then
      Thecomment.Item := new String'(Input_Line(1..Line_Length));
      Thecomment.Line :=  Line_Number;
      Thecomment.Column := 1;
      Thecomment.AfterToken := Current_Token; 
      All_Comments_Count := All_Comments_Count+1;
      All_Comments_Buffer(All_Comments_Count) := Thecomment;
      if All_Comments_Count= All_Comments_Buffer'Last then
         All_Comments := new Comments_Table'(All_Comments.all & All_Comments_Buffer);
         All_Comments_Count:=0;
      end if;
      Line_Length :=0;
      return;
    end if;
    for I in 1..Line_Length loop
      if Input_Line(I)= '"' then
         Inside_String_Literal := not Inside_String_Literal;
      end if;
      if Input_Line(I) not in '0'..'9' and then
         Input_line(I) not in 'A'..'Z' and then
         Input_line(I) not in 'a'..'z' and then
         not Inside_String_Literal and then
          I+1 < Line_Length  and then
         ((Input_Line(I+1) = '-' and then Input_Line(I+2) = '-') or
          (Input_Line(I+1) = '/' and then Input_Line(I+2) = '/')) then
        --
        Thecomment.Item := new String'(Input_Line(I+1..Line_Length));
        Thecomment.Line :=  Line_Number;
        Thecomment.Column := I+1;
        Thecomment.AfterToken := Current_Token;
        All_Comments_Count := All_Comments_Count+1;
        All_Comments_Buffer(All_Comments_Count) := Thecomment;
        if All_Comments_Count= All_Comments_Buffer'Last then
           All_Comments := new Comments_Table'(All_Comments.all & All_Comments_Buffer);
           All_Comments_Count:=0;
        end if;
        Line_Length := I;
        exit;                     --  comment found: line is truncated here
      end if;
    end loop;
  end Remove_Comments;

  procedure Read_New_Line(F: File_Type) is
  begin
    Line_Length :=0;
    if Previous = '^' then
      if not End_of_File(F) then
        Get_Immediate(F,Previous);  -- maybe blank or comment
      else 
        return;
      end if;
    end if;
    -- read until first not blank or comment line
    while Line_Length =0 loop
      -- until a non blank line is found, or end-of-file
        -- leggi fino alla fine di tutta la riga, saltando tutti i caratteri do controllo finali.
        -- read chars 2.. up to end of line
        while Previous /= cr and then Previous /= lf loop
           -- continue reading until end-of-line
            Line_Length := Line_Length+1;
            Input_Line(Line_Length) := Previous;
            if not End_of_File(F) then  
              Get_Immediate(F,Previous); 
            else 
              Previous := '^';
              exit; 
            end if;
        end loop;
        -- skip lf or lf-cr
        if Previous = lf and not End_of_File(F) then 
            Get_Immediate(F,Previous); 
            if Previous = cr and not End_of_File(F) then 
              Get_Immediate(F,Previous); 
              Previous := '^';
            end if; 
        -- skip cr-lf
        elsif Previous = cr and not End_of_File(F) then                  
            Get_Immediate(F,Previous);    
            if Previous = lf and not End_of_File(F) then 
              Get_Immediate(F,Previous); 
              Previous := '^';
            end if;
        end if;
      --
      Line_Number := Line_Number +1;
      if  Line_Length > 0 then
         -- found some characters ...
         -- strip contrls 
         --  save original line in Last_Line
         Last_Line := new String'(CleanControls(Input_Line(1..Line_Length)));
         Input_Line(1..Last_Line.all'Length) := Last_Line.all;
         Line_Length := Last_Line.all'Length;
         Current_Position := 1;
         Remove_Comments;    -- maybe Line_Length  returns to 0
      end if;
      if End_of_File(F) and (Previous = '^' or Previous = cr or Previous = lf) then 
        Previous := '^';
        exit; 
      end if;
    end loop;
  end Read_New_Line;
  
  -----------------------------------------------------------------
  -- reads the input file until a non-white char is found
  --  when EOF, sets Line_Length to 0
  -----------------------------------------------------------------
  procedure Skip_Spaces(F: File_Type) is
  begin
    --
    if Line_Length = 0 and not End_Of_File(F) then
      Read_New_Line(F);
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
--      if End_Of_File(F) then
      if Previous = '^' then
         Line_Length :=0;
         return;
      else 
        Read_New_Line(F);
      end if;
    end loop;
  end Skip_Spaces;
  
  --------------------------------------------------------------------
  -- extracts the next token from the stream
  -- returns a token with empty string item  when EOF;
  --  TOKENS are:
  --   numbers      0..9     0..9 
  --   ids    a..z  A..Z  0..9 _ '
  --   strings  "any"
  --   specials long:  )->  -(  :=  ++ --  ->  >=  <= ==  !=  && ||  /* */   $*
  --   specials short: ( ) , ; = : - [ ] /  $ & * + > < | .  ^ # % ! ?
  --
  --   "*"  is the times operator or the star matching sybol, unless it appears in $*
  --     "/* .. */ " is a comment separator
  --     "// " is a comment separator
  --     "--"  is comment separator only if
  --      a)  appears at the beginnning of the line
  --  or  b)  is preceeded by a white space or non alfanumeric character
  --   N.B.  Tokens are buffered by Read_Tokens
  --------------------------------------------------------------------
  function Get_Token(F:File_Type) return Token is
    This_Token: Token;
    StringBuffer: String(1..Max_Token_Size);
    Item_Size: Natural :=0;
  begin 
    Skip_Spaces(F);
    -- if empty file, Current_Position =1  and 
    This_Token.Line :=  Line_Number;
    This_Token.Line_Ref := Last_Line;
    This_Token.Column :=  Current_Position;
    -- if file empty return null final token
    if Line_Length = 0  then
       This_Token.Item := new String'("");
       Current_Token := Current_Token +1;  --  reset to 1 when all tokens are read
       return  This_Token;   --  Item= null;
    end if;
    --
    -- find the token kind
    --
    if Input_line(Current_Position) in '0'..'9' then
      This_Token.Kind := Num;
    elsif Input_line(Current_Position) in 'a'..'z' or else
          Input_line(Current_Position) in 'A'..'Z' or else
          (Input_line(Current_Position) = '$' and then Input_line(Current_Position+1) /= '*')  or else
          Input_line(Current_Position) = '_'  then
      This_Token.Kind := Id;
    elsif Input_line(Current_Position) = '"' then
      This_Token.Kind := Str;   
    else
      This_Token.Kind := Special;
    end if;
    --
    -- get the token item
    --
    for I in Current_Position .. Line_Length loop
      case This_Token.Kind is
      when Id =>
        Item_Size := Item_Size +1;
        StringBuffer(Item_Size) := Input_line(Current_Position);
        Current_Position := Current_Position +1;
        -- this is the last char of the item
        if Current_Position > Line_Length or else
           (Input_line(Current_Position) not in 'a'..'z' and then
           Input_line(Current_Position) not in 'A'..'Z' and then
           Input_line(Current_Position) not in '0'..'9' and then
           Input_line(Current_Position) /= '_' ) then
           exit;
        end if;
        -- continue the cycle
      when Num =>
        Item_Size := Item_Size +1;
        StringBuffer(Item_Size) := Input_line(Current_Position);
        Current_Position := Current_Position +1;
        -- this is the last char of the item
        if Current_Position > Line_Length or else
           Input_line(Current_Position) not in '0'..'9' then
           exit;
        end if;
        -- continue the cycle
      when Str =>
          Item_Size := Item_Size +1;
          StringBuffer(Item_Size) := Input_line(Current_Position);
        Current_Position := Current_Position +1;
          -- this is the last char of the item
        if Item_Size > 1 and then Input_line(Current_Position) = '"' then
          exit;
        elsif Current_Position > Line_Length then
          Give_Error ("Unterminated String Literal");
          raise Parsing_Error;
        end if;
        -- continue the cycle
        --
      when Special =>
--
--   specials long:  )->  -(  :=  ++ --  ->  >=  <= ==  !=  && || /  /= => .. /* */  $*
--   specials short: ( ) , ; = : - [ ] /  $ & * + > < | .  ^ # % ! ?
--
        Item_Size := Item_Size +1;
        StringBuffer(Item_Size) := Input_line(Current_Position);
        -- this is the last char of the item
--        if Current_Position+2 <=  Line_Length and then
--           Input_Line(Current_Position..Current_Position+2)= ")->" then
--           Item_Size := Item_Size+2;  
--           StringBuffer(2..3) := "->";
--           Current_Position := Current_Position +3;
--           exit; 
--        elsif Current_Position+1 <= Line_Length and then
 --          Input_Line(Current_Position..Current_Position+1)= "-(" then
--           Item_Size := Item_Size+1;
--           StringBuffer(2) := '(';
--           Current_Position := Current_Position +2;
--           exit;
--        elsif Current_Position+1 <= Line_Length and then
        if Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= ".." then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '.';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "$*" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '*';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "/*" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '*';
           Current_Position := Current_Position +2;
           exit;
        elsif Current_Position+1 <= Line_Length and then
           Input_Line(Current_Position..Current_Position+1)= "*/" then
           Item_Size := Item_Size+1;
           StringBuffer(2) := '/';
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
    This_Token.Item := new String'(StringBuffer(1..Item_Size));
    if This_Token.Item.all = "$*" then
     This_Token.Kind := Id;
    end if;
    -- reset to one after all tokens are read
    -- needed to remember the placing of comments extracted by Remove_Comments
    Current_Token := Current_Token+1; 
    return This_Token;
  end Get_Token;

  procedure  Read_Tokens(F:File_Type) is
    Buffer: Tokens_Table(1..1000);  -- Tokens is a vector of Buffers
    Count: Natural := 0;
    Tmp: Token;
  begin
    Tmp:= Get_Token(F);
    if Tmp.Item.all="/*" then
      while Tmp.Item.all /= "*/" and then
             Tmp.Item.all /= "" loop
         Tmp:= Get_Token(F);
      end loop;
      if Tmp.Item.all = "*/" then Tmp:= Get_Token(F); end if;
    end if;
    Count := Count +1;
    Buffer(Count) := Tmp;
    while Tmp.Item.all'Length /= 0  loop
      Tmp:= Get_Token(F);
      if Tmp.Item.all="/*" then
        while Tmp.Item.all /= "*/" and then
               Tmp.Item.all /= "" loop
           Tmp:= Get_Token(F);
        end loop;
        if Tmp.Item.all = "*/" then Tmp:= Get_Token(F); end if;
      end if;
      Count := Count +1;
      Buffer(Count) := Tmp;
      if Count =1000 then
        Tokens := new Tokens_Table'(Tokens.all & Buffer); 
        Count :=0;
      end if;
    end loop;
    Tokens := new Tokens_Table'(Tokens.all & Buffer(1..Count));
    Current_Token := 1;
  end Read_Tokens;



--------------------------------------------------------------------------
--    UML SPECIFIC  PARSIMG ROUTINES
--------------------------------------------------------------------------
-- Parses the uml file indentified by the given name and creates
-- the corresponding uml object in "This_Uml_Object"
--
-- procedure Parse (Tab_File_Name: String);
--
------------------------------------------------------------------------
  -- chart #1 = OUT    chart #2=ERR
------------------------------------------------------------------------

-- Class  MY_STATE_CHART is 
--   Signals 
--     r2,a2,e1,e2,f1,r1,e(x), a(x), OUT.done, ERR.failure
--   Operations
--     o1, o2:int, o3(x,y):obj 
--   Vars  
--     x, y:int:=0, z:obj, w
--   Transitions
--   State S0 = S1, s2, s3  Defers:  a,a(x),c
--   State S0.S1 = S4 / S5 Defers:  a,b(x,y),c
--     S1.S5.s8      -(a(x)/y:=x;e1)->  s3
--     s3            -(a2/e1)->  S1.S5.s9
--     S1.S4.S7.s10  -(e2/e2)->  s3
--     S1.S4.s6      -(r1/a1)->  s2
--     S1            -(e1/-)->   s2
--     s2            -(a1/r2)->  S1
--     (s2)          -(a1/r2)->  (S1)
--     (S1.S4.s6,S1.S5.s8) -(-/-)-> (S1.S4.s6,S1.S5.s8)
-- end MY_STATE_CHART;
--
-- Object obj1: class1;
-- Object obj2: class2 (attr=> value, link => obj3);
-- Object  obj3: class3;
--
--  Chartype.event  is a broacast message to all elems of the type ????

subtype Action_Ref is UML_Types.Action_Ref;

  Current_Object: Natural :=0;
  Current_Class: Natural :=0;
  
  Max_States_Number: Positive := 100000;
  Max_Parallelism: Natural :=0;
--   This_State: State_Ref;
  
  --
  -- side effect of Parse_Action - used by Parse_Transition_Structure
  Return_or_Caller_Found: Boolean := False;  
  -- side effect of Parse_Transition_Structure - used by Parse_Transition_Structure
  Is_Operation_Body: Boolean := False; 
   

  function Parse_Id return String_Ref;
  function Parse_Composite_Name return String_Ref;
  function Parse_Int_Token return String;
  function Parse_Names_List return String_Table_Ref;
  procedure  Parse_Vars;

  function Parse_Actions (TEnv:EventVars_Table_Ref) return Actions_Table_Ref;
  procedure Parse_Class_Body (This_Class_Name: String_Ref);

  function Find_SystemVar(Prefix:String) return Natural; 
  function Parse_IntExpr (Env: EventVars_Table) return IntExpr_Ref;
  function Parse_SimpleExpr(Env: EventVars_Table) return SimpleIntExpr_Ref;
  procedure Parse_Transitions;
  function Find_Var(Prefix:String) return Natural;
--  function Find_Var(The_Class:Natural; Prefix:String) return Natural;
  function Find_State (This_State_Name: String) return State_Ref ;
  function Just_Find_State (This_State_Name: String) return State_Ref ;
  function Add_To_Outgoing_Catalogue (This_Transition: Transition_Ref;
    This_Catalogue: TransitionsCatalogue_Ref) return TransitionsCatalogue_Ref;
  function Build_Outgoing_Catalogue (These_Transitions: Transitions_Table)
            return TransitionsCatalogue_Ref;
  procedure Load_Outgoing_Catalogues (This_State: State_Ref);
  procedure Check_Missing_Definitions;
  procedure Set_Ancestors;
  procedure Set_Finals;
  function Check_Class (Class_Name: String) return Integer;

 function IsNested (s1,s2: State)
 return boolean;

--  function Parse_States (From_String: String) return States_Table;

  function Is_Number (Token: String) return Boolean;

--------------------------------------------------------------------
--    UML specific routines
--------------------------------------------------------------------

  function IsNested (s1,s2: State) return boolean is
  begin
    if s1.FullName.all'Length > s2.FullName.all'Length+1 and then
       s1.FullName.all(1..s2.FullName.all'Length) = s2.FullName.all and then
       s1.FullName.all(s2.FullName.all'Length+1) = '.' then
       return True;
    else
       return False;
    end if;
  end IsNested;

  function Prefix (source: String) return String is
  begin
     for I in reverse source'Range loop
       if source(I) = '.' then 
         return source(source'First .. I-1);
       end if;
     end loop;
     return "";
  end Prefix;

  function Suffix (source: String) return String is
  begin
     for I in reverse source'Range loop
       if source(I) = '.' then
         declare
           Str: String (1..source'Last-I);
         begin
           Str :=  source(I+1 .. source'Last);
           return Str;
         end;
       end if;
     end loop;
     return source;
  end Suffix;

  ----------------------------------------------------------------------
  --  Called by  Parse_Signals when '(' is found (and skipped) after the eventid
  --  returns  a new EventVars_Table 
  --   possibly empty  '(No_EventVars)  
  ----------------------------------------------------------------------
  function Parse_EventVars (AddTo: EventVars_Table) return EventVars_Table is
    This_Var: EventVar_Ref := new EventVar;
    This_Kind: String_Ref;
  begin
    if Current_Item  = ")" then
      return AddTo;
    end if;
    --
    This_Var.Name := Parse_Id;
    This_Var.Num_Key := AddTo'Length+1;
    --
    if Current_Item = ":" then
      Skip_Token;
      This_Kind := Parse_Id;
      if This_kind.all = "obj" then
        This_Var.Kind := Object;
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Objcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Objmatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Objvector;
          end if;
          Skip_Token; -- "]"
        end if;
      elsif This_kind.all = "bool" then
        This_Var.Kind :=  Bool;
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Boolcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Boolmatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Boolvector;
          end if;
          Skip_Token; -- "]"
        end if;
      elsif This_kind.all /= "int"  then --  must be a Class Name
        This_Var.Kind := Object;
        This_Var.Typeinfo := Check_Class(This_kind.all);
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Objcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Objmatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Objvector;
          end if;
          Skip_Token; -- "]"
        end if;
      else
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Numcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Nummatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Numvector;
          end if;
          Skip_Token; -- "]"
        end if;
      end if;
    end if;
    --
    if Current_Item = "," then
      Skip_Token;
      return Parse_EventVars (AddTo & This_Var);
    else
      return (AddTo & This_Var);
    end if;
  end Parse_EventVars;
  
  -------------------------------------------------------------------
  -- CALLED WHEN ELABORATING A CHART Signals or Operations  DECLARATION
  -- (once for each declared event)
  --  a new event declaration is introduced. (if not already there) in the
  --  global  ALL_EVENTS table.
  --  The same Event is also linked in the local CLASS.CHART_EVENTS table
  --
  --  IN THE CASE OF OPERATIONS, THE PROFILE CONTAING ALSO THE CALLER PARAM
  --  Looks into the global ALL_EVENTS table. 
  --  This_Event  can be:  opevent(_caller,arg1,arg2)  or  sigevent(arg1.arg2)
  -------------------------------------------------------------------
  procedure Check_Event (This_Event: Event_Ref) is
    TheseEvents: Events_Table_Ref;
    Declared_Events: Events_Table_Ref;
    This_Size: Natural;
    Tmp_Size: Natural;
--    Renamed_Event: Event_Ref;
  begin
    TheseEvents:=All_Events;
    Declared_Events := All_Classes(Current_Class).ChartEvents;
    --
    --  first, look if a forward definition already exists,
    --    (because a corresponding signal has been elaborated (Kind=Undefined) )
    --
    -- This_Size, Tmp_Size  are the actual parameters count, without considering
    --   the implicit "caller" one.
    This_Size := This_Event.Params.all'Length;
    if This_Event.Kind = Operation then
       This_Size := This_Size -1;  -- the actual params size
    end if;
    --  1 =  "-"
    --  2 =  "lostevent"
    --  3 =  "Run_Time_Error"
    --  4 =  "assign"
    --  5..17 =  OpReturn_Event ... OpReturnBoolC_Event
    --
    for I in OpReturnBoolC_Event.Num_Key+1 .. TheseEvents.all'length loop 
      Tmp_Size := TheseEvents(I).Params.all'Length;
      if TheseEvents(I).Kind = Operation then
        Tmp_Size  := Tmp_Size  -1;   -- the actual params size in All_Events(I)
      end if;
      -- 
      --  if we find a similer event with the same name and the 
      --  same number of parameters we RECYCLE that definition
      --  WITHOUT giving an error if there is a mismatch in the type pf the args 
      --   or in the type of the result
      --
      if TheseEvents(I).Name.all = This_Event.Name.all and then
            This_Size  = Tmp_Size then
        if TheseEvents(I).Kind /= Undefined and then
            TheseEvents(I).Kind /= This_Event.Kind then
           Give_Error("Error: cannod declare a signal and an operation with the same name and args count");
           -- the problem is when we find actions with undeclared event:
           --     XX.foo;     -- maybe intending signal   (introducing undefined uevent foo)
           --     YY.foo;     -- maybe intending operation (fininding undefined event foo)
           --  Signals foo    -- redefining undefined foo as signal (for all cases)
           --  Operations foo -- disallowed or newly introduced but never used 
           raise Parsing_Error;
        end if;
        --
        This_Event.Num_Key := I;  -- necessary in the lazy case
        --
        --  if the event was registered as a signal, (while it is a call),
        --   then adjust its kind and params;
        -- WASN'T SIMPLER  TheseEvents(I) := This_Event ?!?!?!?
        if TheseEvents(I).Kind = Undefined then
          TheseEvents(I).Kind := This_Event.Kind;  -- Operation or Signal
          TheseEvents(I).Params := This_Event.Params;
          TheseEvents(I).Return_Type := This_Event.Return_Type;
        end if;
        --
        -- OK the args length is correct, now check the names.
        --  (in case of multiple class defining the same event)
        --
        --  MAybe we should also check that all the event params
        --  names are matching.  I.e. that no two charts declare
        --   Events:  init(arg_a)     and    Events: init(arg_b)
        --
-- REMOVED  21-12-2010 to allow multipler decalration of events overloading arg names
--        for J in TheseEvents(I).Params.all'Range loop
--             if TheseEvents(I).Params(J) /= null and then
--                TheseEvents(I).Params(J).Name /= null and then
--                This_Event.Params(J) /= null and then
--                This_Event.Params(J).Name /= null and then
--                TheseEvents(I).Params(J).Name.all /=
--                    This_Event.Params(J).Name.all then
--                Give_Error ("Error: type mismatch for event " &
--                   This_Event.Name.all);
--                raise Parsing_Error;
--             end if;
--        end loop;
--        if TheseEvents(I).Return_Type /= This_Event.Return_Type then
--           Give_Error ("Error: type mismatch for event " & This_Event.Name.all);
--           raise Parsing_Error;
--        end if;
        --
        -- when an event is found for the first time inside
        --   a signal action, it does not have the event
        --    param names, hence when its declaration is first
        --    encountered, we must fill its definition with the names
        --   NO MORE - ALL_EVENTS CAN HAVE NO PARAM NAMES
        --  PARAM NAMES AND TYPES ARE CLASS DEPENDENT
--        if TheseEvents(I).Params.all'Length >0 and then
--           (TheseEvents(I).Params(1) = null or else
--           TheseEvents(I).Params(1).Name = null) then 
--           for J in TheseEvents(I).Params.all'Range loop
--            TheseEvents(I).Params(J).Name := This_Event.Params(J).Name;
--            TheseEvents(I).Params(J).Kind := This_Event.Params(J).Kind;
--           end loop;
--        end if;
        -- make a copy of TheseEvents(I) using the current param names
        --  WHY INSTEAD NOT USE JUST THE ORIGINAL THIS_EVENT??????
--        Renamed_Event := new Event'(TheseEvents(I).all); --  adjusr num_key etc
--        Renamed_Event.Params := new EventVars_Table'(TheseEvents(I).Params.all);
--        for J in This_Event.Params.all'Range loop
--          Renamed_Event.Params(J) := new  EventVar'(TheseEvents(I).Params(J).all); 
--          Renamed_Event.Params(J).Name := This_Event.Params(J).Name;
--        end loop;
        All_Classes(Current_Class).ChartEvents :=
--          new Events_Table'(Declared_Events.all & Renamed_Event);
          new Events_Table'(Declared_Events.all & This_Event);
        return;
      end if;
    end loop; 
    --
    -- The event is a new event
    --
    This_Event.Num_Key := All_Events.all'Length+1;
    All_Events := new Events_Table'(TheseEvents.all & This_Event);
    All_Classes(Current_Class).ChartEvents :=
        new Events_Table'(Declared_Events.all & This_Event);
    --
  end Check_Event;

  -------------------------------------------------------------------------
  --  called while parsing the trigger of a transition OR a DEFERRED DECL.
  --  Notice that given an event Name, its declaration is UNIQUE
  --  notice that when checking a trigger, the event MUST be declared
  --  LOOKS INTO  THE LOCAL CHARTEVENTS TABLE
  --  WHEN CHECKING THE CORRECT SIZE, we DO NOT COUNT THE IMPLICIT CALLER
  -- The argslist of "this_event" MAY does NOT contain the implicit caller
  ------------------------------------------------------------------------d
  function Find_Event (This_Event: Event_Ref ) return Event_Ref is
--   This_String_Ref: String_Ref;
   TheseEvents: Events_Table_Ref;
   This_Size: Natural;
--   Lazy_Event: Event_Ref;
  begin
    if This_Event.Name.all = "-" then
      return Null_Event;
    end if;
    --
    if This_Event.Params /= null then
      This_Size := This_Event.Params.all'Length;
    else 
      This_Size :=0;
    end if;
    TheseEvents := All_Classes(Current_Class).ChartEvents;
    for I in TheseEvents.all'Range loop
       if TheseEvents(I).Kind = Signal and then 
          TheseEvents(I).Name.all = This_Event.Name.all and then
          TheseEvents(I).Params.all'Length = This_Size  then
          return TheseEvents(I);
       elsif TheseEvents(I).Kind = Operation and then 
          TheseEvents(I).Name.all = This_Event.Name.all and then
          TheseEvents(I).Params.all'Length = This_Size+1  then
          return TheseEvents(I);
       end if;
    end loop;
    --
    if not Lazy_Parsing then
      Give_Error("Error: Found use of undeclared event " & This_Event.Name.all);
      raise Parsing_Error;
    else
      All_Classes(Current_Class).ChartEvents := 
         new Events_Table'(TheseEvents.all & This_Event);
      This_Event.Kind := Signal; 
      Check_Event(This_Event);  -- adds into All_Events  
      return This_Event;
    end if;
  end Find_Event;

  ----------------------------------------------------------------------
  -- CALLED WHEN PARSING A SIGNAL/CALL OF A TRANSITION ACTION
  --  if the corresponding definition exists, checks its correctness
  --   otherwise introduces a new event placeholder in All_Events.
  --  Looks into the global ALL_EVENTS table.
  --    Obj.foo(x)     matches   opfoo(_caller,x)  and sigfoo(x)
  ----------------------------------------------------------------------
  function Check_Event (This_Event_Name: String;
                            My_Args: umlExpr_Table_Ref) return Event_Ref is
    VK: Value_Kind;
    Tmp: Event_Ref;
  begin
    for I in All_Events.all'Range loop
       if All_Events(I).Name.all = This_Event_Name  then
          if All_Events(I).Kind = Signal or  All_Events(I).Kind = Undefined then
            if All_Events(I).Params.all'Length = My_Args.all'Length then
              if This_Event_Name /= "return" then
                Tmp :=  All_Events(I);
                -- We do not set the Kind to Signal in order to be able to check, at the end,
                -- if all events are declared somewhere, and to distinguish the case of a previous
                -- found signal declaration from the case of a use of yet undeclared event
                exit;
              else
                --  get the Value_Kind VK 
                --  FROM the expression KIND
                if My_Args= null or else My_Args(1) = null then
                    VK := Undefined;
                elsif My_Args(1).umlBool /= null then
                    VK := Bool;
                else
                     VK := IntExpr_Kind(My_Args(1).umlInt.all);
                end if;
                --
                if VK = Undefined then
                   return OpReturn_Event;
                elsif VK = Number then
                   return OpReturnInt_Event;
                elsif VK = Object then
                   return OpReturnObj_Event;
                elsif VK = Bool then
                   return OpReturnBool_Event;
                elsif VK = Numvector then
                   return OpReturnIntV_Event;
                elsif VK = Objvector then
                   return OpReturnObjV_Event;
                elsif VK = Boolvector then
                   return OpReturnBoolV_Event;
                elsif VK = Nummatrix then
                   return OpReturnIntM_Event;
                elsif VK = Objmatrix then
                   return OpReturnObjM_Event;
                elsif VK = Boolmatrix then
                   return OpReturnBoolM_Event;
                elsif VK = Numcube then
                   return OpReturnIntC_Event;
                elsif VK = Objcube then
                   return OpReturnObjC_Event;
                elsif VK = Boolcube then
                   return OpReturnBoolC_Event;
                end if;
              end if; 
            end if;
          else  ---  All_Events(I).Kind = Call
            if All_Events(I).Params.all'Length = My_Args.all'Length+1 then
              Tmp :=  All_Events(I);
              exit;
            end if;
          end if;
       end if;
    end loop;
    if Tmp = null then
      Tmp := new Event;
      Tmp.Name := new String'(This_Event_Name);
      Tmp.Kind := Undefined;
      Tmp.Num_Key := All_Events.all'Length+1;
      Tmp.Params := new EventVars_Table'(1..My_Args.all'Length => new EventVar);
      All_Events := new Events_Table'(All_Events.all & Tmp);
      --
      for I in 1.. My_Args'Length loop
        Tmp.Params(I).Num_Key := I; 
--        if Tmp.Params(I).Kind = Undefined then
--          if My_Args(I).umlInt = null then
--            Tmp.Params(I).Kind := Bool;
--          else
--            Tmp.Params(I).Kind := IntExpr_Kind(My_Args(I).umlInt.all);
--          end if;
--        end if;
      end loop;
    end if;
    return Tmp;
  end Check_Event;

  ---------------------------------------------
  --  adds the local trigger declarations to the All_Events table
  --  (if necessary)
      -- Parse the event Declarations (if any)
      --       Signals: r2,a2,e1,e2,f1,r1,e(x), a(x)
  ---------------------------------------------
  procedure Parse_Signals is
    This_Event: Event_Ref;
  begin
    if Current_Item = "Signals" or else
       Current_Item = "signals"  then
      Skip_Token;
      if Current_Item = ":" then
        Skip_Token;
      end if;
    else
     return;
    end if;
    -- in case of no signals  just return
    if Current_Item = "Operations" or else
       Current_Item = "operations" or else
        Current_Item = "Vars" or else
        Current_Item = "vars" or else
         Current_Item = "State" or else
         Current_Item = "state" then
      return;
    end if;
    loop
    declare
    begin
      This_Event := new Event;
      This_Event.Kind := Signal;
      This_Event.Name := Parse_Id;
      if Current_Item  = "(" then
        Skip_Token;
        This_Event.Params := 
          new EventVars_Table'(Parse_EventVars(No_EventVars));
        Skip_Token(")");
      else
       This_Event.Params := new EventVars_Table'(No_EventVars);
      end if;
      Check_Event(This_Event);
      if Current_Item = "," or Current_Item = ";" then
        Skip_Token;
      else
        if  Current_Item /= "end" and then
            Next_Item /= ":" and then
            Current_Item /= "Vars" and then
            Current_Item /= "State" and then
            Current_Item /= "Transitions" and then
            Current_Item /= "Behavior" and then
            Current_Item /= "Behaviour" and then
            Current_Item /= "behavior" and then
            Current_Item /= "behaviour" and then
            Current_Item /= "Operations" and then
            Next_Item /= "->" then
          Give_Error("Error: Expecting "","" or "";"" or Vars Section or State definitions.");
          if Parsing_Errors > 5 then
             raise Parsing_Error;
          else
            Parsing_Failed := True;
            Parsing_Errors := Parsing_Errors+1;
            while  Current_Item /= "end" loop
              Skip_Token;
            end loop;
          end if;
        end if;
        exit;
      end if;
      --  error case:     Signals:  foo; fee;    Vars:
      if Next_Item = ":" or
         Current_Item="Vars" or 
         Current_Item="vars" or 
         Current_Item="Signals" or
         Current_Item="signals" or
         Current_Item="Operations"  or
         Current_Item="operations"  or
         Current_Item="State" or
         Current_Item="state" or
         Current_Item="Class"  or
         Current_Item="class"  or
         Current_Item="end"  or
         Current_Item="Behavior"  or
         Current_Item="behavior"  or
         Current_Item="Behaviour"  or
         Current_Item="behaviour"  or
         Current_Item="Transitions"  or
         Current_Item="transitions"  then
        exit;
      end if;
  exception
     when others =>
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
         raise Parsing_Error;
      else
        while  Current_Item /= "" and then
             Current_Item /= ";" and then
             Current_Item /= "State" and then
             Current_Item /= "state" and then
             Current_Item /= "Vars" and then
             Current_Item /= "vars" and then
             Current_Item /= "Behavior" and then
             Current_Item /= "behavior" and then
             Current_Item /= "Behaviour" and then
             Current_Item /= "behaviour" and then
             Current_Item /= "Transitions" and then
             Current_Item /= "transitions" and then
             Current_Item /= "Class" and then
             Current_Item /= "class" and then
             Current_Item /= "Operations" and then
             Current_Item /= "operations" and then
             Current_Item /= "Objects" and then
             Current_Item /= "objects" loop
           Skip_Token;
        end loop;
        if Current_Item = ";" then Skip_Token; end if;
      end if;
    end;
    end loop;
--    All_Classes(Current_Class).ObservedEvents := 
--      new Bool_Table'(
--         1..All_Classes(Current_Class).ChartEvents.all'Length => True);
  exception
   when others =>
     Parsing_Failed := True;
     if Parsing_Errors > 5 then
       raise Parsing_Error;
     else
       while  Current_Item /= "" and then
           Current_Item /= "State" and then
           Current_Item /= "state" and then
           Current_Item /= "Vars" and then
           Current_Item /= "vars" and then
           Current_Item /= "Behavior" and then
           Current_Item /= "behavior" and then
           Current_Item /= "Behaviour" and then
           Current_Item /= "behaviour" and then
           Current_Item /= "Transitions" and then
           Current_Item /= "transitions" and then
           Current_Item /= "Class" and then
           Current_Item /= "class" and then
           Current_Item /= "Operations" and then
           Current_Item /= "operations" and then
           Current_Item /= "Objects" and then
           Current_Item /= "objects" loop
         Skip_Token;
       end loop;
     end if;
  end  Parse_Signals;
  --
  ---------------------------------------------
  --  adds the local trigger declarations to the All_Events table
  --  (if necessary)
  -- Parse the event Declarations (if any)
  --       Events: r2,a2,e1,e2,f1,r1,e(x), a(x)
  --  Operation Events DO HAVE an additional IMPLICIT first parameter 
  --    for the ("caller")
  ---------------------------------------------
  procedure Parse_Operations is
    This_Event: Event_Ref;
    This_Kind: String_Ref;
  begin
    if Current_Item = "Operations" or
       Current_Item = "operations" then
      Skip_Token;
      if Current_Item = ":" then
        Skip_Token;
      end if;
    else
      return;
    end if;
    -- in case of no operations  just return
    if Current_Item = "Vars" or else
       Current_Item = "vars" or else
       Current_Item = "Signals" or else
       Current_Item = "signals" or else
         Current_Item = "State" or else
         Current_Item = "state" then
      return;
    end if;
    loop
    declare
    begin
      This_Event := new Event;
      This_Event.Kind := Operation;
      if Current_Item = "Vars"  or else
         Current_Item = "State" or else
         Current_Item = "Signals" or else
         Current_Item = "Operations" or else
         Next_Item = "->" or else
         Current_Item = "end" then
       return;
      end if;
      This_Event.Name := Parse_Id;
      if Current_Item  = "(" then
        Skip_Token;
        This_Event.Params := 
           new EventVars_Table'(Parse_EventVars((1..1 => null)));
        Skip_Token(")");
      else
       This_Event.Params := new EventVars_Table'(
                              (1..1 => null));
      end if;
      This_Event.Params(1) := new EventVar;
      This_Event.Params(1).Name := new String'("_caller");
      This_Event.Params(1).Kind := Object;
      This_Event.Params(1).Num_Key := 1;
      if Current_Item  = ":" then
        Skip_Token;
        --
        This_Kind := Parse_Id;
        if This_kind.all = "obj" then
          This_Event.Return_Type := Object;
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              if Current_Item = "[" then
                Skip_Token; -- "["
                This_Event.Return_Type := Objcube;
                Skip_Token("]"); 
              else
                This_Event.Return_Type := Objmatrix;
              end if;
              Skip_Token("]"); -- "]"
            else
              This_Event.Return_Type := Objvector;
            end if;
            Skip_Token("]"); -- "]"
          end if;
        elsif This_kind.all = "bool" then
          This_Event.Return_Type :=  Bool;
          if Current_Item = "[" then
            Skip_Token; -- "["
           if Current_Item = "[" then
              Skip_Token; -- "["
              if Current_Item = "[" then
                Skip_Token("]"); -- "["
                This_Event.Return_Type := Boolcube;
                Skip_Token("]"); -- "]"
              else
                This_Event.Return_Type := Boolmatrix;
              end if;
              Skip_Token("]"); -- "]"
            else
              This_Event.Return_Type := Boolvector;
            end if;
            Skip_Token("]"); -- "]"
          end if;
        elsif This_kind.all = "int" then
          This_Event.Return_Type :=  Number;
          if Current_Item = "[" then
            Skip_Token; -- "["
           if Current_Item = "[" then
              Skip_Token; -- "["
              if Current_Item = "[" then
                Skip_Token; -- "["
                This_Event.Return_Type := Numcube;
                Skip_Token; -- "]"
              else
                This_Event.Return_Type := Nummatrix;
              end if;
              Skip_Token("]"); -- "]"
            else
              This_Event.Return_Type := Numvector;
            end if;
            Skip_Token("]"); -- "]"
          end if;
        else  -- Kind = Class_Name
          This_Event.Return_Type:= Object;
          if Current_Item = "[" then
            Skip_Token; -- "["
            Skip_Token("]"); -- "]"
            This_Event.Return_Type := Objvector;
          end if;
        end if;
      end if;
      Check_Event(This_Event);
      --
      --  add also implicit the corresponding "return"  Event
      --
      --    nk = Find_Event(This_Event);
      --    Check_Event(return#nk, [rv:This_Event.Return_Type])
      --  WHAT ABOUT  ALLOWING DIFFERENT TYPE PROFILES INSTEAD OF GIVING ERR?
      --   NOT POSSIBLE BECAUSE WHEN WE FIND A PLACEHOLDER WE MAY NOT KNOW THE
      --    CURRENT ARGS and RESULT TYPE (Find_Event uses only the event NAME)
      --
      if Current_Item = "," or Current_Item = ";" then
        Skip_Token;
      else
         exit;
      end if;
      if Current_Item="Vars" or
         Current_Item="vars" or
         Current_Item="Signals" or
         Current_Item="signals" or
         Current_Item="Operations"  or
         Current_Item="operations"  or
         Current_Item="State" or
         Current_Item="state" or
         Current_Item="Class"  or
         Current_Item="class"  or
         Current_Item="end"  or
         Current_Item="Behavior"  or
         Current_Item="behaviour"  or
         Current_Item="Behaviour"  or
         Current_Item="behavior"  or
         Current_Item="Transitions"  or
         Current_Item="transitions"  then
        exit;
       end if;
  exception
     when others =>
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
         raise Parsing_Error;
      else
        while  Current_Item /= "" and then
             Current_Item /= ";" and then
             Current_Item /= "State" and then
             Current_Item /= "state" and then
             Current_Item /= "Vars" and then
             Current_Item /= "vars" and then
             Current_Item /= "Behavior" and then
             Current_Item /= "behavior" and then
             Current_Item /= "Behaviour" and then
             Current_Item /= "behaviour" and then
             Current_Item /= "Transitions" and then
             Current_Item /= "transitions" and then
             Current_Item /= "Class" and then
             Current_Item /= "class" and then
             Current_Item /= "Objects" and then
             Current_Item /= "objects" loop
           Skip_Token;
        end loop;
        if Current_Item = ";" then Skip_Token; end if;
      end if;
    end;
    end loop;
--    All_Classes(Current_Class).ObservedEvents :=
--      new Bool_Table'(
--         1..All_Classes(Current_Class).ChartEvents.all'Length => True);
  exception
   when others =>
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
         raise Parsing_Error;
      else
        while  Current_Item /= "" and then
             Current_Item /= "State" and then
             Current_Item /= "state" and then
             Current_Item /= "Vars" and then
             Current_Item /= "vars" and then
             Current_Item /= "Behavior" and then
             Current_Item /= "behavior" and then
             Current_Item /= "Behaviour" and then
             Current_Item /= "behaviour" and then
             Current_Item /= "Transitions" and then
             Current_Item /= "transitions" and then
             Current_Item /= "Class" and then
             Current_Item /= "class" and then
             Current_Item /= "Signals" and then
             Current_Item /= "signals" and then
             Current_Item /= "Objects" and then
             Current_Item /= "objects" loop
           Skip_Token;
         end loop;
     end if;
  end  Parse_Operations;

  -------------------------------------------------------------------------------------
  -- elaborates the sequence of var declarations which appear inside a class definition
  -- Notice that vars Placeholders may already exists, in which case the placeholder
  --   definitions should be completed (redefined).
  -------------------------------------------------------------------------------------
  
  function Parse_VarDecl return SystemVar_Ref is
    This_Var: SystemVar_Ref := new SystemVar;
    This_Kind: String_Ref;
    PlaceHolder: Natural :=0;
--    Tmp: Int_Table_Ref;
    This_Simple: SimpleIntExpr;
  begin
   This_Var.Name := Parse_Id;
   This_Var.Chart := Current_Class;
    --
    if Current_Item = ":" then
      Skip_Token;
      This_Kind := Parse_Id;
      if This_kind.all = "obj" then
        This_Var.Initial := NullObject.Simple;
        This_Var.Kind := Object;
        if Current_Item = "[" then
          This_Var.Initial := NullStruct.Simple;
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Objcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Objmatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Objvector;
          end if;
          Skip_Token; -- "]"
        end if;
      elsif This_kind.all = "bool" then
        This_Var.Kind :=  Bool;
        This_Var.Initial := BoolFalse.Simple;
        if Current_Item = "[" then
          This_Var.Initial := NullStruct.Simple;
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Boolcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Boolmatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Boolvector;
          end if;
          Skip_Token; -- "]"
        end if;
      elsif This_kind.all = "int" then
        This_Var.Kind :=  Number;
        This_Var.Initial := Zero.Simple;
        if Current_Item = "[" then
          This_Var.Initial := NullStruct.Simple;
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Numcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Nummatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Numvector;
          end if;
          Skip_Token; -- "]"
        end if;
      else  -- Kind = Class_Name
        This_Var.Kind := Object;
        This_Var.Initial := NullObject.Simple;
        This_Var.Typeinfo := Check_Class(This_kind.all);
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Var.Kind := Objcube;
              Skip_Token; -- "]"
            else
              This_Var.Kind := Objmatrix;
            end if;
            Skip_Token; -- "]"
          else
            This_Var.Kind := Objvector;
          end if;
          Skip_Token; -- "]"
        end if;
      end if;
    end if;
    --
    if Current_Item = ":=" or else Current_Item = "=" then
      Skip_Token;
      This_Var.Initial := Parse_SimpleExpr(No_EventVars);  
      if Current_Item = "or" or else 
         Current_Item = "and" or else
         Current_Item = "+" or else
         Current_Item = "-" or else
         Current_Item(1) = '>' or else
         Current_Item(1) = '<' or else
         Current_Item(1) = '/' or else
         Current_Item(1) = '!' or else
         Current_Item(1) = '&' or else
         Current_Item(1) = '|' or else
         Current_Item(1) = '*' then
        Give_Error("no operators allowed in this context.");
        raise Parsing_Error;      
      end if;
      if Current_Item = "." then
        Give_Error("Attribute selection not allowed.");
        raise Parsing_Error;
      end if;
--      if This_Var.Kind = Undefined then
--        This_Var.Kind:= This_Var.Initial.Kind;
--      end if;
--      if This_Var.Initial.Kind = Boolvector  or
--       This_Var.Initial.Kind = Numvector or
--       This_Var.Initial.Kind = Objvector or
--       This_Var.Initial.Kind = Composite then
--      else
--        Tmp := new Int_Table(This_Var.Initial.Is_Vector.all'Range);
--        for I in This_Var.Initial.Is_Vector.all'Range loop
--          Tmp(I) := This_Var.Initial.Is_Vector(I).Literal_Value);
--        end loop;
--        This_Var.Initial.Literal_Value := Vectors_DB.NickNum(Tmp.all);
--      end if;
        --
    end if;
    return This_Var;
  end  Parse_VarDecl;

  --------------------------------------------------------------
  --  called after finding Vars:   in
  --   Vars:  x:int=3, z:obj, k,l,m 
  --------------------------------------------------------------
  procedure Parse_Vars is
--    Result: Vars_Table_Ref;
    This_Var: SystemVar_Ref;
  begin
    if  Current_Item = "Vars" or
        Current_Item = "vars" then
      Skip_Token;
      All_Classes(Current_Class).Top_State_Num := 1;
      if Current_Item = ":" then
        Skip_Token;
      end if;
      --  
      --  check for empty Vars region
      --
      --
--      while  Current_Item = "," or else Current_Item = ";" loop
      loop   -- form each variable declaration
      declare
      begin
--        Skip_Token;
        -- end of sequence of variables (maybe empty region)
        if Current_Item="Vars" or 
           Current_Item="vars" or 
           Current_Item="Signals" or
           Current_Item="signals" or
           Current_Item="Operations"  or
           Current_Item="operations"  or
           Current_Item="end" or
           Current_Item = "(" or
           Next_Item = "->" or
           Next_Item = "." or
           Current_Item="State" or
           Current_Item="state" or
           Current_Item="Class"  or
           Current_Item="class"  or
           Current_Item="Behavior"  or
           Current_Item="behavior"  or
           Current_Item="Behaviour"  or
           Current_Item="behaviour"  or
           Current_Item="Transitions"  or
           Current_Item="transitions"  then
          exit;
        end if;
        This_Var := Parse_VarDecl;
        -- check if var placeholder already exists
        for I in All_Classes(Current_Class).ChartVars.all'Range loop
          if All_Classes(Current_Class).ChartVars(I).Name.all =  This_Var.Name.all then
             This_Var.Local_Num_Key := I;
             All_Classes(Current_Class).ChartVars(I) := This_Var;
             This_Var:= null;
             exit;
          end if;
        end loop;
        if This_Var /= null then
        This_Var.Local_Num_Key := All_Classes(Current_Class).ChartVars.all'length+1;
          All_Classes(Current_Class).ChartVars := 
             new Vars_Table'(All_Classes(Current_Class).ChartVars.all & This_Var); 
        end if;
      exception
      when others =>
        Parsing_Failed := True;
        if Parsing_Errors > 5 then
           raise Parsing_Error;
        else
         while Current_Item /= "" and then
            Current_Item /= ";" and then
            Current_Item /= "State" and then
            Current_Item /= "end" and then
            Current_Item /= "state" and then
            Current_Item /= "Class" and then
            Current_Item /= "class" and then
             Current_Item /= "Behavior" and then
             Current_Item /= "behavior" and then
             Current_Item /= "Behaviour" and then
             Current_Item /= "behaviour" and then
             Current_Item /= "Transitions" and then
             Current_Item /= "transitions" and then
            Current_Item /= "Objects" and then
            Current_Item /= "objects" loop
           Skip_Token;
         end loop;
         if Current_Item = ";" then Skip_Token; end if;
         end if;
      end;
      if Current_Item = ";" or Current_Item = "," then 
           Skip_Token;
      end if;
      end loop;  -- for all variables
    end if;
  end Parse_Vars;
  
  -----------------------------------------------------------------
  --  given  non-empty sequence of token like:
  --    aa . bb . cc , dd . ee , ff
  --
  --  returns the table: [  "aa.bb.cc" , "dd.ee", "ff" ]
  --  where each item is the full name of a state.
  -----------------------------------------------------------------
  function Parse_Names_List return String_Table_Ref is
     Tmp_Name: String_Ref;
  begin
    Tmp_Name := Parse_Composite_Name;
    if Current_Item = "," then
      Skip_Token;
      declare
         More_Ids: String_Table_Ref := Parse_Names_List;
      begin
         return new String_Table'(Tmp_Name & More_Ids.all);
      end;
    else
      return new String_Table'(1..1 => Tmp_Name);
    end if;
  end Parse_Names_List;
  
  
  -----------------------------------------------------
  --  given a possily NON empty sequence of
  --   id , id(...,...) , id   returns the corresponding String_Table
  -----------------------------------------------------
  procedure Parse_Substates_List(Parent_State: State_Ref) is
     This_Ident: String_Ref;
     Current_State: State_Ref;
  begin
    if Tokens(Current_Token).Kind /= Id then
      Give_Error("State id expected");
      raise Parsing_Error;
    end if;
    This_Ident := Parse_Id;   
    --
       if Parent_State.SubStates /= null and then 
          Parent_State.SubStates.all'Length >0 and then
          This_Ident.all = "initial" then
          Give_Error (" ""initial"" must be the first of a substates list.");
          raise Parsing_Error;
       end if;
    --
    -- CREATE A NEW STATE for the class WITH GIVEN FULL_NAME
    --
    Current_State :=  Find_State(Parent_State.FullName.all & '.' & This_Ident.all);
    Current_State.Parent := Parent_State;
    Current_State.Depth := Parent_State.Depth + 1;
    Current_State.Priority := Parent_State.Priority + 1;
    Parent_State.SubStates := new States_table'(Parent_State.SubStates.all & Current_State);
    --
    if Current_Item = "(" and then Next_Next_Item /= "[" then  -- this is a composite substate
      Current_State.Kind := Composite;
      Current_State.Priority := Parent_State.Priority + 1;
      Skip_Token ( "(" );
      Parse_Substates_List(Current_State);
      Skip_Token ( ")" );
    elsif Current_Item = "(" and then Next_Next_Item = "[" then  -- this is a composite substate
      Current_State.Kind := Parallel;
      Current_State.Priority := Parent_State.Priority + 1;
      Skip_Token ( "(" );
      Parse_Substates_List(Current_State);
      Skip_Token ( ")" );
    elsif Current_Item = "[" then  -- this is a composite substate
      Current_State.Kind := Composite;
      if Parent_State.Kind /= Parallel then
         Give_Error ("Error: Unexpected Region state inside composite sequential state");
         raise Parsing_Error;
      end if;
      Current_State.Priority := Parent_State.Priority + 1;
      Skip_Token ( "[" );
      Parse_Substates_List(Current_State);
      Skip_Token ( "]" );
    end if;
    --
    if Current_Item = "," then    -- this is just a simple (or deferred) substate
      Skip_Token;
      Parse_Substates_List(Parent_State);
    end if;
  end Parse_Substates_List;


  procedure Parse_Regions_List (Parent_State: State_Ref) is
     This_Ident: String_Ref;
     Current_State: State_Ref;
  begin
    if Tokens(Current_Token).Kind /= Id then
      Give_Error("State id expected");
      raise Parsing_Error;
    end if;
    This_Ident := Parse_Id;
    --
    -- CREATE A NEW STATE for the class WITH GIVEN FULL_NAME
    --
    Current_State :=  Find_State(Parent_State.FullName.all & '.' & This_Ident.all);
    Current_State.Parent := Parent_State;
    Current_State.Depth := Parent_State.Depth + 1;
    Current_State.Priority := Parent_State.Priority + 1;
    Parent_State.SubStates := new States_table'(Parent_State.SubStates.all & Current_State);
    --
    if Current_Item = "[" then  -- this is a composite substate
      if Parent_State.Parent=null or else
           Parent_State.Parent.Kind = Parallel then
         Give_Error ("Error:  Concurrent regions must appear inside a composite sequentual state");
         raise Parsing_Error;
      end if;
      if Parent_State.Kind /= Parallel then
         Give_Error ("Error: Unexpected Region state inside composite sequential state");
         raise Parsing_Error;
      end if;
      Skip_Token ( "[" );
      Parse_Substates_List(Current_State);
      Skip_Token ( "]" );
    end if;
    --
    if Current_Item = "/"  or Current_Item = "," then
      Skip_Token;
      Parse_Regions_List(Parent_State);
    end if;
  end Parse_Regions_List;

  -------------------------------------------------------------------------
  -- If a state with the given Name is already present in the All_State table
  -- then its state ref is returned, otherwise a new state is created and 
  -- introduced in the table, and the ref of the new state returned.
  --
  -- The given This_State_Name  must univoquely identify the state. 
  -- If the state has  name  "top" (or other simple name) and it is not already
  --  defined, that this is the STATECHART  TOPLEVEL STATE.
  --  Clearly it must be the FIRST STATE DEFINITION !!! 
  --  In all other cases the state will be already definied as it has been
  --   introduced by the declaration of the parent state structure.
  --  -----------------------
  -- if the lazy_parsing  flas is set, we try to handle inside Parse_transition
  --  previously undeclared state names.
  --  When we try to find "aa.bb", if it does not exist, we look for "aa" and create bb
  --  as substate.  If neithe "aa" exists, it is created as well.
  -------------------------------------------------------------------------
  function Find_State (This_State_Name: String) return State_Ref is 
     Result: State_Ref;
     These_States: States_Table_Ref;
     Last_State: State_Ref;
     Last_Count: Natural;
     FullName: String_Ref;
  begin
    These_States := All_Classes(Current_Class).ChartStates;
    for I in These_States.all'Range loop
      FullName := These_States(I).FullName;
      if FullName.all = This_State_Name  then
        return These_States(I);
      elsif FullName'Length > This_State_Name'Length and then
        -- the given name is a unique suffix
        This_State_Name = 
          FullName(FullName'Length-This_State_Name'Length+1 .. FullName'Length) and then
          FullName(FullName'Length-This_State_Name'Length) = '.' then
           if Result = null then
               Result := These_States(I);
           else
               Give_Error("Error: Ambiguous State Name (" &
                 FullName.all & " or " &
                 Result.FullName.all & ") - please use a full pathname.");
               raise Parsing_Error;
           end if;
      end if;
    end loop;
    if Result /= null then
       return Result;
    end if;
    --
    -- if not found, then the given name is added to the states
    --  This happens for the TOPLEVEL state declaration and
    --   for the NESTED SUBSTATES FOUND when parsing a composite name stucture
    Last_State := new State;
    Last_State.FullName := new String'(This_State_Name);
    --
    if (This_State_Name /= "initial" and then
         (This_State_Name'Length <= 8 or else 
            This_State_Name(This_State_Name'Last-7 .. This_State_Name'Last) /= ".initial")) then
      Last_Count := These_States.all'Length + 1;
      Last_State.Num_Key := These_States.all'Length +1;
      All_Classes(Current_Class).ChartStates :=
           new States_Table'(These_States.all & Last_State);
    end if;
    --
    -- Should FREE the old Table ...
    --
    return Last_State;
  end Find_State; 


  -------------------------------
  --  Called by Parse Transitions when the -lazy flag is set
  --  The stste structure of the class is deduced by the transitions structure
  --  As new State Names  are found, they are introduced in the class ChartStates table.
  -------------------------------
  function Lazy_Find_State (This_State_Name: String) return State_Ref is
    Top_State: State_Ref;
    Parent: State_Ref;
    This_State: State_Ref;
    --
    Result: State_Ref;
    These_States: States_Table_Ref;
    FullName: String_Ref;
  begin
    if not Lazy_Parsing and not Almost_Lazy_Parsing then
       return Find_State (This_State_Name);
    end if;
    --
    -- if lazy_parsing but the State already exists, return it!
    --  
    These_States := All_Classes(Current_Class).ChartStates;
    for I in These_States.all'Range loop
      FullName := These_States(I).FullName;
      if FullName.all = This_State_Name  then
        return These_States(I);
      elsif FullName'Length > This_State_Name'Length and then
        -- the given name is a unique suffix
        This_State_Name =
          FullName(FullName'Length-This_State_Name'Length+1 .. FullName'Length) and then
          FullName(FullName'Length-This_State_Name'Length) = '.' then
           if Result = null then
               Result := These_States(I);
           else
               Give_Error("Error: Ambiguous State Name (" &
                 FullName.all & " or " &
                 Result.FullName.all & ") - please use a full pathname.");
               raise Parsing_Error;
           end if;
      end if;
    end loop;
    if Result /= null then
       return Result;
    end if;
    --
    if All_Classes(Current_Class).ChartStates.all'Length = 0 then
      --  missing Top State 
      Top_State := Find_State("Top");  -- inserts Top_State in ChartStates
      All_Classes(Current_Class).Top_State_Num := 1;
      Top_State.Kind := Composite;
     else
      Top_State := All_Classes(Current_Class).ChartStates(1);
    end if;
    --
    -- find the parent state (possibly creating it)
    if Prefix(This_State_Name) = "" then
       Parent := Top_State;
    else
       Parent :=  Lazy_Find_State(Prefix(This_State_Name));
       -- with Full_name preperly adjusted
    end if; 
    if This_State_Name = "Top" then 
        return Top_State; 
    end if;
    --if This_State_Name /= "initial" and then
    --         (This_State_Name'Length < 8 or else
    --           This_State_Name(This_State_Name'Last-7 .. This_State_Name'Last) 
    --              /= ".initial") then
      This_State := Find_State(Parent.FullName.all & '.' & Suffix(This_State_Name));
    --else
     --  This_State:= new State;
     --  This_State.FullName := 
     --    new String'(Parent.FullName.all & '.' & Suffix(This_State_Name));
    --end if;
    -- BUG   This_State=NULL when initial
    -- 
    if This_State.Parent = null then
      -- was just created, must be properly initialized
      This_State.Parent := Parent;
      This_State.Depth := Parent.Depth+1;
      This_State.Priority := Parent.Priority+1;
      if Parent.Kind = Simple then
         Parent.Kind := Composite;
      end if;
      -- ADD TO PARENT SUBSTATES ONLY IF NOT DEFAULT_INITIAL
      if This_State /= null and then 
            This_State.FullName.all /= "initial" and then
             (This_State.FullName.all'Length < 8 or else
               This_State.FullName(This_State.FullName'Last-7 .. This_State.FullName'Last) 
                  /= ".initial") then
        Parent.SubStates :=
          new States_Table'(Parent.SubStates.all & This_State);
       end if;
    end if;
    -- in any case we return This_State, even if not linked to the parent.
    return This_State;
  end Lazy_Find_State;


  -------------------------------------------------------------------------
  -- If a state with the given Name is already present in the All_State table
  -- then its state ref is returned, otherwise returns null;
  -------------------------------------------------------------------------
  function Just_Find_State (This_State_Name: String) return State_Ref is
    Result: State_Ref;
    These_States: States_Table_Ref;
  begin
    These_States := All_Classes(Current_Class).ChartStates;
    for I in These_States.all'Range  loop
      if These_States(I).FullName.all = 
                 This_State_Name then
        Result := These_States(I);
        exit;
      elsif These_States(I).FullName'Length > This_State_Name'Length and then
        -- the given name is a unique suffix
        This_State_Name = These_States(I).FullName(
             These_States(I).FullName'Length -This_State_Name'Length +1 ..
               These_States(I).FullName'Length) then
           if Result = null then
               Result := These_States(I);
           else
               Give_Error("Error: Ambiguous State Name (" &
                 These_States(I).FullName.all & " or " &
                 Result.FullName.all & ") - please use a full pathname.");
               raise Parsing_Error;
           end if;
      end if;
    end loop;
    --
    return Result;
  end Just_Find_State;


  -- 
  -- return the position id of the given Chart, 
  --  (for a new chart return a new position id)
  --
  function Check_Object (Chart_Name: String) return Integer is
     Tmp: Chart;
  begin
    for I in 1..All_Charts.all'Length loop
       if All_Charts(I).Name.all = Chart_Name then
         return I;
       end if;
    end loop;
    Tmp.Name := new String'(Chart_Name);
    --  symbolic: record the last poimnt of use 
    Tmp.Top_State_Num := -Tokens(Current_Token).Line;
    All_Charts := new Chart_Table'(All_Charts.all & Tmp);
    return All_Charts.all'Length;
  end Check_Object;

  --
  -- return the position index (in All_Classes) of the given class.
  --  if not present, a class placeholder is created.
  -- 
  --  COME DISTINGUERE PLACEHOLDERS noin definiti (errori) da
  --  classi vuote (e.g. Tokens?)
  --
  function Check_Class (Class_Name: String) return Integer is
     Tmp: Chart;
  begin
    for I in 1..All_Classes.all'Length loop
       if All_Classes(I).Name.all = Class_Name then
         return I;
       end if;
    end loop;
    Tmp.Name := new String'(Class_Name);
    All_Classes := new Chart_Table'(All_Classes.all & Tmp);
    return All_Classes.all'Length;
  end Check_Class;

  function Find_Class (Class_Name: String) return Integer is
     Tmp: Chart;
  begin
    for I in 1..All_Classes.all'Length loop
       if All_Classes(I).Name.all = Class_Name then
         return I;
       end if;
    end loop;
    return 0;
  end Find_Class;

  function Find_Object (Chart_Name: String) return Integer is
     Tmp: Chart;
  begin
    for I in 1..All_Charts.all'Length loop
       if All_Charts(I).Name.all = Chart_Name then
         return I;
       end if;
    end loop;
    return 0;
  end Find_Object;
  --
  -- return the position id of the given Chart,
  --  (for a new chart return a new position id)
  --  if ChartName is a system var or event var returns -1
  --
--  function Check_Chart (Chart_Name: String; 
--                         Env: EventVars_Table) return Integer is
--    This_Simple: SimpleIntExpr;
----    Tmp_Ref: String_Ref;
--    Tmp: Chart;
--  begin
--    --
--    This_Simple.Local_Variable := Find_Var(Chart_Name);
--    if This_Simple.Local_Variable /= 0 then 
--      return -1 ;
--    end if;
--    for I in Env'Range loop
--      if Env(I).Name.all = Chart_Name then
--         This_Simple.Event_Variable :=  Env(I);
--         return -1;
--      end if; 
 --   end loop;
--    for I in 1..All_Charts.all'Length loop
--       if All_Charts(I).Name.all = Chart_Name then
--         return I;
--       end if;
--    end loop;
--    Tmp.Name := new String'(Chart_Name);
--    All_Charts :=
--       new Chart_Table'(All_Charts.all & Tmp);
--    return All_Charts.all'Length;
--    --
--  end Check_Chart;


  ----------------------------------
  -- called by Parse_Object_Declarations when
  --  we find a "Chart" declaration
  --  we find an object declaration    Obj: Class ( var -> Value)
  --  we find a sequence of object declarations  Obj1,Obj2,Obj3: Class
  --  FAILS in the case:   Obj1,Obj2,Obj3: Class (var -> Value)
  ----------------------------------
  procedure Create_Object_Instance (This_Object_Name: String_Ref;
                                    This_Class_Name: String_Ref) is
    Vars_Count: Natural; 
    This_Var: SystemVar_Ref; 
--    This_State: State_Ref;
    This_Object: Chart;
    This_UML_Object: UML_Object;
    Saved_Lazy_Flag: Boolean;
  begin
    Saved_Lazy_Flag := Flags.Lazy_Parsing;
    Flags.Lazy_Parsing := False;
    --
    --  objects can be named even before declaration 
    --   if this happen, the object instance is already present in All_Charts
    --   and its name is already defined (all other fields not)
    --   (in particular, until fully defined, Object.ChartParent = 0)
    --
    --  ERR and OUT  objects are teh only oly ones which can be redeclared.
    --   (overloading their default definitions)
    --
    Current_Object := Find_Object(This_Object_Name.all);
    if Current_Object > 2 and then 
        All_Charts(Current_Object).ChartParent > 0 then
      Give_Error ("Error: Object " & This_Object_Name.all &
                    " already defined!");
      raise Parsing_Error;
    end if;
    --
    if Current_Object in 1..2  and then
        All_Charts(Current_Object).Top_State_Num > 0 then
      Give_Error ("Error: Object " & This_Object_Name.all &
                    " already defined!");
      raise Parsing_Error;
    end if;
    --
    if Current_Object = 0 then
      This_Object.Name := This_Object_Name;
      All_Charts :=
         new Chart_Table'(All_Charts.all & This_Object);
      Current_Object := All_Charts.all'Length;
    end if;
    --
    This_UML_Object.The_Object := Current_Object;
    --
    -- if Current_Object = 1 or 2 or  >2&ChartParent=0 then the existing
    --   instance is recycled
    --
    Current_Class:= Find_Class(This_Class_Name.all);
    if Current_Class = 0 then
      Give_Error ("Error: Chart " & This_Class_Name.all & " not yet defined!");
      raise Parsing_Error;
    end if;
    All_Charts(Current_Object).ChartParent := Current_Class;
    All_Charts(Current_Object).NiceParallelism := All_Classes(Current_Class).NiceParallelism;
    All_Charts(Current_Object).Top_State_Num := 
         All_Classes(Current_Class).Top_State_Num;
    --
    if All_Charts(Current_Object).Top_State_Num > 0 then
      Active_Charts := new Num_Table'(Active_Charts.all & Current_Object);
      All_Charts(Current_Object).Chart_Position := Active_Charts.all'Length;
    end if;
    --
    --  vars cannot be simply shared because the All_Vars table must be extended
    --  and new initial values maybe provided.
    -- (we might adopt a chart "vars_Base" and chart vars_initial)
    --
    Vars_Count := All_Classes(Current_Class).ChartVars.all'Length;
    --
    All_Charts(Current_Object).ChartVars :=
         new Vars_Table(1..Vars_Count);
    for I in 1..Vars_Count loop
      This_Var := new SystemVar;
      This_Var.Name := All_Classes(Current_Class).ChartVars(I).Name;
      This_Var.Local_Num_Key := I;
      This_Var.Chart := Current_Object;
      This_Var.Observed  := False;
      This_Var.Initial := All_Classes(Current_Class).ChartVars(I).Initial;
      This_Var.Kind := All_Classes(Current_Class).ChartVars(I).Kind;
      This_Var.TypeInfo := All_Classes(Current_Class).ChartVars(I).TypeInfo;
      All_Charts(Current_Object).ChartVars(I) := This_Var;
    end loop;
    --
--   All_Vars := new Vars_Table'(All_Vars.all &
--                        All_Charts(Current_Object).ChartVars.all);
   --
   --  events can be shared
   --
   All_Charts(Current_Object).ChartEvents := 
        All_Classes(Current_Class).ChartEvents;  
   --
   --  observations cannot be shared because they are selective
   --
--   All_Charts(Current_Object).ObservedEvents :=
--      new Bool_Table'
--          (1..All_Classes(Current_Class).ObservedEvents.all'Length => true);
   --
   -- States can  be shared, because their runtime_position is now local
   --
   All_Charts(Current_Object).ChartStates :=
        All_Classes(Current_Class).ChartStates;
   --
   --   transitions are shared with the parent
   --
   All_Charts(Current_Object).ChartTransitions :=
       All_Classes(Current_Class).ChartTransitions;
   --
   All_Charts(Current_Object).ChartInterferences :=
       All_Classes(Current_Class).ChartInterferences;
   --
   -- Active_Charts is expanded each time an active  Chart or Object
   --  declaration is found N.B:, only if object isi active.
   --
   All_Charts(Current_Object).ChartStates_Base := 0;
   if All_Charts(Current_Object).Top_State_Num > 0 and  then
        Active_Charts.all'Length > 1 and then
       Current_Object = Active_Charts(Active_Charts.all'length) then
     --
     All_Charts(Current_Object).ChartStates_Base := 
       All_Charts(Active_Charts(Active_Charts.all'length-1)).ChartStates_Base +
       All_Charts(Active_Charts(Active_Charts.all'length-1)
                ).ChartStates(1).LastRuntimePosition;
   end if;

   --  
   --  parse the object attribute initializations ...
   -- 
   if Current_Item  = "(" then
     Skip_Token;
     loop
       declare
	 This_Item: String := Parse_Id.all;
	 This_Var: Natural := Find_Var(This_Item);
	 This_Expr: SimpleIntExpr_Ref;
	 This_SVar: SystemVar_Ref;
       begin
         if This_Var = 0 then
            Give_Error ("Error: Undefined attribute name """ &This_Item & """");
            raise Parsing_Error;
         end if;
	 if Current_Item  = "=>" or 
             Current_Item  = "="  or 
              Current_Item  = "->"  then
    	    Skip_Token;
         else
           Give_Error (" ""=>"" expected");
           raise Parsing_Error;
         end if;
         --  at this point, event var initialization can contain Global Object names!!!
         -- we should not create new implicit vars...
	 This_Expr := Parse_SimpleExpr(No_EventVars);
         if Current_Item = "." then
           Give_Error("Attribute selection not allowed.");
           raise Parsing_Error;
         end if;
	 This_SVar := All_Charts(Current_Object).ChartVars(This_Var);
	 This_SVar.Initial := This_Expr;
--         if This_SVar.Kind = Undefined then
--            This_SVar.Kind := This_Expr.Kind;
--         end if;
         -- if Kind = Object maybe we should set the var.TypeInfo
         This_UML_Object.Bindings := 
           new Bindings_Table'(This_UML_Object.Bindings.all &
                 Binding'(This_SVar.Name, This_SVar.Initial.Image) );
       end;
       -- 
       if Current_Item = ")" then
	 Skip_Token;
         if Current_Item = ";" then
	   Skip_Token;
         end if;
	 exit;
       elsif Current_Item = ";" then
        Skip_Token;
       end if;
       if Current_Item /= "," then
          Give_Error("ERROR: Expecting a "","" or a "")"" to continue or end the parameters list.");
          raise Parsing_Error;
       end if;
       Skip_Token (",");
     end loop;
    end if;
    UML_Configuration := new Objects_Table'(UML_Configuration.all & This_UML_Object);
    Flags.Lazy_Parsing := Saved_Lazy_Flag;
  end Create_Object_Instance;

  function Parse_Observations(MyObservations: Observations_Table)
                      return Observations_Table;


  procedure Parse_Object_Declarations is
    This_Object_Name: String_Ref;
    This_Class_Name: String_Ref;
    These_Objects: String_Table_Ref := new String_Table(1..0);
    Saved_Token: Natural :=0;
  begin
    loop
      This_Object_Name :=  Parse_Id;
      These_Objects := new String_Table'(These_Objects.all & This_Object_Name);
      if Current_Item = "," then
        Skip_Token; -- ,
      else
        -- now we have all objects names in These_Objects
        Skip_Token(":");
        exit;
      end if;
    end loop;
    --
    This_Class_Name :=  Parse_Id;
    Saved_Token := Current_Token;
    for K in These_Objects.all'Range loop
      -- adjust Current_Token - to be recycled for all instances
      Current_Token := Saved_Token;
      Create_Object_Instance (These_Objects(K), This_Class_Name);
    end loop;
    if Current_Item = ";" then
      Skip_Token;
    end if;
    --
  exception
    when others =>
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
        raise;
      else
        while Current_Item /= "" and then
            Current_Item /= "class" and then
            Current_Item /= "Class" and then
            Current_Item /= "Objects" and then
            Current_Item /= "objects" and then
            Current_Item /= "abstractions" and then
            Current_Item /= "Abstractions" and then
            Next_Item /= ":" loop
          Skip_Token;
        end loop;
      end if;
  end Parse_Object_Declarations;

  procedure ParseIncluded (F: File_Type);

  -------------------------------------------------------------------------
  --  Elaborates the list of States definitions,
  -- uploads all of them in the "All_States" tables,
  -- uploads all the transitions if the "ChartTransitions" table,
  -- uploads all the events (Triggers/Actins) in the "ChartEvents" table,
  -- initializes the "This_UML_Object"  variable
  --  (which might become a table in the case of multiple machine)
  -------------------------------------------------------------------------
  procedure Parse (Uml_File_Name: String) is
  begin
    Parsing_Failed := False;
    Parsing_Errors :=0;
    Main_File_Name := new String'(Uml_File_Name);
    ---
    Previous := '^';
    Current_File_Name := new String'(Uml_File_Name);
    Tokens := new Tokens_Table(1..0);
    --
    -- check existence and readability of file, otherwise exits
    --
    begin
      Open(F, In_File,Uml_File_Name);
      Read_Tokens(F);
      Close(F);
      if Current_Item = "" then
        Give_Error ("The UML file is EMPTY ...");
        raise Parsing_Error;
      end if;
    exception
      when others =>
         Give_Error ("The UML file is not readable ...");
         if Is_Open(F) then Close(F); end if;
         raise Parsing_Error;
    end;
    --
    ParseIncluded(F);
    --
    if All_Observations.all'Length=0 then
        All_Observations := new Observations_Table'(Default_Rules);
    end if;
    --
    Check_Missing_Definitions;
    --
    if Active_Charts.all'Length < 1 then
--      Give_Error (" Error: No object declaration found !!!! ");
      Put_line(Current_Error," Error: No object declaration found !!!! ");
      raise Parsing_Error;
    end if;
    --
    --  copy class OUT /ERR  events to chart instances
    --
    All_Charts(1).ChartEvents := All_Classes(1).ChartEvents;
    All_Charts(2).ChartEvents := All_Classes(2).ChartEvents;
    --
--
--    if Active_Charts.all'Length >= 1 then
--      UML_Types.This_UML_Object := All_Charts(Active_Charts(1)).ChartStates(1);
--    end if;
   --
   if Parsing_Failed or Parsing_Errors >0 then
      raise Parsing_Error;
   end if;
   --
   exception
    when UML_Error =>
        Close(F);
        raise;
    when Parsing_Error =>
        if Is_Open(F) then Close(F); end if;
        raise;
    end Parse;
  -----------------------------------------------------------------


  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure ParseIncluded (F:File_Type) is
    This_Object_Name: String_Ref;
    This_Class_Name: String_Ref;
    Line: Natural :=0;
  begin 
      --  until EOF
    while Current_Item /= "" loop  
     declare
     begin
      --
      if Current_Item = "Chart" then
      ---------------------------------------
      --   Chart  OO is
      --     ...
      --   end OO;
      ---------------------------------------
         --- DEPRECATED
        Skip_Token;
        --
        -- in general, a chart id may already exist if a signal
        --  addressed to it has already been eleborated
        -- (if not already existing, create a default Chart object)
        -- THIS IS NOW FALSE:  CHARTS EXIST ONLU AFTER DECLARATION
        --  AND CANNOT BE MENTIONED BEFORE DECLARATION AS TARGET OF SIGNALS.
        --  CHARTS MAY PERHAPS BE REFINED ???
        --  CHARTS CAN BE NON ACTIVE  (e.g. OUT, ERR)
        --
        This_Class_Name :=  Parse_Id;
        Parse_Class_Body (This_Class_Name);
        This_Object_Name := This_Class_Name;
        Create_Object_Instance (This_Object_Name, This_Class_Name);
        --
      elsif Current_Item = "Class" or else
            Current_Item = "class" then
        -----------------------------------
        --   Class  MyClass is
        --     ....
        --   end MyClass;
        ------------------------------------
        Skip_Token;
        This_Class_Name :=  Parse_Id;
        Parse_Class_Body(This_Class_Name);
        --
     elsif  Current_Item = "Objects" or
            Current_Item = "objects" then
        ---------------------------------
        --   Objects:
        ---------------------------------
        Skip_Token;  -- Objects
        if Current_Item = ":" then
           Skip_Token;  -- ;
        end if;  
        --
      elsif Current_Item = "Object" then
        -----------------------------------
        --    Object OO: MyClass;
        -----------------------------------
        -- DEPRECATED  !!!!
        Skip_Token;
        Parse_Object_Declarations;
        --
     elsif Next_Item=":" or Next_Item="," then 
        --------------------------------------
        --    OO: MyClass;
        --------------------------------------
        Parse_Object_Declarations;
        --
      elsif  Current_Token /= Tokens.all'Length and then
          (Current_Item = "Abstractions" or else
            Current_Item = "abstractions") then
        ----------------------------------------
        --  Abstractions {
        --     ...
        --  };
        ------------------------------------------
        Skip_Token;
        Skip_Token ("{");
        All_Observations :=
           new Observations_Table'(Parse_Observations(All_Observations.all));
        Skip_Token("}");
      elsif Current_Item = "#" and then Next_Item = "include" then
        ------------------------------------------
        Skip_Token("#");
        Skip_Token("include");
        if Included_File_Name /= null then
           Give_Error("#include directive can only appear in main system file");
           Parsing_Errors := Parsing_Errors+1;
           raise Parsing_Error;
        end if;
        Included_File_Name := Parse_Id;
        while Current_Item = "." loop
          Skip_Token(".");
          Included_File_Name := 
            new String'(Included_File_Name.all & "." & Parse_Id.all);
        end loop;
        --  #include included.txt
        --
        --  BACKUP MAIN PARSING DATA
        --
        MainTokens := Tokens;
        MainCurrent_Token := Current_Token;
        MainCurrent_Position := Current_Position;
        MainLine_Number := Line_Number;
        MainLast_Line := Last_Line;
        MainInput_line := Input_line;
        MainPrevious := Previous;
        MainLine_Length := Line_Length;
        --
        --  INITIAL SETUP FOR INCLUDED FILE DATA
        --
        Current_File_Name := Included_File_Name;
        Tokens := new Tokens_Table(1..0);
        Line_Number :=0;
        Current_Token :=0;
        Previous := '^';
        Line_Length :=0; 
        --
        begin
          Open(Fincluded, In_File, Included_File_Name.all);
          Read_Tokens(Fincluded);
              Close(Fincluded);
          if Current_Item = "" then
            Give_Error ("The UML file is EMPTY ...");
            raise Parsing_Error;
          end if;
          -------
          ParseIncluded(Fincluded);
          -------
        exception
          when others =>
             Give_Error ("The UML file is not readable ...");
             if Is_Open(Fincluded) then Close(Fincluded); end if;
             raise Parsing_Error;
        end;
        --
        --  RESTORE MAIN PARSING DATA
        --
        Tokens := MainTokens;
        Current_Token := MainCurrent_Token;
        Current_Position := MainCurrent_Position;
        Line_Number := MainLine_Number;
        Last_Line := MainLast_Line;
        Input_line := MainInput_line;
        Previous := MainPrevious;
        Line_Length := MainLine_Length;
        Current_File_Name := Main_File_Name;
        Included_File_Name := null;
        ------------------------------------------
      --
      else
        Give_Error( "Class  or Object  declaration expected");
        Parsing_Failed := True;
        if Parsing_Errors > 5 then
          raise Parsing_Error;
        else
          Parsing_Failed := True;
          Parsing_Errors := Parsing_Errors+1;
          while Current_Item /= "" and then
               Current_Item /= "Class" and then
               Current_Item /= "class" and then
               Current_Item /= "Abstractions" and then
               Current_Item /= "abstractions" loop
            Skip_Token;
          end loop;
        end if;
      end if;
     end;
    end loop;
    --
    --  end main loop
  end ParseIncluded;
  -----------------------------------------------------------------


  ---------------------------------------------------------------
  --  ids=  obj.attr,  literal, special_token, globalobj
  ---------------------------------------------------------------
  procedure Check_Term (Ids: String_Table) is
    Result: Natural := 0;
    TheseVars: Vars_Table_Ref;
  begin
    if Ids'Length =2 then
      for I in 1..All_Charts.all'Length loop
         if All_Charts(I).Name.all = Ids(1).all then
           Result := I;
           exit;  
         end if;
      end loop;       
      if Result = 0 then
         Give_Error (" Object name expected ...  (obj.attr)" );
         raise Parsing_Error;
      end if;
      if Ids(2).all = "queuesize" then return; end if;
      TheseVars :=  All_Charts(Result).ChartVars;
      for I in TheseVars.all'Range loop
        if TheseVars(I).all.Name.all = Ids(2).all then
          return;
        end if;
      end loop;
     Give_Error (" Attribute name expected ...  (obj.attr)" );
     raise Parsing_Error;      
     --
    elsif Ids'Length =1 then
      if Is_Number(Ids(1).all) or else
         Ids(1).all = "null"  or else
         Ids(1).all = "true"  or else
         Ids(1).all = "True"  or else
         Ids(1).all = "TRUE"  or else
         Ids(1).all = "false"  or else
         Ids(1).all = "False"  or else
         Ids(1).all = "FALSE"  or else
         Ids(1).all = "maxqueuesize"  then
         return;
      else
        for I in 1..All_Charts.all'Length loop
           if All_Charts(I).Name.all = Ids(1).all then
             return;
           end if;
        end loop;
        Give_Error (" Literal or object name expected .. " );
        raise Parsing_Error;
      end if;
    end if;
    --
  end Check_Term;


  procedure Check_Unused(This: Rule_Left; Varname: String) is
  begin
    for J in This'Range loop
      --
      if  This(J).LTerm/= null and then 
        This(J).LTerm.all = Varname then
        Give_Error("Illegal multiple use  of variable in observation rule");
        raise Parsing_Error;
      end if; 
      if This(J).LPartner /= null and then
        This(J).LPartner.all = Varname then
        Give_Error("Illegal multiple use of variable in observation rule");
        raise Parsing_Error;
      end if;
      if This(J).LOp /= null and then
        This(J).LOp.all = Varname then
        Give_Error("Illegal multiple use of variable in observation rule");
        raise Parsing_Error;
      end if;
      if This(J).LArgs /= null then
        for A in This(J).LArgs.all'Range loop
          if This(J).LArgs(A).all = Varname then
            Give_Error("Illegal multiple use of variable in observation rule");
            raise Parsing_Error;
          end if;
        end loop;
      end if; 
      --
      if This(J).IdsR /= null then
        for I in This(J).IdsR.all'Range loop 
          if This(J).IdsR(I).all = Varname then
            Give_Error("Illegal multiple use  use of variable in observation rule");
            raise Parsing_Error;
          end if;
        end loop;
      end if;
    end loop;
  end Check_Unused;


  procedure Check_Used(This: Rule_Left; Varname: String) is
  begin
    for J in This'Range loop
      --
      if  This(J).LTerm/= null and then
        This(J).LTerm.all = Varname then
        return;
      end if;
      if This(J).LPartner /= null and then
        This(J).LPartner.all = Varname then
        return;
      end if;
      if This(J).LOp /= null and then
        This(J).LOp.all = Varname then
        return;
      end if;
      if This(J).LArgs /= null then
        for A in This(J).LArgs.all'Range loop
          if This(J).LArgs(A).all = Varname then
             return;
          end if;
        end loop;
      end if;
      --
      if This(J).IdsR /= null then
        for I in This(J).IdsR.all'Range loop
          if This(J).IdsR(I).all = Varname then
             return;
          end if;
        end loop;
      end if;
    end loop;
    --
    Give_Error ("Abstraction Rule contains undefined variables ...");
    raise Parsing_Error;
    --
  end Check_Used;

  ------------------------------------------------------
  -- First check:  $v are NOT decalred twice
  -- Second check  $v used in RLabels ARE declared
  --  TO BE ADJUSTED     Largs NOT checked ...
  -- 
  ------------------------------------------------------
  procedure Check_Observation(OP: Observation_Rule) is
     OK: Boolean := False;
     Declared: String_Table_Ref := Empty_String_Table_Ref;
  begin
    -- restricted check for Action rules to allow
    --    foo($1,$2)  and $1 = $2  -> bb
    --
    if Op.Kind= ActionKind then
       if OP.Left(1).LTerm /= null and then
          OP.Left(1).LPartner /= null and then
          OP.Left(1).LPartner(1) = '$' and then
          OP.Left(1).LPartner.all = OP.Left(1).LTerm.all then
         Give_Error("Illegal multiple use  use of variable in observation rule");
         raise Parsing_Error;
       end if;
       if OP.Left(1).LTerm /= null and then
          OP.Left(1).LOp /= null and then
          OP.Left(1).LOp(1) = '$' and then
          OP.Left(1).LOp.all = OP.Left(1).LTerm.all then
         Give_Error("Illegal multiple use  use of variable in observation rule");
       end if;
       if OP.Left(1).LPartner /= null and then
          OP.Left(1).LOp /= null and then 
          OP.Left(1).LOp(1) = '$' and then 
          OP.Left(1).LOp.all = OP.Left(1).LPartner.all then 
         Give_Error("Illegal multiple use  use of variable in observation rule");
         raise Parsing_Error;
       end if;
       --
       --
       for I in OP.Rlabels.all'Range loop
          if OP.Rlabels(I)(1)= '$' then
            Check_Used(OP.Left.all, OP.Rlabels(I).all);
          end if;
       end loop;
    end if;
    --
    -- CHECK FOR STATE OBSERVATIONS:
    --  1)  First occurrence of a var in the LHS is inside a valid var declaration (obj.attr[u] = $id)
    --  2)  All vars in the  RHS have been declared in the LHS
    --
    if Op.Kind /= ActionKind then  -- i.e. StateKind
      for I in OP.Left.all'Range loop
        --
        -- InState(Obj, Top.S1.s2)
        -- no variables allowed in this context
        --
        if OP.Left(I).Largs /= null then
          for J in OP.Left(I).Largs.all'Range loop
            if OP.Left(I).Largs(J)(1) = '$' then
              Give_Error("Error: no $ variables allowed in this context");
              raise Parsing_Error;
            end if;
          end loop;
        end if;
        --
        --  obj.var.ind1ind2 op  RHS
        -- A) No undlared Vars in LHS
        -- B) LeftOp is "=" and LHS'Length=1 and LHS(1)="$" --> declare LHS(1) 
        -- C) No undlared Vars in RHS
        --
        if OP.Left(I).IdsL /= null then
          -- A) No undlared Vars in LHS
          for J in OP.Left(I).IdsL.all'Range loop
            if OP.Left(I).IdsL(J)(1) = '$' then
               -- check variable is declared
               OK := False;
               for K in Declared'Range loop
                 if Declared(K).all = OP.Left(I).IdsL(J).all then
                   OK := True;
                   exit;
                 end if;
               end loop;
               if not OK then
                 Give_Error("Error: no $ variables allowed in this context");
                 raise Parsing_Error;
               end if;
            end if;
          end loop;
          --
          if OP.Left(I).LeftOp = EQ and then
             OP.Left(I).IdsR.all'Length = 1 and then
             OP.Left(I).IdsR(1)(1) = '$' then
            -- B) LeftOp is "=" and LHS'Length=1 and LHS(1)="$" --> declare LHS(1)
            -- if not declared yet, declare it
            OK := False;
            for K in Declared'Range loop
              if Declared(K).all = OP.Left(I).IdsR(1).all then
                OK := True;
                exit;
              end if;
            end loop;            
            if not OK then
               Declared := new String_Table'(Declared.all & OP.Left(I).IdsR(1));
            end if;
          else
            -- C) No undlared Vars in RHS
            for J in OP.Left(I).IdsR.all'Range loop
              if OP.Left(I).IdsR(J)(1) = '$' then
                 -- check variable is declared
                 OK := False;
                 for K in Declared'Range loop
                   if Declared(K).all = OP.Left(I).IdsR(J).all then
                     OK := True;
                     exit;
                   end if;
                 end loop;
                 if not OK then
                   Give_Error("Error: no $ variables allowed in this context");
                   raise Parsing_Error;
                 end if;
              end if;
            end loop;  -- check inside IdsR
          end if;  -- IdsR not single var
        end if;  -- IdsL /= null
      end loop;  -- check inside all Left side compponents of the rule
      --
      -- check inside Rlabels
      for RL in OP.Rlabels.all'Range loop
        if OP.Rlabels(RL)(1)= '$' then
          -- check variable is declared
          OK := False;
          for K in Declared'Range loop
            if Declared(K).all = OP.Rlabels(RL).all then
              OK := True;
              exit;
            end if;
          end loop;
          if not OK then
            Give_Error("Error: $ variable not defined  ");
            raise Parsing_Error;
          end if;
        end if;
     end loop;
    end if;   -- state / Action rule
    --
  end Check_Observation;

  procedure MarK_Observed_Var (Name: String_Ref) is 
  begin
    for O in All_Charts.all'Range loop
      if All_Charts(O).ChartVars /= null then
          for V in All_Charts(O).ChartVars.all'Range loop
            if All_Charts(O).ChartVars(V).Name.all = Name.all then
                 All_Charts(O).ChartVars(V).Observed := True;
            end if;
          end loop;
      end if;
    end loop;
    null;
  end MarK_Observed_Var;


  function Has_Been_Defined (TheVar: String; Left_Side: Rule_Left) return Boolean is
  begin
     if Left_Side'Length = 0 then return False; end if;
      if Left_Side(1).LTerm /= null and then TheVar = Left_Side(1).LTerm.all then return True; end if;
      if Left_Side(1).LPartner /= null and then TheVar = Left_Side(1).LPartner.all then return True; end if;
      if Left_Side(1).LOp /= null and then TheVar = Left_Side(1).LOp.all then return True; end if;
      if Left_Side(1).LArgs /= null then
         for J in Left_Side(1).LArgs.all'Range loop
            if Left_Side(1).LArgs(J) /= null and then Left_Side(1).LArgs(J).all = TheVar then return True; end if;
         end loop;
      end if;
      return False;
  end Has_Been_Defined;

  function Parse_Left_Side (Kind: AbstractionKind;
                             Previous: Rule_Left) return Rule_Left is
    This: Rule_Left_Elem;
    OK: Boolean;
  begin
    --  Tutte le  Abstrction Rules hanno la forma: 
    --
    --       leftside {and leftsize} -> rightside
    --
    if Kind = ActionKind then
      ----------------------------   ACTION ---------------------------------
      --
      --   First Rule_Left_Item
      --
      --  Assignments are observed at low level by  
      --  $obj:assign(var,$value)  -> assign
      --  $obj:assign(var,$index,$value)  -> assign
      --  $obj:assign(var,$index,$index2,$value)  -> assign
      --
      --  $obj:assign($var,$value)  -> assign   #####  SHOULD BE AN ERROR!!!!
      --
      --  obj:target.foo(arg1)  ->  changed(foo,x)
      --  obj:accept(event,args) -> executed(evenet)
      --  obj:lostevent(event,args) -> discarded(event)
      --  target.foo(*,$1,$2,$*)      ->  afoo($1,$2,$*)
      --  foo(*,$1,$2,$*)      ->  afoo($1,$2,$*)
      --
      --  Secondary Rule_Left_Items 
      --
      --   $1 releop  $2
      --   $1  relop literal
      --   literal relop $2
      --   literal relop literal
      --       (relop is   =, ==, /=, != )   -- MISSING  ">" e "<"
      --
      --    leftsideprefix   and
      --      leftside  -> 
      --
      -- Parse term identifier or executing object
      --
      --  NON POSSIAMO PERMETTERE multiple src:target.obj rules  perche' la valutazione
      --  delle abstract labels (AbstractView) sarebbe molto piu' complessa e andrebbe
      --  riscritta
      --   I.e.   Action    obj1:foo and obj2:bar  ->  foobar
      --
      if Previous'Length =0 and then Next_Item =":" then
        This.LTerm := Tokens(Current_Token).Item;
        if  Current_Item = "$*" then
          Give_Error("  ""$*"" not allowed in this context.");
          raise Parsing_error;
        end if;
        Skip_Token;  -- obj:
        Skip_Token (":");
      end if;
      --
      --  parse event target
      if Previous'Length =0 and then Next_Item ="." then
        This.LPartner := Tokens(Current_Token).Item;
        if  Current_Item = "$*" then
          Give_Error("  ""$*"" not allowed in this context.");
          raise Parsing_error;
        end if;
        Skip_Token;   -- target.
        Skip_Token (".");
      end if;
      --
      -- parse eventname and params
      if Previous'Length =0 and then (
         Next_Item = "(" or else
         Next_Item = "->" or else
         Next_Item =  "and" )then
        This.LOp := Tokens(Current_Token).Item;  
        if  Current_Item = "$*" then
          Give_Error("  ""$*"" not allowed in this context.");
          raise Parsing_error;
        end if;
        Skip_Token;   -- foo (
        --
        -- 
        if This.LOp.all =  "assign"  and then Current_Item /= "(" then
          Give_Error("  Incorrect structure of assign rule");
          raise Parsing_error;
        end if;
        --
        -- parse event parameters
        if Current_Item ="(" then
          Skip_Token ("(");
          if (This.LOp.all = "discarded" or
              This.LOp.all =  "accept" or
              This.LOp.all =  "assign") and then
              Current_Item = "$*" then
            Give_Error("  ""$*"" not allowed in this context.");
            raise Parsing_error;
          end if;
          if ( This.LOp.all =  "assign") and then
              Current_Item(1) = '$' then
            Give_Error("  ""$vars"" not allowed in this context.");
            raise Parsing_error;
          end if;
          This.LArgs := new String_table'(Empty_String_Table);
          while Current_Item/= ")" loop
            This.LArgs := new String_Table'(This.LArgs.all & Tokens(Current_Token).Item);
            Skip_Token;
             while Current_Item ="," or Current_Item ="[" or Current_Item ="]" loop
              Skip_Token;
            end loop;
          end loop;
          Skip_Token (")");
          if ( This.LOp.all =  "assign") then 
            if This.LArgs.all'Length <2 then
              Give_Error("  Incorrect structure of assign rule");
              raise Parsing_error;
            else
               MarK_Observed_Var(This.LArgs(1));
            end if;
          end if;
        end if;
      end if;
      --
      -------------------------------------------------------------------
      --   NOW the parsing for the rule left following the first one
      --     (preceeded by "and" )
      --   All the variable names appearing here must be previously defined in Rule_Left(1)
      -------------------------------------------------------------------
      --
      if Previous'Length > 0 and then 
         (Next_Item = "=" or else
          Next_Item = "==" or else
          Next_Item = ">=" or else
          Next_Item = "<=" or else
          Next_Item = ">" or else
          Next_Item = "<" or else
          Next_Item = "!=" or else
          Next_Item = "/=" )then
         This.IdsL := new String_Table'(1..1 => Tokens(Current_Token).Item);
         --
         -- if $i  check $i already defined!!!
         if This.IdsL(1)(1)='$' and not Has_Been_Defined(This.IdsL(1).all, Previous) then
            Give_Error("  """& This.IdsL(1).all &""" not defined");
            raise Parsing_error;
         end if;
         Skip_Token;
         --
         if Current_Item = "="  or else
            Current_Item = "==" then 
              This.LeftOp := EQ;
              Skip_Token;
         elsif Current_Item = "!="  or else
            Current_Item = "/=" then 
              This.LeftOp := NE; 
              Skip_Token; 
         elsif Current_Item = "<" then
              This.LeftOp := LT;
              Skip_Token;
         elsif Current_Item = ">"  then
              This.LeftOp := GT;
              Skip_Token;
         elsif Current_Item = "<=" then
              This.LeftOp := LE;
              Skip_Token;
         elsif Current_Item = ">="  or else
            Current_Item = ">=" then
              This.LeftOp := GE;
              Skip_Token;
         else
            Give_Error("Error: Unexpected binary operator " & Current_Item );
            raise Parsing_Error;
         end if;
         --
         This.IdsR := new String_Table'(1..1 => Tokens(Current_Token).Item);
         if This.IdsR(1)(1)='$' and not Has_Been_Defined(This.IdsR(1).all, Previous) then
            Give_Error("  """& This.IdsR(1).all &""" not defined"); 
            raise Parsing_error;
         end if;
         --
         -- if $i  check $i already defined!!!
         --  modified  08-03-2013 to allow    ...  and  $v= -1
         --
         if Current_Item = "-" and then Next_Item(1) in '0'..'9' then
            Skip_Token;
            This.IdsR(1) := new String'("-" & Tokens(Current_Token).Item.all);
         end if;
         Skip_Token;
         --
      elsif Previous'Length > 0 then
         Give_Error("Error: Invalid structure of Action Abstraction");
         raise Parsing_Error;
      end if;
      --
      if Current_Item = "->" then
         return Previous & This;
      elsif Current_Item = "and" then
        Skip_Token ("and");
        return Parse_Left_Side (Kind, Previous & This);
      else
        Give_Error("""and""  or ""->"" expected (action rules can at most be conjunctions)");
        raise Parsing_Error;
      end if;
      --
    end if;
    ---------------------------  END ACTION -----------------------------------
    --
    ----------------------------  STATE -----------------------------------
      --
      --  inState(obj.s1)  ->  ok($1)
      --
      --  obj.x = $1   and      
      --  obj.y[0] > 10   and
      --  obj.y[$1][0] > obj2.y   and
      --  $1.head = 123  and
      --  $1.length=3
      --  maxqueuesize=$1    obj.queuesize=$1
      --  $1 /= $2 
      --  $1 <= $2   
      --
      if Current_Item = "final" then
        Skip_Token;
        This.IdsL := new String_Table'(1=> new String'("final"));
        --
      elsif Current_Item = "inState" or
            Current_Item = "InState" then
        --
        --  inState(obj.s1)  ->  ok($1)
        --
        Skip_Token;
        Skip_Token("(");
        This.LOp := Tokens(Current_Token).Item;   -- obj
        Skip_Token;
        if Current_Item = "." or else
           Current_Item = "," then 
            Skip_Token;
        end if;
        while Current_Item /=")" loop           --  top.S1.s2
          if This.LArgs =null then
            This.LArgs := new String_Table'(1..1 => Tokens(Current_Token).Item);
          else
            This.LArgs := new String_Table'(This.LArgs.all & Tokens(Current_Token).Item);
          end if;
          Skip_Token;
          if Current_Item ="." then
            Skip_Token;
          end if;
        end loop;
        Skip_Token (")");
        --
        if Current_Item = "->" then
           return Previous & This;
        else
          Skip_Token ("and");
          return Parse_Left_Side (Kind, Previous & This);
        end if;
        --
      else  -- not InState
        --
        --  obj.x = $1   and
        --  obj.y[0] > 10   and
        --  obj.y[$1].head[0] > obj2.y   and
        --  x, x[    when 1 implicit active object
        --  $1.head = 123  and
        --  $1.length=3
        --  obj.queuesize=$1
        --  //  removed maxqueuesize=$1
        --  $1 /= $2
        --  $1 <= $2
        --
        -- CHECK if Current_Item /= objectname and and Current_Item /=$i
        --   if Active_Charts'Length >1 then
        --      give error
        --   else  add implicit object name
        --
        if Current_Item(1) /= '$' and then Current_Item(1) /= '[' then
          -- check Current_Item is an active object name
          declare
            isobjectname:Boolean := False;
          begin
            for Z in Active_Charts.all'Range loop
              if All_Charts(Active_Charts(Z)).Name.all = Current_Item then
                isobjectname := True;
                exit;
              end if;
            end loop;
            if not isobjectname then
              if Active_Charts.all'Length >1 then 
                -- give_error
                Give_Error("Error: Missing Object Name in class attribute selection.");
                raise Parsing_Error;
              else
                --  MISSING THE CHECK THAT  The var name actually denotes an existing class attribute!!
                --
                -- add implicit object name
                This.IdsL := new String_Table'(This.IdsL.all & All_Charts(Active_Charts(1)).Name);
              end if;
            end if;
          end;
        end if;
        --
        loop
          --
          -- do not allow composite literals inside left sides
          if Current_Item(1) = '[' then
             Give_Error ("Composite literals not allowed in this context ...");
             raise Parsing_Error;
          end if;
          --
          -- check that the $variable has been previous declared!!
          if Current_Item(1) = '$'  then
            OK := False;
            for Index in Previous'Range loop
              if Previous(Index).LeftOp = EQ and then
                 Previous(Index).IdsR.all'Length =1 and then
                 Previous(Index).IdsR(1).all = Current_Item then
                OK := True;
                exit;
              end if;
            end loop; 
            if not OK then
              Give_Error("Found use of undefined variable " & Current_Item);
              raise Parsing_Error;
            end if;
          end if;
          --
          --  do not allow *
          if Current_Item(1) = '*' then
             Give_Error("Error: cannot use dollar or asterisk in this context");
             raise Parsing_Error;
          end if;
          --
          -- get the prefix/selector
          --
          This.IdsL := new String_Table'(This.IdsL.all & Tokens(Current_Token).Item);
          Skip_Token; -- the  var name
          -- MISSING THE CHECK THAT  The var name actually denotes an existing class attribute!!
          --
          if Current_Item = "]" then Skip_Token; end if;
          --
          if Current_Item = "[" or Current_Item = "." then 
            Skip_Token;
          else 
            exit; 
          end if;
          --  var.head, var.tail, var[0].head
        end loop;
  
        if Current_Item = "<=" then
          Skip_Token ("<=");
          This.LeftOp := LE;
        elsif Current_Item =  "!=" then
          Skip_Token ("!=");
          This.LeftOp := NE;
        elsif Current_Item = "/=" then
          Skip_Token ("/=");
          This.LeftOp := NE;
        elsif Current_Item = "==" then
          Skip_Token ("==");
          This.LeftOp := EQ;
        elsif Current_Item = ">=" then
          Skip_Token (">=");
          This.LeftOp := GE;
        elsif Current_Item = "=" then
          Skip_Token ("=");
          This.LeftOp := EQ;
        elsif Current_Item  = "<" then
          Skip_Token ("<");
          This.LeftOp := LT;
        elsif Current_Item  = ">" then
          Skip_Token (">");
          This.LeftOp := GT;
        else
          Give_Error ("Relational operator expected");
          raise Parsing_Error;
        end if;
       
        loop
          --
          -- do not allow composite literals inside left sides  like oo.x = oo.y[2]
          --  but allow   composite as "oo.x = [1,2,3]"
          --
          if This.IdsR.all'Length > 0 and then Current_Item(1) = '[' then
             Give_Error ("Composite literals not allowed in this context ...");
             raise Parsing_Error;
          end if;
          --
          -- check that the variable has been previous declared!!
          if Current_Item(1) = '$'  and then This.IdsR.all'Length > 0 then
            OK := False;
            for Index in Previous'Range loop
              if Previous(Index).LeftOp = EQ and then
                 Previous(Index).IdsR.all'Length =1 and then
                 Previous(Index).IdsR(1).all = Current_Item then
                OK := True;
                exit;
              end if;
            end loop;
            if not OK then
              -- this is a new var 
              if This.IdsR.all'Length > 0 then
                Give_Error("Found use of undefined variable " & Current_Item);
                raise Parsing_Error;
              end if;
              if Next_Item /= "->" then
                Give_Error(" ""->"" expected ...");
                raise Parsing_Error;
              end if;
            end if;
          end if;
          --
          --  do not allow *
          if Current_Item(1) = '*' then
             Give_Error("Error: cannot use dollar or asterisk in this context");
             raise Parsing_Error;
          end if;
          --
          -- get the prefix/selector
          --   
          if Current_Item /= "[" then
            --  oo.y,  123, AA
            This.IdsR := new String_Table'(This.IdsR.all & Tokens(Current_Token).Item);
            Skip_Token;
            if Current_Item = "]" then Skip_Token; end if;
            --
            if Current_Item = "[" or Current_Item = "." then
              Skip_Token;
            else
              exit;
            end if;
          else
            --  "[]"  "[1,2,3]  -- DAFARE  BUGGED NEL CASO [[1],[b]]
            This.IdsR := new String_Table'(1..1 => Tokens(Current_Token).Item);
            Skip_Token;
            while Current_Item /= "]" loop
              This.IdsR(1)  := new String'(This.IdsR(1).all & Tokens(Current_Token).Item.all);
              Skip_Token;
            end loop;
            This.IdsR(1)  := new String'(This.IdsR(1).all & Tokens(Current_Token).Item.all);
            Skip_Token;
            exit;
          end if;
        end loop;
        --
      end if;   -- inState or relation
      --
      if Current_Item = "->" then
        return Previous & This;
      else
        Skip_Token ("and");
        return Parse_Left_Side (Kind, Previous & This);
      end if;
    -----------------------------   END STATE --------------------------------
    --
  end Parse_Left_Side;

  procedure Check_Dollar (OP: Observation_Rule; D: String) is
    Found: Boolean;
  begin
         Found := False;
         for J in OP.Left.all'Range loop
           if OP.Left(J).LTerm /= null and then
               OP.Left(J).Lterm.all = D then
                 Found := true;
                 exit;
           end if;
           if OP.Left(J).LPartner /= null and then
               OP.Left(J).LPartner.all = D then
                 Found := true;
                 exit;
           end if;
           if OP.Left(J).LOp /= null and then
               OP.Left(J).LOp.all = D then
                 Found := true;
                 exit;
           end if;
           if OP.Left(J).LArgs /= null then
             for K in OP.Left(J).Largs.all'Range loop
               if OP.Left(J).Largs(K) /= null and then
                 OP.Left(J).Largs(K).all = D then
                   Found := true;
                   exit;
               end if;
             end loop;
           end if;
           if OP.Left(J).LeftOp /= NOOP and then
              OP.Left(J).IdsR /= null and then
              OP.Left(J).IdsR.all'Length=1 and then
              OP.Left(J).IdsR(1).all = D then
               Found := True;
           end if;
         end loop;
         if not Found then
           Give_Error("Undefined variable " & D);
           raise Parsing_Error;
         end if;
  end Check_Dollar;

  ------------------------------------------------------------------------------------
   -- Observations {
   -- State:   is_active(obj,state) and obj.var=3  ->  label(op,op,1)
   -- State:   inState(obj.state) and obj.var=3  ->  label(op,op,1)
   -- State:   obj.var1 > obj.var2  ->  label(op,op,1)
   -- Action:  term:part.op!<$1,$2,*,a>   ->  label(op,$2,$1)
   -- $1:$2.$3!<>   ->  label(op,$2,$1)
   -- Action   foo($*)   ->  afoo($*)   -- all parameters are observed!!!
   -- Action    $1 -> $1    sould be alloweed!!!
   ------------------------------------------------------------------------------------
   -- MISSING CHECKS:   no duplicates  $v,   $* as last event arg
   ------------------------------------------------------------------------------------
-- Durante il parsing vengono generate 3 rules per  x=1 and y=2  and z=3  ->  fooo
-- State:   x=1  and  ...  con EMPTY_RIGHTSIDE verra' considerata una and-prefix-rule
-- State:   y=2  and ...   con EMPTY_RIGHTSIDE verra' considerata una and-prefix-rule
-- State:   z=3  -> foo    conclude la rule.
-- 
-- Durante la AbstractView, quando le rules vengono naviagate, 
-- i dollarnames/dollarcount/dollarvectors  vengono resettati solo alla fine di una concluding rule
-- (opopure all'inizio di una rule che non sia stata precedura da una continuation rule).

   ------------------------------------------------------------------------------------
  function Parse_Observations(MyObservations: Observations_Table)
                      return Observations_Table  is

    OP: Observation_Rule;
    Empty_Left_Side: Rule_Left(1..0);
  begin
    if Current_Item = "}" or  Current_Item = "" then
      return MyObservations;     ----  end of Observations { } section
    end if;
    --
    --  Parse State or Action  rule qualifier
    --
    if Current_Item ="State" or
       Current_Item ="state" then
      OP.Kind := StateKind;
      Skip_Token;
    elsif Current_Item ="Action" or
          Current_Item ="action" then
      OP.Kind := ActionKind;
      Skip_Token;
    else
      Give_Error("State or Action keyword expected");
      raise Parsing_Error;
    end if;
    if Current_Item = ":" then
      Skip_Token(":");
    end if;
    --
    --
    OP.Left := new Rule_Left'(Parse_Left_Side(OP.Kind, Empty_Left_Side));
    --
    Skip_Token ("->");
    --
    --
    if Tokens(Current_Token).Item(1) ='$' then 
        Check_Dollar(Op,Tokens(Current_Token).Item.all); 
    end if;
    OP.RLabels :=                    -- initial main label
      new String_Table'(OP.RLabels.all & Tokens(Current_Token).Item);
    if Op.Kind= StateKind then
         Abstract_Predicates := 
           new String_Table'(Abstract_Predicates.all & Tokens(Current_Token).Item);
    end if;
    Skip_Token;
    --
    if Current_Item = "(" then
      Skip_Token;
      while Current_Item /= ")" loop
        if Tokens(Current_Token).Item(1) ='$' then 
            Check_Dollar(Op,Tokens(Current_Token).Item.all); 
        end if;
        if Current_Item /= "$" or else
            Next_Item /= "*"  then
          OP.RLabels := new String_Table'(OP.RLabels.all & Tokens(Current_Token).Item);
          Skip_Token;
          if Current_Item ="," then
            Skip_Token;
          end if;
        else
          OP.RLabels := new String_Table'(OP.RLabels.all & new String'("$*"));
          Skip_Token("$");
          Skip_Token("*");
        end if;
      end loop;
      Skip_Token(")");
    end if;
    --
    Check_Observation(OP);
    --
    return Parse_Observations(MyObservations & OP);
  exception
   when others =>
     Parsing_Failed := True;
     if Parsing_Errors > 5 then
        raise;
     else
       while  Current_Item /= "" and then
          Current_Item /= "}"  and then
          Current_Item /= "State"  and then
          Current_Item /= "state"  and then
          Current_Item /= "Action" and then
          Current_Item /= "action" loop
         Skip_Token;
       end loop;
       if Current_Item /= "" and then Current_Item /=  "}" then
         return Parse_Observations(MyObservations & OP);
       else
         return MyObservations;
       end if;
     end if;
  end Parse_Observations;


 -----------------------------------------------------------
 -- This procedure recursively assigns to each state
 -- its position in the RunTimeState vector.
 -- The size of the RunTimeState is hence defined as the
 -- used highest position of a simple state, and corresponds
 -- to un upper bound of the degree of parallelism of the model
 --
 -- In the case of Composite or parallel states the FirstRuntimePosition
 --  indicates the runtimeposition of the first subleaf of the state
 -- In the case of Composite or parallel states the LastRuntimePosition
 --  indicates the maximum runtimeposition for any  subleaf of the state
 -- For Simple states FirstRuntimePosition = LastRuntimePosition.
 --
 -- (this allows to find the "Active Substate) of a generic state
 -- of depth N in th following way:
 --  Leaf: State_Ref := States_Table_Ref(This.FirstRuntimePosition);
 --  ActiveSubstate  :=  Leaf.Ancestors(N-1);
 -----------------------------------------------------------
 function Eval_Max_Parallelism_and_SetPositions (Current_Position: Positive;
			  Current_State: State_Ref) return Positive is
   Result : Positive := 1;
   Tmp: Positive := 1;
 begin
   case Current_State.Kind is
     when Simple =>
       Current_State.FirstRuntimePosition := Current_Position;
       Current_State.LastRuntimePosition := Current_Position;
     when Composite =>
       for I in Current_State.Substates.all'Range loop
	 Tmp :=
	  Eval_Max_Parallelism_and_SetPositions (
            Current_Position, Current_State.Substates(I));
	 if Tmp > Result then
	    Result := Tmp;
	 end if;
       end loop;
       Current_State.FirstRuntimePosition := Current_Position;
       Current_State.LastRuntimePosition := Current_Position+Result-1;
     when Parallel =>
       Tmp := Eval_Max_Parallelism_and_SetPositions(
           Current_Position, Current_State.Substates(1));
       for I in 2..Current_State.Substates.all'Length loop
	 Tmp := Tmp + Eval_Max_Parallelism_and_SetPositions(
                       Current_Position+Tmp, Current_State.Substates(I));
       end loop;
       Result := Tmp;
       Current_State.FirstRuntimePosition := Current_Position;
       Current_State.LastRuntimePosition := Current_Position+Result-1;
     end case;
   return Result;
 end Eval_Max_Parallelism_and_SetPositions;

 -- Given the pointer "This_RState", and the state "Current_State",
 -- updates the table pointed by This_RState by setting the reference
 --  to the Current_State in the correct position in the table.
 -- If the Current State is a composite state, navigates all its substates
 --  setting the reference to currently active substates in the appropriate
 --  positions of the table.
 --
 --  Compute_Mask.
 --
 procedure Local_Assign (Current_State: State_Ref; 
			This_RState: States_Table_Ref) is
--    Region: State_Ref;
 begin
   case Current_State.Kind is
     when Simple =>
       This_RState(Current_State.FirstRuntimePosition) := Current_State;
     when Composite =>
       Local_Assign(Current_State.Substates(1),This_RState);
     when Parallel =>
       for I in Current_State.SubStates.all'Range loop
	 Local_Assign(Current_State.Substates(I),This_RState);
       end loop;
   end case;
 end Local_Assign;

  ------------------------------------------------------------------
  -- Give a transition, returns a runtime state in which the
  --  positions included in the range of the target REGION
  --  are properly defined in agreement with the target of the
  --  transition.
  --
  -- The target of a transitions is in general a tuple [s1, s2, ..]
  --
  -- For each state si in the target tuple:
  -- 1) the state 'si' is assigned to the result
  -- 2) for all the ancestors of 'si',
  --      which are descendents of the 'owner' of the transition
  --      which are parallel states,
  --      for all regions R of these ancestors which do NOT include
  --          any of target states sj of [s1, s2, ..]
  --        The region R is assigned to the result.
  ------------------------------------------------------------------
  function Compute_Mask (This_Transition: Transition_Ref)
	  return States_Table_Ref is
    Chart_Parallelism: Natural := 
	All_Classes(Current_Class).ChartStates(1).LastRunTimePosition;
    Result : States_Table_Ref := new States_Table(1..Chart_Parallelism);
    These_Target_States: States_Table := This_Transition.Target.all;
    Tmp: State_Ref;
    To_Be_Assigned : Boolean;
  begin
    for I in These_Target_States'Range loop
      Tmp :=  These_Target_States(I);
      Local_Assign(Tmp, Result);
      while Tmp /= This_Transition.Owner loop
	Tmp := Tmp.Parent;
	if Tmp.Kind = Parallel then
	 for J in Tmp.Substates.all'Range loop   -- for all regions
	   To_Be_Assigned := True;
	   for K in These_Target_States'Range loop
	     if IsNested (These_Target_States(K).all, Tmp.Substates(J).all) then
	       To_Be_Assigned := False;
	       exit;
	     end if;
	   end loop; -- for all taget states
	   if To_Be_Assigned then
	     Local_Assign (Tmp.Substates(J), Result);
	   end if;
	 end loop;  -- for all regions
	end if;
      end loop;  -- for all ancestrors
    end loop;  -- for all target states
    return Result;
  end Compute_Mask;


  procedure Check_States_Structure is
  begin
    for I in All_Classes(Current_Class).ChartStates.all'Range loop
      if All_Classes(Current_Class).ChartStates(I).FirstRuntimePosition = 
	0 then
	 Put_Line (Current_Error,
	   "Syntax Error: State " &
	     All_Classes(Current_Class).ChartStates(I).fullname.all &
	      " not appearing as direct or indirect substate of " &
	    All_Classes(Current_Class).ChartStates(1).fullname.all);
	    raise Parsing_Error;
      end if;
    end loop;
  end Check_States_Structure;

  procedure Set_Interferences is
  begin
    null;
    All_Classes(Current_Class).ChartInterferences :=
       new BoolMat(1..All_Classes(Current_Class).ChartTransitions.all'length,
                      1..All_Classes(Current_Class).ChartTransitions.all'length); 
  end Set_Interferences;


  procedure Compute_All_Masks is
    These_Transitions: Transitions_Table_Ref;
  begin
    Check_States_Structure;
    These_Transitions := All_Classes(Current_Class).ChartTransitions;
   for I in These_Transitions.all'Range loop
     These_Transitions(I).Mask := Compute_Mask(These_Transitions(I));
   end loop;
  end Compute_All_Masks;

  procedure Parse_Deferred_Declarations (Current_State: State_Ref) is
     This_Event: Event_Ref;
  begin
    if Current_item = "Defers" or else
       Current_item = "defers" then
      Skip_Token;   -- "Defers"
       if Current_Item  = ":" then
         Skip_Token;
       end if;
      loop
        This_Event := new Event;
        This_Event.Name := Parse_Id;  
        if Current_Item ="(" then
          Skip_Token;
          This_Event.Params :=
            new EventVars_Table'(Parse_EventVars(No_EventVars));
          Skip_Token(")");
        else
          This_Event.Params := new EventVars_Table'(No_EventVars);
        end if;
        --
        Current_State.Deferred :=
           new Events_Table'(Current_State.Deferred.all & Find_Event(This_Event));
        --
        if Current_Item = ","  -- or Current_Item = ";"  
        then
          Skip_Token;
        else
          exit;
        end if;
        if Current_Item = ";" then
          Skip_Token;
        end if;
      end loop;
    end if;
      -- all composite state inherit deferred states from the ancestors.
  end Parse_Deferred_Declarations;

  ------------------------------------
  --  State    Top.X.Y =  aa ,bb, ccc  Defers  hhh
  --  State    Top.X.Y =  aa(bb, cc) ,bb, ccc    -- new
  --  State    Top.X.Y =  R1[aa,bb], R2[aa,bb]   -- new
  --  State    Top.X.Y   Defers  hhh
  ------------------------------------
  procedure Parse_States is
    This_State_Name : String_Ref;
    First_Name: String_Ref;
    Current_State: State_Ref;
    Parent_State: State_Ref;
  begin
    if Current_Item = "Behavior" or else
       Current_Item = "behavior" or else
       Current_Item = "Behaviour" or else
       Current_Item = "behaviour" or else
       Current_Item = "Transitions" or else
       Current_Item = "transitions" then
       Skip_Token;
       if Current_Item = ":" then Skip_Token; end if;
    end if;
    while Current_Item = "State" or
          Current_Item = "state"  loop
      Skip_Token;
      This_State_Name := Parse_Composite_Name;
      --
      -- if it is just a deferred addition to an existing name hadle it.
      --
    if Current_Item = "Defers" or else
       Current_Item = "defers" then
      --
      Current_State := Lazy_Find_State(This_State_Name.all);
      Parse_Deferred_Declarations(Current_State);
      --
    else
      --
      All_Classes(Current_Class).Top_State_Num := 1;
      Skip_Token ("=");
      --  Check: Parent = null  =>  full_name without '.'
      --  TO BE DONE AT THE END FOR ALL_STATES
      --
      --  CREATE  a new unique State univoquely identified by the name "This_State_Name"
      -- 
      Current_State := Find_State(This_State_Name.all);
      if Current_State.Kind /= Simple then
        Give_Error("Error: state " & This_State_Name.all & "  already defined.");
        raise Parsing_error;
      end if;
      --
      --  This_State is a GLOBAL variable so that further TRANSITION implicitly refers
      --   to substates of this_state
      --
      --  Now we start parsing the state structure.
      --
      if Next_Item = "," or Next_Item = "(" then
        Current_State.Kind := Composite;
        Parse_Substates_List(Current_State);
        --
      elsif Next_Item = "/" or Next_Item = "[" then
        Current_State.Kind := Parallel;
        Parse_Regions_List(Current_State);
         --
      else
        -- this is a composite state with just one nested stste
        --
        First_Name := Parse_Id;
        Current_State.Kind := Composite;
        Parent_State := Current_State;
        Current_State :=  Find_State(Parent_State.FullName.all & '.' & First_Name.all);
        Current_State.Parent := Parent_State;
        Current_State.Depth := Parent_State.Depth + 1;
        if Parent_State.Kind = Composite then
          Current_State.Priority := Parent_State.Priority + 1;
        elsif Parent_State.Kind = Parallel then
          Current_State.Priority := Parent_State.Priority;
        end if;
        Parent_State.SubStates := new States_table'(Parent_State.SubStates.all & Current_State);
        --
      end if;
      -- Finally, if present, we parse any deferred declarations ..
      --
      Parse_Deferred_Declarations(Current_State);
      --
     end if;
    end loop;
    --
  exception
     when others =>
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
         raise;
      else
        while  Current_Item /= "" or else
             Current_Item /= "end" or else
             Current_Item /= "Behavior" or else
             Current_Item /= "behavior" or else
             Current_Item /= "Behaviour" or else
             Current_Item /= "behaviour" or else
             Current_Item /= "Transitions" or else
             Current_Item /= "transitions" or else
             Current_Item /= "Class" or else
             Current_Item /= "class" or else
             Current_Item /= "Objects" or else
             Current_Item /= "objects" loop
           Skip_Token;
        end loop;
     end if;
  end Parse_States;


  ---------------------------------------------------------------
  -- Called when Lazy_parsing flag is set after all transizions have been elaborated
  -- Identifies the concurrent structured of substates markign as concurrent those states which
  -- a) have no simple or pseudo substates
  -- b) have more than one composite substate
  -- c) are not owners of transitions UNLESS THEY ARE FORK OR JOIN
  ------------------------------------------------------
  procedure Recognize_Concurrency(TheState: State_Ref) is
    Is_Parallel: Boolean := True;
  begin
    if TheState.Kind = Simple then 
       return;
    end if;
    if TheState.SubStates.all'Length > 1 then
       for I in TheState.SubStates.all'Range loop
         if TheState.SubStates(I).Kind = Simple then
           Is_Parallel := False;
           exit;
         end if;
       end loop;
    else
      Is_Parallel := False;
    end if;  
    for J in All_Classes(Current_Class).ChartTransitions.all' Range loop
       if All_Classes(Current_Class).ChartTransitions(J).Owner = TheState  then 
          -- WE DO NOT ALLOW INTER REGION TRANSITIOINS OF ANY KIND
          -- All_Classes(Current_Class).ChartTransitions(J).Source.all'Length =1 and then
          -- All_Classes(Current_Class).ChartTransitions(J).Target.all'Length=1 then
         Is_Parallel := False;
         exit;
       end if;
    end loop;
    if Is_Parallel then
      TheState.Kind := Parallel;
    end if;
    for I in TheState.SubStates.all'Range loop
       Recognize_Concurrency(TheState.SubStates(I));
    end loop;
    --  added since we allow fork lazy parsing with forks.
    --
    -- BUG BUG  local transitions of a parallel state 
    --    are moved to the parent Composite state 
    --  In this way we lose their parallel composition.
    -- We should keep then where they are and handle
    --   their parallel composition with 
    --
    --if Is_Parallel and then TheState.Kind = Composite then
    --  TheState.Kind := Parallel;
    --  if TheState.parent.localtransitions.all'Length =0 and then
    --     TheState.localtransitions.all'Length >0 then
    --      Tmp:= TheState.parent.localtransitions;
    --      TheState.parent.localtransitions := TheState.localtransitions;
    --      TheState.localtransitions := Tmp;
    --  elsif TheState.parent.localtransitions.all'Length >0 and then
    --     TheState.localtransitions.all'Length >0 then
    --      Tmp:= TheState.parent.localtransitions;
     --     TheState.parent.localtransitions := 
     --        new Transitions_Table'(Tmp.all & TheState.localtransitions.all);
     --     TheState.localtransitions := new Transitions_Table(1..0);
     -- end if;
    -- end if;
    --
    -- CHECK THAT REGIONS DO NOT APPEAR AS SOURCE OR TARGET OF TRANSITIONS.
    -- CHECK THAT FORKS TARGET DIFFERENT REGION OF THE SAME PARALLEL STATE
    -- CHECK THAT JOIN ORIGINATE FROM DIFFERENR SOURCES OF THE SAME PARALLEL STATE
   end Recognize_Concurrency;

  --------------------------------------
  --  
  --  Parse the local declarations of the Current Class or Chart
  --
  --------------------------------------
  procedure Parse_Class_Body (This_Class_Name:String_Ref) is 
    Max_Parallelism: Natural := 0;
    This_Class: Chart;
    Progress: Natural :=0; -- signals, operations, vars, state transitions end class
    isnice: Boolean := False;
  begin
    if Current_Item = "with" then
      Skip_Token;
      if Current_Item = "niceparallelism" then
        isnice := True;
        Skip_Token;
      else
       Give_Error("""niceparallelism"" expected");
       raise Parsing_Error;
      end if;
    end if;
    if Current_Item = "is" then
      Skip_Token;
    end if;
    --
    -- check this is not a duplicate
    --
    Current_Class:= Find_Class(This_Class_Name.all);
    if Current_Class = 0 then
      Current_Class := All_Classes.all'Length +1;
      All_Classes := new Chart_Table'(All_Classes.all & This_Class);
      All_Classes(Current_Class).Name := This_Class_Name;
    elsif All_Classes(Current_Class).Top_State_Num > 0 then
      Give_Error ("Error: Chart " & This_Class_Name.all & 
		    " already defined!");
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
         raise Parsing_Error;
      else
        Parsing_Failed := True;
        Parsing_Errors := Parsing_Errors+1;
        while Current_Item /= "" or else
             Current_Item /= "Class" or else
             Current_Item /= "class" or else
             Current_Item /= "Objects" or else
             Current_Item /= "objects" loop
          Skip_Token;
          -- continue the parsing from the next Class or obejct Decl
        end loop;
     end if;
    end if;
    -- Top_State_Num=-1  indice un class place_holder
    All_Classes(Current_Class).Top_State_Num := 0; 
    All_Classes(Current_Class).NiceParallelism := isnice; 
      --
      --  Top_State_Num=0 indica una classe passiva (senza transizioni), 
      --  eventualmente una classe con solo attributi pubblicamente accessibili,
      --  che e' eventualmente possibile estendere in un tempo successivo con un body.
      --  (che puo aggiungere nuovi attributi e nuovo behavior)
      -- Top_State_Num=1 indica una classe di cui e' stato definito il body
      --   e quindi la cui definizione e' congelata.    
    --
    -------  START THE MAIN TOP_DECL PARSING SEQUENCE --------
    ----  after the loop we perfomrm some final checking & update
    --  
    -- Parse the event Declarations (if any)     
    --    Signals;
    if Current_Item = "Signals" or else
       Current_Item = "signals"  then
        Progress :=1;
        Parse_Signals;
    end if;
    --
    if Current_Item = ""  or else
       Current_Item = "Chart" or else
       Current_Item = "chart" or else
       Current_Item = "Class" or else
       Current_Item = "class" then 
       return;
    elsif Current_Item = "end" then
      Skip_Token;
      if Current_Item = ";" then
         Skip_Token;
      elsif Current_Item = All_Classes(Current_Class).Name.all  then
        Skip_Token;
        if Current_Item = ";" then
          Skip_Token;
        end if;
      else
        Give_Error (" Class Name mismatch in ""end"" clause");
        raise Parsing_Error;
      end if;
      return;
    end if;
    --
    if Current_Item = "Operations" or
       Current_Item = "operations" then
      Progress := 2;
      Parse_Operations;
    end if;
    --
    if Current_Item = "Vars" or
       Current_Item = "vars" then
       Progress := 3;
      Parse_Vars;
    end if;
    if Current_Item = "Signals" or
       Current_Item = "signals" or
       Current_Item = "Operations" or
       Current_Item = "operations" or
       Current_Item = "Vars" or
       Current_Item = "vars" then
      Give_Error("Error: misplaced Vars section?");
      Parsing_Failed := True;
        if Parsing_Errors > 5 then
           raise Parsing_Error;
        else
          Parsing_Failed := True;
          Parsing_Errors := Parsing_Errors+1;
          while Current_Item /= "" and then
            Current_Item /= "Class" and then
             Current_Item /= "class" and then
             Current_Item /= "Objects" and then
             Current_Item /= "objects" loop
            Skip_Token;
          end loop;
        end if;
    end if;
    --
    -- parse the states
    --
    Parse_States;
    --
    Parse_Transitions;
    --
    if Flags.Lazy_Parsing or Flags.Almost_Lazy_Parsing then
       -- Recognize COncurrent States as those with only multiple composite
       -- substates and not owning any transitions.
       --Recognize_Concurrency(TheStare: State_Ref);
       if All_Classes(Current_Class).ChartStates.all'length >0 then
         Recognize_Concurrency(All_Classes(Current_Class).ChartStates(1)); 
       else
          Give_Error("Error:  Active class without transitions ??? ");
          raise Parsing_Error;
       end if;
    end if; 
    --
    if All_Classes(Current_Class).Top_State_Num = 0 then
        if Progress = 2 then
          Give_Error("Error: ""Vars"" or ""State"" expected here");
        elsif Progress =3 then
         Give_Error("Error: ""State"" or ""Transitions"" expected here");
        end if;
        Parsing_Failed := True;
        if Parsing_Errors > 5 then
           raise Parsing_Error;
        else
          Parsing_Failed := True;
          Parsing_Errors := Parsing_Errors+1;
          while Current_Item /= "" and then
            Current_Item /= "Class" and then
             Current_Item /= "class" and then
             Current_Item /= "Objects" and then
             Current_Item /= "objects" loop
            Skip_Token;
          end loop;
        end if;
    end if;

    
    for I in All_Classes(Current_Class).ChartStates.all'Range loop
       Load_Outgoing_Catalogues (All_Classes(Current_Class).ChartStates(I));
    end loop;

    for I in All_Classes(Current_Class).ChartStates.all'Range loop
      if All_Classes(Current_Class).ChartStates(I).fullname.all'last > 8 and then
          All_Classes(Current_Class).ChartStates(I).fullname(
            All_Classes(Current_Class).ChartStates(I).fullname.all'last - 7  ..
            All_Classes(Current_Class).ChartStates(I).fullname.all'last) = ".initial" and then
         (All_Classes(Current_Class).ChartStates(I).OutgoingTransitions = null or else 
           All_Classes(Current_Class).ChartStates(I).OutgoingTransitions.all'Length = 0) then
         Give_Error ("Missing transition for default initial state");
         raise parsing_error;
      end if;
    end loop;
    --
    Set_Ancestors;
    Set_Finals;
    Max_Parallelism := 
      Eval_Max_Parallelism_and_SetPositions  (
         1,All_Classes(Current_Class).ChartStates(1) );
    Compute_All_Masks;
    Set_Interferences; 
     --
     if Parsing_Errors > 0 then
        raise Parsing_Error;
     end if;
  exception
   when others =>
     Parsing_Failed := True;
     if Parsing_Errors > 5 then
        raise Parsing_Error;
     else
       while  Current_Item /= "" and then
          Current_Item /= "Class"  and then
          Current_Item /= "class"  and then
          Current_Item /= "Objects" and then
          Current_Item /= "objects" loop
        Skip_Token;
        end loop;
     end if;
  end Parse_Class_Body;

  procedure Set_Finals is
    Tmp: State_Ref;
    Tmp_Finals: States_Table_Ref;
    Tmp_States: States_Table_Ref;
    C: Natural := Current_Class;
  begin
    Tmp_States := All_Classes(C).ChartStates;
    for I in Tmp_States.all'Range loop
       Tmp:= Tmp_States(I);
       if Tmp.Kind = Composite then
	  for J in Tmp.SubStates.all'Range loop
	     if Is_Final(Tmp.SubStates(J)) then
	       Tmp.Finals := new States_Table'(1..1 => Tmp.SubStates(J));
	       exit;
	     end if;
	  end loop; 
       end if;
       --
       if Tmp.Kind = Parallel then
	 Tmp_Finals := new States_Table(1..Tmp.SubStates.all'Length);
	 -- for all regions
	 for J in Tmp.SubStates.all'Range loop
	    for K in Tmp.Substates(J).Substates.all'Range loop
	       if Is_Final(Tmp.Substates(J).Substates(K)) then
		  Tmp_Finals(J) := Tmp.Substates(J).Substates(K);
		  exit;
	       end if; 
	    end loop;
	    if Tmp_Finals(J) = null then
		Tmp_Finals := null;
		exit;
	    end if;
	 end loop;
	 Tmp.Finals := Tmp_Finals;
       end if;
    end loop;
  end Set_Finals;


  procedure Set_Ancestors is
    Depth: Positive;
    These_Ancestors: States_Table_Ref;
    Tmp: State_Ref;
    C: Natural;
    Tmp_States : States_Table_Ref;
  begin
     C := Current_Class;
     Tmp_States := All_Classes(C).ChartStates;
     for I in Tmp_States.all'Range  loop
       Depth := Tmp_States(I).Depth;
       These_Ancestors := new States_Table(1..Depth);
       These_Ancestors(Depth) := Tmp_States(I);
       Tmp:= Tmp_States(I);
       for J in reverse 1..Depth-1 loop
	  Tmp := Tmp.Parent;
	  These_Ancestors(J):= Tmp;
       end loop;
       Tmp_States(I).Ancestors := These_Ancestors;
     end loop;
  end Set_Ancestors;
  ---

  procedure Check_Missing_Definitions is
    Tmp_States: States_Table_Ref;
  begin
   --
   for C in All_Charts.all'Range loop
     if All_Charts(C).ChartParent = 0 THen
       Put_Line(Current_Error,  
          "#### Error: at line " & Integer'Image(-All_Charts(C).Top_State_Num) & ": " &
          " Missing declaration for entity: """ & All_Charts(C).Name.all & """");
       Parsing_Failed := True;
       if Parsing_Errors > 5 then
         raise Parsing_Error;
       end if;
     end if;
      --
      --  instead of giving an error, we might consider these as implicit Token Objects.
      -- notice however, that in this way if there is really a missing active object declalation   
      -- we will generate loss of events or deadlocks at runtime.
      -- We should give the error if the prefetched object is supposed to be active
      --  i.e.  is target of signals or calls.  But this is hard to be statically checked
      -- problem:  { - / obj1.sendsignal(undefined)}  ...   {sendsignal(x) / y :=x;   y.foo}
      --  we might request:
      --      for any signal/call action   x.foo, the class of x should be DEFINED explicitly
      --      but this is not sufficient ...  
      --      a second pass check of all actions is needed to check the consstency of all used of undefined names
      --  All_Charts(C).ChartParent := TokensChart.ChartParent; -- 3 index in all_classes
      -- for all uses of implicit tokens, their names should not appear 
      --    in assign not to token vars 
      --    in target of events
      --    in not simple uml expressions (apart relations aa=bb) , 
      --    in int bool or obj expressions
   end loop;
   --
   for C in All_Classes.all'Range loop
     if All_Classes(C).Top_State_Num  <0 then
       Put_Line(Current_Error, 
                  "Error: Missing class definition for type """ & All_Classes(C).name.all & """");
       Parsing_Failed := True;
       if Parsing_Errors > 5 then
         raise Parsing_Error;
       end if; 
     end if;
   end loop;
   --
   for C in All_Classes.all'Range loop
     Tmp_States := All_Classes(C).ChartStates;
    for I in  Tmp_States.all'Range loop
      if Tmp_States(I).Parent = null then
         for J in Tmp_States(I).FullName.all'Range loop
            if Tmp_States(I).FullName(J) = '.' then
               Put_Line(Current_Error,
                 "Error: Missing or incomplete definition of the " & 
                 "parent of state: " & Tmp_States(I).FullName.all);
              Parsing_Failed := True;
              if Parsing_Errors > 5 then
                raise Parsing_Error;
              end if;
            end if;
         end loop;
      end if;
    end loop;
   end loop;
   --
   -- we should check if we send a signal to an object which does
   --  not declare it. !!!!!
   -- This should be a runtime-check?????? (since the target is dynamic!?!?!)
   -- The check should be done based on the target-type. !!!!
   -- this means a lot of changes ...

  -- We check that, for all events declared and used inside signal actions, they
  --  are actually declared by some class (this does not mean that it is declared in the
  --   correct target class ..)
  --
  for C in All_Events.all'Range loop
    if All_Events(C).Kind = Undefined then
       Put_Line(Current_Error, "Error: Missing declaration for event """ &
         All_Events(C).Name.all & """ ");
       Parsing_Failed := True;
       if Parsing_Errors > 5 then
         raise Parsing_Error;
       end if;
    end if;
  end loop;

  --
  --  if priorities are enabled, check if an active object has "priority" as 
  --   first or second attribute.
  --
  if Priorities_Enabled then 
      Priorities_Defined := False;
      for I in Active_Charts.all'Range loop
        if All_Charts(Active_Charts(I)).ChartVars.all'Length >0 and then
           All_Charts(Active_Charts(I)).ChartVars(1).Name.all = "Priority"  then  
           Priorities_Defined := True;
           exit;
        elsif All_Charts(Active_Charts(I)).ChartVars.all'Length >1 and then
           All_Charts(Active_Charts(I)).ChartVars(2).Name.all = "Priority"  then
           Priorities_Defined := True;
           exit;
        end if;
      end loop;
  end if;
 
  end Check_Missing_Definitions;


  procedure Load_Outgoing_Catalogues (This_State: State_Ref) is
     These_Outgoing: Transitions_Table
          (1..This_State.LocalTransitions.all'Length) :=
                          This_State.LocalTransitions.all;
     OutGoing_Count: Natural;
     This_Substate: State_Ref;
     This_Transition: Transition_Ref;
  begin
     if This_State.SubStates= null then 
       return;
     end if;
     for I in This_State.SubStates.all'Range loop
       This_Substate := This_State.Substates(I);
       OutGoing_Count :=0;
       --
       -- Select outgoing transitions for the substate.
       -- The transitions to be selected are those whose
       --  first source is (o is nested inside) This_Substate;
       --
       for J in This_State.LocalTransitions.all'Range loop
         This_Transition := This_State.LocalTransitions(J);
         if This_Transition.Source(1) = This_Substate or else
            IsNested (This_Transition.Source(1).all, This_Substate.all) then
            OutGoing_Count := OutGoing_Count +1;
            These_Outgoing(OutGoing_Count) := This_Transition;
         end if;
       end loop;
       --
       -- build its catalogue;
     This_Substate.OutgoingTransitions := 
            Build_Outgoing_Catalogue(These_Outgoing(1..OutGoing_Count));
     end loop;
  end Load_Outgoing_Catalogues;

  ---------------------------------------------------------
  -- allowed Event names are:
  --   123, aaa, bbBB, a_b_c, _abc
  ---------------------------------------------------------
  function Parse_Id return String_Ref is
    TheId: String_Ref;
  begin
    if Tokens(Current_Token).Kind = Id then
      TheId := Tokens(Current_Token).Item;
      Skip_Token;
      return TheId;
    else
      Give_Error ("Identifier Expected");
      raise Parsing_Error;
    end if;
  end Parse_Id;


  -- Called by Create_Object_Instance, Parse_SimpleExpr
  function Find_Var(Prefix:String) return Natural is
    Result: Natural := 0;
    TheseVars: Vars_Table_Ref := All_Classes(Current_Class).ChartVars;
    This_Var: SystemVar_Ref;
  begin
    for I in TheseVars.all'Range loop
      if TheseVars(I).all.Name.all = Prefix then
         Result := I;
         exit;
      end if;
    end loop;
    if Result = 0 and then
       Flags.Lazy_Parsing and then 
       Prefix(Prefix'First) not in 'A'..'Z' then
      --
      --  in LAZY_PARSING mode when we find an undeclared use of "x" we declare it
      --  if we find a use of "X" we do not declare it since it is supposed to be an object
      --
      This_Var := new SystemVar;
      This_Var.Chart := Current_Class;
      This_Var.Name := new String'(Prefix);
      This_Var.Local_Num_Key := TheseVars.all'Length+1;
      All_Classes(Current_Class).ChartVars := new Vars_Table'(TheseVars.all & This_Var);
      --
      -- when called by Create_object_Instance  "Obj: Class (x -> 3)" if x in Undeclared
      -- we should add the vars also to all the already created objects 
      -- the var is introduced in the current_object by Create_Object_Instance
      --
      if Current_object /= 0 then
        for I in All_Charts.all'Range loop
           if All_Charts(I).ChartParent = Current_Class and then
                All_Charts(I).ChartVars.all'Length = This_Var.Local_Num_Key-1 then
             This_Var := new SystemVar'(This_Var.all);
             This_Var.Chart := I;
             All_Charts(I).ChartVars := 
               new Vars_Table'(All_Charts(I).ChartVars.all & This_Var );
           end if;
        end loop; 
      end if; 
      Result := This_Var.Local_Num_Key;
    end if;
    return Result;
  end Find_Var;
 
  function Find_SystemVar(Prefix:String) return Natural is
    Result: Natural := 0;
    TheseVars: Vars_Table_Ref := All_Classes(Current_Class).ChartVars;
    This_Var: SystemVar_Ref;
  begin
    for I in TheseVars.all'Range loop
      if TheseVars(I).all.Name.all = Prefix then
         Result := I;
         return Result;
      end if;
    end loop;
    if Flags.Lazy_Parsing then
      This_Var := new SystemVar;
      This_Var.Chart := Current_Class;
      This_Var.Name := new String'(Prefix);
      This_Var.Local_Num_Key := TheseVars.all'Length+1;
      All_Classes(Current_Class).ChartVars := new Vars_Table'(TheseVars.all & This_Var);
      Result := This_Var.Local_Num_Key;
    end if;
    return Result;
  end Find_SystemVar;

  function Find_TVar (Prefix: String; TEnv: EventVars_Table_Ref) return Integer is
    Result: Natural := 0;
  begin
    for I in TEnv.all'Range loop
      if TEnv(I).all.Name.all = Prefix then
         Result := I;
         exit;
      end if;
    end loop;
    return Result;
  end Find_TVar;

  function Parse_Int_Token return String is
    Is_Negative_Literal: Boolean := False;
  begin
    if Tokens(Current_Token).Kind = Num then
      Current_Token := Current_Token +1;  
      return Tokens(Current_Token-1).Item.all;
    end if;
    if Current_Item = "-" and then
       Tokens(Current_Token+1).Kind = Num then
      Current_Token := Current_Token +2;
      return "-" & Tokens(Current_Token-1).Item.all;
    end if;
     Give_Error ("Parsing Error while looking for a int literal");
     raise Parsing_Error;
  end Parse_Int_Token;


  function Is_Number (Token: String) return Boolean is
  begin
    if Token(Token'First) /= '-' then
      for I in Token'Range loop
        if Token(I) not in '0'..'9' then
           return False;
        end if;
      end loop;
      return True;
    elsif Token'Length >1 then
      for I in Token'First+1 .. Token'Last loop
        if Token(I) not in '0'..'9' then
         return False;
        end if;
      end loop;
      return True;
    else 
      return False;
    end if;
  end Is_Number;
  
  ---------------------------------------------------------------------
  --  SimpleIntExpr is   
  --       Literal_Value: Integer :=0;
  --       Image: String_Ref;
  --       Local_Variable: Natural :=0 ;   -- ChartVars(E.Local_variable) = V
  --       Target_Object: Natural :=0;     ---   targerobj.remoreattr
  --       Event_Variable: EventVar_Ref;
  --       Remote_Variable: Natural :=0;     ---  localvar.remotevar  ,  eventvar.remotevar
  --       Special_Token: String_Ref;
  --       Kind: Value_Kind := Number;    -- Number, Composite, ...???
  --       Is_Vector: SimpleIntExpr_Table_Ref;
  --       Is_Indexing: IntExpr_Table_Ref;          -- indexing
  --       Head_Tail_Data : SimpleIntExpr_Ref;   --- added 30-06-2016
  ---------------------------------------------------------------------
  function Parse_SimpleExpr(Env: EventVars_Table) return SimpleIntExpr_Ref is
     This_Simple: SimpleIntExpr_Ref := new SimpleIntExpr;
     Tmp: SimpleIntExpr_Ref;
     Tmpv: Int_Table_Ref;
     This_Item: String := Current_Item;
     This_Index:IntExpr_Ref;
  begin
    Skip_Token;   --(This_Item=Curent_Item)
    --
    --
    --   [x,y,z]    VECTOR LITERAL
    --
    --       since v := [[1,2], [3,4]]  is OK  (nested vectors)
    --        WHAT ABOUT v[2][1]   (nested indexing)
    if This_Item = "["  then
     This_Simple.Kind := Composite;  -- for now  we will see later the kind of vector
     This_Simple.Is_Vector := new SimpleIntExpr_Table(1..0);
     This_Simple.Image := new String'("[");
     while Current_Item /= "]" loop
       Tmp := Parse_SimpleExpr(Env);
       if Tmp.Kind = Number then 
           This_Simple.Kind:= Numvector;
       elsif Tmp.Kind = Object then
           This_Simple.Kind:= Objvector;
       elsif Tmp.Kind = Bool then
           This_Simple.Kind:= Boolvector;
       elsif Tmp.Kind = Numvector then
           This_Simple.Kind:= Nummatrix;
       elsif Tmp.Kind = Objvector then
           This_Simple.Kind:= Objmatrix;
       elsif Tmp.Kind = Boolvector then
           This_Simple.Kind:= Boolmatrix;
       elsif Tmp.Kind = Nummatrix then
           This_Simple.Kind:= Numcube;
       elsif Tmp.Kind = Objmatrix then
           This_Simple.Kind:= Objcube;
       elsif Tmp.Kind = Boolmatrix then
           This_Simple.Kind:= Boolcube;
       end if;
       This_Simple.Is_Vector := new SimpleIntExpr_Table'(This_Simple.Is_Vector.all & Tmp);
       This_Simple.Image := new String'(This_Simple.Image.all & Tmp.Image.all);
       if Current_Item = "," then
           Skip_Token;
           This_Simple.Image := new String'(This_Simple.Image.all & ",");
       end if;
     end loop;
      Tmpv := new Int_Table(This_Simple.Is_Vector.all'Range);
      for I in This_Simple.Is_Vector.all'Range loop
        Tmpv(I) := This_Simple.Is_Vector(I).Literal_Value;
      end loop;
      This_Simple.Literal_Value := StructBase - Vectors_DB.NickNum(Tmpv.all);
      Skip_Token; -- "]"
      This_Simple.Image := new String'(This_Simple.Image.all & "]");
      if Current_Item = "[" then
        Give_Error("Indexing not allowed in this context.");
        raise Parsing_Error;
      end if;
      if Current_Item = "." then
        Give_Error(".selection not allowed in this context.");
        raise Parsing_Error;
      end if;
      return This_Simple;
      -- do not allow   [1,2,3].tail or [1,2,3][1]
    end if;
    --
    --  "0"  "1"  "2" ..     NUMBER LITERAL
    --
    if Is_Number(This_Item) then
      This_Simple.Literal_Value := Integer'Value(This_Item);
      This_Simple.Image := new String'(This_Item);
      This_Simple.Kind := Number;
      if Current_Item = "[" then
        Give_Error("Indexing not allowed in this context.");
        raise Parsing_Error;
      end if;
      if Current_Item = "." then
        Give_Error(".selection not allowed in this context.");
        raise Parsing_Error;
      end if;
      return This_Simple;
    end if;

    if This_Item= "-" and then
       Is_Number(Current_Item) then
      This_Simple.Literal_Value := -Integer'Value(Current_Item);
      This_Simple.Image := new String'("-" & Current_Item);
      This_Simple.Kind := Number;
      Skip_Token;
      if Current_Item = "[" then
        Give_Error("Indexing not allowed in this context.");
        raise Parsing_Error;
      end if;
      if Current_Item = "." then
        Give_Error(".selection not allowed in this context.");
        raise Parsing_Error;
      end if;
      return This_Simple;
      
    end if;
    --
    --  "null"       NULL OBJECT 
    --
     if This_Item = "null"  or This_Item = "Null" or This_Item = "NULL" then 
        This_Simple.Special_Token := new String'("null");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := Object;
        This_Simple.Literal_Value := NullObject.Simple.Literal_Value;
        return This_Simple;
     end if;
    --
    -- "self"      SELF OBJECT
    --
     if This_Item = "self"  or This_Item = "Self" or This_Item = "SELF" then
        This_Simple.Special_Token := new String'("self");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := Object;
      if Current_Item = "[" then
        Give_Error("Indexing not allowed in this context.");
        raise Parsing_Error;
      end if;
--      if Current_Item = "." then
--        self.mysignal  is OK
--        Give_Error(".selection not allowed in this context.");
--        raise Parsing_Error;
--      end if;
         return This_Simple;
      end if;
    --
    -- "this"      THIS OBJECT
    --
     if This_Item = "this"  or This_Item = "This" or This_Item = "THIS" then 
        This_Simple.Special_Token := new String'("this");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := Object;
         return This_Simple;
      end if;
    --
    --  "true"  "false"    BOOLEAN LITERAL
    --
     if This_Item = "true"  or This_Item = "True" or This_Item = "TRUE" then 
        This_Simple.Special_Token := new String'("true");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := Bool;
        This_Simple.Literal_Value := IntTrue;
        return This_Simple;
     end if;
     if This_Item = "false" or This_Item = "False" or This_Item = "FALSE" then 
        This_Simple.Special_Token := new String'("false");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := Bool;
        This_Simple.Literal_Value := IntFalse;
        return This_Simple;
     end if;
     --
     --  "emptyqueue"    "queuesize"       EMPTYQUEUE  BOOLEAN   TOKEN ??
     --
     if This_Item = "emptyqueue" then
        This_Simple.Special_Token := new String'("emptyqueue");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := SpecialToken;
        return This_Simple;
     end if;
     if This_Item = "queuesize" then
        This_Simple.Special_Token := new String'("queuesize");
        This_Simple.Image := new String'(This_Item);
        This_Simple.Kind := SpecialToken;
        return This_Simple;
     end if;
     --
     --   event_variable                TRANSITION VARIABLE   TVAR
     --
     --  Come distinguiamo "ss" come "self.ss" da "ss" come variabile???
     --  Se ss non e' una TVAR o una CLASSVAR allora deve essere un signal o operation
     --
     --  Come distinguiamo vv.length come espressione intera da vv.lll come signal o call?
     --  Durante la Parse_Action, riconosciamo <token><"."><"Length"> come caso speciale
     --  (Il problema si pone nel caso dell'asegnamento  v := foo.length)
     --
     for I in reverse Env'Range loop
        if Env(I).Name.all = This_Item then
          This_Simple.Event_Variable :=  Env(I);
          This_Simple.Kind := Env(I).Kind;
          This_Simple.Image := new String'(This_Item);
        end if;
      end loop;
     --
     --   local_variable     CLASS VARIABLE    CVAR
     --
     if This_Simple.Event_Variable = null then
       This_Simple.Local_Variable := Find_Var(This_Item);
       if This_Simple.Local_Variable /= 0 then
         This_Simple.Kind :=
            All_Classes(Current_Class).
               ChartVars(This_Simple.Local_Variable).Kind;
          This_Simple.Image := new String'(This_Item);
       else
         null;  -- can still be the name of a global object
       end if;
     end if;
     -- 
     -- some kind of var
     --
     if This_Simple.Event_Variable /= null or else This_Simple.Local_Variable /= 0 then
        while Current_Item = "." or Current_Item = "[" loop
          if Current_Item = "." and then 
             ( Tokens(Current_Token+1).Item.all = "length" or
                Tokens(Current_Token+1).Item.all = "Length")  then
            Skip_Token;   -- "."
            --- new 30-06
            Tmp := This_Simple;
            This_Simple := new SimpleIntExpr;
            This_Simple.Head_Tail_Data := Tmp;
            This_Simple.Kind := Number;
            This_Simple.Special_Token := new String'("Length");
            This_Simple.Image := new String '(This_Simple.Head_Tail_Data.Image.all & ".Length");
            Skip_Token; -- "length"
            return This_Simple;
          --
          --  if followed by "head" ... evaluate it
          --
          elsif Current_Item = "." and then
             ( Tokens(Current_Token+1).Item.all = "head" or
                Tokens(Current_Token+1).Item.all = "Head")  then
            Skip_Token;   -- "."
            --  new 30-06
            Tmp := This_Simple;
            This_Simple := new SimpleIntExpr;
            This_Simple.Head_Tail_Data := Tmp;
            This_Simple.Special_Token := new String'("Head");
            This_Simple.Image := new String '(This_Simple.Head_Tail_Data.Image.all & ".Head");
            --
            if This_Simple.Head_Tail_Data.Kind= Numvector then
                        This_Simple.Kind := Number;
            elsif This_Simple.Head_Tail_Data.Kind= Boolvector then
                      This_Simple.Kind := Bool;
            elsif This_Simple.Head_Tail_Data.Kind= Objvector then
                        This_Simple.Kind := Object;
            elsif This_Simple.Head_Tail_Data.Kind= Nummatrix then
                        This_Simple.Kind := Numvector;
            elsif This_Simple.Head_Tail_Data.Kind= Boolmatrix then
                      This_Simple.Kind := Boolvector;
            elsif This_Simple.Head_Tail_Data.Kind= Objmatrix then
                        This_Simple.Kind := Objvector;
            elsif This_Simple.Head_Tail_Data.Kind= Numcube then
                        This_Simple.Kind := Nummatrix;
            elsif This_Simple.Head_Tail_Data.Kind= Boolcube then
                      This_Simple.Kind := Boolmatrix;
            elsif This_Simple.Head_Tail_Data.Kind= Objcube then
                        This_Simple.Kind := Objmatrix;
            end if;
            Skip_Token; -- "head"
            -- do allow  var.head.head , var.head.tail, ... 
            --  return This_Simple;
          --
          --  if followed by "tail" ... evaluate it
          --
          elsif Current_Item = "." and then
             ( Tokens(Current_Token+1).Item.all = "tail" or
                Tokens(Current_Token+1).Item.all = "Tail")  then
            Skip_Token;   -- "."
            --  new 30-06
            Tmp := This_Simple;
            This_Simple := new SimpleIntExpr;
            This_Simple.Head_Tail_Data := Tmp;
            This_Simple.Special_Token := new String'("Tail");
            This_Simple.Image := new String '(This_Simple.Head_Tail_Data.Image.all & ".Tail");
            This_Simple.Kind := This_Simple.Head_Tail_Data.Kind;
            Skip_Token; -- "tail"
          -- 
          --  FIXEDBUG 01-08-2016
          --
          --  CASE:    object.signal  return  expr for "object" only
          elsif Current_Item = "." then exit;
          --
          --
          -- if followed by indexing, evaluate it.
          --
          elsif Current_Item = "[" then
            if This_Simple.Special_Token /= null and then
               This_Simple.Special_Token.all = "Tail" then
              Give_Error("Indexing not allowed after tail.");
              raise Parsing_Error; 
              --
            else        
            Tmp := This_Simple;
            This_Simple := new SimpleIntExpr;
            This_Simple.Head_Tail_Data := Tmp;
           This_Simple.Is_Indexing := Empty_IntExpr_Table_Ref; 
            This_Simple.Image := This_Simple.Head_Tail_Data.Image;
            while Current_Item = "[" loop
              Skip_Token;   -- "["
              if This_Simple.Head_Tail_Data.Kind= Numvector then
                        This_Simple.Kind := Number;
              elsif This_Simple.Head_Tail_Data.Kind= Boolvector then
                      This_Simple.Kind := Bool;
              elsif This_Simple.Head_Tail_Data.Kind= Objvector then
                        This_Simple.Kind := Object;
              elsif This_Simple.Head_Tail_Data.Kind= Nummatrix then
                        This_Simple.Kind := Numvector;
              elsif This_Simple.Head_Tail_Data.Kind= Boolmatrix then
                      This_Simple.Kind := Boolvector;
              elsif This_Simple.Head_Tail_Data.Kind= Objmatrix then
                        This_Simple.Kind := Objvector;
              elsif This_Simple.Head_Tail_Data.Kind= Numcube then
                        This_Simple.Kind := Nummatrix;
              elsif This_Simple.Head_Tail_Data.Kind= Boolcube then
                      This_Simple.Kind := Boolmatrix;
              elsif This_Simple.Head_Tail_Data.Kind= Objcube then
                        This_Simple.Kind := Objmatrix;
              end if;
              This_Index := Parse_IntExpr(Env);
              This_Simple.Is_Indexing := 
                  new IntExpr_Table'(This_Simple.Is_Indexing.all & This_Index);  
              This_Simple.Image :=  
              --   new String'(This_Simple.Head_Tail_Data.Image.all & "[" &
              --       IntExpr_Image(This_Index.all)  & "]");
                 new String'(This_Simple.Image.all & "[" &
                     IntExpr_Image(This_Index.all)  & "]");
              Skip_Token("]");   -- "]"
            end loop;
            end if; -- not tail
          end if;  -- Current_Item =  = "." or Current_Item = "["
       end loop;  -- while Current_Item = "." or Current_Item = "["
       --
       return This_Simple;
    end if; --- some kinf of var
    --
    --  global_object
    --
    -- Object Values are in the Range below 2_000_000_000 (ObjectBase)
    This_Simple.Literal_Value := ObjectBase - Check_Object(This_Item); 
      --  se ci fosse un parametro typeInfo potremmo aggiustare il classtype.
    This_Simple.Kind := Object;
    This_Simple.Image := new String'(This_Item);
    return This_Simple;
    --
    --  if Current_Item = "."  or "+"  or "*"  or "&"  or "|"  or "and " or "or" or "~" or "!"  
    --   then give error .....
    --
  end Parse_SimpleExpr;

  function Parse_IntBoolExpr (Env: EventVars_Table) return IntBoolExpr is
     Result: IntBoolExpr;
     Is_Inside: Boolean := False;
  begin
     if Current_Item = "(" then
       Skip_Token;
       Is_Inside := True;
     end if;
     Result.Left := Parse_IntExpr(Env);
     if  Current_Item = ">=" then
         Skip_Token;
         Result.Op := GE;
         Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = "<=" then
           Skip_Token;
           Result.Op := LE;
           Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = "/=" then
           Skip_Token;
           Result.Op := NE;
           Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = "!=" then
           Skip_Token;
           Result.Op := NE;
           Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = "<" then
           Skip_Token;
           Result.Op := LT;
           Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = ">" then
           Skip_Token;
           Result.Op := GT;
           Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = "==" then
           Skip_Token;
           Result.Op := EQ;
           Result.Right := Parse_IntExpr(Env);
           --
    elsif Current_Item = "=" then
           Skip_Token;
           Result.Op := EQ;
           Result.Right := Parse_IntExpr(Env);
           --
    else
           Result.Op := NE;
           Result.Right :=  BoolFalse;
           --  [x]   means   [x != 0]   and evals to Number(x)  NE 0=false
    end if;
     if Is_Inside and then Current_Item = ")" then
       Skip_Token;
       Is_Inside := True;
     end if;
    return Result;
  end Parse_IntBoolExpr;


  function Parse_Guard_Body (Env: EventVars_Table) return Guard_Ref;

  ---------------------------------------------------
  --   when called on "  (a=1) & (b=2)  returns ONLY a ref to (a=1)
  --  used when found a negation  (which binds more strictly than  & |)
  --------------------------------------------------
  function Parse_One_BoolBoolExpr (Env: EventVars_Table) return Guard_Ref is
    Compare: IntBoolExpr;
    First: Guard_Ref;
  begin
    if Current_Item = "(" then
      Skip_Token;
      First := Parse_Guard_Body (Env);
      Skip_Token ( ")"  );
      return First;
    else
      Compare := Parse_IntBoolExpr(Env);
      return new BoolBoolExpr'(NoOp, Compare);
    end if;
  end Parse_One_BoolBoolExpr;

  ---------------------------------------------------
  -- sample guards are:
  --  [   va[vb[0]] = xx ]
  --  [ x mod 2=0  & z+y>0 & x<4] x=y] [x=1 | x=2]  [x>=1 | y /= 0]
  --  [ ((aa = 0)  & bb )  || (cc & dd=1 & ee=1)]
  --     (aa) = 12    -- not allowed
  --  [ x = y ] ??????  ALLOWED?
  --  [  (x mod 2) = 0 ]   NOT ALLOWED   [ x mod 2 = 0 ]  is OK
  --------------------------------------------------
  function Parse_Guard_Body (Env: EventVars_Table) return Guard_Ref is
    Compare: IntBoolExpr;
    First: Guard_Ref;
    Result: Guard_Ref;
    OpKind: BoolBoolOp := Noop;
  begin
    if Current_Item = "!" then
       Skip_Token;
       OpKind := NotOp;
       First := new BoolBoolExpr(NotOp);
       First.Left := Parse_One_BoolBoolExpr (Env);
    elsif Current_Item = "not" then
       Skip_Token;
       OpKind := NotOp;
       First := new BoolBoolExpr(NotOp);
       First.left := Parse_One_BoolBoolExpr (Env);
    elsif Current_Item = "(" then
      Skip_Token;
      First := Parse_Guard_Body (Env);
      Skip_Token(")");
    else 
      Compare := Parse_IntBoolExpr(Env);
      First :=  new BoolBoolExpr'(NoOp, Compare);
      OpKind := Noop;
    end if;
    --
    if Current_Item = "]" or else 
        Current_Item = ")" then
        -- do not skip the token because it is of higher level
        return First;
    end if;
    if Current_Item = "&&" or else 
        Current_Item = "&"  or else 
         Current_Item = "and" then
       Skip_Token;
       OpKind := AndOp;
    end if;
    if Current_Item = "||" or else
       Current_Item = "|" or else
       Current_Item = "or" then
       Skip_Token;
       OpKind := OrOp;
    end if;
    if OpKind = Noop or OpKind = NotOp then
      Result := First;
    else
      Result := new BoolBoolExpr(OpKind);
      Result.Left := First;
      Result.Right :=  Parse_Guard_Body (Env);
    end if;
    return Result;
  end  Parse_Guard_Body;

  --------------------------------------------------
  function Parse_umlExpr (Env: EventVars_Table) return umlExpr_Ref is
    First_IE: IntExpr_Ref;
    First_BB: BoolBoolExpr_Ref;
    First: umlExpr_Ref;
    Result: umlExpr_Ref;
  begin
    First := new umlExpr;
    if Current_Item = "!" or else Current_Item = "not" then
       Skip_Token;
       First_BB := new BoolBoolExpr(NotOp);
       First_BB.Left := Parse_One_BoolBoolExpr (Env);
       First.umlBool := First_BB;
    else
      First_IE := Parse_IntExpr(Env);
      if Current_Item = ">=" then
         Skip_Token;
         First_BB := new BoolBoolExpr(NoOP);
         First_BB.Compare.Left :=   First_IE;
         First_BB.Compare.Op := GE;
         First_BB.Compare.Right := Parse_IntExpr(Env);
         First.umlBool := First_BB;
      elsif Current_Item = ">" then
         Skip_Token;
         First_BB := new BoolBoolExpr(NoOP);
         First_BB.Compare.Left :=   First_IE;
         First_BB.Compare.Op := GT;
         First_BB.Compare.Right := Parse_IntExpr(Env);
         First.umlBool := First_BB;
      elsif Current_Item = "<" then
         Skip_Token;
         First_BB := new BoolBoolExpr(NoOP);
         First_BB.Compare.Left :=   First_IE;
         First_BB.Compare.Op := LT;
         First_BB.Compare.Right := Parse_IntExpr(Env);
         First.umlBool := First_BB;
      elsif Current_Item = "<=" then
         Skip_Token;
         First_BB := new BoolBoolExpr(NoOP);
         First_BB.Compare.Left :=   First_IE;
         First_BB.Compare.Op := LE;
         First_BB.Compare.Right := Parse_IntExpr(Env);
         First.umlBool := First_BB;
      elsif Current_Item = "/=" or else
             Current_Item = "!=" then
         Skip_Token;
         First_BB := new BoolBoolExpr(NoOP);
         First_BB.Compare.Left :=   First_IE;
         First_BB.Compare.Op := NE;
         First_BB.Compare.Right := Parse_IntExpr(Env);
         First.umlBool := First_BB; 
      elsif Current_Item = "==" or else
             Current_Item = "="  then
         Skip_Token;
         First_BB := new BoolBoolExpr(NoOP);
         First_BB.Compare.Left :=   First_IE;
         First_BB.Compare.Op := EQ;
         First_BB.Compare.Right := Parse_IntExpr(Env);
         First.umlBool := First_BB;
      else
        First.umlInt := First_IE;
      end if;
    end if;
    --   now in First we have the initial part of the expression
    --  so far  (x)  is considered an integer expr, but if we later
    --    find  (x) &&  ...   we should change x to  x /=0
    --    or to x = True if x is a boolean var (boolbool_expr cannot
    --    plain bool vars or values ?!?!?!?!?!
    --
    if Current_Item = "&&" or else
        Current_Item = "&"  or else
         Current_Item = "and" then
       Skip_Token;
       Result := new umlExpr;
       Result.umlBool := new BoolBoolExpr(AndOp);
       if First.umlBool=null and then
          First.umlInt.Op = No_Op and then
          First.umlInt.Simple.Local_Variable /= 0 and then
          All_Classes(Current_Class).ChartVars
            (First.umlInt.Simple.Local_Variable).Kind = Bool then 
          First.umlBool := new BoolBoolExpr(NoOp);
          First.umlBool.Compare.Left := First.umlInt;
          First.umlBool.Compare.Op := EQ;
          First.umlBool.Compare.Right := BoolTrue;
          First.umlInt := null;
       elsif First.umlBool=null then
          First.umlBool := new BoolBoolExpr(NoOp);
          First.umlBool.Compare.Left := First.umlInt;
          First.umlBool.Compare.Op := NE;
          First.umlBool.Compare.Right := BoolFalse;
          First.umlInt := null;
       end if;
       Result.umlBool.Left := First.umlBool;
       Result.umlBool.Right :=  Parse_Guard_Body (Env);
       return Result;
    end if;
    if Current_Item = "||" or else
       Current_Item = "|" or else
       Current_Item = "or" then
       Skip_Token;
       Result := new umlExpr;
       Result.umlBool := new BoolBoolExpr(OrOp);
       if First.umlBool =null then
          First.umlBool := new BoolBoolExpr(NoOp);
          First.umlBool.Compare.Left := First.umlInt;
          First.umlBool.Compare.Op := NE;
          First.umlBool.Compare.Right := BoolFalse;
          First.umlInt := null;
       end if;
       Result.umlBool.Left := First.umlBool;
       Result.umlBool.Right :=  Parse_Guard_Body (Env);
       return Result;
    end if;
    Result := First;
    Return Result;
  end  Parse_umlExpr;

  function Is_Vector(The_Expr: IntExpr) return Boolean is
  begin
     -- The_Expr.Simple.Is_vector is never null;
     if The_Expr.Left = null then
         return (The_Expr.Simple.Kind = Objvector or else
                 The_Expr.Simple.Kind = Boolvector or else
                 The_Expr.Simple.Kind = Numvector or else
                 The_Expr.Simple.Kind = Objmatrix or else
                 The_Expr.Simple.Kind = Boolmatrix or else
                 The_Expr.Simple.Kind = Nummatrix or else
                 The_Expr.Simple.Kind = Objcube or else
                 The_Expr.Simple.Kind = Boolcube or else
                 The_Expr.Simple.Kind = Numcube or else
                 The_Expr.Simple.Kind = Composite);
     else
        return Is_Vector(The_Expr.Left.all);
     end if;
  end Is_Vector;


  function Parse_Factors (Env: EventVars_Table) return IntExpr_Ref is
    This_Expr: IntExpr_Ref;
    Tmp: IntExpr_Ref;
  begin
    if Current_Item = "(" then
      Skip_Token;
      This_Expr := Parse_IntExpr(Env);
      Skip_Token(")");
    else
      This_Expr := new IntExpr;
      This_Expr.Op := No_Op;
      This_Expr.Simple :=  Parse_SimpleExpr(Env);
    end if;
    while Current_Item = "*" or else Current_Item = "/" or else Current_Item = "mod" loop
       -- abbiamo altri termini del prodotto
       Tmp := new IntExpr;
       Tmp.Left := This_Expr;
       if Current_Item = "*" then 
         Tmp.Op := Times; 
       elsif Current_Item = "/" then 
         Tmp.Op := Div;
       elsif Current_Item = "mod" then
         Tmp.Op := Modulus;
       end if;
       Skip_Token;
       Tmp.Right := new IntExpr;
       Tmp.Right.Simple :=  Parse_SimpleExpr(Env);
       This_Expr := Tmp;      
    end loop;
    return This_Expr;
  end Parse_Factors;

  function Parse_Sum (Env: EventVars_Table) return IntExpr_Ref is
    This_Expr: IntExpr_Ref;
    Tmp: IntExpr_Ref;
  begin
    if Current_Item = "(" then
      Skip_Token;
      This_Expr := Parse_IntExpr(Env);
      Skip_Token(")");
      --
    elsif Current_Item = "-" then
         -- UNARY minus (can olnly appear as the beginning of an intexpr)
         This_Expr := new IntExpr;
         This_Expr.Op := No_Op;
         Skip_Token;
         This_Expr.Left := new IntExpr;
         This_Expr.Left.Simple :=
              new SimpleIntExpr'(0, new String'("0"),0,null,0,null,Number,null,null,null);
         This_Expr.Op := Minus;
         This_Expr.Right := Parse_Factors(Env);
    else
        if Current_Item = "+" then Skip_Token; end if;
        This_Expr :=  Parse_Factors(Env);
    end if;
    --  Abbiamo la prima expr binaria
    while Current_Item = "+" or else Current_Item = "-" loop
       -- abbiamo altri termini della somma
       Tmp := new IntExpr;
       Tmp.Left := This_Expr;
       Tmp.Op := Plus;
       if Current_Item = "-" then Tmp.Op := Minus; end if;
       Skip_Token;
       Tmp.Right := Parse_Factors(Env); 
       This_Expr := Tmp;
    end loop;
    return This_Expr;
  end Parse_Sum;
  -------------------------------------------------------------------
  --
  --  IntExpr is  (Left:SimpleIntExpr, Op:IntOp, Right:SimpleIntExpr)  
  -- 
  -- IntExpr appear as part of Actions, 
  --     in the Right side of an Assignment
  --  or as argument of a signalled event
  ---------------------------------------------------------------------
  --  called  by  function  Add_Action
  ---------------------------------------------------------------------
  function Parse_IntExpr (Env: EventVars_Table) return IntExpr_Ref is
     This_Expr: IntExpr_Ref := new IntExpr;
  begin
     if Current_Item = "(" then
       Skip_Token;
       This_Expr.Left := Parse_IntExpr(Env);
       Skip_Token(")");
    else
      return Parse_Sum(Env);
    end if;
    --  here MAY continue with:  +,-,*,/ , mod
    if Current_Item = "+" then
      Skip_Token;
      This_Expr.Op := Plus;

      if Current_Item = "(" then
        Skip_Token;
        This_Expr.Right := Parse_IntExpr(Env);
        Skip_Token(")");
      else
        This_Expr.Right := new IntExpr;
        This_Expr.Right.Simple :=  Parse_SimpleExpr(Env);
      end if;

    elsif Current_Item = "-" then
      Skip_Token;
      This_Expr.Op := Minus;
      if Current_Item = "(" then
        Skip_Token;
        This_Expr.Right := Parse_IntExpr(Env);
        Skip_Token(")");
      else
        This_Expr.Right := new IntExpr;
        This_Expr.Right.Simple :=  Parse_SimpleExpr(Env);
      end if;
   elsif Current_Item = "*" then
      Skip_Token;
      This_Expr.Op := Times;
      if Current_Item = "(" then
        Skip_Token;
        This_Expr.Right := Parse_IntExpr(Env);
        Skip_Token(")");
      else
        This_Expr.Right := new IntExpr;
        This_Expr.Right.Simple :=  Parse_SimpleExpr(Env);
      end if;
   elsif Current_Item = "/" then
      Skip_Token;
      This_Expr.Op := Div;
      if Current_Item = "(" then
        Skip_Token;
        This_Expr.Right := Parse_IntExpr(Env);
        Skip_Token(")");
      else
        This_Expr.Right := new IntExpr;
        This_Expr.Right.Simple :=  Parse_SimpleExpr(Env);
      end if;
   elsif Current_Item = "mod" then
     Skip_Token;
     This_Expr.Op := Modulus;
      if Current_Item = "(" then
        Skip_Token;
        This_Expr.Right := Parse_IntExpr(Env);
        Skip_Token(")");
      else
        This_Expr.Right := new IntExpr;
        This_Expr.Right.Simple :=  Parse_SimpleExpr(Env);
      end if;
   else
       This_Expr.Op := No_Op;
   end if;
   return This_Expr;
  end Parse_IntExpr;


  ---------------------------------------------------------------
  -- When called,  Current_Item is an identifier.
  --   var := ...    OK
  --   var[expr] := ..  OK
  --   var[[expr]][..] := ..  OK
  --   var[expr].event   KO not an assignment !
  ---------------------------------------------------------------
  function Is_Assignment return boolean is
     nesting: Natural :=0;
     Index: Natural := Current_Token+1;
  begin
    if Tokens(Index).Item.all  = ":=" or else   -- ASSIGNMENT
        Tokens(Index).Item.all = "=" then --  C-STYLE/JAVA-STYLE ASSIGNMEMT
      return True;
    end if;
    while Tokens(Index).Item.all = "["  loop
      nesting := nesting+1;
      Index := Index+1;
      while nesting /= 0 loop
        if Tokens(Index).Item.all = "["  then
          nesting := nesting+1;
          Index := Index+1;
        elsif Tokens(Index).Item.all = "]" then
          nesting := nesting-1;
          Index := Index+1;
        elsif Tokens(Index).Item.all = ";" or else
          Tokens(Index).Item.all = "}" or else
          Tokens(Index).Item.all = "{" then
          Give_Error ("Errror: Found token " & Tokens(Index).Item.all &
                       " while looking for a ""]""" );
          raise Parsing_Error;
        else
          Index := Index+1;
        end if;
      end loop;
    end loop;
    if Tokens(Index).Item.all  = ":=" or else   -- ASSIGNMENT
        Tokens(Index).Item.all = "=" then --  C-STYLE/JAVA-STYLE ASSIGNMEMT
      return True;
    else return False;
    end if;       
  end Is_Assignment;

  ----------------------------------
  --  called after a :=
  --  can find   "n"   "[a,b]"  "n[a]" "n[a].Tail"  "obj.fun(n)"   "obj[n].fun(x)"
  ---------------------------------
  function Is_Function_Call return Boolean is
     nesting: Natural :=0;
     Index: Natural := Current_Token;
  begin
    while Tokens(Index).Item.all /= "."  and
          Tokens(Index).Item.all /= ";"  and
          Tokens(Index).Item.all /= "}"  and
          Tokens(Index).Item.all /= "{"  loop
     -- skip all inside [] and the rest until ";" or "}" 
     if Tokens(Index).Item.all = "["  then
       nesting := nesting+1;
       Index := Index+1;
       while nesting /= 0 loop
         if Tokens(Index).Item.all = "["  then
           nesting := nesting+1;
           Index := Index+1;
         elsif Tokens(Index).Item.all = "]" then
           nesting := nesting-1; 
           Index := Index+1;
         elsif Tokens(Index).Item.all = ";" or else
           Tokens(Index).Item.all = "}" or else
           Tokens(Index).Item.all = "{" then
           Give_Error ("Errror: Found token " & Tokens(Index).Item.all &
                        " while looking for a ""]""" );
           raise Parsing_Error;
         else
           Index := Index+1;
         end if;
       end loop;
     else
      Index := Index+1; 
     end if;
    end loop;
    if Tokens(Index).Item.all  = "."  and then
            (Tokens(Index+1).Item.all /= "Length" and then
             Tokens(Index+1).Item.all /= "length" and then
             Tokens(Index+1).Item.all /= "head" and then
             Tokens(Index+1).Item.all /= "Head" and then
             Tokens(Index+1).Item.all /= "tail" and then
             Tokens(Index+1).Item.all /= "Tail" ) then
      return True;
    else
      return False;
    end if;
  end Is_Function_Call;

  ---------------------------------------------------------------
  --  data una Actions Table corrispondente alle azioni della
  --    transizione gia parsate, aggiunge alla tabella la sucessiva
  --    azione da considerare
  --              actions are
  --
  --   var := obj.funcall(args);
  --   var = obj.funcall(args);
  --   var[intexp] := obj.funcall(args);
  --   var[intexp] = obj.funcall(args);
  --   var[intexp][..][..] := obj.funcall(args);
  --   var[intexp][..][..]= obj.funcall(args);
  --   var := expr;
  --   var = expr;
  --   var[intexp] := expr;
  --   var[intexp] = expr;
  --   var++
  --   var--
  --   event(args)  
  --   obj.event(args)
  --   objvar.event(args)  event = signal or operation     
  --   objvar[index].event(args)
  --   return (v);
  --   return;     --- in the context of call triggered transitions
  --   exit   -- from cycle
  --   if cond then {thenpart} else {elsepart}
  --   if (cond) then thenpart  endif
  --   if (cond) then {thenpart} 
  --   if (cond) then thenpart else elsepart endif
  --   while (cond) {loopbody}
  --   while cond {loopbody}
  --   while (cond) loopbody endloop
  --   for V in min .. max {loopbody}
  --   for V in min .. max loopbody endloop
  --   Var : Tyope;
  --   Var : Type := Value;
  --   Var : Type = Value;
  ---------------------------------------------------------------
  function Parse_Action (AddTo: Actions_Table; 
                       TEnv: EventVars_Table_Ref) return Actions_Table is
    This_Action: Action_Ref := new Action;
    This_Var: Systemvar_Ref;
    My_Args: umlExpr_Table_Ref;
    My_Expr: umlExpr_Ref;
    Prefix: String_Ref;
    This_Kind: String_Ref;
    The_Target: SimpleIntExpr_Ref;
    Event_Name: String_Ref;
    Next_TEnv: EventVars_Table_Ref := TEnv;
    VK: Value_Kind;
  begin
    This_Action.Env_Depth := TEnv.all'Length;
    --
    --  EMPTY LIST OF ACTIONS
    --
    if Current_Item = "}" or else 
        Current_Item = "else" or else
        Current_Item = "endif" or else
        Current_Item = "endloop" then
       return AddTo;
    end if;
    --
    --
    if Current_item="exit" then
      Skip_Token;   -- exit
      This_Action.Kind := Exitloop;
    --
    elsif Current_item="if" then
      --
      --   this is an  IF STATEMENT
      --   if <cond> { <thenpart> } else { <elsepart> }
      --
      Skip_Token;   -- "if"
      This_Action.Kind := Conditional;
      if Current_item = "(" then
         Skip_Token;
      end if;
      This_Action.IfCond := Parse_Guard_Body(TEnv.all);
      if Current_item = ")" then
         Skip_Token;
      end if;
      if Current_item = "then" then
         Skip_Token;
      end if;
        Skip_Token ("{");
      This_Action.ThenBody :=
         new Actions_Table'(Parse_Action(No_Actions, TEnv));
      if Current_Item = "}" or else
         Current_Item = "endif"  then
         Skip_Token;
      end if;
      if Current_item="else" then
        Skip_Token("else");
        if Current_Item = "{" then
          Skip_Token ("{");
        end if;
          This_Action.ElseBody :=
             new Actions_Table'(Parse_Action(No_Actions, TEnv));
        if Current_Item = "}" or else 
           Current_Item = "endif"  then
           Skip_Token;
        end if;
      else
          This_Action.ElseBody := new Actions_Table(1..0);
      end if;
      --
      --
    elsif Current_item="while" then
      --
      --   this is a WHILELOOP construct
      --
      Skip_Token;   -- "while"
      This_Action.Kind := Whileloop;
      -- parse the lopo var
      if Current_item = "(" then
         Skip_Token;
      end if;
      This_Action.LoopCOnd := Parse_Guard_Body(TEnv.all);
      if Current_item = ")" then
         Skip_Token;
      end if;
      if Current_Item = "{" then
        Skip_Token ("{");
      end if;
      This_Action.LoopBody :=
           new Actions_Table'(Parse_Action(No_Actions, TEnv));
        if Current_Item = "}" or else
           Current_Item = "endloop"  then
           Skip_Token;
        end if;
      --
      --
    elsif Current_item="for" then
      --  
      --   this is a FORLOOP construct
      --
      Skip_Token;   -- "for"
      This_Action.Kind := Forloop;
      -- parse the lopo var
      This_Action.For_Var := new EventVar;
      This_Action.For_Var.Name := new String'(Current_Item);
      This_Action.For_Var.Num_Key := TEnv'Length+1;
      This_Action.For_Var.Kind := Number;
      This_Action.For_Var.TypeInfo := 0;
      This_Action.For_VarMin := new EventVar;
      This_Action.For_VarMin.Name := new String'(Current_Item & "_Min");
      This_Action.For_VarMin.Num_Key := TEnv'Length+2;
      This_Action.For_VarMin.Kind := Number;
      This_Action.For_VarMin.TypeInfo := 0;
      This_Action.For_VarMax := new EventVar;
      This_Action.For_VarMax.Name := new String'(Current_Item & "_Max");
      This_Action.For_VarMax.Num_Key := TEnv'Length+3;
      This_Action.For_VarMax.Kind := Number;
      This_Action.For_VarMax.TypeInfo := 0;
      Skip_Token;  -- varid
      --
      Skip_Token ("in");
      This_Action.For_Min := Parse_IntExpr(TEnv.all);
      Skip_Token ("..");
      This_Action.For_Max := Parse_IntExpr(TEnv.all);
      Skip_Token ("{");
      This_Action.LoopBody := 
           new Actions_Table'(Parse_Action(No_Actions, 
                   new EventVars_Table'(TEnv.all & (This_Action.For_Var, 
                                                  This_Action.For_VarMin, 
                                                  This_Action.For_VarMax))));
      if Current_Item = "{" then
        Skip_Token ("{");
      end if;
        if Current_Item = "}" or else
           Current_Item = "endloop"  then
           Skip_Token;
        end if;
      --
      --
    elsif Current_Item = "return" then   --  return  <expr>
      --
      -- this is a RETURN ACTION
      --
      -- remember this to allow some sanity check inside Parse_Transition_Structure
      Return_or_Caller_Found := True;
      --
      Skip_Token;
      This_Action.Kind := OpReturn ;
      My_Args := new umlExpr_Table(1..1);
      if Current_Item /= ";"  and then Current_Item /= "}" and then
           Current_Item /= ")" and then Current_Item /= ")->" then
         My_Expr := Parse_umlExpr(TEnv.all);
         My_Args(1) := My_Expr;
      end if;
      -- 
      --  get the Value_Kind VK of the return param 
      --  (from the trigger profile or from the exression kind?)
      --  FROM the expression KIND
      if My_Args(1) = null then 
          VK := Undefined;
      elsif My_Args(1).umlBool /= null then
          VK := Bool;
      else
           VK := IntExpr_Kind(My_Args(1).umlInt.all);
      end if;
      --
      if VK = Number then
         This_Action.Signalled_Event.The_Event := OpReturnInt_Event;
      elsif VK = Object then
         This_Action.Signalled_Event.The_Event := OpReturnObj_Event;
      elsif VK = Bool then
         This_Action.Signalled_Event.The_Event := OpReturnBool_Event;
      elsif VK = Numvector then
         This_Action.Signalled_Event.The_Event := OpReturnIntV_Event;
      elsif VK = Objvector then
         This_Action.Signalled_Event.The_Event := OpReturnObjV_Event;
      elsif VK = Boolvector then
         This_Action.Signalled_Event.The_Event := OpReturnBoolV_Event;
      elsif VK = Nummatrix then
         This_Action.Signalled_Event.The_Event := OpReturnIntM_Event;
      elsif VK = Objmatrix then
         This_Action.Signalled_Event.The_Event := OpReturnObjM_Event;
      elsif VK = Boolmatrix then
         This_Action.Signalled_Event.The_Event := OpReturnBoolM_Event;
      elsif VK = Numcube then
         This_Action.Signalled_Event.The_Event := OpReturnIntC_Event;
      elsif VK = Objcube then
         This_Action.Signalled_Event.The_Event := OpReturnObjC_Event;
      elsif VK = Boolcube then
         This_Action.Signalled_Event.The_Event := OpReturnBoolC_Event;
      else
         This_Action.Signalled_Event.The_Event := OpReturn_Event;
      end if;
      This_Action.Signalled_Event.The_Args := My_Args;
      ---  when return;   My_Args(1) = null !!!
      --
    elsif Tokens(Current_Token+1).Item.all  = ":" then  -- VARIABLE DECLARATION
      --  
      --  thid is a Transition VARIABLE DECLARATION 
      --
      This_Action.Kind := VarDecl;
      Prefix := Parse_Id; 
      This_Action.TVar := new EventVar;
      This_Action.TVar.Name := Prefix;
      This_Action.TVar.Num_Key := TEnv'Length+1;
      This_Action.TVar.Kind := Undefined;
      This_Action.TVar.TypeInfo := 0;  ---  index in All_Classes
      --
      Skip_Token(":");
      This_Kind := Parse_Id;
      if This_kind.all = "obj" then
        This_Action.TVar.Kind := Object;
        if Current_Item = "[" then
          Skip_Token; -- "["
          Skip_Token; -- "]"
          This_Action.TVar.Kind := Objvector;
        end if;
      elsif This_kind.all = "bool" then
        This_Action.TVar.Kind :=  Bool;
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Action.TVar.Kind := Boolcube;
              Skip_Token; -- "]"
            else
              This_Action.TVar.Kind := Boolmatrix;
            end if;
            Skip_Token; -- "]"
          else 
            This_Action.TVar.Kind := Boolvector;
          end if;
          Skip_Token; -- "]"
        end if;
      elsif This_kind.all = "int" then
        This_Action.TVar.Kind :=  Number;
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Action.TVar.Kind := Numcube;
              Skip_Token; -- "]"
            else
              This_Action.TVar.Kind := Nummatrix;
            end if;
            Skip_Token; -- "]"
          else 
            This_Action.TVar.Kind := Numvector;
          end if;
          Skip_Token; -- "]"
        end if;
      else  -- Kind = Class_Name
        This_Action.TVar.Kind := Object;
        This_Action.TVar.Typeinfo := Check_Class(This_kind.all);
        if Current_Item = "[" then
          Skip_Token; -- "["
          if Current_Item = "[" then
            Skip_Token; -- "["
            if Current_Item = "[" then
              Skip_Token; -- "["
              This_Action.TVar.Kind := Objcube;
              Skip_Token; -- "]"
            else
              This_Action.TVar.Kind := Objmatrix;
            end if;
            Skip_Token; -- "]";
          else
            This_Action.TVar.Kind := Objvector;
          end if;
          Skip_Token; -- "]";
        end if;
      end if;
      --
      if Current_Item = ":=" or else Current_Item = "=" then
        Skip_Token;
        This_Action.TValue := Parse_umlExpr(TEnv.all);
        -- sanity check for opcall body -  used by Parse_Transition_Structure
        if This_Action.TValue.umlInt /= null and then
              This_Action.TValue.umlInt.Simple /= null and then
              This_Action.TValue.umlInt.Simple.Image /= null and then
              This_Action.TValue.umlInt.Simple.Image.all = "_caller" then
           Return_or_Caller_Found := True;
        end if;
        if Current_Item = "." then
          Give_Error("Attribute selection not allowed.");
          raise Parsing_Error;
        end if;
      else
         -- set default initial value
         if This_Action.TVar.Kind = Object then
            This_Action.TValue := new umlExpr'(NullObject,null);
         elsif This_Action.TVar.Kind = Bool then
            This_Action.TValue := new umlExpr'(BoolFalse,null);
         elsif This_Action.TVar.Kind = Number then
            This_Action.TValue := new umlExpr'(Zero,null);
         else  -- structure
            This_Action.TValue := new umlExpr'(NullStruct,null);
         end if;
      end if;
      --
      Next_Tenv :=  new EventVars_Table'(Tenv.all & This_Action.TVar);
      --
    elsif Is_Assignment then
      --  THIS IS AN ASSIGNMENT   
      --  left side = "var" or  "var[index][..] :="
      --
      Prefix := Parse_Id;
      This_Action.Assignment_Left :=  - Find_TVar(Prefix.all, TEnv);
        -- the index is saved with negative value to distinguish Tvar from Attributes.
      if This_Action.Assignment_Left = 0 then
         This_Action.Assignment_Left := Find_SystemVar(Prefix.all);
      end if;
      This_Action.Assignment_Left_Image := new String'(Prefix.all);
      if This_Action.Assignment_Left = 0 then
         Give_Error ("Error: Variable " & Prefix.all & " is not declared.");
         raise Parsing_Error;
      end if;
      while Current_Item = "[" loop
         --  ASSIGNMENT TO VECTOR COMPONENT    "var[index] :="
         Skip_Token;   -- "["
         This_Action.Assignment_Left_Indexes := 
            new umlExpr_Table'(
                  This_Action.Assignment_Left_Indexes.all & Parse_umlExpr(TEnv.all));
         Skip_Token("]");   -- "]"
         This_Action.Assignment_Left_Image :=
             new String'(Prefix.all & 
               "[" & 
                 intExpr_Image(This_Action.Assignment_Left_Indexes(
                    This_Action.Assignment_Left_Indexes.all'Last).umlInt.all) &
               "]");
      end loop; 
      Skip_Token;    -- ":="   or "="
      --    
      if Is_Function_Call then
        --
        -- this is an ASSIGNMENT with remote FUNCTION CALL 
        --    "var := obj.ev(v)"  "var[expr] = obj.ev(v) "
        --
        This_Action.Kind := Function_Call;
        The_Target:= Parse_SimpleExpr(TEnv.all);  --  "obj"
        if Flags.Lazy_Parsing and then The_Target.Kind = Undefined then
             The_Target.Kind := Object;
        end if;
        Skip_Token(".");
        Event_Name := Tokens(Current_Token).Item;
        Skip_Token; -- "ev"
        --
        My_Args := new umlExpr_Table'(Empty_umlExpr_Table);
          --  parse the arguments, if any
        if Current_Item = "(" then    --  what about "()"????
          Skip_Token;
          if Current_Item /= ")" then
          -- this is a signal with parameters
          My_Expr := Parse_umlExpr(TEnv.all);
          -- sanity check for opcall body -  used by Parse_Transition_Structure
          if My_Expr.umlInt /= null and then
                My_Expr.umlInt.Simple /= null and then
                My_Expr.umlInt.Simple.Image /= null and then
                My_Expr.umlInt.Simple.Image.all = "_caller" then
             Return_or_Caller_Found := True;
          end if;
          My_Args := new umlExpr_Table'(My_Args.all & My_Expr);
            while Current_Item  = "," loop
              Skip_Token;
              My_Expr := Parse_umlExpr(TEnv.all);
              -- sanity check for opcall body -  used by Parse_Transition_Structure
              if My_Expr.umlInt /= null and then
                    My_Expr.umlInt.Simple /= null and then
                    My_Expr.umlInt.Simple.Image /= null and then
                    My_Expr.umlInt.Simple.Image.all = "_caller" then
                 Return_or_Caller_Found := True;
              end if;
              My_Args := new umlExpr_Table'(My_Args.all & My_Expr);
              if Current_Item = ")" then
                exit;
              end if;
            end loop;
            end if;
          Skip_Token (")"); 
        end if;
        This_Action.Signalled_Event.The_Args := My_Args;
        This_Action.Signalled_Event.The_Target := The_Target;
        This_Action.Signalled_Event.The_Event :=
            Check_Event(Event_Name.all, My_Args);
          --
      else
        --
        -- this is a PLAIN ASSIGNMENT action 
        --  "SystemVar :=  umlExpr "  "Tvar := umlExpr"
        --  "SystemVar[expr] :=  umlExpr "  "Tvar[expr] := umlExpr"
        --
        This_Action.Kind := Assignment;
        This_Action.Assignment_Right := Parse_umlExpr(TEnv.all);
        -- sanity check for opcall body -  used by Parse_Transition_Structure
        if This_Action.Assignment_Right.umlInt /= null and then
           This_Action.Assignment_Right.umlInt.Simple /= null and then
           This_Action.Assignment_Right.umlInt.Simple.Image /= null and then
           This_Action.Assignment_Right.umlInt.Simple.Image.all = "_caller" then
           Return_or_Caller_Found := True;
        end if;
        --
        --   Propagation of type from right to left of assignment
        --  Supprressed becuase BUGGED (missing case of Tvar) ans unncessary (since dyntyping)
        --This_Var := 
        --  All_Classes(Current_Class).ChartVars(This_Action.Assignment_Left);
        --if This_Var.Kind = Undefined then
        --  if This_Action.Assignment_Right.umlInt /= null then
        --     This_Var.Kind := IntExpr_Kind(This_Action.Assignment_Right.umlInt.all);
        --  else
        --    This_Var.Kind := Bool;
        --  end if;
        --end if;
      end if;  -- ASSIGNMENT
      --
    elsif Next_Item = "++" then
      --
      --  This is an INCREMENT ASSIGNMENT
      --
      Prefix := Parse_Id;
      This_Action.Kind := Assignment;
      This_Action.Assignment_Left :=  - Find_TVar(Prefix.all, TEnv);
        -- the index is saved with negative value to distinguish Tvar from Attributes.
      if This_Action.Assignment_Left = 0 then
         This_Action.Assignment_Left := Find_SystemVar(Prefix.all);
      end if;
      This_Action.Assignment_Left_Image := new String'(Prefix.all);
      if This_Action.Assignment_Left = 0 then
         Give_Error ("Error: Variable " & Prefix.all & " is not declared.");
         raise Parsing_Error;
      end if;
      Skip_Token;   -- ++
      This_Action.Assignment_Right := new umlExpr;
      This_Action.Assignment_Right.umlInt := new IntExpr;
      This_Action.Assignment_Right.umlInt.Left  := new IntExpr;
      --
      if This_Action.Assignment_Left > 0 then
         -- this is an increment of a Class Var
        This_Action.Assignment_Right.umlInt.Left.Simple := 
           new SimpleIntExpr'(0, new String'(Current_Item & "++"), This_Action.Assignment_Left,
             null, 0, null, Number,null,null,null);
        This_Var :=
          All_Classes(Current_Class).ChartVars(This_Action.Assignment_Left);
        if This_Var.Kind = Undefined then
          This_Var.Kind := Number;
        end if;
      else
         -- this is an increment of a Transition Var (negative index);
        This_Action.Assignment_Right.umlInt.Left.Simple :=
           new SimpleIntExpr'(0, new String'(Current_Item & "++"),  0,
             TEnv( - This_Action.Assignment_Left),0, null, Number,null,null,null);
      end if;
      This_Action.Assignment_Right.umlInt.Op := Plus;
      This_Action.Assignment_Right.umlInt.Right :=  Uno;
      --
    elsif Next_Item = "--" then
      --
      --   this is a DECREMENT ASSIGNMENT
      --
      Prefix := Parse_Id;
      This_Action.Kind := Assignment;
      This_Action.Assignment_Left :=  - Find_TVar(Prefix.all, TEnv);
        -- the index is saved with negative value to distinguish Tvar from Attributes.
      if This_Action.Assignment_Left = 0 then
         This_Action.Assignment_Left := Find_SystemVar(Prefix.all);
      end if;
      This_Action.Assignment_Left_Image := new String'(Prefix.all);
      if This_Action.Assignment_Left = 0 then
         Give_Error ("Error: Variable " & Prefix.all & " is not declared.");
         raise Parsing_Error;
      end if;
      Skip_Token;    -- 
      This_Action.Assignment_Right := new umlExpr;
      This_Action.Assignment_Right.umlInt := new IntExpr;
      This_Action.Assignment_Right.umlInt.Left := new IntExpr;
      This_Action.Assignment_Right.umlInt.Left.Simple := 
         new SimpleIntExpr'(0, new String'(Current_Item & "--"), This_Action.Assignment_Left,
          null, 0,null, Number,null,null,null);
      This_Action.Assignment_Right.umlInt.Op := Minus;
      This_Action.Assignment_Right.umlInt.Right :=  Uno;
      This_Var :=
        All_Classes(Current_Class).ChartVars(This_Action.Assignment_Left);
      if This_Var.Kind = Undefined then
        This_Var.Kind := This_Action.Assignment_Right.umlInt.Simple.Kind;
      end if;
      --
    else 
      --
      --   this is a SIGNAL or a CALL action
      --
      --   OUT.event(args)  CHART.event(args)   event(args)   var.event(args)
      --   var[expr].event(args)
      --
      -- Target e' in genere una Parse_SimpleExpr
      --
      This_Action.Kind := Signal;
      --       sign;  sign(..);   sign}  -- implicit OUT target object
      if Next_Item=";" or else Next_Item="(" or else Next_Item="}" then
        The_Target := UML_TYpes.OUTobject;
      else
        The_Target:= Parse_SimpleExpr(TEnv.all);  --  "obj"  "vect[i]" "var"
        Skip_Token(".");
      end if;
      Event_Name := Tokens(Current_Token).Item;
      Skip_Token; -- "ev"
      --
      My_Args := new umlExpr_Table'(Empty_umlExpr_Table);
      -- 
      --  parse the arguments, if any
      --
      if Current_Item = "(" then
        Skip_Token;
        if Current_Item /= ")" then
          -- this is a signal or call  with parameters
          My_Expr := Parse_umlExpr(TEnv.all);
          -- sanity check for opcall body -  used by Parse_Transition_Structure
          if My_Expr.umlInt /= null and then
            My_Expr.umlInt.Simple /= null and then
            My_Expr.umlInt.Simple.Image /= null and then
            My_Expr.umlInt.Simple.Image.all = "_caller" then
            Return_or_Caller_Found := True;
          end if;
          My_Args := new umlExpr_Table'(My_Args.all & My_Expr);
          while Current_Item  = "," loop
            Skip_Token;
            My_Expr := Parse_umlExpr(TEnv.all);
            -- sanity check for opcall body -  used by Parse_Transition_Structure
            if My_Expr.umlInt /= null and then
              My_Expr.umlInt.Simple /= null and then
              My_Expr.umlInt.Simple.Image /= null and then
              My_Expr.umlInt.Simple.Image.all = "_caller" then
              Return_or_Caller_Found := True;
            end if;
            My_Args := new umlExpr_Table'(My_Args.all & My_Expr);
            if Current_Item = ")" then
              exit;
            end if;
          end loop;
        end if;
        Skip_Token (")");
      end if;
      --
      -- assign and check My_Args (possibly introducing a new event profile)
      --
      This_Action.Signalled_Event.The_Args := My_Args;
      This_Action.Signalled_Event.The_Target := The_Target;
      This_Action.Signalled_Event.The_Event := Check_Event(Event_Name.all, My_Args);
      if This_Action.Signalled_Event.The_Target.Image.all = "OUT" or else
         This_Action.Signalled_Event.The_Target.Image.all = "ERR" then
         This_Action.Signalled_Event.The_Event.Kind := Signal;
      end if;
      --
    end if;  --  assignment or signal
      --
      --   look if the action is composite
    if Current_Item  = ";" then
      Skip_Token;
      if Current_Item  = ")->" or else
          Current_Item = ")"  then
          -- it was just an final semicolon .  Do not Skip the token
          return (AddTo & This_Action);
      else
        -- it is a true composite action  CONTINUE THE PARSING
        return Parse_Action (AddTo & This_Action, Next_Tenv);
      end if;
    elsif Current_Item  = ")->" or else
          Current_Item = ")"  or else
          Current_Item = "}"  then
       return (AddTo & This_Action);
    else
      Give_Error (" Unxepected stuff after action: missing ""}"" , ""("", "";"" ?");
      raise Parsing_Error;
    end if;
--  exception
--     when others => 
--        Parsing_Failed := True;
----        Give_Error("Failure during parsing of transition structure");
--        if Parsing_Errors > 5 then
--          raise Parsing_Error;
--        else
--          while  Current_Item /= "" and then
--              Current_Item /= "}" and then Next_Item /= ";" loop
--            Skip_Token;
--          end loop;
--        if Current_Item= ";" then Skip_Token; end if;
--       end if;
--       return AddTo;
  end Parse_Action ;

  -------------------------------------------------------------------------
  -- called by Parse_Transition_Body
  -- parses the sequence of actions after ther trigger-guard and
  --  returns the table of actions of the transition
  -- Action_Kind is (Signal, Assignment
  -- Env is the list of event variables of the transition trigger
  -------------------------------------------------------------------------
  function Parse_Actions (TEnv: EventVars_Table_Ref) return Actions_Table_Ref is
  begin
    if Current_Item = ")->" or else
       Current_Item = ")" then
      -- do not skip the token
      return new Actions_Table'(No_Actions);
    end if;
    -- some action IS present
    return new Actions_Table'(Parse_Action(No_Actions, Tenv));
  end Parse_Actions;

  function Add_To_Outgoing_Catalogue (This_Transition: Transition_Ref;
                                      This_Catalogue: TransitionsCatalogue_Ref)
                     return TransitionsCatalogue_Ref is
    New_Catalogue: TransitionsCatalogue_Ref := This_Catalogue;
    Found: Natural :=0;
  begin
     for I in This_Catalogue.all'Range loop
       if This_Catalogue(I).This_Event = This_Transition.Trigger then
          Found := I;
          exit;
       end if;
     end loop; 
     if Found > 0 then
        New_Catalogue(Found).These_Transitions := 
           new Transitions_Table'(New_Catalogue(Found).These_Transitions.all &
                                This_Transition);
     else
       New_Catalogue := 
           new TransitionsCatalogue'(New_Catalogue.all & 
                Triggered_Transitions'( This_Event => This_Transition.Trigger,
                  These_Transitions => 
                        new Transitions_Table'(1..1 => This_Transition)));
     end if;
     return New_Catalogue;
  end Add_To_Outgoing_Catalogue;

  -------------------------------------------------------------------------
  -- Given a table of transitions, 
  --  returns a pointer to a table of triggered transitions;
  -- Interal Transitions (if any) are stored in the first position of 
  --   the catalogue
  -------------------------------------------------------------------------
  function Build_Outgoing_Catalogue (These_Transitions: Transitions_Table)
                     return TransitionsCatalogue_Ref is
    --
    This_Catalogue: TransitionsCatalogue_Ref;
    TT_Count: Natural;
    TT_Ref: Transitions_Table_Ref;
    TT_Table: Transitions_Table(1..These_Transitions'Length);
    --
  begin
    This_Catalogue := new TransitionsCatalogue(1..0); 
    --
    for J in All_Events.all'Range loop
      -- Load the set of triggered transitions
      TT_Count := 0;
      for I in These_Transitions'Range loop
         if These_Transitions(I).Trigger.Num_Key = 
               All_Events(J).Num_Key then
            TT_Count := TT_Count +1;
            TT_Table(TT_Count) := These_Transitions(I);
         end if;
      end loop;
      if TT_Count > 0 then
        TT_Ref := new Transitions_Table'(TT_Table(1..TT_Count));
        This_Catalogue := 
          new TransitionsCatalogue'(This_Catalogue.all & 
             Triggered_Transitions'(All_Events(J), TT_Ref) );
      end if;
    end loop;
    return This_Catalogue;
  end Build_Outgoing_Catalogue;


  function Parse_Composite_Name return String_Ref is 
    Tmp: String_Ref;
  begin
    Tmp := Parse_Id;
    while Current_Item = "." loop
      Skip_Token;
      Tmp := new String'(Tmp.all & "." & Parse_Id.all);
      -- Free ?
    end loop;
    return Tmp;
  end Parse_Composite_Name;


  ----------------------------
  -- called by Parse_Transition 
  -- parses the content inside {..} (trigger/actions)
  ----------------------------
  procedure Parse_Transition_Structure (This_Transition: Transition_Ref) is
    Transition_Env: EventVars_Table_Ref;
    previousPE: Natural := Parsing_Errors;
  begin
      Return_or_Caller_Found := False;
      Is_Operation_Body := False;
      --
      -- parse the transition trigger (and load the trigger ENV)
      --
      This_Transition.Trigger := new Event;
      if Current_Item = "-" then
        This_Transition.Trigger.Name := Tokens(Current_Token).Item;
        Skip_Token;
      elsif Current_Item = "[" or else
           Current_Item = "/" or else
           Current_Item = "}" then
         This_Transition.Trigger := new Event;
         This_Transition.Trigger.Name := new String'("-");
         -- was:
         --   Give_Error("Error: Transition trigger expected  (maybe ""-"" ?)");
         --   raise Parsing_Error;
         -- end if;
      else
        This_Transition.Trigger.Name := Parse_Id;
        if Current_Item  = "(" then
          Skip_Token;
            if  Current_Item  /= ")" then
              This_Transition.Trigger.Params :=
                new EventVars_Table'(Parse_EventVars(No_EventVars));
            end if;
          Skip_Token(")");
        else
          This_Transition.Trigger.Params := new EventVars_Table'(No_EventVars);
        end if;
      end if;
      This_Transition.Trigger := Find_Event(This_Transition.Trigger);
      if This_Transition.Trigger.Kind = Operation then
        Is_Operation_Body := True;
      end if;
      Transition_Env := This_Transition.Trigger.Params;
      --
      -- parse the transition guard, if any
      --
      if Current_Item = "[" then
        if  Next_Item /= "]"  then
          Skip_Token;
          This_Transition.Guard := Parse_Guard_Body(Transition_Env.all);
          Skip_Token ("]");
        else
          Skip_Token;
          Skip_Token;
        end if;
      end if;
      --
      -- parse the transitions Action list (if any)
      --
      if Current_Item = "/" then
        Skip_Token;
        This_Transition.Actions :=  Parse_Actions(This_Transition.Trigger.Params);
      elsif Current_Item = "}" then
        This_Transition.Actions := new Actions_Table'(No_Actions);
      elsif Current_Item = ";" then
        Give_Error("Error: unexpected "";"" ");
        raise Parsing_Error;
      else 
        Give_Error("Error:  Transition syntax ::= ""{"" [trigger] [guard] [actions]""}"" ");
        Give_Error("        Where guard ::= ""["" bool expr ""]"" and actions ::= ""/""[action;]{action;}");
        raise Parsing_Error;
      end if;
      --
      if Flags.Lazy_Parsing and then
          Return_or_Caller_Found and then
          This_Transition.Trigger /= Null_Event then
          -- we have introduced a Signal (maybe)
          This_Transition.Trigger.Kind := Operation;
      end if;
      --
      if Is_Operation_Body and then not Return_or_Caller_Found then
        -- add implicit return statement when possible
        if This_Transition.Trigger.Return_Type = Undefined then
          declare
             Implicit: Action_Ref := new Action;
          begin
             Implicit.Kind := OpReturn;
             Implicit.Signalled_Event.The_Event := OpReturn_Event;
             Implicit.Signalled_Event.The_Args := new umlExpr_Table(1..1);
              -- 
            This_Transition.Actions := 
              new Actions_Table'(This_Transition.Actions.all & Implicit);
          end;
        else
          Give_Error ("Missing ""return"" action"); 
          raise Parsing_Error;
        end if;
      end if;
      --
      Return_or_Caller_Found := False;
      Is_Operation_Body := False;
  exception
     when others =>  
        if PreviousPE = Parsing_errors then
          Give_Error("Failure during parsing of transition structure");
        end if;
        if Parsing_Errors > 5 then
          raise Parsing_Error;
        else
          while  Current_Item /= "" and then
              (Current_Item /= "}" or  
               (Current_Item = "}" and Next_Item = ";")) loop 
            Skip_Token;
          end loop;
       end if;
  end Parse_Transition_Structure;

  -------------------------------------------------------------------------
  -- A  Transition has the form:
  --  <label>:  <source> -( <trigger> / <actions> )-> <target>  or
  --  <label>:  <source> -> <target>
  --    (with some components potentially missing)
  --     path.initial  -> path.initialstate
  --  where
  --  
  -- e.g.
  --   t1:  S1.R4.s6 -> s2
  --   t2:  (S1.R4.s6,S1.R5.s2) -> s2     join
  --   t3:  S1  -( - / x := x+1; obj.target(v) )->  s2
  --      (S1.R4.s6,S1.R5.s9) -( trigger  )-> s2     # join
  -------------------------------------------------------------------------
  procedure Parse_Transition is
    This_Transition: Transition_Ref;
    These_Names: String_Table_Ref;
    This_Name : String_Ref;
    DefInitial : State_Ref;
    Tmp_Owner: State_Ref;
    This_Label: String_Ref;
  begin
      This_Transition := new Transition;
      --
      --
      -- parse the src and label of the transition:
      --    ti:   (xxx.xxx, yyy.yyy)
      --    tn:   xxx.yyy
      --          yyyy.zzzz
      --          (aaaa.bbb, ccc.ddd)
      --
      -- Parse the label (if any)  but IGNORE IT !!!!!???!?!?
      --
      This_Label := null;
      if Next_Item = ":" then
         This_Label := Parse_Id;
         Skip_Token;    -- next_item
      end if;
      --
      --  parse the source
      --
      if Current_Item  = "(" then
        Skip_Token;
        These_Names:= Parse_Names_List;
        This_Transition.Source := new States_Table(These_Names.all'Range);
        for I in This_Transition.Source.all'Range loop
          This_Transition.Source(I) := Find_State (These_Names(I).all);
        end loop;
        Skip_Token (")");
      else
        This_Name := Parse_Composite_Name;
        --
        This_Transition.Source :=
           new States_Table'( 1..1 => Lazy_Find_State (This_Name.all) );
        -- 
        if This_Name /= null and then 
            (This_Name.all = "initial" or else
             (This_Name.all'Length > 8 and then
               This_Name(This_Name'Last-7 .. This_Name'Last) = ".initial")) then
           DefInitial := This_Transition.Source(1);
           if (Current_Item /= "->") then
             Give_Error (" no trigger / actions allowed for default initial transitions");
             raise Parsing_Error; 
           end if;
        else
          DefInitial :=null;
        end if;
      end if;
      --
      --   parse the trigger/guard/actions  structure
      --
      if Current_item = "-(" or else
         Current_item = "(" then
        Skip_Token;
        Parse_Transition_Structure(This_Transition);
        -- this sets Return_or_Caller_Found
      end if;
      --
  
      if Current_Item = ")->"  or else
          Current_Item = "->"  then
        Skip_Token;
      else
        Give_Error  (" --> Transition target expected (found """& Current_Item & """, maybe missing "";""?) ");
        raise Parsing_Error;
      end if;
      --
      --  parse the transition target:  (xxx.xxx, yyy.yy, ..)
      --
      if Current_Item = "(" then
        Skip_Token;
        These_Names:= Parse_Names_List;
        This_Transition.Target := new States_Table(These_Names.all'Range);
        for I in This_Transition.Target.all'Range loop
         This_Transition.Target(I) :=
           Lazy_Find_State (These_Names(I).all); -- WAS BUG??
        end loop;
        Skip_Token (")");
      else
        This_Name := Parse_Composite_Name;
        This_Transition.Target :=
           new States_Table'( 1..1 =>
               Lazy_Find_State (This_Name.all) );
        if DefInitial /= null then
           if (Current_Item = "{") then
             Give_Error (" no trigger / actions allowed for default initial transitions");
             raise Parsing_Error; 
           elsif This_Transition.Target(1).parent /= DefInitial.parent then
             Give_Error (" target state must be have the same parent state of the source state");
             raise Parsing_Error; 
           end if;
           -- just make sure target is first state of the parent and return
           -- the defauklt ìintial node has not been linked to the parent by Lazy_Find_State
           -- remove explicit "initial" node
           declare
             parent: State_ref := This_Transition.Target(1).parent;
             target: State_ref := This_Transition.Target(1);
             tmp: State_ref;
           begin
             for I in 2 .. parent.substates.all'last loop
                if parent.substates(I)= target then
                  tmp := parent.substates(1);
                  parent.substates(1) := target;
                  parent.substates(I) := tmp;
                  exit;
                end if;
             end loop;
             -- return without further deading with this pseudo transition
             return;
           end;
        end if;
      end if;
      --
      -- parse the HUGO flavour of transition body or
      --  ...
      --
      --  Further transition elaboration
      --  add this transition to the table of ChartTransitions
      --
      This_Transition.Num_Key := 
            All_Classes(Current_Class).ChartTransitions.all'Length + 1;
      All_Classes(Current_Class).ChartTransitions :=
        new Transitions_Table'(
           All_Classes(Current_Class).ChartTransitions.all & This_Transition);
      --
      -- WE STILL HAVE:
      --   TO SET THE TRANSITION OWNER
      --   TO ADD IT TO THE OWNER'S LOCAL TRANSITIONS
      --   TO ADD IT TO THE SOURCE(1).OUTGOING CATALOGUE
      declare
        Current_State: State_Ref;
        Found: Boolean;
      begin
        -- find the LCA (least common ancestor) of the sources
        --
        Tmp_Owner := This_Transition.Source(1).Parent;
        for I in 2 .. This_Transition.Source.all'Length loop
          Found := False;
          while (not Found) and Tmp_Owner /= null loop
            Current_State := This_Transition.Source(I);
            Found := False;
            while (not Found) and Current_State /= null loop
              if Current_State.Parent = Tmp_Owner then
                Found := True;
                exit;
              end if;
              Current_State := Current_State.Parent;
            end loop;
            if not Found then
              Tmp_Owner :=  Tmp_Owner.Parent;
            end if;
          end loop;
        end loop;
        --
        -- find the LCA of the sources+target
        --
        for I in This_Transition.Target.all'Range loop
          Found := False;
          while (not Found) and Tmp_Owner /= null loop
            Current_State := This_Transition.Target(I);
            Found := False;
            while (not Found) and Current_State /= null loop
              if Current_State.Parent = Tmp_Owner then
                Found := True;
                exit;
              end if;
              Current_State :=  Current_State.Parent;
            end loop;
            if not Found then
              Tmp_Owner :=  Tmp_Owner.Parent;
            end if;
          end loop;
        end loop;
        --
        -- Tmp_Owner = LCA
        --
        if Tmp_Owner = null then
          Give_Error ("Error in State Name???");
          raise Parsing_Error;
        else
          --  THIS IS A BUG 
          -- WE CANNOT MOVE LOCAL TRANSITION OWNDER BY A PARALLEL STATE
          -- TO ITS PARENT, OTHERWISE WE LOSE THEIR PARALLEL COMPOSITION
          -- while Tmp_Owner.Kind = Parallel loop
          --   Tmp_Owner :=  Tmp_Owner.Parent;
          --   --  NOTICE when Lazy_Parsing the Kind can still be Composite being later
          --   --   changed into Parallel. In the case we should move out the localtransitions.
          --   if Tmp_Owner = null then
          --     Give_Error ("Invalid model with top level parallel state");
          --     raise Parsing_Error;
          --   end if;
          -- end loop;
          This_Transition.Owner := Tmp_Owner;
          if Tmp_Owner.Kind = Parallel then
             Give_Error ("Transitions inside a parallel state cannot connect different regions.");
             raise Parsing_Error;
          end if;
        end if;
      end;
      --
      Tmp_Owner.LocalTransitions := 
        new Transitions_Table'(
          Tmp_Owner.LocalTransitions.all & This_Transition);
                --
      -- parse the HUGO flavour of transition body or
      --  complete the null transition
      --
      if This_Transition.Trigger = null then
        This_Transition.Trigger :=  Null_Event;
        This_Transition.Actions := new Actions_Table'(No_Actions);
        if Current_item = "{"  then
             -- HUGO FLAVOUR
          Skip_Token;
          Parse_Transition_Structure(This_Transition);
          -- this sets  Return_or_Caller_Found
          Skip_Token("}");
        end if;
      end if;
      --
      -- OutGoing_Catalogue ...
      --
--     declare
--       This_Substate: State_Ref;
--     begin
--       for I in Tmp_Owner.SubStates.all'Range loop
--         This_Substate := Tmp_Owner.Substates(I);
--         --
--         if This_Transition.Source(1) = This_Substate or else
--            IsNested (This_Transition.Source(1).all, This_Substate.all) then
--           --
--           -- build its catalogue;
--           This_Substate.OutgoingTransitions :=
--              Add_To_Outgoing_Catalogue (This_Transition,
--                                          This_Substate.OutgoingTransitions); 
--           exit;
--         end if;
--         --
--        end loop;
--      end;
      --
  exception 
    when others =>
      Parsing_Failed := True;
      if Parsing_Errors > 5 then
        raise Parsing_Error;
      else
        while  Current_Item /= "" and then 
              (Current_Item /= "}"  or else
               Next_Item = "}") and then
               Current_Item /= "Class" and then
               Current_Item /= "class" and then
               Current_Item /= "end" and then
               Next_Item /= ":" and then           --  Obj: Class
               Next_Item /= "." and then           --  S1.s2 -> s3
               Next_Item /= "->" and then          --  s2 -> s4
               Current_Item /= "Objects" and then
               Current_Item /= "objects" loop
            Skip_Token;  -- skip items until ond of transition
        end loop;
      if Current_Item= "}" then Skip_Token; end if;
    end if; 
    --
  end Parse_Transition;

  procedure Parse_Transitions is
  begin
    if Current_Item = "Transitions" or else
       Current_Item = "transitions" or else
       Current_Item = "behaviour" or else
       Current_Item = "Behaviour" or else
       Current_Item = "behavior" or else
       Current_Item = "Behavior" then
      Skip_Token;
    end if;
    if Current_item = ":" then
      Skip_Token;
    end if;
    -- Class /Chart Definition MUST be ended by "end".
    while Current_Item /= "" and then
          Current_Item /= "end"  loop
      Parse_Transition;
    end loop;
    --
    if Current_Item = "end"  then
      Skip_Token;
      if Current_Item = All_Classes(Current_Class).Name.all  then
        Skip_Token;
      end if;
      if Current_Item = ";" then
         Skip_Token;
      end if;
    end if;
  end Parse_Transitions;

--  New Action  "return <expr>" required for trans triggered by operations
--  Check Add_Action and adjust evaluation Env so that return type is known
--  New special signal "return(v)"  "return"
--  OOPS  CALL  events MUST HAVE AN IMPLICIT caller:obj PARAMETER!!!
--  
begin
  null;
end UML_Parser;
