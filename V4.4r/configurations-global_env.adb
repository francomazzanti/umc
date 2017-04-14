with Flags; use Flags;
with Ada.Text_IO; use Ada.Text_Io;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
separate (Configurations)
package body Global_Env is
--
-- MEMORY FRIENDLY
--

  -- "This" is inserted in "Here" before the first element greater then "This"
  function InsertSorted (This: Int64; Here: Int64_Table) return Int64_Table is
    Result: Int64_Table(1..Here'Length+1);
    Index: Natural :=0;
    J: Natural :=1;
  begin
    --PRE
    for I in Here'Range loop
      if This > Here(I) then 
         Index := Index+1;
         Result(Index) := Here(I);
         J := I+1;
      else 
       exit;
      end if;
    end loop;
    -- THIS
    Index := Index+1;
    Result(Index) := This;
    --POST
    for I in J .. Here'Last loop
      Index := Index+1;
      Result(Index) := Here(I);
    end loop;
    return Result;
  end InsertSorted;

 
  -------------
  -- used by Computations_DB.Set_Status
  -------------
  procedure Mark_Step is
  begin
    All_Steps := All_Steps+1;
    if Verbose_Eval then
    if All_Steps < 10000 and then
        All_Steps mod 1000 =0 then
        Put_line ("... " & Int64'Image (All_Steps));
    elsif All_Steps mod 2000 =0 then 
        Put_line ("... " & Int64'Image (All_Steps));
    end if;
   end if;
  end Mark_Step;


  ------------------------------------------------------------------
  --   used by ACTL_Parser
  -- called on the result of Parse_Identitier (the APPLY id)
  -- called on the result of Parse_Action_Label  (as label id)
  ------------------------------------------------------------------
  function Normalised_Label (Label : String) return String is
  --
  --  Removes strange unexpected  chars 
  --   i.e. NOT spaces, letters, digits, .,?, !, _,),(,#, :, =, ;  ....
  -- 
  --  Translates "i"  into  "tau" 
  --  Moves  ! and ? from the end to the beginning of the string
  --  Removes initial and final "  (if present)
  --
    Transformed: String(1..Label'Length);
    New_Length: Natural;
  begin
--     New_Length := 0;
---    for J in Label'range loop
--      if Label(J) in 'a'..'z'  or
--         Label(J) in 'A'..'Z'  or
--          Label(J) in '0'..'9'  or
--          Label(J) = '?'  or
--          Label(J) = '+'  or
--          Label(J) = '!'  or
--          Label(J) = '('  or
--          Label(J) = '#'  or
--          Label(J) = ')'  or
--          Label(J) = '/'  or
--          Label(J) = '<'  or
--          Label(J) = '>'  or
--          Label(J) = ']'  or
--          Label(J) = '['  or
--          Label(J) = '}'  or
--          Label(J) = '{'  or
--          Label(J) = ','  or    -- added after UMC
--          Label(J) = ':'  or    --- added by UMC
--          Label(J) = ';'  or    --- added by UMC
--          Label(J) = '.'  or    --- added by UMC
--          Label(J) = '\'  or    --- added by UMC
--          Label(J) = ' '  or    --- added by UMC
 --         Label(J) = '='  or    --- added by UMC
 --         Label(J) = '-'  or    --- added by UMC
 --         Label(J) = '_'  then
 --         New_Length := New_Length +1;
 --         Transformed(New_Length) := Label(J);
 --       end if;
 --   end loop;
    if Label'Length = 0 then
       return Label;
    end if;
    --
    -- strip the quotation marks
    if Label(Label'First) = '"' and then 
       Label(Label'Last) = '"' then
       New_Length := Label'Length -2;
       Transformed(1..New_Length) := Label(Label'First+1 .. Label'Last-1);
    else
      New_Length := Label'Length;
      Transformed := Label;
    end if;
--    if New_Length=0 then
--       Put_line("WARNING: Invalid action label: " & Label);
--       raise UML_Error;
--    end if;
    if Transformed(New_Length) = '!'  or
       Transformed(New_Length) = '?'  then
       Transformed(1..New_length) :=
          Transformed(New_length) & Transformed(1..New_Length-1);
    end if;
    if New_Length =1 and then Transformed(1) = 'i' then
       return "tau";
    else
      return Transformed(1..New_length);
    end if;
  end Normalised_Label;

  function Trim (Source: String) return String is
    Last_B_Pos : Natural :=0;
  begin
    for I in Source'Range loop
      if Source(I) = ' ' then
        Last_B_Pos := I;
      end if;
    end loop;
    if Last_B_Pos=0 then
       return Source;
    else
     declare
       This_tail: String(1..Source'Last-Last_B_Pos);
     begin
       This_tail := Source(Last_B_Pos+1.. Source'Last);
       return This_tail;
     end;
    end if;
  end Trim;

  function Tail (Source: String) return String is
    Last_Dot_Pos: Natural := 0 ;
  begin
    for I in Source'Range loop
      if Source(I) = '.' then 
        Last_Dot_Pos := I;
      end if;
    end loop;
    if Last_Dot_Pos=0 then 
       return Source;
    else
     declare
       This_tail: String(1..Source'Last-Last_Dot_Pos);
     begin
       This_tail := Source(Last_Dot_Pos+1.. Source'Last);
       return This_tail;
     end;
    end if;
  end Tail;

  function Add_Line_Breaks(Source:String) return String is
    Index: Natural := 0;
    Result: String(1..1000);
  begin
--    if Observation_Mode /= CUSTOM and then 
--       Observation_Mode /= WHITE_BOX then
--      return Source;
--    end if;
    if Source'length < 10 then
        return Source;
    end if;
    for I in Source'Range loop
      if Source(I)=';' then
        Index := Index+1;
        Result(Index) := ';';
        Result(Index+1) := '\';
        Result(Index+2) := 'n';
        Index := Index +2;
      elsif Source(I)=':' and then
            I < Source'Last and then Source(I+1) /= '='  then
        Index := Index+1;
        Result(Index) := ':';
        Result(Index+1) := '\';
        Result(Index+2) := 'n';
        Index := Index +2;
      else
        Index := Index+1;
        Result(Index) := Source(I);
      end if;
    end loop;
    return Result(1..Index);
  end Add_Line_Breaks;

function Factorial (N: Positive) return Positive is
  begin
    if N = 1 then
       return 1;
    else
       return N * Factorial(N-1);
    end if;
  end Factorial;


 function HTML_Format (Source:String) return String is
    Result:String(1..1000);
   OUTC: Natural := 1;
 begin
   for I in Source'Range loop
     if Source(I)='<' then
        Result(OUTC..OUTC+3) := "&lt;";
        OUTC := OUTC+4;
     elsif Source(I)='>' then
        Result(OUTC..OUTC+3) := "&gt;";
        OUTC := OUTC+4;
     else
        Result(OUTC) := Source(I);
        OUTC := OUTC+1;
     end if;
   end loop;
   return Result (1..OUTC-1);
 end;

  function Join (Source: String_Table) return String is
  begin
     if Source'Length=0 then
        return "";
     elsif Source(Source'First) = null then
         return Join(Source(Source'First+1.. Source'Last));
     else
        return Source(Source'First).all & Join(Source(Source'First+1.. Source'Last));
     end if;
  end Join;

end Global_Env;
