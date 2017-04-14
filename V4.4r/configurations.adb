with Flags; use Flags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
package body Configurations is
--
--  private
--    type System_Configuration is new Integer;
--    --
--    type XXX_Iterator;
--    type Evolutions_Iterator is access XXX_Iterator;
--    --
--   end Kernel;
--   use kernel;
--
--   type System_Evolution is record
--     Source: System_Configuration;
--     Target; System_Configuration;
--     Abstract_Labels: String_Tables_Vector_Ref;
--     Ground_Labels: String_Table_Ref;
--   end record;
--
--   function No_Evolution return System_Evolution;
--   function Get_Evolution_Data (It: Evolutions_Iterator) return System_Evolution;
--   --
--   function Get_Source(The_Evolution: System_Evolution) return System_Configuration;
--   function Get_Target(The_Evolution: System_Evolution) return System_Configuration;
--   function Get_Abstract_Action_Labels (This_Evolution: System_Evolution) return String_Tables_Vector;
--   function Get_Ground_Action_Labels (This_Evolution: System_Evolution) return String_Table;
--   function Get_Abstract_Action_Label_Images (This_Evolution: System_Evolution) return String_Table;
--   --
--   function NickName (This_Conf: System_Configuration; Prefix: String) return String;
--   function Get_Abstract_Action_Label_Images (It: Evolutions_Iterator) return String_Table;
--   -- -- --
--   function AbstractLabels (Thelabels: String_Tables_Vector) return String;
--   function Display_AbstractLabels(Thelabels: String_Tables_Vector; ShowEmpty: Boolean := False) return String;
--   function Display_AbstractStateLabels(Thelabels: String_Tables_Vector) return String;
--   function Abstract_Action_Labels (This_Evolution: Evolutions_Iterator) return String;
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
   function Compare (X: String_Table; Y: String_Table) return Integer is
       -- returns -1 if X <Y, 0 if X=Y,  1 if X >Y
   begin
     --   "" < "a"
     if X'Length = 0 and then X'Length = 0 then return 0; end if;
     if X'Length = 0 then return -1; end if;
     if Y'Length = 0 then return 1; end if;
     if X(X'first).all < Y(Y'First).all then return -1; end if;
     if X(X'first).all > Y(Y'First).all then return 1; end if;
     return Compare(X(X'First+1..X'Last), Y(Y'First+1..Y'Last));
   end Compare;

   ---   called to sort the sets of lables on edges or states:
   --     {b,a,b}  ->  {a,b}
   function SortUnique (What: String_Tables_Vector_Ref) return String_Tables_Vector_Ref is
      Sorted: String_Tables_Vector := What.all;
      Tmp: String_Table_Ref;
      ord: Integer;
      removed: Integer:=0;
      Old: String_Tables_Vector_Ref := What;
   begin
      for I in Sorted'First .. Sorted'Last-1 loop
         for J in I+1..Sorted'Last loop
            -- Sorted[n] == null -> greatest in the sorting
           if Sorted(I) = null then
              Sorted(I) := Sorted(J);
              Sorted(J) := null;
           elsif Sorted(J) /= null then
               ord :=Compare(Sorted(I).all,Sorted(J).all);
               if ord =0 then
                  Sorted(J) := null;
                  removed := removed+1;
               elsif ord =1 then
                 Tmp:= Sorted(I);
                 Sorted(I) := Sorted(J);
                 Sorted(J) := Tmp;
               end if;
           end if;
         end loop;
      end loop;
      if removed =0 then
         What.all := Sorted;
         return What;
      else
         Old := What;
         if Old /= Empty_String_Tables_Vector_Ref then Free(Old); end if;
         return new String_Tables_Vector'(Sorted(Sorted'First ..
                                                   Sorted'Last-removed));
      end if;
   end SortUnique;

   function No_Evolution return System_Evolution is
     Result: System_Evolution;
   begin
     Result.Source := Undefined_Configuration;
     Result.Target := Undefined_Configuration;
     -- Abstract_Labels -> null;
     -- Ground_Labels -> null;
     return Result;
   end No_Evolution;

   function Get_Evolution_Data (It: Evolutions_Iterator) return System_Evolution is
     Result: System_Evolution;
   begin
     Result.Source := Kernel.Get_Source_Configuration(It);
     Result.Target := Kernel.Get_Target_Configuration(It);
     Result.Abstract_Labels := Kernel.Get_Abstract_Action_Labels(It);
     if Flags.Ground_Action_Labels_Needed then
       Result.Ground_Labels := Kernel.Get_Ground_Action_Labels(It);
     else
       Result.Ground_Labels := Empty_String_Table_Ref;
     end if;
     return Result;
   end Get_Evolution_Data;
  
   function Get_Source(The_Evolution: System_Evolution) return System_Configuration is
   begin
      return The_Evolution.Source;
   end Get_Source;

   function Get_Target(The_Evolution: System_Evolution) return System_Configuration is
   begin
      return The_Evolution.Target;
   end Get_Target;

   function Get_Abstract_Action_Labels(This_Evolution: System_Evolution) return String_Tables_Vector is
   begin
      return This_Evolution.Abstract_Labels.all;
   end Get_Abstract_Action_Labels;

   function Get_Ground_Action_Labels(This_Evolution: System_Evolution) return String_Table is
   begin
       if This_Evolution.Ground_Labels /= null then
         return This_Evolution.Ground_Labels.all;
       else
         return Empty_String_Table;
       end if;
   end Get_Ground_Action_Labels;

   function Get_Abstract_Action_Label_Images (This_Evolution: System_Evolution) return String_Table is
     TheLabels: String_Tables_Vector := This_Evolution.Abstract_Labels.all;
     Result: String_Table(1..1);
   begin
     Result(1) := new String'(AbstractLabels(TheLabels));
     return Result;
   end Get_Abstract_Action_Label_Images;

  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------

   function Get_Abstract_Action_Label_Images (It: Evolutions_Iterator) return String_Table is
     TheLabels: String_Tables_Vector := Get_Abstract_Action_Labels(It);
     Result: String_Table(1..1);
   begin
     Result(1) := new String'(AbstractLabels(TheLabels));
     return Result;
   end Get_Abstract_Action_Label_Images;

  -----------------------------------------------------------------------------
   function NickName (This_Conf: System_Configuration; Prefix: String) return String is
     Tmp: String := Integer'Image(Progressive(This_Conf));
   begin
     -- add Prefix and skip leading blank
     return Prefix & Tmp(Tmp'First+1..Tmp'Last);
   end NickName;

  -----------------------------------------------------------------------------
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
  end HTML_Format;

  ---------------------------------------------------------------------------
  function ArgsImage (TheArgs: String_Table) return String is
   begin
     if TheArgs'Length =0 then return ""; end if;
     if TheArgs'Length =1 then return
          TheArgs(TheArgs'First).all;
     end if;
     return TheArgs(TheArgs'First).all & "," &
          ArgsImage(TheArgs(TheArgs'First+1 .. TheArgs'Last));
   end ArgsImage;

   --------------------------------------------------------------------
   --  (a,b,c)(d,e)    -->  "a(b,c),d(e)"
   --------------------------------------------------------------------
   function AbstractLabels (Thelabels: String_Tables_Vector) return String is
   begin
     if Thelabels'Length = 0 then return ""; end if;
     if Thelabels'Length =1 then
     if Thelabels(Thelabels'First)'Length = 0 then return ""; end if;
       if Thelabels(Thelabels'First)'Length = 1 then
              return Thelabels(Thelabels'First)
                 (Thelabels(Thelabels'First)'First).all;
       end if;
       return
         Thelabels(Thelabels'First)(Thelabels(Thelabels'First)'First).all &
           "(" & ArgsImage(
             Thelabels(Thelabels'First)
                (Thelabels(Thelabels'First)'First+1 ..
                 Thelabels(Thelabels'First)'Last)) &
           ")";
       end if;
       --
       if Thelabels(Thelabels'First)'Length = 0 then
          return AbstractLabels(Thelabels(Thelabels'First+1..Thelabels'Last));
       end if;
       if Thelabels(Thelabels'First)'Length = 1 then
          return Thelabels(Thelabels'First)
                   (Thelabels(Thelabels'First)'First).all & ", " &
                 AbstractLabels(Thelabels(Thelabels'First+1..Thelabels'Last));
       end if;
         return
         Thelabels(Thelabels'First)(Thelabels(Thelabels'First)'First).all &
           "(" & ArgsImage(
           Thelabels(Thelabels'First)
             (Thelabels(Thelabels'First)'First+1 ..
              Thelabels(Thelabels'First)'Last)) &
           ")" & ", " &
        AbstractLabels(Thelabels(Thelabels'First+1..Thelabels'Last));
   end AbstractLabels;

   --------------------------------------------------------------------
   function Display_AbstractLabels(Thelabels: String_Tables_Vector; ShowEmpty: Boolean := False) return String is
   begin
     if TheLabels'Length=0 and then not ShowEmpty then return ""; end if;
     return
        "{" & AbstractLabels(Thelabels)  & "}" ;
   end Display_AbstractLabels;

   --------------------------------------------------------------------
   function Display_AbstractStateLabels(Thelabels: String_Tables_Vector) return String is
   begin
     if TheLabels'Length=0 then return ""; end if;
     return AbstractLabels(Thelabels);
   end  Display_AbstractStateLabels;

   --------------------------------------------------------------------
  function Abstract_Action_Labels (This_Evolution: Evolutions_Iterator) return String is
      SS: String := Display_AbstractLabels(Get_Abstract_Action_Labels(This_Evolution));
  begin
    for I in SS'Range loop
      if SS(I) = '"' then SS(I..I) := "'"; end if;
    end loop;
    if SS = "" then
        return "{}";
    else
        return SS;
    end if;
  end Abstract_Action_Labels;
  ---------------------------------------------------------------------------
 
  function Get_Abstract_State_Label_Images(Current_Conf: System_Configuration) return String_Table is
     TheLabels: String_Tables_Vector := Get_Abstract_State_Labels(Current_Conf);
     Result: String_Table(1..1);
  begin      
     Result(1) := new String'(AbstractLabels(TheLabels));
     return Result;
  end Get_Abstract_State_Label_Images;

  package body Global_Env is separate;
  package body Kernel is separate;

begin
  null;
end Configurations;
