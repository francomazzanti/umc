with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Flags; use Flags;
generic
package Configurations is

   ---------------------------------------------------------------------------
   Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref;
   pragma Volatile(Abstract_Predicates);
   -- -- --  configuration options -- -- --
   MemorySaving: Boolean := False;
   pragma Volatile(MemorySaving);
   ------------------------------------------------------------------
   --------  Basic database types and utilitities ---------
   ------------------------------------------------------------------
  package Global_Env is
    --
   function Normalised_Label (Label : String) return String;
   function Tail (Source: String) return String;
   function Trim (Source: String) return String;
   function Factorial (N: Positive) return Positive;
   function Add_Line_Breaks(Source:String) return String;
   function Join (Source: String_Table) return String;
   function InsertSorted (This: Int64; Here: Int64_Table) return Int64_Table;

    -- used by VMC/FMC
    Cache_Modules: array (1..6) of Positive :=
      (131071, 524287, 1048577, 2097151, 4194305, 8388607);
    Cache_Max: Natural := Cache_Modules(6);

  end Global_Env;
  use Global_Env;

   -------------------------------------------------------------------------
   type Constraint_Rule (Size: Natural) is record
       constraint_kind : String_ref; -- da fare una enumeration
       constraint_parts: String_Table(1 .. Size);
       constraint_negations: Bool_Table(1..Size);
   end record;
   type Constraint_Rule_Ref is access Constraint_Rule;
   --
   type Constraints_Table is array (Positive Range <>) of Constraint_Rule_Ref;
   type Constraints_Table_Ref is access Constraints_Table;
   --
   All_Constraints:Constraints_Table_Ref := new Constraints_Table(1 .. 0);
   -------------------------------------------------------------------------

   package Kernel is
     -- -- -- --
--     type System_Configuration is private;
     type System_Configuration is new Integer;
     --
     function "=" (left: System_Configuration; right: System_Configuration) return Boolean;
     function Initial_Configuration return System_Configuration;
     function Undefined_Configuration return System_Configuration;
     --
     function Get_Abstract_State_Labels(Current_Conf: System_Configuration) return String_Tables_Vector;
     function Get_Ground_State_Label_Images(Current_Conf: System_Configuration) return String_Table;
     function Progressive (This_Conf: System_Configuration) return Integer;
     function Configuration_Satisfies(This_SRC: System_Configuration;
                                      State_predicate: String_Table) return Boolean;
     -- -- -- --
     type Evolutions_Iterator is private;
     --
     procedure Iterator_Initialize ( It: in out Evolutions_Iterator;
                                     This_Conf: System_Configuration;
                                     Active_Object: Natural :=0);
     procedure Iterator_Restart (It: in out Evolutions_Iterator);
     procedure Iterator_Finalize (It: in out Evolutions_Iterator);
     function  Has_System_Transition (It: Evolutions_Iterator) return Boolean;
     procedure Iterator_Advance (It: in out Evolutions_Iterator);
     function Get_Source_Configuration (It: Evolutions_Iterator) return System_Configuration;
     function Get_Target_Configuration (It: Evolutions_Iterator) return System_Configuration;
     function Get_Abstract_Action_Labels (It: Evolutions_Iterator) return String_Tables_Vector_Ref;
     function Get_Abstract_Action_Labels (It: Evolutions_Iterator) return String_Tables_Vector;
     function Get_Ground_Action_Labels (It: Evolutions_Iterator) return String_Table_Ref;
     function Get_Ground_Action_Labels (It: Evolutions_Iterator) return String_Table;
     -- -- -- --
     function StatesSpace_Size return Int64;
     procedure Print_StatesSpace_Stats;
     function FindFromKey(Element_Key: String_Table_Ref) return System_Configuration;
     function GetProgressive(Num: Integer) return System_Configuration;
     function GetUniqueKey(This_Conf: System_Configuration) return String_Table_Ref;
     --
     procedure Load_Model (File_Name: String);
     procedure Print_Configuration(This_Conf: System_Configuration);
     procedure HTML_Print_Configuration(HTML_File: File_Type; This_Conf: System_Configuration);
     procedure HTML_Print_Model(HTML_File: File_Type);
     procedure HTML_Print_System_Structure(HTML_File: File_Type; This_Conf: System_Configuration);
     procedure HTML_Print_Possible_Evolutions (HTML_File: File_Type; This_Conf: System_Configuration);
    -- --  VMC liveness check-- -- --
     procedure Check_Liveness(This_Conf: System_Configuration);
    -- --  debugging -- -- --
    procedure Dump_States;
    --
    Live_Model: Boolean;
    pragma Volatile(Live_Model);
    --
  private
--    type System_Configuration is new Integer;
    --
    type XXX_Iterator;
    type Evolutions_Iterator is access XXX_Iterator;
    --
   end Kernel;
    --------------------------------------------------------------------------
   use kernel;


   type System_Evolution is record
     Source: System_Configuration;
     Target: System_Configuration;
     Abstract_Labels: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
     Ground_Labels: String_Table_Ref;
   end record;

   function No_Evolution return System_Evolution;
   function Get_Evolution_Data (It: Evolutions_Iterator) return System_Evolution;
   --
   function Get_Source(The_Evolution: System_Evolution) return System_Configuration;
   function Get_Target(The_Evolution: System_Evolution) return System_Configuration;
   function Get_Abstract_Action_Labels (This_Evolution: System_Evolution) return String_Tables_Vector;
   function Get_Ground_Action_Labels (This_Evolution: System_Evolution) return String_Table;
   function Get_Abstract_Action_Label_Images (This_Evolution: System_Evolution) return String_Table;
   --
   function NickName (This_Conf: System_Configuration; Prefix: String) return String;
   function Get_Abstract_Action_Label_Images (It: Evolutions_Iterator) return String_Table;
   -- -- --
   function AbstractLabels (Thelabels: String_Tables_Vector) return String;

   function Display_AbstractLabels(Thelabels: String_Tables_Vector; ShowEmpty: Boolean := False) return String;
   function Display_AbstractStateLabels(Thelabels: String_Tables_Vector) return String;
   function Abstract_Action_Labels (This_Evolution: Evolutions_Iterator) return String;
   --
end Configurations;
