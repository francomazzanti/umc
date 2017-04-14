generic
package MC is
  ---------------------- TXT MODE -----------------------
  procedure Load_and_Eval (Model_FileName:String; Formula_FileName: String; OK : out Boolean); 
  procedure LoadModel (Model_FileName: String; LoadOK: out Boolean);
  procedure Start_ModelExploration;
  procedure Select_History(Input_Line: String);
  procedure Select_State(Input_Line: String);
  procedure Select_Evolution(Input_Line: String);
  procedure EvalFromFile(Form_FileName: String; Eval_OK: out Boolean);
  procedure EvalFromString(FormCode: String; Eval_OK: out Boolean);
  procedure ExplainEvaluation(Comp: Integer :=1);
  procedure Configuration_Info;
  procedure Display_Status;
  --
  procedure Set_Ground;
  procedure Set_Abstract(OK: out Boolean);
  --------------------------------------------------------
  procedure Parse_Formula (Formula_FileName: String; Parse_OK: out Boolean); 

  ---------------------- HTML MODE -----------------------
  procedure HTML_LoadModel (Model_FileName: String; 
                            HTML_FileName: String;  LoadOK: out Boolean);
  procedure HTML_Start_ModelExploration (HTML_FileName: String);
  procedure HTML_Select_History (HTML_FileName: String; Input_Line: String);
  procedure HTML_Select_State (HTML_FileName: String; Input_Line: String);
  procedure HTML_Select_Evolution (HTML_FileName: String; Input_Line: String);
  procedure HTML_EvaluateIt (Elog_FileName: String;
                             Out_FileName: String;
                             Eval_OK: out Boolean);

  procedure HTML_EvalFromFile (HTML_FileName: String; Form_FileName: String; Eval_OK: out Boolean);
  procedure HTML_ExplainEvaluation (HTML_FileName: String; Comp: Integer :=1);
  procedure HTML_Configuration_Info (HTML_FileName: String);

-------------------------------------------------------------
  procedure GenerateDOTfile(Root:Integer :=1);
  ----------------------  DEBUGGING ------------------------
  procedure Debug(Str:String);
  procedure Dump_States;

-----------------------------------------------------------------
---      INTERNAL STRUCTURE ---
--
--   global definitions  (String_Ref, etc)
--
--   package Basic Properties
--
--   package Configurations is separate
--     type System_Configuration,  function "=", Initial_Configuration
--     type  System_Evolution is private     function No_Evolution
--     type  Evolutions_Iterator is private
--       procedure Iterator_Initialize, Get_Target_Configuration, Iterator_Advance
--       function Has_System_Transition
--     function FindFromKey, GetUniqueKey, Progressive
--     function Get_Evolution_Data
--     function Get_Abstract_Action_Label_Images, Get_Ground_Action_Label_Images
--     function Get_Abstract_State_Label_Images, Get_Ground_State_Label_Images
--     function Get_Source,  Get_Target
--     function StatesSpace_Size, NickName
--     procedure Load_Model
--     procedure Ex2tab
--     procedure Display_Status, HTML_Display_Status
--     procedure Print_Configuration
--     procedure HTML_Print_Configuration 
----   Evolution_Satisfies_with_Param  -- to be moved in UCTL
----   Evolution_Bindings
----   Evolution_Satisfies_Tau
----   Configuration_Satisfies
------ local_vars:   VarsNames,VarsValues, VarsCount, BindingsCount, CurrentBindings
--     --- body
--     package COW_Types
--     package COW_Parser
--     package COW_Utilities
--     package COW_System
--   
--   more defs  (Abstract_Label_Image, Bilabelled ...)
--   
--   package  UCTL  is separate
--      procedure EvalFromFile
--      procedure EvalFromString
--      procedure HTML_EvalFromFile
--      procedure HTML_ExplainEvaluation
--      procedure Parse_Formula
--      procedure EvaluateIt
--      procedure HTML_EvaluateIt
--      --- body ---
--      package UCTL_Types
--      package UCTL_Parser
--      package UCTL_Utilities
--      Computations_DB
--      UCTL_Logics
--   
--   proc Export2Tab is separate;
--   
--   more auxliary defs
--      Start_ModelExploration
--   
--   implementations
--

end MC;
