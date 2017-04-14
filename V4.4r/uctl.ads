with Flags; use Flags;
with Ada.Unchecked_Deallocation;
with Ada.Text_Io; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Sets;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Characters.Latin_1;
with Configurations;
generic
 with package MyConfigurations is new Configurations(<>); 
package UCTL is
  --   VMC SUBSETS
  Subsets: array(1..4) of Boolean  := (others => False);
  -- Subsets(1)= VACTBL_BOX_TRUE
  -- Subsets(2)= VACTBL_BOX_FALSE
  -- Subsets(3)= VACTBL_BOX_LIVE_TRUE
  --
  procedure Parse_Formula (Formula_FileName: String; Parse_OK: out Boolean);
  --
  procedure HTML_EvaluateIt (Elog_FileName: String;
                             Out_FileName: String;
                             Eval_OK: out Boolean);
  procedure HTML_EvalFromFile
     (HTML_FileName: String; Form_FileName: String; Eval_OK: out Boolean);
  procedure HTML_ExplainEvaluation (HTML_FileName: String;Comp: Integer :=1);
  --
  procedure EvaluateIt (Eval_OK: out Boolean);
  procedure EvalFromFile (Form_FileName: String; Eval_OK: out Boolean);
  procedure EvalFromString(FormCode: String; Eval_OK: out Boolean);
  procedure ExplainEvaluation(Comp: Integer :=1);
  --
  type Property_Status is (True, False, Aborted, Unchecked);
  type State_Property is record
     Status: Property_Status;
     PImage: String_Ref;
     CompRef: Integer;
  end record;
  --
  type State_Properties is array (Positive range <>) of State_Property;
  type State_Properties_Ref is access State_Properties;
  No_properties: constant State_Properties_Ref := new State_Properties(1..0);
  --
  function Get_State_Properties (The_State: MyConfigurations.Kernel.System_Configuration) return State_Properties;
end UCTL;

