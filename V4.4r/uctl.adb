with Flags; use Flags;
with Ada.Text_Io; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation; 
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1;
with NickNames; 
with Ada.Containers; use Ada.Containers;
package body UCTL is
---------------------------------------------------------------------------------
-- DAFARE:  PRINT  formula:  scrive su standard output invece che su HTML_FILE
-- nel caso di HTML_EVALUATE_IT, ed il risultato e' impasticciato (persi i line-break)
-- (vedi "print"  ancora gestito come assertion in uctl_logic.adb
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Subsets: array(1..4) of Boolean  := (others => False);
-- procedure Parse_Formula (Formula_FileName: String; Parse_OK: out Boolean);
-- 
-- procedure HTML_EvaluateIt (Elog_FileName: String;
--                            Out_FileName: String;
--                            Eval_OK: out Boolean);
-- procedure HTML_EvalFromFile
--    (HTML_FileName: String; Form_FileName: String; Eval_OK: out Boolean);
-- procedure HTML_ExplainEvaluation (HTML_FileName: String; Comp: Integer :=1);
--
-- procedure EvaluateIt (Eval_OK: out Boolean);
-- procedure EvalFromFile (Form_FileName: String; Eval_OK: out Boolean);
-- procedure EvalFromString(FormCode: String; Eval_OK: out Boolean);
-- procedure ExplainEvaluation(Comp: Integer :=1);
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- Subsets(1) = true if the formula holds for all products
-- Subsets(2) = true if the formula does not hold for any product
-- Subsets(3) = true if the formula holds for all live products    NO MORE!!!!! NOT VALID

-- Subset(1) =  VACTL-BOX =  POS + not NEG
-- POS = true, false, max, min, and, or, <>#, [], EF#, EF#{}, AF#, AF#{}, AG, AG#
-- NEG = true, false, max, min, and, or, <>, EF, AF, AF[]

-- Subset(2) =  LIVE-VACTL-BOX =  POS + not NEG
-- POS = true, false, max, min, and, or, <>#, [], EF#, EF#{}, AF#, AF#{}, AG, AG#, AF, AF{}
-- NEG = true, false, max, min, and, or, <>, EF, AF, AF[]

-- Subset(3) =  VACTL-BOXNEG =  NEG + not POS

use Ada.Text_IO;
use Ada.Exceptions; 
  ---
  package MyIO is new Fixed_IO(Duration);
  package Configurations renames  MyConfigurations;
  use Configurations;
  use Configurations.Kernel;

  T1, T2: Time; -- used to compute the actual evaluation time
  DD: Duration;
  Elapsed: String(1..30);
  FragmentsCount: Natural :=0;
  ---------------------------------------------------------------------------------
  ---------------------------------------------------------------------------------
  type Computation_Status is
    (Not_Yet_Started, In_Progress, Aborted,
      Tmp_True, Found_True, Tmp_False, Found_False);
  pragma Volatile(Computation_Status);

  Max_Explanation_Depth: constant Positive := 5000;

  -- THREAD SAFETY: non essential but desirable
  -- maybe let's reduce the prints in case of parallelism
  Lines: Natural := 0;
  procedure Mark_Step is
  begin
    All_Steps := All_Steps+1;
    if Verbose_Eval and then
     ((All_Computations_Count mod 1_000_000=0) or else
      (All_Computations_Count < 1000000 and then All_Computations_Count mod 100000 =0) or else
      (All_Computations_Count < 100000 and then All_Computations_Count mod 10000 =0) or else
      (All_Computations_Count < 10000 and then All_Computations_Count mod 1000 =0) or else
      (All_Computations_Count < 1000 and then All_Computations_Count mod 100 =0)) then
       --
      Lines := Lines +1;
      Put_line (" "  &
             Int64'Image (Done_Computations) & " /" &
             Int64'Image (All_Computations_Count)  & " computations)" );
      if Lines = 10  then
         Put_Line ("  ------------------------------------------");
         Configurations.Kernel.Print_StatesSpace_Stats;
         Put_Line ("  ------------------------------------------");
         Lines := 0;
      end if;
      Flush(Current_Output);
    end if;
  end Mark_Step;

  ---------------------------------------------------------------------------
  type Evolutions_Table is array(Positive Range <>) of Configurations.System_Evolution;
  type Evolutions_Table_Ref is access Evolutions_Table;
  procedure Free is new Ada.Unchecked_Deallocation(Evolutions_Table,Evolutions_Table_Ref);

  function Abstract_Label_Image
       (This_Evolution: Configurations.System_Evolution) return String is
    TheLabels: String_Tables_Vector := Get_Abstract_Action_Labels(This_Evolution);
  begin
     return Display_AbstractLabels(TheLabels,True);
  end Abstract_Label_Image;

  function Joined (Vector: String_Table) return String is
  begin
     if Vector'Length =1 and then
           Vector(Vector'First) /= null then
         return Vector(Vector'First).all;
     elsif Vector'Length >1 then
        if Vector(Vector'First) /= null then
          return Vector(Vector'First).all & "," & Joined(Vector(Vector'First+1 .. Vector'Last));
        else
          return " ," & Joined(Vector(Vector'First+1 .. Vector'Last));
        end if;
    else
         return "";
    end if;
  end Joined;


  -- used during path descrition inside interactive formula explanation 
  -- e.g.:   C1 --> C2  {}  /* ground labels */  ( as tooltiops or plain text)
  function Ground_Label_Image
       (This_Evolution: Configurations.System_Evolution) return String is
    ALI: String_Table := Get_Ground_Action_Labels(This_Evolution);
  begin
     return Joined(ALI);
  end Ground_Label_Image;

  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------------
  ---------------------------------------------------------------------------------

--###################################################################################
package UCTL_Types is
  ---------------------------------------------------------------------------------
  ---------------------------------------------------------------------------------

    type AssertOp is (NOOP, EQ, GT, LT, LE, GE, NE);   -- GE LE NE to be added
    type AssertBinOp is (NOOP, PLUS);   -- Minus, ... to be added

    -- Currently the parser recognizes only Aid-kind basic actions, of the form label(arg,arg)
    -- Aas-kind basic actions were once recognized in the form  obj.x'  = obj.x+1 (inspired by TLA)
    --    but now are no longer supported.
    type BAKind is (Aid, Aas);
    type Basic_Action (Kind: BAKind) is record
      case Kind is
      when Aid =>
         Source:String_Ref;
         Target: String_Ref;
         Event: String_Ref;
         Modality: String_Ref;
         Params: String_Table_Ref; ---:= Empty_String_Table_Ref;  -- or null
         Labels: String_Table_Ref; --- := Empty_String_Table_Ref;  -- or null
     when Aas =>
        Left_Ids: String_Table_Ref;
        Op:AssertOp := NOOP;
        Right_Ids: String_Table_Ref;
        RightOp: AssertBinOp := NOOP;
        More_Right_Ids: String_Table_Ref;
      end case;
    end record;
    type Basic_Action_Ref is access Basic_Action;

    type Basic_Predicate is record
         Left_Ids: String_Table_Ref;
         Left_Index: String_Ref;
         LeftOp: AssertBinOp := NOOP;
         More_Left_Ids : String_Table_Ref;
         More_Left_Index: String_Ref;
         Op: AssertOp := NOOP;
         Right_Ids: String_Table_Ref;
         Right_Index: String_Ref;
         RightOp: AssertBinOp := NOOP;
         More_Right_Ids : String_Table_Ref;
         More_Right_Index: String_Ref;
    end record;
    type Basic_Predicate_Ref is access Basic_Predicate;

  subtype VarsRange is Natural range 0..100;
  type BindValues (VarsCount: VarsRange := 0) is record
    VarValues: String_Table(1..VarsCount);
  end record;

  type BindTable  is array (Natural range <> ) of BindValues;

  type AllBindings (VarsCount: Natural; BindCount: Natural) is record
     MatchOK: Boolean := False;
     VarNames: String_Table(1..VarsCount);
     AllValues:  BindTable(1..BindCount);
  end record;

  type VarBindings (Size: Natural) is record
     VarNames: String_Table(1..Size);
     VarValues: String_Table(1..Size);
  end record;

  --
  type Formula;
  type Formula_Ref is access Formula;
  type Action;
  type Action_Ref is access Action;
  type Path;
  type Path_Ref is access Path;

  type Form_Table is array (Positive range <>) of Formula_Ref;
  type Form_Table_Ref is access Form_Table;
  Empty_Form_Table : constant Form_Table := (1..0 => null);

  type Fkind is ( Ftrue, Ffalse, Fnot, Foor, Fand, Fimply, Fapply,
         Fmax, Fmin, Fangle, FWangle, FWsquare, Fsquare,
         Fexist, Fall, Assertion);

--  type AssertOp is (NOOP, EQ, GT, LT, LE, GE, NE);   -- GE LE NE to be added
--  type AssertBinOp is (NOOP, PLUS);   -- Minus, ... to be added

  ------------------------------------------------------------------------
  -- se FF = " (AG ( Y | [true] X) & Z "
  --    FF.Free_Vars = [X.Fulldef, Y.FullDef, Z.FullDef] (ordine non signif.)
  ------------------------------------------------------------------------
  -- se FF1 =  AG Y
  --  e FF2 =  max Y:  X & AG Y
  --    FF1.Free_Vars=[Y.Fulldef],  FF1.Closed_Context=[X.FullDef,Y.Fulldef]
  --   (i.e. per poter valutare Y devo sapere anche cosa e' X]
  ------------------------------------------------------------------------
  --   Sia Env e' lo stack degli identificatori di formule max e min in cui Form e' inclusa.
  --   (usato solo durante il parsing)
  --
  --   Il Closed_Context della formula e' la lista
  --     delle forumule ricorsive (max e min) di cui la sua valutazione
  --      fa uso (diretto o indiretto) -- settato da Set_Closed_Context in uctl_utilities
  --
  --  In generale Env'Length => Context'Length
  --
  --  Env_Selector, invece e' la tabella costituita dagli INDICI,
  --   relativi ad ENV degli elementi costituenti il CONTEXT
  --
  --  In questo modo, non e' necessario analizzare tutto l'env per
  --  individuare la parte che serve per valutare la formula, inoltre
  --  l' ENv_Selector permette di selezionare solo la sottoparte
  --   dell'Env da salvare assieme alla formula (Trimmed_Context)
  ------------------------------------------------------------------------
  --
  ------------------------------------------------------------------------
  type Formula (Kind: Fkind) is record
    Fimage: String_Ref;              -- used essentially for debugging purposes
    AGIndex: Natural :=0;            -- used by NWO Parallel NonIntercative AG evaluations
    Depth:Integer :=0;               -- ????? UNUSED
    Free_Vars: Form_Table_Ref;       -- used statically to compute losed_Context
    Closed_Context: Form_Table_Ref;  -- used statically to compute Env_Selector
    Env_Selector: Num_Table_Ref;     -- used during evaluation to compute the dynamic context
    case Kind is
      when Ftrue | Ffalse   =>
         null;
      when Fnot  =>
         NotRef: Formula_Ref;
      when Fand | Foor | Fimply   =>
         LeftRef: Formula_Ref;
         RightRef: Formula_Ref;
      when Fapply =>
         IDen: String_Ref;
         FullDef: Formula_Ref;
         Optargs: String_Table_Ref;
      when Fmax | Fmin =>
         IDef: String_Ref;
         FDef: Formula_Ref;
         Context_Size:Natural;
      when Fangle | Fsquare | FWangle | FWsquare =>
         ARef: Action_Ref;
         AVec : Num_Table_Ref;
         FormRef: Formula_Ref;
      when Fexist | Fall  =>
         PRef: Path_Ref;
      when Assertion =>
         Pred: Basic_Predicate_Ref;
    end case;
  end record;

  True_Formula: constant Formula_Ref := 
      new Formula'(Ftrue,new String'("true"),0,0,null,null,null);
  False_Formula: constant Formula_Ref := 
      new Formula'(Ffalse,new String'("false"),0,0,null,null,null);
      
  type PKind is (Act_Next,  Until1, Wuntil1, Until2 ,Wuntil2, Eventually, Always);

  type Path (Kind: Pkind) is record
    case Kind is
      when  Eventually | Always =>
        TForm : Formula_Ref;
      when Act_Next =>
        AARef : Action_Ref;
        AFormRef: Formula_Ref;
      when Until1 | Wuntil1 =>
        U1FormRef1: Formula_Ref;
        U1ARef : Action_Ref;
        U1FormRef2: Formula_Ref;
      when Until2 | Wuntil2 =>
        U2FormRef1: Formula_Ref;
        U2ARef1 : Action_Ref;
        U2ARef2 : Action_Ref;
        U2FormRef2: Formula_Ref;
    end case;
  end record;

  type Akind is ( Atrue, Afalse,  Anot, Aand, Aor,  Aid, Aas);
  type Action (Kind: Akind) is record
    case Kind is
      when Atrue | Afalse   =>  null;
      when Anot  =>
         Anot: Action_Ref;
      when Aand | Aor   =>
         Aref1: Action_Ref;
         Aref2: Action_Ref;
      when Aid =>
         AidPred: Basic_Action_Ref := new Basic_Action(Aid);
       when Aas =>
         AasPred: Basic_Action_Ref := new basic_Action(Aas);
    end case;
  end record;

  type Result_Data is record
     ResultValue: Computation_Status := NOT_YET_STARTED;
     EvalTime: Duration := 0.0;
  end record;
  type Results_Table is array (Positive range <>) of Result_Data;
  type Results_Table_Ref is access Results_Table;
  
  type Subsets_Data is array (1..4) of Boolean;   -- 4 for Live_Model
  type Subsets_Table is array (Positive range <>) of Subsets_Data;
  type Subsets_Table_Ref is access Subsets_Table;
  
  True_Action: constant Action_Ref := new Action(Atrue);
  False_Action: constant Action_Ref := new Action(Afalse);
  NOTMay_Action: constant Action_Ref := 
      new Action'(
        Anot,
        new Action'(
          Aid,
          new Basic_Action'(Aid,null,null,null,null,null,new String_Table'(1=> new String'("may")))
        ));

  Predefined_Formulas: constant String_Table := (
      new String'("FINAL"),
      new String'("DEPTH_GT_"),
      new String'("DEPTH_LT_"),
      new String'("DEPTH_EQ_"),
      new String'("DEPTH_NE_"),
      new String'("PRINT_ONCE"),
      new String'("PRINT"),
      new String'("COUNT"),
      new String'("STEPS_GT_"));
  --
  -- the global variable where the parsing result is saved
  This_Formula: Formula_Ref; 
  All_Formulas: Form_Table_Ref;
  All_Results: Results_Table_Ref;
  All_Subsets: Subsets_Table_Ref;
  All_Prints: Bool_Table_Ref;
  -- 
  -- THE FOLLOWING SHOULD BE ADJUSTED EACH TIME FOR EACH FORMULA IN ALL_FORMULAS
  Formula_contains_PRINT: Boolean := False; -- set by UCTL_parser
  Formula_contains_COUNT: Boolean := False; -- set by UCTL_parser
  Requested_Counts: Integer := 0;
--  Counting_Requested: Boolean := False;     -- set by Eval ?!?

end UCTL_Types;
--######################################################################################


--######################################################################################
package UCTL_Parser is
  use UCTL_Types;
  Max_Line_Length: Positive := 10000;
  Max_Empty_lines: Natural := 500;
  Max_Comment_lines: Natural := 500;
  --
  Max_Components: Positive := 500;
  Max_Synchronizations: Positive := 10000;
  --
  -- Parses the actl file indentified by the given name and creates
  -- the corresponding uctl object in UCTL_Types.This_Formula
  --
  procedure Parse (Tab_File_Name: String);
  procedure Parse_From_String (Input_Line: String);
--  Formula_contains_PRINT: Boolean := False;
   --
end UCTL_Parser;
--######################################################################################
use UCTL_Parser;


  

--######################################################################################
package UCTL_Utilities is
use UCTL_Types;
  -- checks that the not operator is used correctly inside rec definitions
  --
  function Is_Monotone (Form: Formula_Ref;
                       Id: String_Ref  := null;
                       Direction: Boolean := True) return Boolean;
  -- 
  -- Insert full definitions in Apply nodes ..
  -- Evaluates DEPTH_GT actions
  -- Checks existence of used action tokens 
  --                            
  procedure Prepare_Formula (Form: Formula_Ref);
  --                    
  --  procedure Set_Free_Vars (Form: Formula_Ref);
  --                
  procedure Check_Actions (Form: Formula_Ref);
  --                   
  procedure Print (Formula: Formula_Ref; More_Lines: Boolean := True);
  --                   
  procedure Print_Formula (Margin: String;
                           Form: Formula_Ref;
                           More_Lines: Boolean := True);
  procedure Print_Action (Action: Action_Ref);
  function Aimage (Action: Action_Ref) return String;
  --
  function NewInstance(Form: Formula_Ref; TheBindings: VarBindings) return Formula_Ref;
  --
  --  function NewInstance(Form: Formula_Ref; BindVar: String_Ref; Bound_value: String_Ref) return Formula_Ref;
  --  function Binding_Var (Form: Formula_Ref) return String_Ref;

  procedure Check_Params (Form: Formula_Ref);
end UCTL_Utilities;
--######################################################################################
use UCTL_Utilities;






--######################################################################################
--
--######################################################################################
package Computations_DB is
use UCTL_Types;
use Ada.Strings;
--!use MyConfigurations;
use Configurations;

------------------------------------------------------------------
   --------  Basic database types and utilitities ---------
------------------------------------------------------------------

  subtype Evolution_Data  is System_Evolution;

  type Computations_Table;
  type Computations_Table_Ref is access Computations_Table;

  type Computation_Element is record   -- the structure of actual database items
    -- key identifying part
    FormImage: String_Ref;
    ContextImage: String_Ref;
    OwnerImage: String_Ref;
    StateImage: String_Ref;
    --  stable part
    Form: Formula_Ref;
    Context: Computations_Table_Ref;
    Owner: System_Configuration;
    State: System_Configuration;
    Progressive: String_Ref;
    --  modifiable part
    Status: Computation_Status := Not_Yet_Started;
    SubComputations:  Computations_Table_Ref;
    SubEvolutions:  Evolutions_Table_Ref;
    SuperComputations:  Computations_Table_Ref;
    Last_Exploration: Natural := 0;
    Max_LTS_Depth: Natural := 0;
    Rec_Depth: Natural :=0;
  end record;
  pragma Volatile(Computation_Element);

  type Computation_Step is new Integer;

  type Computations_Table is array (Positive Range <>) of Computation_Step;
  Empty_Computations_Table: Computations_Table(1..0);
  No_Computations: constant Computations_Table_Ref := new Computations_Table(1..0);

-----------------------------------------------------------------------
    ----------- Dealloation Utilities ------------------
-----------------------------------------------------------------------
procedure Free is
   new Ada.Unchecked_Deallocation (Computations_Table,Computations_Table_Ref);

-----------------------------------------------------------------------
    ---------------   Display Utilities  ------------------
-----------------------------------------------------------------------

function NickName (Cref: Computation_Step;
                  Prefix: String := "X") return String;

-----------------------------------------------------------------------
      ---------  UCTL Computation management utilities ----------
------------------------------------------------------------------------
 -- Looks if a similar computation has already been included
 --    in the DB.
 -- It is already there, returns its identifying nicknum as a
 --    positive number.
 -- If it is not already there it adds the item
 --    into the DB, returning its identifying nicknum as a negative number
 --
  procedure Check_Computation (Form: Formula_Ref;
                            Context: Computations_Table;
                            Owner: System_Configuration;
                            State: System_Configuration;
                            Cref:  out Computation_Step;
                            Status: out Computation_Status;
                            Rec_Depth: in out Natural);
  --
  procedure Add_Subcomputation (Cref: Computation_Step;
                                SubCref: Computation_Step;
                                with_Evolution: Evolution_Data  := No_Evolution);
  --
  procedure Set_Subcomputation (Cref: Computation_Step;
                                SubCref: Computation_Step;
                                with_Evolution: Evolution_Data  := No_Evolution);
  --
  procedure Set_Status (Cref: Computation_Step;
                         Status: Computation_Status;
                           Rec_Depth: Natural :=0);
  --
  function  Get_Status (Cref: Computation_Step) return Computation_Status;
  function  Get_Subcomputations (Cref: Computation_Step)
          return Computations_Table;
  function  Get_Formula (Cref: Computation_Step) return Formula_Ref;
  function  Get_State (Cref: Computation_Step) return System_Configuration;
  function  Get_SubEvolutions (Cref: Computation_Step) return Evolutions_Table;
  function  Get_Context (Cref: Computation_Step) return Computations_Table;
  function  Get_Owner (Cref: Computation_Step) return System_Configuration;
  --
  procedure Start_Exploration;
  function  Already_Explored (Cref: Computation_Step) return Boolean;
  --
  function ComputationsSpace_Size return Natural;
  --
  procedure Dump_Computations;
  --
  function Get_State_Computations(State: System_Configuration) return Computations_Table;
  --
  procedure Initialize_DB;

end Computations_DB;
use Computations_DB;
--######################################################################################

--#####################################
  --
  --   HTML_Mode: Boolean:= False;  -- DEFINED in FLAGS.ADS
  --
  --  initialized by EvaluateIt here,  
  --    used by uctl_logic.Eval_Predicate   (PRINT  formula) (BUT NOW BUGGED!!!!)
  -- will contain the HTML description of the evaluation results
  HTML_File: File_Type;

  procedure Print_COUNT_Info is
  begin
      if UCTL_Types.Formula_contains_COUNT then
        Put_Line ("**** Requested_Counts:" & Integer'Image(UCTL_Types.Requested_Counts) & " ****");
      end if;
--
-- BUG BUG BUG HTML IS NOT OPEN/CREATED
--      if HTML_Mode then
--        Put_Line (HTML_File, "**** Requested_Counts:" & Integer'Image(UCTL_Types.Requested_Counts) & " ****");
--      end if;
-- BUG BUG BUG HTML IS NOT OPEN/CREATED
  end Print_COUNT_Info;

  procedure Print_DEPTH (Str: String; Depth:Integer) is
  begin
      Put_Line ("**** Requested_Depth:" & Integer'Image(Depth) & " (" & Str & ") ****");
  end;
  function DefaultString return String is begin return ""; end DefaultString;
  function StringKey (S: String;
           Cache_Max: Natural := Key_Module) return Positive is 
  begin 
     return Natural(abs(Ada.Strings.Hash(S) mod 1000)) +1; 
  end;

  package Messages_DB is new NickNames(String,DefaultString,StringKey, "=");

  procedure Print_Message(This_Conf: System_Configuration; Optargs: String_Table_Ref) is
    Thislabels: String_Tables_Vector := Get_Abstract_State_Labels(This_Conf);
  begin
    if HTML_Mode then
      if Optargs = null then
        Put(HTML_File, "<b>MESSAGE</b> from state " & 
             "<A HREF='javascript:top.sendcommand(""" & Configurations.NickName(This_Conf,"C")  &
             """)'>" & Configurations.NickName(This_Conf,"C") & "</A>");
        Put(HTML_File, " [");
        for J in Thislabels'Range loop
            Put(HTML_File, Thislabels(J)(1).all);
            if Thislabels(J).all'Length >1 then
              Put(HTML_File,"(");
              for K in 2..Thislabels(J).all'Last loop
                 Put(HTML_File, Thislabels(J)(K).all);
                if K /= Thislabels(J).all'Last then  Put(HTML_File, ","); end if;
              end loop;
              Put(HTML_File,")");
            end if;
          if J /= Thislabels'Last then Put(HTML_File, ","); end if;
        end loop;
        Put_Line(HTML_File, "]:<br>&nbsp;&nbsp;&nbsp<i> PRINT</i><br>");
      else
        Put_Line(HTML_File, "<b>MESSAGE</b> from state " & 
             "<A HREF='javascript:top.sendcommand(""" & Configurations.NickName(This_Conf,"C")  &
             """)'>" & Configurations.NickName(This_Conf,"C") & "</A>");
        Put(HTML_File, " [");
        for J in Thislabels'Range loop
            Put(HTML_File, Thislabels(J)(1).all);
            if Thislabels(J).all'Length >1 then
              Put(HTML_File,"(");
              for K in 2..Thislabels(J).all'Last loop
                 Put(HTML_File, Thislabels(J)(K).all);
                if K /= Thislabels(J).all'Last then  Put(HTML_File, ","); end if;
              end loop;
              Put(HTML_File,")");
            end if;
          if J /= Thislabels'Last then Put(HTML_File, ","); end if;
        end loop;
        Put(HTML_File, "]:<br>&nbsp;&nbsp;&nbsp<i> PRINT(");
        for I in Optargs.all'Range loop
          Put(HTML_File, Optargs(I).all);
          if I /= Optargs.all'Last then
            Put(HTML_File, ",");
          end if;
        end loop;
        Put_Line(HTML_File, ")</i><br>");
      end if;
    --
    else  -- TXT
      if Optargs = null then
        Put("MESSAGE from state " & Configurations.NickName(This_Conf,"C") & " [");
        for J in Thislabels'Range loop
            Put(Thislabels(J)(1).all);
            if Thislabels(J).all'Length >1 then
              Put("(");
              for K in 2..Thislabels(J).all'Last loop
                 Put(Thislabels(J)(K).all);
                if K /= Thislabels(J).all'Last then  Put(","); end if;
              end loop;
              Put(")");
            end if;
          if J /= Thislabels'Last then Put(","); end if;
        end loop;
        Put_line("]: PRINT");
      else
        Put("MESSAGE from state " & Configurations.NickName(This_Conf,"C") & "[");
        for J in Thislabels'Range loop
            Put(Thislabels(J)(1).all);
            if Thislabels(J).all'Length >1 then
              Put("(");
              for K in 2..Thislabels(J).all'Last loop
                 Put(Thislabels(J)(K).all);
                if K /= Thislabels(J).all'Last then  Put(","); end if;
              end loop;
              Put(")");
            end if;
          if J /= Thislabels'Last then Put(","); end if;
        end loop;
        Put(": PRINT(");
        for I in Optargs.all'Range loop
          Put(Optargs(I).all);
          if I /= Optargs.all'Last then
            Put(",");
          end if;
        end loop;
        Put_Line(")");
      end if;
    end if;
  end Print_Message;
  
  function Build (Optargs: String_Table) return String is 
  begin
     if Optargs'Length=0 then 
       return "";
     else
       return Optargs(Optargs'First).all & Build (Optargs(Optargs'First+1 .. Optargs'Last));
     end if;
  end;
  
  procedure PrintONCE_Message(This_Conf: System_Configuration; Optargs: String_Table) is
     Print_Data: String_Table(1..Optargs'Length);
     Print_Count: Natural := 0;
     X,N: Integer;
  begin
     N := Optargs'First-1;
     --
     while N < Optargs'Last loop
       N:= N+1;
    --if Optargs(N) /= null and then
    --     Optargs(N).all = "#" then
    --    N := N+1;   
    --    Print_Count := Print_Count+1;
    --    Print_Data(Print_Count) := new String'(Get_Configuration_Data(This_Conf, Optargs(N).all)); 
    -- else 
          Print_Count := Print_Count+1;
          if Optargs(N)/= null and then
             Optargs(N).all'Length >= 2 and then
             Optargs(N)(Optargs(N).all'First) ='"' and then
             Optargs(N)(Optargs(N).all'Last) ='"' then
            Print_Data(Print_Count) := 
              new String'(Optargs(N).all(Optargs(N).all'First+1..Optargs(N).all'Last-1));
          else
            Print_Data(Print_Count) := Optargs(N);
          end if;
       --end if; 
     end loop;
     --
     declare
       MSG: String  := Build(Print_Data(1..Print_Count));
  begin
    X := Messages_DB.CheckNum(MSG);
    if X < 0 then
      Put_Line("### [" & MSG & "]  (" & Configurations.NickName(This_Conf,"C") & ")" );
    end if;
    if HTML_Mode then
      Put_Line(HTML_File, 
        "### [" & MSG & "]  (" & Configurations.NickName(This_Conf,"C") & ")<br>");
    end if;
    end;
  end PrintONCE_Message;
--  procedure PrintONCE_Message(This_Conf: System_Configuration; Optargs: String_Table) is
--    -- build string
--    MSG: String := Build(Optargs);
--    -- check string
--    X: Integer; 
--  begin
--    -- posibily print message
--    if Optargs'Length =0 then return; end if;
--    if Optargs(Optargs'First)(1) /= 'a' then 
--      X := Messages_DB.CheckNum(MSG);
--      if X < 0 then
--        if HTML_Mode then
--          Put_Line("### [" & MSG & "]  (" & Configurations.NickName(This_Conf,"C") & ")" );
--        else
--          Put_Line("### [" & MSG & "]  (" & Configurations.NickName(This_Conf,"C") & ")" );
--        end if;
--      end if;
--    else
--      -- PRINT_ONCE(a163, ...);
--      declare
--        str: String := Get_Configuration_Data(This_Conf, Optargs(Optargs'First).all);
--        MSG: String :=str & Build(Optargs(Optargs'First+1..Optargs'LAst));
--      begin
--        X := Messages_DB.CheckNum(MSG);
--        if X < 0 then
--           Put_Line("### [" & MSG & "]");
--        end if;
--      end;
--    end if;
--  end PrintONCE_Message;
--#####################################


--######################################################################################
package UCTL_Logics is
  -- usa UCTL.HTML_File durante Eval 
  use Computations_DB;
  use Configurations;
  use UCTL_Types;
  --
  function Eval (Formula: UCTL_Types.Formula_Ref;
                  State: System_Configuration) return Computation_Status;
  --
--  procedure Print_Info;
end UCTL_Logics;
--######################################################################################
use UCTL_Logics;



package body UCTL_Parser is separate;
package body UCTL_Logics is separate;
package body UCTL_Utilities is separate;
package body Computations_DB is separate;

use UCTL_Types;

 function HTML_Format (Source:String) return String is
   Result:String(1..10000);
   OUTC: Natural := 0;
   I: Natural :=1;
 begin
   for J in Source'Range loop
     if I not in Source'Range then exit; end if;
     if Source(I)='<' then
        Result(OUTC+1..OUTC+4) := "&lt;";
        OUTC := OUTC+4;
        I := I+1;
     elsif Source(I)='>' then
        Result(OUTC+1..OUTC+4) := "&gt;";
        OUTC := OUTC+4;
        I := I+1;
     else
        Result(OUTC+1) := Source(I);
        OUTC := OUTC+1;
        I := I+1;
     end if;
   end loop;
   return Result (1..OUTC);
 end HTML_Format;


  function Check_VactlBoxTrue  (F:Formula_Ref) return Boolean;
  function Check_VactlBoxFalse  (F:Formula_Ref) return Boolean;
  function Check_VactlBoxLiveTrue  (F:Formula_Ref) return Boolean;

  ----------------------------------------------------------------------
  ----------------------------------------------------------------------
  procedure HTML_EvaluateIt (Elog_FileName: String;  -- "evalog.html"
                             Out_FileName: String;   -- "evalout.html"
                             Eval_OK: out Boolean) is
    ELog_File: File_Type;
    Result: Computation_Status := Aborted;
    The_Formula: Formula_Ref;
    Current_Conf: System_Configuration;
--    HTML_File: File_Type;
  begin
    All_Results := new Results_Table(All_Formulas'Range);
       -- ResultValue: Computation_Status := NOT_YET_STARTED;
       -- EvalTime: Duration := 0.0;
    --
    Eval_OK := True;
    
    The_Formula := UCTL_Types.This_Formula;
    Current_Conf := Initial_Configuration;
    --
    --  We want to include here all the current logging. But that requires a concurrent
    --  access to the external evalog.html file, and the semantics of that is implementation
    --  dependent. The dafult GNAT semantics is the raising of USe_Error.
    -- We can achieve that either from inside a different process, as the evalcheck cgi script.
    -- OR WE CAN DO THAT USING GNAT DEPENDENT FORM STRIBN  "shared=no" (which adopts the same
    -- semantics of multple independent C steams linked to the same object).
    --
    Create(ELOG_File, Out_File, Elog_FileName,"shared=no");
    -- "evalog.html" contains RUNTIME  UCTL OUTPUTS AND ERRORS
    Create(HTML_File, Out_File, Out_FileName);
    -- "evalout.html" contains the the final evaluation results
    Put_Line(HTML_File, "<html><body>");
    Put_line(HTML_File, "<P>");
    -----
    begin
      Set_Output(ELOG_File);
      Set_Error(ELOG_File);
      --
      for K in All_Formulas.all'Range loop
        Subsets(1) := All_Subsets(K)(1);  --   All_Subsets set by Parse_Formula
        Subsets(2) := All_Subsets(K)(2);
        Subsets(3) := All_Subsets(K)(3);
        UCTL_TYPES.formula_contains_print := All_Prints(K);
        All_Results(K).ResultValue := NOT_YET_STARTED;
        All_Results(K).EvalTime := 0.0;
        T1 := Ada.Calendar.Clock; 
        -----------------------------
        Live_Model := True;  -- set to False as soon as a node with no must transitions is found)
        ----------------------------
        The_Formula:= All_Formulas(K);
        Result := Eval(The_Formula, Initial_Configuration);
        ----------------------------
        All_Results(K).ResultValue := Result;
        T2 := Ada.Calendar.Clock;
        All_Results(K).EvalTime := T2-T1;
        All_Subsets(K)(1) := Subsets(1);
        All_Subsets(K)(2) := Subsets(2);
        All_Subsets(K)(3) := Subsets(3);
        All_Subsets(K)(4) := Live_Model;
      end loop;
      --
      Set_Output(Standard_Output);
      Set_Error(Standard_Error);
      Close(ELOG_File);
    exception
    when Event: others =>
      Set_Output(Standard_Output);
      Set_Error(Standard_Error);
      Put_line (ELOG_File," Sorry! An unexpected error occurred during the evaluation");
      Put_Line (ELOG_File,Exception_Information(Event));
      New_Line (ELOG_File);
      if Is_Open(ELOG_File) then Close(ELOG_File); end if;
      --if Is_Open(HTML_File) then Close(HTML_File); end if;
      Eval_OK := False;
      return;
    end;
    --

    for X in All_Formulas.all'Range loop
      --
      Put_line(HTML_File, "<P>");
    --  COPY UCTL OUTPUT  (PRINT & COUNT COMMANDS) FROM ELOG_FILE TO HTML_FILE
--    declare
--       ELOG: File_Type;
--       Line: String(1..100);
--       Count: Natural :=0;
--    begin
--      -- HTMLprinting now anticipated inside Print_Message
--       Open(ELOG, In_File,Elog_FileName);
--       while not End_of_File(ELOG) loop
--          Get_Line(ELOG,Line,Count);
--          if Count >8 and then
--             Line(1..8) = "MESSAGE " then
--             Put_Line(HTML_File,Line(1..Count) & "<br>");
--          end if;
--       end loop;
--       Close(ELOG);
--    end;

      Elapsed := (Others => ' ');
      MyIO.Put(Elapsed,All_Results(X).EvalTime,3);
      Result := All_Results(X).ResultValue;
      The_Formula:= All_Formulas(X);
      UCTL_TYPES.formula_contains_print := All_Prints(X);
      Subsets(1) := All_Subsets(X)(1);
      Subsets(2) := All_Subsets(X)(2);
      Subsets(3) := All_Subsets(X)(3);
      Live_Model := All_Subsets(X)(4);
      --
    if Result=Found_True or Result=TMP_TRUE then
      Put_Line(HTML_File, "<hr><b>The Formula:</b> " & 
                        "<span style=""background-color:lightgreen;width=100%""><em> " &
                          HTML_Format(The_Formula.Fimage.all) &
                          "</em></span><br>" & "<b> is TRUE</b><br>");

      if not UCTL_Types.Formula_contains_PRINT then
        if product_families and then Subsets(1) then  
         --VACT_BOX_TRUE
         Put_Line(HTML_File, "<table border='5'  bordercolor='lightgreen'>");
         Put_Line(HTML_File, "<tr><td>The formula holds for ALL the valid MTS products<td></tr>");
         Put_Line(HTML_File, "</table>");
         --
        elsif product_families and then Subsets(3) and then Live_Model then  
         --VACT_BOXLIVE_TRUE
         Put_Line(HTML_File, "<table border='5'  bordercolor='lightgreen'>");
         Put_Line(HTML_File, "<tr><td>The formula holds for ALL the valid MTS products<td></tr>");
         Put_Line(HTML_File, "</table>");
         -- 
        elsif product_families then
         Put_Line(HTML_File, "<table border='5'  bordercolor='gray'>");
         Put_Line(HTML_File, "<tr><td>Even if the formula is TRUE for the MTS, its validity<br>");
         Put_Line(HTML_File, "is not necessarily preserved by the MTS products<td></tr>");
         Put_Line(HTML_File, "</table>");
         --
        end if;
      end if;
      --
    elsif Result=Found_False then
      Put_Line(HTML_File, "<hr><b>The Formula:</b> " &   
                          "<span style=""background-color:pink;width=100%""><em> " &
                          HTML_Format(The_Formula.Fimage.all) &
                          "</em></span><br>" & "<b>is FALSE</b><p>");
      if not UCTL_Types.Formula_contains_PRINT then
        if product_families and then Subsets(2) then  
         --VACT_BOX_FALSE
         Put_Line(HTML_File, "<table border='5'  bordercolor='pink'>");
         Put_Line(HTML_File, "<tr><td>The formula DOES NOT hold for ANY valid MTS product<td></tr>");
         Put_Line(HTML_File, "</table>");
         --
        elsif product_families then
         Put_Line(HTML_File, "<table border='5'  bordercolor='gray'>");
         Put_Line(HTML_File, "<tr><td>Even if the formula is FALSE for the MTS,its validity<br>");
         Put_Line(HTML_File, "is not necessarily preserved by the MTS products<td></tr>");
         Put_Line(HTML_File, "</table>");
         --
        end if;
      end if;
      --
    elsif Static_Max_Depth and Result=Aborted then
      Put_Line(HTML_File, 
         "<hr><b>The validity of the Formula:</b> " &
         "<span style=""background-color:pink;width=100%""><em> " & 
         HTML_Format(The_Formula.Fimage.all) & "</em>/span><br>" & 
         "<b>cannot be established with the current Static_MAX_Depth</b> <p>");
      --
    else
      Put_Line(HTML_File, 
          "<hr><b>The Formula:</b> " &
                          "<span style=""background-color:pink;width=100%""><em> " & 
                          HTML_Format(The_Formula.Fimage.all) &
                          "</em>/span><br>" & "<b>is ??? (sorry! an error occurred)</b> <p>");
      Eval_OK := True;
    end if;
    --
    Put_Line(HTML_File, "(evaluation time= " & Ada.Strings.Fixed.Trim (Elapsed, Left) & " sec.)" );
    
--    Put_Line(HTML_File,"(states generated=" &
--       Int64'Image(StatesSpace_Size) &
--        ", computations fragments generated=" &
--        Int64'Image(Computations_DB.ComputationsSpace_Size) &
--        ", evaluation time= " & Ada.Strings.Fixed.Trim (Elapsed, Left) & " sec.)<p>" );
    --
    end loop;
    --
    Elapsed := (Others => ' ');
    dd :=0.0;
    for I in All_Results.all'Range loop
       dd := dd + All_Results(I).EvalTime;
    end loop;
    MyIO.Put(Elapsed,dd,3);
    Put_Line(HTML_File,"<hr>(total states generated=" &
       Int64'Image(StatesSpace_Size) &
        ", computations fragments generated=" &
        Int64'Image(Computations_DB.ComputationsSpace_Size) &
        ", total evaluation time= " & Ada.Strings.Fixed.Trim (Elapsed, Left) & " sec.)<p>" );
    Put_Line (HTML_File, "</body></html>");
    Eval_OK := True;
    if Is_Open(ELOG_File) then Close(ELOG_File); end if;
    if Is_Open(HTML_File) then Close(HTML_File); end if;
    --
  exception
    when Event: others =>
      if Is_Open(HTML_File) then Close(HTML_File); end if;
      Put_line (Current_Error," Error in HTML_EvaluateIt !");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
      Eval_OK := False;
  end HTML_EvaluateIt;



  procedure EvaluateIt (Eval_OK: out Boolean) is
    Result: Computation_Status := Aborted;
    The_Formula: Formula_ref;
    Current_Conf: System_Configuration;
  begin
    Messages_DB.Reinitialize_DB;
    Eval_OK := True;
    The_Formula := UCTL_Types.This_Formula;
    Current_Conf := Initial_Configuration;
    --
    begin
      T1 := Ada.Calendar.Clock;
      --------------------------------------------------------------------
      Live_Model := True;
      --------------------------------------------------------------------
      Result := Eval(The_Formula, Initial_Configuration);
      --------------------------------------------------------------------
      T2 := Ada.Calendar.Clock;
      dd := T2-T1;
      MyIO.Put(Elapsed,dd,3);
    exception
    when Event: others =>
      Put_line (Current_Error," Sorry! An unexpected error occurred during the evaluation");
      Put_Line (Current_Error,Exception_Information(Event));
      Eval_OK := False;
      return;
    end;
    --
    if Flags.Debug then
       Configurations.Kernel.Print_StatesSpace_Stats;
    end if;
    if Result=Found_True then
      Put_Line("The Formula: """ & This_Formula.Fimage.all & """" );
      Put_Line ("is: TRUE ");
      if not UCTL_Types.Formula_contains_PRINT then
      if product_families and then Subsets(1) then  --VACT_BOX_TRUE
         Put_Line("*************************************************");
         Put_Line("** The formula holds also for all the valid MTS products **");
         Put_Line("*************************************************");
      elsif product_families
          and then Subsets(3) and then Live_Model then
         --VACT_BOXLIVE_TRUE
         Put_Line("*************************************************");
         Put_Line("** The formula holds also for all the valid MTS products **");
         Put_Line("*************************************************");
       elsif product_families then
         Put_Line("***********************************************************");
         Put_Line("** Even if the formula is TRUE for the MTS, its validity **");
         Put_Line("** is not necessarily preserved by the MTS products      **");
         Put_Line("***********************************************************");
      end if;
      end if;
    elsif Result=Found_False then
      Put_Line("The Formula: """ & This_Formula.Fimage.all & """" );
      Put_Line ("is: FALSE ");
      if not UCTL_Types.Formula_contains_PRINT then
      if product_families and then Subsets(2) then  --VACT_BOX_TRUE
         Put_Line("****************************************************");
         Put_Line("** The formula does NOT hold for any valid MTS product **");
         Put_Line("****************************************************");
      elsif product_families then
         Put_Line("************************************************************");
         Put_Line("** Even if the formula is FALSE for the MTS, its validity **");
         Put_Line("** is not necessarily preserved by the MTS products       **");
         Put_Line("************************************************************");
      end if;
      end if;
    elsif Static_Max_Depth and Result=Aborted then
      Put_Line ("The validity of the Formula: """ & This_Formula.Fimage.all & """" );
      Put_Line ("  cannot be established with the current Static_MAX_Depth ");
    else
      Put_Line ("The Formula: """ & This_Formula.Fimage.all & """" );
      Put_Line ("is: ??? (sorry! an error occurred) ");
      Eval_OK := False;
      return;
    end if;
    Put_Line ("(states generated=" &
        Natural'Image(StatesSpace_Size)  &
        ", computations fragments generated=" &
        Natural'Image(UCTL.FragmentsCount)  &
        -- Natural'Image(Computations_DB.ComputationsSpace_Size)  &
        ", evaluation time= " & Ada.Strings.Fixed.Trim (Elapsed, Left) & " sec.)" );
     Put_line ("--------------------------------------------------");
    Eval_OK := True;
  exception
    when Event: others =>
      Put_line (Current_Error," Error in EvaluateIt !");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
      Eval_OK := False;
  end EvaluateIt;


 ---------------------------------------------------------
  --  used in:
  -- onmouseover="Tip(" & HTML_Literal_Hack(text) & ")"
  --   text like p.o<a1,a2>   should become:
  --       'p.o&'+'lt;a1,a2&gt;'
  --   because we should escape the '<' and '>'
   --  and prevent "&lt;" to be interpreted back as '<'
  -- (is this a browser or tooltio lib problem?)
 ---------------------------------------------------------
 function HTML_Literal_Hack (Source:String) return String is
   Result:String(1..10000);
   OUTC: Natural := 1;
 begin
   Result(1..1) := "'";
   OUTC :=2;
   for I in Source'Range loop
     if Source(I)='<' then
        Result(OUTC..OUTC+6) := "&'+'lt;";
        OUTC := OUTC+7;
     elsif Source(I)='>' then
        Result(OUTC..OUTC+3) := "&gt;";
        OUTC := OUTC+4;
     else
        Result(OUTC) := Source(I);
        OUTC := OUTC+1;
     end if;
   end loop;
   Result(OUTC..OUTC) := "'";
   return Result (1..OUTC);
 end HTML_Literal_Hack;


  -----------------------------------------------------------------
  -- Rec Path e' la lista di computazioni di punto fisso annidate che fanno parte della
  -- explanation di una formula.
  -- Viene utilizzata quando viene trovata una formula di tipo fapply, per cui bisogna capire
  -- se essa chiude un ciclo oppure no.
  -- (nota, essa puo' chiudere un ciclo anche il valore e' FOUND__XXX, anzi in teoria al momento
  -- di dare la explanation tutte le subcomputations dovrebbero avere valore FOUND_XXX e non TMP_XXX)
  ---------------------------------------------------------------
  procedure Text_Explain_Computation (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type);


  procedure Explain_EX_False (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    if This_Subcomputations'Length = 0 then
      if HTML_Mode then
        -- HTML MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            Put_Line (HTML_File, "<b>This happens because there is no evolution <br>");
            Put_Line (HTML_File, "starting from State" &
                                  Configurations.NickName(This_State,"C") &
                                 "<br> (The state is final)</b>");
          else
            Put_Line (HTML_File, "<b>This happens because </b><br>");
            while Has_System_Transition (My_Iter) loop
              Put_Line (HTML_File,
                  "&nbsp;&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
                  " --&gt; " &
                  Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                  "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter)  & "<br>");
              Iterator_Advance(My_Iter);
            end loop;
            Put_Line (HTML_File, "<b> and no evolutions satisfy the action expression </b>&nbsp;&nbsp;<em>");
            Set_Output(HTML_File);
            UCTL_Utilities.Print_Action(Form.Pref.AAref);
            Set_Output(Standard_Output);
            Put_Line (HTML_File, "</em><br>");
          end if;
        end;
      else
        --  TEXT MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            Put_Line ("This happens because there is no evolution ");
            Put ("starting from State ");
            Put (Configurations.NickName(This_State,"C"));
            New_Line;
            Put ("(The state is final)");
            New_Line;
          else
            Put_Line ("This happens because ");
            while Has_System_Transition (My_Iter) loop
              Put ("  " & Configurations.NickName(This_State,"C") &
                   " --> " &
                  Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                  "  " & Abstract_Action_Labels(My_Iter) );
              New_Line;
              Iterator_Advance(My_Iter);
            end loop;
            Put (" and no evolutions satisfy the action expression """);
            UCTL_Utilities.Print_Action(Form.Pref.AAref);
            Put("""");
            New_Line;
          end if;
          Iterator_Finalize (My_Iter);
        end;
      end if;
    else -- subcomputations'Length >0
      -- i.e. not Final
      if HTML_Mode then
        -- HTML MODE
        declare
          My_Iter: Evolutions_Iterator;
          Transitions_Count: Natural := 0;
        begin
          Put_Line (HTML_File, "<b>This happens because</b><br> ");
          Iterator_Initialize (My_Iter, This_State);
          while Has_System_Transition (My_Iter) loop
            Transitions_Count := Transitions_Count+1;
            Put_Line (HTML_File, 
                "&nbsp;&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
                 " --&gt; " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter) & " <br>");
             Iterator_Advance(My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
          if Transitions_Count > This_SubComputations'Length then
            Put_Line (HTML_File, "<b> And all these evolutions either do not satisfy the action expression</b><em>");
            Set_Output(HTML_File);
            UCTL_Utilities.Print_Action(Form.Pref.AAref);
            Set_Output(Standard_Output);
            Put_Line(HTML_File, "</em><br>");
            Put_Line (HTML_File, "<b> or their target state does NOT satisfy the subformula:</b><br>");
            Put_Line (HTML_File, 
                "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.AFormRef.Fimage.all) & "</em><br>");
          else
            Put_Line (HTML_File, 
              "<b> And none of the successor states satisfy the subformula:</b><br>");
            Put_Line (HTML_File, 
              "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.AFormRef.Fimage.all) & "</em><br>");
          end if;
        end;
        if This_Subcomputations'Length>0 and then
            (Get_Formula(This_Subcomputations(1)).Fimage.all /= "false" or else
             (Get_Formula(This_Subcomputations(1)).Fimage.all'Length <10 or else
              Get_Formula(This_Subcomputations(1)).Fimage.all(1..10) /= "PRINT_ONCE")) then
        Put_Line (HTML_File, "<br>In particular: <br>");
          for J in This_Subcomputations'Range loop
            Put_Line (HTML_File, "<b> In state </b>" &
            Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
             "<b> the subformula:</b><br>");
            Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp;" &
            "<em>" & HTML_Format(Get_Formula(This_SubComputations(J)).Fimage.all) & "</em>");
            Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(This_Subcomputations(J)) & "');""" &
             ">Does NOT Hold</A>" );
            Put_Line (HTML_File, ".</b><br>");
          end loop;
        end if;
      else
        -- TEXT MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Put_Line ("This happens because ");
          Iterator_Initialize (My_Iter, This_State);
          while Has_System_Transition (My_Iter) loop
            Put (Configurations.NickName(This_State,"C") &
                 " --> " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "  " & Abstract_Action_Labels(My_Iter) );
              New_Line;
             Iterator_Advance(My_Iter);
          end loop;
          Put (" And all these evolutions either do not satisfy the action expression """);
          UCTL_Utilities.Print_Action(Form.Pref.AAref);
          Put("""");
          New_Line;
          Put (" or their target state does NOT satisfy the subformula:""");
          UCTL_Utilities.Print(Form.PRef.AFormRef, False);
          Put_Line("""");
          Iterator_Finalize (My_Iter);
        end;
        Put_Line ("In particular:");
        if This_Subcomputations'Length>0 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all = "false" then
            Put ("The subformula ""false"" Does NEVER Hold in any state.");
        elsif This_Subcomputations'Length>0 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all'Length >9 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all(1..10)= "PRINT_ONCE" then
            Put ("The subformula ""PRINT_ONCE"" Does NEVER Hold in any state.");
        else 
          for J in This_Subcomputations'Range loop
            Put ("In State " & Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
               " the subformula: " & Get_Formula(This_SubComputations(J)).Fimage.all );
            Put_Line ("  Does NOT Hold.");
          end loop;
        end if;
      end if;
    end if;  -- subcomputations'Length >0
    -- Iterates Inside
    for J in This_Subcomputations'Range loop 
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,This_SubComputations(J),CurDepth+1,Form,False,HTML_File);
    end loop;
  end Explain_EX_False;

  procedure Explain_AX_False (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
  begin
    if This_Subcomputations'Length = 0 then
      if HTML_Mode then
        -- HTML MODE
        Iterator_Initialize (My_Iter, This_State);
        if not Has_System_Transition (My_Iter)  then
          Put_Line (HTML_File, "<b>This happens because there is no evolution <br>");
          Put_Line (HTML_File, "starting from State &nbsp;&nbsp;" &
                                Configurations.NickName(This_State,"C") &
                               "<br> (The state is final)</b>");
        else
          Put_Line (HTML_File, 
               "<b>This happens because not all evolutions starting from state </b>" &
               "&nbsp;" & Configurations.NickName(This_State,"C") & "<br>" &
               "<b> satisfy the action expression </b>&nbsp;&nbsp;<em>");
          Set_Output(HTML_File);
          UCTL_Utilities.Print_Action(Form.Pref.AAref);
          Set_Output(Standard_Output);
          Put_Line (HTML_File, "</em><br>");
          Put_Line (HTML_File, "In particular: <br>");
          Put_Line (HTML_File,
                "&nbsp;&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
                " --&gt; " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter)  & "<br>");
          Put_Line (HTML_File, 
            "and this evolution label does not satisfy the action expression &nbsp;<em>");
          Set_Output(HTML_File);
          UCTL_Utilities.Print_Action(Form.Pref.AAref);
          Set_Output(Standard_Output);
          Put_Line (HTML_File, "</em><br>");
        end if;
        Iterator_Finalize (My_Iter);
      else
        --  TEXT MODE
        Iterator_Initialize (My_Iter, This_State);
        if not Has_System_Transition (My_Iter)  then
          Put_Line ("This happens because there is no evolution ");
          Put ("starting from State ");
          Put (Configurations.NickName(This_State,"C"));
          New_Line;
          Put ("(The state is final)");
          New_Line;
        else
          Put_Line ("This happens because not all evolutions starting from State " &
                                Configurations.NickName(This_State,"C"));
          Put ("satisfy the action expression """);
          UCTL_Utilities.Print_Action(Form.Pref.AAref);
          Put_Line("""");
          Put_Line("In Particlular:");
          Put ("  " & Configurations.NickName(This_State,"C") &
                 " --> " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "  " & Abstract_Action_Labels(My_Iter) );
          New_Line;
          Put (" and this evolution label does not satisfy the action expression """);
          UCTL_Utilities.Print_Action(Form.Pref.AAref);
          Put("""");
          New_Line;
        end if;
        Iterator_Finalize (My_Iter);
      end if;  -- TEXT/HTML_MODE
      --
    else -- subcomputations'Length >0
      -- i.e. not Final
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, "<b>This happens because</b><br> ");
        Put_Line (HTML_File,
           "&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
           " --&gt; " &
          Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
          "&nbsp;&nbsp;" & Abstract_Label_Image(This_Subevolutions(1)) & "<br>" );
        Put_Line (HTML_File, "<b> the transition label satisfies the action expression</b><em> ");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Pref.AAref);
        Set_Output(Standard_Output);
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Pref.AAref);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, "</em><br><b> BUT in State </b> " &
             Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
            " <b>the subformula:</b><br><em> " &
             HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
        Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:pink'>");
        Put_Line(HTML_File,
                     "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
                     " TITLE='Explain more ...'>Is Not Satisfied</b></A>" );
        Put_Line (HTML_File, "</spawn>.</b><br>");
      else
        -- TEXT MODE
        Put_Line ("This happens because ");
        Put (Configurations.NickName(This_State,"C") &
               " --> " &
              Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
              "  " & Abstract_Label_Image(This_Subevolutions(1)) );
        New_Line;
        Put ("the transition label satisfies the action expression ");
        UCTL_Utilities.Print_Action(Form.Pref.AAref);
        New_Line;
        Put ("BUT in State " & Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
           " the subformula: " & Get_Formula(This_SubComputations(1)).Fimage.all );
        Put_Line ("  Is Not Satisfied.");
      end if;    -- HTML/TEXT MODE
      -- Iterates Inside
      for J in This_Subcomputations'Range loop  -- just J=1
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(J),CurDepth+1,Form,False,HTML_File);
      end loop;
    end if;
  end Explain_AX_False;

  procedure Explain_AX_True (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    if HTML_Mode then
        -- HTML MODE
        declare
          My_Iter: Evolutions_Iterator;
          Transitions_Count: Natural := 0;
        begin
          Put_Line (HTML_File, "<b>This happens because</b><br> ");
          Iterator_Initialize (My_Iter, This_State);
          while Has_System_Transition (My_Iter) loop
            Transitions_Count := Transitions_Count+1;
            Put_Line (HTML_File,
                "&nbsp;&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
                 " --&gt; " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter) & " <br>");
             Iterator_Advance(My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
            Put_Line (HTML_File, "<b> And all these evolutions satisfy the action expression</b><em>");
            Set_Output(HTML_File);
            UCTL_Utilities.Print_Action(Form.Pref.AAref);
            Set_Output(Standard_Output);
            Put_Line(HTML_File, "</em><br>");
            Put_Line (HTML_File, "<b> and all the target states satisfy the subformula:</b><br>");
            Put_Line (HTML_File,
                "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.AFormRef.Fimage.all) & "</em><br>");
        end;
        Put_Line (HTML_File, "<br>In particular: <br>");
        for J in This_Subcomputations'Range loop
          Put_Line (HTML_File, "<b> In state </b>" &
          Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
             "<b> the subformula:</b><br>");
          Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp;" &
            "<em>" & HTML_Format(Get_Formula(This_SubComputations(J)).Fimage.all) & "</em>");
          Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(This_Subcomputations(J)) & "');""" &
             ">Is Satisfied</A>" );
          Put_Line (HTML_File, ".</b><br>");
        end loop;
      else
        -- TEXT MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Put_Line ("This happens because ");
          Iterator_Initialize (My_Iter, This_State);
          while Has_System_Transition (My_Iter) loop
            Put (Configurations.NickName(This_State,"C") &
                 " --> " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "  " & Abstract_Action_Labels(My_Iter) );
              New_Line;
             Iterator_Advance(My_Iter);
          end loop;
          Put (" And all these evolutions satisfy the action expression """);
          UCTL_Utilities.Print_Action(Form.Pref.AAref);
          Put("""");
          New_Line;
          Put_Line (" and all the target states satisfy the subformula:<br><em>");
          UCTL_Utilities.Print(Form.PRef.AFormRef, False);
          New_Line;
          Iterator_Finalize (My_Iter);
        end;
        Put_Line ("In particular:");
        for J in This_Subcomputations'Range loop
          Put ("In State " & Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
             " the subformula: " & Get_Formula(This_SubComputations(J)).Fimage.all );
          Put_Line ("  Is Satisfied.");
        end loop;
      end if;
    -- Iterates Inside
    for J in This_Subcomputations'Range loop
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,This_SubComputations(J),CurDepth+1,Form,False,HTML_File);
    end loop;
  end Explain_AX_True;

  procedure Explain_Angle_False (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    -----------
    -- CASE 1)  the state is FINAL, or no evolutions satisfy the action expression,
    --          (therefore there are no subcomputations)
    -----------
    if This_Subcomputations'Length = 0 then
      if HTML_Mode then
        -- HTML MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            Put_Line (HTML_File, "<b>This happens because there is no evolution <br>");
            Put_Line (HTML_File, "starting from State " &
                                 "<a href='javascript:top.sendcommand(""" & 
                                  Configurations.NickName(This_State,"C") & """)'>" &
                                  Configurations.NickName(This_State,"C") &"</a>" &
                                 "<br> (The state is final)</b>");
          else
            Put_Line (HTML_File, "<b>This happens because </b><br>");
            while Has_System_Transition (My_Iter) loop
              Put_Line (HTML_File,
                  "&nbsp;&nbsp;&nbsp;" & 
                  "<a href='javascript:top.sendcommand(""" & Configurations.NickName(This_State,"C") &
                  """)'>" & Configurations.NickName(This_State,"C") & "</a>" &
                  " --&gt; " &
                  Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                  "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter)  & "<br>");
              Iterator_Advance(My_Iter);
            end loop;
            Iterator_Finalize (My_Iter);
            Put_Line (HTML_File, 
               "<b> and no evolution from " & Configurations.NickName(This_State,"C") &
               " satisfies the action expression </b>&nbsp;&nbsp;<em>");
            Set_Output(HTML_File);
            UCTL_Utilities.Print_Action(Form.Aref);
            Set_Output(Standard_Output);
            Put_Line (HTML_File, "</em><br>");
          end if;
        end;
        return;
      else
        --  TEXT MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            Put_Line ("This happens because there is no evolution ");
            Put ("starting from State ");
            Put (Configurations.NickName(This_State,"C"));
            New_Line;
            Put ("(The state is final)");
            New_Line;
          else
            Put_Line ("This happens because ");
            while Has_System_Transition (My_Iter) loop
              Put ("  " & Configurations.NickName(This_State,"C") &
                   " --> " &
                  Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                  "  " & Abstract_Action_Labels(My_Iter) );
              New_Line;
              Iterator_Advance(My_Iter);
            end loop;
            Iterator_Finalize (My_Iter);
            Put (" and no evolution from " & Configurations.NickName(This_State,"C") &
                 " satisfies the action expression """);
            UCTL_Utilities.Print_Action(Form.Aref);
            Put("""");
            New_Line;
          end if;
        end;
      end if;
      return;
    end if;
  
    ----------------------------
    -- CASE 2)  State is not final and exist some transition satisfying the action expression
    -- subcomputations'Length >0
    ---------------------------
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, "<b>This happens because</b><br> ");
        Put_Line (HTML_File, 
           "<b> All the evolutions from " & Configurations.NickName(This_State,"C")  &
           " either do not satisfy the action expression</b><em>");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Aref);
        Set_Output(Standard_Output);
        Put_Line(HTML_File, "</em><br>");
        Put_Line (HTML_File, "<b> or their target state does NOT satisfy the subformula:</b><br>");
        Put_Line (HTML_File, 
                 "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.FormRef.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<br>In particular: <br>");
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          while Has_System_Transition (My_Iter) loop
            Put_Line (HTML_File,
                "&nbsp;&nbsp;&nbsp;" & 
                 "<a href='javascript:top.sendcommand(""" & Configurations.NickName(This_State,"C") &
                 """)'>" & Configurations.NickName(This_State,"C") & "</a>" &
                 " --&gt; &nbsp; " &
                 "<a href='javascript:top.sendcommand(""" & 
                   Configurations.NickName(Get_Target_Configuration(My_Iter),"C") & """)'>" &
                 Configurations.NickName(Get_Target_Configuration(My_Iter),"C") & "</a>" &
                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter) & " <br>");
             Iterator_Advance(My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
        end;
        --
        if This_Subcomputations'Length>0 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all = "false" then
            Put_Line (HTML_File," and the subformula ""false"" Does NEVER Hold in any state.<br>");
        elsif This_Subcomputations'Length>0 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all'Length >9 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all(1..10)= "PRINT_ONCE" then
            Put_Line (HTML_File," and the subformula ""PRINT_ONCE"" Does NEVER Hold in any state.<br>");
        else 
          for J in This_Subcomputations'Range loop
            --
            Put_Line (HTML_File, " <b>and in state " &
                    Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
                   " the subformula:</b><br>");
            Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp;" &
            "<em>" & HTML_Format(Get_Formula(This_SubComputations(J)).Fimage.all) & "</em>");
            Put_Line(HTML_File,
             "&nbsp;&nbsp;&nbsp;<A HREF=""javascript:show('" & NickName(This_Subcomputations(J)) & "');""" &
             "TITLE='Explain more ...'>Does NOT Hold</A>" );
            Put_Line (HTML_File, ".<br>");
          end loop;
        end if;
      else
        -- TEXT MODE
        Put_Line ("This happens because ");
        Put (" All the evolutions from "  & Configurations.NickName(This_State,"C") &
              " either do not satisfy the action expression """);
        UCTL_Utilities.Print_Action(Form.Aref);
        Put("""");
        New_Line;
        Put_Line (" or their target state does NOT satisfy the subformula:<br><em>");
        UCTL_Utilities.Print(Form.FormRef, False);
        New_Line;
        Put_Line ("In particular:");
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          while Has_System_Transition (My_Iter) loop
            Put (Configurations.NickName(This_State,"C") &
                 " --> " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "  " & Abstract_Action_Labels(My_Iter) );
              New_Line;
              
             Iterator_Advance(My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
        end;
        if This_Subcomputations'Length>0 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all = "false" then
            Put_Line (" and the subformula ""false"" Does NEVER Hold in any state.");
        elsif This_Subcomputations'Length>0 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all'Length >9 and then
            Get_Formula(This_Subcomputations(1)).Fimage.all(1..10)= "PRINT_ONCE" then
            Put_Line (" and the subformula ""PRINT_ONCE"" Does NEVER Hold in any state.");
        else 
          for J in This_Subcomputations'Range loop
            Put (" and in state " & Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
             " the subformula: " & Get_Formula(This_SubComputations(J)).Fimage.all );
            Put_Line ("  Does NOT Hold.");
        end loop;
        end if;
      end if;
    
    -- Iterates Inside
    for J in This_Subcomputations'Range loop
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,This_SubComputations(J),CurDepth+1,Form,False,HTML_File);
    end loop;
  end Explain_Angle_False;


  procedure Explain_EX_True (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_Context: Computations_Table := Get_Context(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    if HTML_Mode then
      -- HTML MODE
      Put_Line (HTML_File, "<b>This happens because</b><br> ");
      Put_Line (HTML_File, 
             "&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
             " --&gt; " &
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
            "&nbsp;&nbsp;" & Abstract_Label_Image(This_Subevolutions(1)) & "<br>" );
      Put_Line (HTML_File, "<b> the transition label satisfies the action expression</b><em> ");
      Set_Output(HTML_File);
      UCTL_Utilities.Print_Action(Form.Pref.AAref);
      Set_Output(Standard_Output);
      Put_Line (HTML_File, "</em><br><b> and in State </b> " &
           Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
          " <b>the subformula:</b><br><em> " & 
           HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
      Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:lightgreen'>");
      Put_Line(HTML_File,
                   "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
                   " TITLE='Explain more ...'>Is Satisfied</b></A>" );
      Put_Line (HTML_File, "</spawn>.</b><br>");
    else
      -- TEXT MODE
      Put_Line ("This happens because ");
      Put (Configurations.NickName(This_State,"C") &
             " --> " &
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
            "  " & Abstract_Label_Image(This_Subevolutions(1)) );
      New_Line;
      Put ("the transition label satisfies the action expression ");
      UCTL_Utilities.Print_Action(Form.Pref.AAref);
      New_Line;
      Put ("and in State " & Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
         " the subformula: " & Get_Formula(This_SubComputations(1)).Fimage.all );
      Put_Line ("  Is Satisfied.");
    end if;
    -- Iterates Inside
    if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
    Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
  end Explain_EX_True;

  procedure Explain_Angle_True (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_Context: Computations_Table := Get_Context(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    if HTML_Mode then
      -- HTML MODE
      Put_Line (HTML_File, "<b>This happens because</b><br> ");
      Put_Line (HTML_File,
             "&nbsp;&nbsp;"  &  "<a href='javascript:top.sendcommand(""" &
             Configurations.NickName(This_State,"C") & """)'>" &
             Configurations.NickName(This_State,"C") &  "</a>" &
             " --&gt; " &
             "<a href='javascript:top.sendcommand(""" &     
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") & """)'>" & 
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") & "</a>" &
            "&nbsp;&nbsp;" & Abstract_Label_Image(This_Subevolutions(1)) & "<br>" );
      Put_Line (HTML_File, "<b> the transition label satisfies the action expression</b><em> ");
      Set_Output(HTML_File);
      UCTL_Utilities.Print_Action(Form.Aref);
      Set_Output(Standard_Output);
      Put_Line (HTML_File, "</em><br><b> and in State </b> " &
           Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
          " <b>the subformula:</b><br><em> " &
             HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
      Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:lightgreen'>");
      Put_Line(HTML_File,
                   "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
                   " TITLE='Explain more ...'>Is Satisfied</b></A>" );
      Put_Line (HTML_File, "</spawn>.</b><br>");
    else
      -- TEXT MODE
      Put_Line ("This happens because ");
      Put (Configurations.NickName(This_State,"C") &
             " --> " &
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
            "  " & Abstract_Label_Image(This_Subevolutions(1)) );
      New_Line;
      Put ("the transition label satisfies the action expression ");
      UCTL_Utilities.Print_Action(Form.Aref);
      New_Line;
      Put ("and in State " & Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
         " the subformula: " & Get_Formula(This_SubComputations(1)).Fimage.all );
      Put_Line ("  Is Satisfied.");
    end if;
    -- Iterates Inside
    if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
    Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
  end Explain_Angle_True;

  procedure Explain_Square_False (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref;
                        Skip_Header: Boolean := False;
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_Context: Computations_Table := Get_Context(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    -----------------------------
    -- [act]  FFF  is  False  when
    --  at least ONE   evolution satisfies "act" but  the target state 
    --  does not satisfy "FFF"  
    --  The explanation Structure is exaclty that of EX {act} FFF  being TRUE
    ------------------------------
    if HTML_Mode then
      -- HTML MODE
      Put_Line (HTML_File, "<b>This happens because</b><br> ");
      Put_Line (HTML_File,
             "&nbsp;&nbsp;" &  "<a href='javascript:top.sendcommand(""" & 
             Configurations.NickName(This_State,"C") & """)'>" & 
             Configurations.NickName(This_State,"C") & "</a>" &
             " --&gt; " & "<a href='javascript:top.sendcommand(""" & 
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") & """)'>" & 
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") & "</a>" &
            "&nbsp;&nbsp;" & Abstract_Label_Image(This_Subevolutions(1)) & "<br>" );
      Put_Line (HTML_File, "<b> the transition label satisfies the action expression</b><em> ");
      Set_Output(HTML_File);
      UCTL_Utilities.Print_Action(Form.Aref);
      Set_Output(Standard_Output);
      Put_Line (HTML_File, "</em><br><b> but in State </b> " &
           Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
          " <b>the subformula:</b><br><em> " &
           HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
      Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:pink'>");
      Put_Line(HTML_File,
                   "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
                   " TITLE='Explain more ...'>Is Not Satisfied</A>" );
      Put_Line (HTML_File, "</spawn>.</b><br>");
    else
      -- TEXT MODE
      Put_Line ("This happens because ");
        Put (Configurations.NickName(This_State,"C") &
             " --> " &
            Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
            "  " & Abstract_Label_Image(This_Subevolutions(1)) );
          New_Line;
      Put("the transition label satisfies the action expression</b><em> ");
      UCTL_Utilities.Print_Action(Form.Aref);
      Put ("but in State " & Configurations.NickName(Get_State(This_SubComputations(1)),"C") &
         " the subformula: " & Get_Formula(This_SubComputations(1)).Fimage.all );
      Put_Line ("  Is Not Satisfied.");
    end if;
    --
    -- Iterates Inside
    if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
    Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
  end Explain_Square_False;


  procedure Explain_Square_True (RecPath:Computations_Table;
                        Cref: Computation_Step;
                        CurDepth:Positive;
                        Previous: Formula_Ref; 
                        Skip_Header: Boolean := False; 
                        HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref); 
    This_Context: Computations_Table := Get_Context(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    -----------------------------
    -- [act]  FFF  is  True  when
    --  1) The current state is FINAL
    --  or 2) The current state has no evolutions satisfying "act"
    --  or 3) All evolutions satisfying "act"  also satisfy "FFF"
    -----------------------------
    -----------
    -- CASE 1)  the state is FINAL, or no evolutions satisfy the action expression,
    --          (therefore there are no subcomputaions)
    -----------
    if This_Subcomputations'Length = 0 then
      if HTML_Mode then
        -- HTML MODE
        declare
          My_Iter: Evolutions_Iterator;
          Tcount: natural :=0;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            Put_Line (HTML_File, "<b>This happens because there is no evolution <br>");
            Put_Line (HTML_File, "starting from State" &
                                  Configurations.NickName(This_State,"C") &
                                 "<br> (The state is final)</b>");
          else
            Put_Line (HTML_File, "<b>This happens because </b><br>");
            while Has_System_Transition (My_Iter) loop
              Tcount := Tcount+1;
              Put_Line (HTML_File,
                  "&nbsp;&nbsp;&nbsp;" & "<a href='javascript:top.sendcommand(""" &  
                  Configurations.NickName(This_State,"C") & """)'>" & 
                  Configurations.NickName(This_State,"C") & "</a>" &
                  " --&gt; " &
                  Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                  "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter)  & "<br>");
              Iterator_Advance(My_Iter);
            end loop;
            if Tcount >1 then
              Put_Line (HTML_File, 
                "<b> and no evolution from " & Configurations.NickName(This_State,"C") &
                " satisfies the action expression </b>&nbsp;&nbsp;<em>");
              Set_Output(HTML_File);
              UCTL_Utilities.Print_Action(Form.Aref);
              Set_Output(Standard_Output);
              Put_Line (HTML_File, "</em><br>");
            else
              Put_Line (HTML_File, 
                "<b> and this evolution does not satisfy the action expression </b>&nbsp;&nbsp;<em>"); 
              Set_Output(HTML_File);
              UCTL_Utilities.Print_Action(Form.Aref);
              Set_Output(Standard_Output);
              Put_Line (HTML_File, "</em>");
              Put_Line (HTML_File, "<br>(no other evolutions are possible)");
            end if;
          end if;
        end;
      else
        --  TEXT MODE
        declare
          My_Iter: Evolutions_Iterator;
          Tcount: natural :=0;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            Put_Line ("This happens because there is no evolution ");
            Put ("starting from State ");
            Put (Configurations.NickName(This_State,"C"));
            New_Line;
            Put ("(The state is final)");
            New_Line;
          else
            Put_Line ("This happens because ");
            while Has_System_Transition (My_Iter) loop
              Tcount := Tcount+1;
              Put ("  " & Configurations.NickName(This_State,"C") &
                   " --> " &
                  Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                  "  " & Abstract_Action_Labels(My_Iter) );
              New_Line;
              Iterator_Advance(My_Iter);
            end loop;
            if Tcount >1 then
              Put (" and no evolution from " & Configurations.NickName(This_State,"C") &
                   "  satisfies the action expression """);
              UCTL_Utilities.Print_Action(Form.Aref);
              Put_Line("""");
            else
              Put (" and this evolution does not satisfy the action expression ");
              UCTL_Utilities.Print_Action(Form.Aref);
              Put_Line("""");
            end if;
            Put_Line ("(The state is final)");
          end if;
          Iterator_Finalize (My_Iter);
        end;
      end if;
      return;
    end if;

    ----------------------------
    -- CASE 2)  State is not final and exist some transition satisfying the action expression
    -- subcomputations'Length >0
    ---------------------------
    if HTML_Mode then
      -- HTML MODE
      Put_Line (HTML_File, "<b>This happens because</b><br> ");
--      Put_Line (HTML_File, "<b> And the evolutions which satisfy the action expression</b><em>");
--      Set_Output(HTML_File);
--      UCTL_Utilities.Print_Action(Form.Aref);
--      Set_Output(Standard_Output);
--      Put_Line(HTML_File, "</em><br>");
--      Put_Line (HTML_File, "<b> also have a target state which satisfies the subformula:</b><br>");
--      Put_Line (HTML_File,
--         "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.FormRef.Fimage.all) & "</em><br>");
--      --
--      Put_Line (HTML_File, "In particular: <br>");
      declare
        My_Iter: Evolutions_Iterator;
        Transitions_Count: Natural := 0;
      begin
        Iterator_Initialize (My_Iter, This_State);
        while Has_System_Transition (My_Iter) loop
          Transitions_Count := Transitions_Count+1;
          Put_Line (HTML_File,
              "&nbsp;&nbsp;&nbsp;" &  "<a href='javascript:top.sendcommand(""" &
              Configurations.NickName(This_State,"C") & """)'>" &  
              Configurations.NickName(This_State,"C") & "</a>" &
               " --&gt; " & "<a href='javascript:top.sendcommand(""" &
              Configurations.NickName(Get_Target_Configuration(My_Iter),"C") & """)'>" & 
              Configurations.NickName(Get_Target_Configuration(My_Iter),"C") & "</a>" &
              "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter) & " <br>");
           Iterator_Advance(My_Iter);
        end loop;
        Iterator_Finalize (My_Iter);
      end;
      for J in This_Subcomputations'Range loop
        Put_Line (HTML_File, " <b>and in state " &
                  Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
                 " the subformula:</b> <br>");
        Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp;" &
          "<em>" & HTML_Format(Get_Formula(This_SubComputations(J)).Fimage.all) & "</em>");
        Put_Line(HTML_File,
           "<A HREF=""javascript:show('" & NickName(This_Subcomputations(J)) & "');""" &
           "TITLE='Explain more ...'>Is Satisfied</A>" );
        Put_Line (HTML_File, ".<br>");
      end loop;        
    else
      -- TEXT MODE
      Put_Line ("This happens because ");
--      Put (" all the evolutions which satisfy the action expression """);
--      UCTL_Utilities.Print_Action(Form.Aref);
--      Put("""");
--      New_Line;
--      Put_Line (" also have a target state which satisfies the subformula:<br><em>");
--      UCTL_Utilities.Print(Form.FormRef, False);
--      New_Line;
--      Put_Line ("In particular:");
      declare
        My_Iter: Evolutions_Iterator;
      begin
        Iterator_Initialize (My_Iter, This_State);
        while Has_System_Transition (My_Iter) loop
          Put (Configurations.NickName(This_State,"C") &
               " --> " &
              Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
              "  " & Abstract_Action_Labels(My_Iter) );
            New_Line;
           Iterator_Advance(My_Iter);
        end loop;
        Iterator_Finalize (My_Iter);
      end;
      for J in This_Subcomputations'Range loop
        Put ("and in State " & Configurations.NickName(Get_State(This_SubComputations(J)),"C") &
           " the subformula: " & Get_Formula(This_SubComputations(J)).Fimage.all );
        Put_Line ("  Is Satisfied.");
      end loop;
    end if;

    -- Iterates Inside
    for J in This_Subcomputations'Range loop
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,This_SubComputations(J),CurDepth+1,Form,False,HTML_File);
    end loop;
    --
  end Explain_Square_True;

  type System_Configuration_Table is array(Positive range <>) of System_Configuration;
  Empty_System_Configuration_Table: System_Configuration_Table(1..0);

  ---------------------------------------------------
  -- called by Explain_EG_True, AF_False, AFact_false, EWuntil1_True
  --  called also by Explain EF_True, EFact_True, EUntil1_True, .. ... ...
  --  
  --  it prints:
  --  si -> sj
  --   ...
  --  sj -> sk  -- (and sk is ok)
  --  sj -> sk  (and sk is final)
  --  sj -> sk (and sk closes a loop)]
  --  
  -- sets:
  --  Rec_Cref.State==FINAL;      Last_Cref:= Cref;      when FINAL with no subcomputations 
  --  Rec_Cref.State==FINAL;      Last_Cref.Form==Form2 when FINAL
  --  Rec_Cref.Form==Form         Last_Cref==Rec_Cref    when LOOP
  --  Rec_Cref.State!==FINAL;     Last_Cref.Form==Form1 when aborted PATH (Form1=false)
  --  Rec_Cref.State!==FINAL;     Last_Cref.Form==Form2 when aborted PATH (Form1=true,Form2=False) no more steps,
  --  Rec_Cref==initial_Cref      Last_CRef/=initial_Cref  when NO PATH
  --
  --  If there are NO subcomputations (i.e. Form1=true is not computed, which happens 
  --    ONLY during the evaluation of "AG true") returns Cref itself.
  --
  --  the Last_Cref (return value) is used only when called
  --     by Explain_EF_True or Explain_EFact_True, EUntil1_True, EWuntil1_True
  ---------------------------------------------------
  procedure Explain_Rec_Path(RecPath:Computations_Table;
                         Cref: Computation_Step;
                         Last_Cref: out Computation_Step;
                         Rec_Cref: out Computation_Step;
                         Previouses: System_Configuration_Table;
                         HTML_File: File_Type) is
    --
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Form: Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Iter: Evolutions_Iterator;
    Sub_Cref : Computation_Step;   --  used only when called by Explain_EF_True
    Sub_Form: Formula_Ref;
    Sub_State: System_Configuration;
    Sub_Evol: System_Evolution;
  begin
    Rec_Cref := Cref;
    if This_Subcomputations'length = 0 then 
       -- can happen only in the case "AG true" on a FINAL state
       Last_Cref:= Cref;   
       return;
    else
      Sub_Cref := This_Subcomputations(This_Subcomputations'last);
      Sub_State := Get_State(Sub_Cref);
      Sub_Form := Get_Formula(Sub_Cref);
      Sub_Evol := This_Subevolutions(This_Subevolutions'Last);
    end if;
    --
    if  Form /= Sub_Form  then
      -- no more recursion
      --
      Iterator_Initialize (This_Iter, This_State);  
      if not Has_System_Transition(This_Iter) then
        -- this is  FINAL STATE
        --  Rec_Cref==Cref, Last_Cref==Cref(Form1)
        --
        if HTML_MODE then
           Put_Line(HTML_FILE, "&nbsp;&nbsp;&nbsp;<b>(" &
                Configurations.NickName(This_State,"C") & " is final)</b><br>");
        else
           Put_Line("(" & Configurations.NickName(This_State,"C") & " is final)");
        end if;
      end if;
      Iterator_Finalize(This_Iter);
      Last_Cref:= Sub_Cref;
      return;
    end if;
    --
    --  Form= Sub_Form then
    --
    Rec_Cref := Sub_Cref;
    for I in Previouses'Range loop
        if This_State = Previouses(I) then
          --
          -- this path is a LOOP
          -- Rec_Cref==Sub_Cref (start of loop), Last_Cref==Rec_Cref
          --
          if HTML_MODE then
            Put_Line(HTML_FILE, "&nbsp;&nbsp;&nbsp;<b>(" &
              Configurations.NickName(This_State,"C") & " closes a loop)</b><br>");
          else
            Put_Line("(" & Configurations.NickName(This_State,"C") & " closes a loop)");
          end if;
          Last_Cref:=  Sub_Cref;
          return;
        end if;
    end loop;
   --
   -- this path ends to a state from which not {act2} "Form2" transitions are possible
   -- Rec_Cref==Cref (last rec state), Last_Cref==Sub_Cref (either form1 or form2)
   --
    if HTML_Mode then
      -- HTML MODE
      if  Previouses'Length >0 then Put_Line (HTML_File, "<br>"); end if;
      Put_Line (HTML_File,
         "&nbsp;&nbsp;&nbsp;<a href='javascript:top.sendcommand(""" &
            Configurations.NickName(This_State,"C") & """)'>" &
            Configurations.NickName(This_State,"C") &
          "</a>");
        Put (HTML_File," &nbsp;--&gt; &nbsp; ");
        Put_Line (HTML_File,
          "<a href='javascript:top.sendcommand(""" & Configurations.NickName(Sub_State,"C") & """)'>" &
          Configurations.NickName(Sub_State,"C") &
          "</a>");
        Put_Line (HTML_File, "&nbsp;&nbsp; " & Abstract_Label_Image(Sub_Evol));
        declare
          ThisLabel: String := Ground_Label_Image(Sub_Evol);
        begin
          if ThisLabel /= "" then
            -- TOOLTIP CODE
            -- currently unused because during evaluation no ground data is collected
            Put(HTML_FIle,"<label href=""index.htm"" onmouseover=""Tip(");
            Put(HTML_File,"' /* '+" &  HTML_Literal_Hack(ThisLabel) & "+' */'");
            Put(HTML_File,
              ")"" onmouseout=""UnTip()""><FONT color=""blue"">&nbsp;/* ... */</font></label>");
            New_Line(HTML_File);
          end if;
        end;
      else
        -- TEXT MODE
        -- if  Previouses'Length >0 then New_Line; end if;
        Put ("  " & Configurations.NickName(This_State,"C"));
        Put (" --> " & Configurations.NickName(Sub_State,"C"));
        Put ("  " & Abstract_Label_Image(Sub_Evol) );
        declare
          ThisLabel: String := Ground_Label_Image(Sub_Evol);
        begin
          if ThisLabel /= "" then
            -- TOOLTIP CODE
            -- currently unused because during evaluation no ground data is collected
            Put_Line (" /* " &  ThisLabel & " */");
          else
            New_Line;
          end if;
        end;
    end if;
    Explain_Rec_Path(RecPath,Sub_Cref, Last_Cref, Rec_Cref, Previouses & This_State, HTML_File);
    --
  end Explain_Rec_Path;


--  ---------------------------------------------------
--  -- called by Explain EFact_True EUntil2_True 
--  --
--  --  it prints:
--  --  si -> sj
--  --   ...
--  --  sj -> sk  
--  --  sj -xx-> sk    (and -xx-> sk is ok)
--  --
--  --  the Last_Cref (return value) is used only when called
--  --     by Explain_EFact_True,
--  ---------------------------------------------------
--  function Explain_Rec2_Path(RecPath:Computations_Table;
--                         Cref: Computation_Step;
--                         Previouses: System_Configuration_Table;
--                         HTML_File: File_Type) return Computation_Step is
--    --
--    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
--    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
--    Form: Formula_Ref := Get_Formula(Cref);
--    This_State: System_Configuration := Get_State(Cref);
--    This_Iter: Evolutions_Iterator;
--    Sub_Cref : Computation_Step;   --  used only when called by Explain_EF_True
--    Sub_Form: Formula_Ref;
--    Sub_State: System_Configuration;
--  begin
----  if EF{act}FF is true, the subcomputation is always 1
----    if This_Subcomputations'length > 0 then  -- (only when AFact_false)
--      Sub_Cref := This_Subcomputations(1);
--      Sub_State := Get_State(This_Subcomputations(1));
--      Sub_Form := Get_Formula(This_Subcomputations(1));
----   end if;
--    --
--    if HTML_Mode then
--      -- HTML MODE
--      Put_Line (HTML_File,
--         "&nbsp;&nbsp;&nbsp;<a href='javascript:top.sendcommand(""" &
--            Configurations.NickName(This_State,"C") & """)'>" &
--            Configurations.NickName(This_State,"C") &
--          "</a>");
--        Put (HTML_File," &nbsp;--&gt; &nbsp; ");
--        Put_Line (HTML_File,
--          "<a href='javascript:top.sendcommand(""" & Configurations.NickName(Sub_State,"C") & """)'>" &
--          Configurations.NickName(Sub_State,"C") &
--          "</a>");
--        Put_Line (HTML_File, "&nbsp;&nbsp; " & Abstract_Label_Image(This_Subevolutions(1)));
--        declare
--          ThisLabel: String := Ground_Label_Image(This_Subevolutions(1));
--        begin
--          if ThisLabel /= "" then
--            -- TOOLTIP CODE
--            -- currently unused because during evaluation no ground data is collected
--            Put(HTML_FIle,"<label href=""index.htm"" onmouseover=""Tip(");
--            Put(HTML_File,"' /* '+" &  HTML_Literal_Hack(ThisLabel) & "+' */'");
--            Put(HTML_File,
--              ")"" onmouseout=""UnTip()""><FONT color=""blue"">&nbsp;/* ... */</font></label>");
--            New_Line(HTML_File);
--          end if;
--        end;
--        Put_Line (HTML_File, "<br>");
--      else
--        -- TEXT MODE
--        Put ("  " & Configurations.NickName(This_State,"C"));
--        Put (" --> " & Configurations.NickName(Sub_State,"C"));
--        Put ("  " & Abstract_Label_Image(This_Subevolutions(1)) );
--        declare
--          ThisLabel: String := Ground_Label_Image(This_Subevolutions(1));
--        begin
--          if ThisLabel /= "" then
--            -- TOOLTIP CODE
--            -- currently unused because during evaluation no ground data is collected
--            Put (" /* " &  ThisLabel & " */");
--          end if;
--        end;
--        New_Line;
--    end if;
--    if  Form= Sub_Form then
--      return Explain_Rec2_Path(RecPath,This_Subcomputations(1), Previouses & This_State, HTML_File);
--    else
--       return Sub_Cref;
--    end if;
--  end Explain_Rec2_Path;


  procedure Explain_EF_True (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Form : Formula_Ref; 
    Sub_State: System_Configuration;
    Sub_Cref : Computation_Step;
    --Last_Cref: Computation_Step;
    Rec_Cref:  Computation_Step;
  begin
    ------------------------------------
    -- FORM =  EF FORM1
    --   either  FORM1 is True  in "This_State" ( 1 Subcomputation)
    --   or  FORM1 is False  in "This_State"   
    --           but  "EF FORM1" is True in a substate. 
    --  (still 1 subcomputation because the last last success replces all others)
    ------------------------------------
    --
    --   "FORM1" is True in This_State
    --
    Sub_Form := Get_Formula(This_Subcomputations(1));
    if Previous /= Form and then Form /= Sub_Form  then
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is already ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        UCTL_Utilities.Print(Sub_Form, False);
        New_Line;
        Put(" is already Satisfied ");
        Put (" in State ");
        Put (Configurations.NickName(This_State,"C"));
        New_Line;
      end if;
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then 
           Put_Line(HTML_File, "</div>"); 
        end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    --   "FORM1" is True in a substate.
    --
    if HTML_Mode then
      Put_Line (HTML_File, "<b>This happens because, after the path: </b><br>");
    else
      Put_Line ("This happens because, after the path:");
    end if;
    --
    --  si -> sj .. -> sk  and (sk is ok)
    Explain_Rec_Path(RecPath,Cref, SUB_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State(Sub_Cref);
    --
    if HTML_Mode then
      Put_Line (HTML_File,
             "<br><b>the subformula:</b>");
      Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
      Put (HTML_File, "<b> ");
      Put (HTML_File, " <spawn style='background-color:lightgreen'> is finally ");
      Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(Sub_Cref) & "');""" &
             " TITLE='Explain more ...'>Satisfied</A>" );
      Put_line(HTML_File, "</spawn>");
      Put (HTML_File, " in State ");
      Put (HTML_File, Configurations.NickName(Sub_State,"C"));
      Put_Line(HTML_File, ".</b><br>");
    else
      -- TEXT MODE
      Put_Line ("the subformula:");
      UCTL_Utilities.Print(Sub_Form, False);
      Put(" is finally Satisfied ");
      Put (" in State ");
      Put (Configurations.NickName(Sub_State,"C"));
      New_Line;
    end if;
    if not Already_Explored (Sub_Cref) then
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,Form,False,HTML_File);
    end if;
    return;
  end Explain_EF_True;


  procedure Explain_EFact_True (RecPath:Computations_Table;
                                Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    This_Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    Sub_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    ------------------------------------
    --  EF {act} FORM is a syntactic sugar for  E[ true {true} U {act} FORM1]
    ------------------------------------
    -- FORM =  EF {act}  FORM1
    --   either  <act> FORM1 is True  in "This_State" ( 1 Subcomputation)
    --   or  <act> FORM1 is False  in "This_State"
    --           but  "EF {act} FORM1" is True in a substate.
    ------------------------------------
    --
    --  CASE 0)  FINAL STATE     (not possible for "true" result)
    --  CASE 1)  IMMEDIATE LOOP  (not possible for "true" result)
    --  CASE 2)  LONG LOOP       (not possible for "true" result)
    --  CASE 3)  IMMEDIATE RESULT  ({act} FORM --> true )
    --  CASE 4)  DELAYED RESULT    ( .. ... {act} FORM --> true )
    --
    if HTML_Mode then
      Put_Line (HTML_File, "<b>This happens because</b><br> ");
    else
      Put_Line ("This happens because ");
    end if;
    --
    Explain_Rec_Path(RecPath,Cref, SUB_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    --
    declare
      -- This_Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
      Last_State: System_Configuration := Get_State(Rec_Cref);
      --This_Subcomputations: Computations_Table := Get_Subcomputations(REC_Cref);
      This_Subevolutions: Evolutions_Table := Get_Subevolutions(REC_Cref);
      Sub_Form : Formula_Ref;
      Sub_State: System_Configuration := Get_State(Sub_Cref);
      Sub_Evol:System_Evolution := This_Subevolutions(This_Subevolutions'Last);
      ThisLabel: String := Ground_Label_Image(Sub_Evol);
    begin
      Sub_Form := Get_Formula(Sub_Cref);
      Sub_State := Get_State(Sub_Cref);
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "&nbsp;&nbsp;"  &  "<a href='javascript:top.sendcommand(""" &
               Configurations.NickName(Last_State,"C") & """)'>" &
               Configurations.NickName(Last_State,"C") &  "</a>" &
               " --&gt; " &
               "<a href='javascript:top.sendcommand(""" &     
              Configurations.NickName(Sub_State,"C") & """)'>" & 
              Configurations.NickName(Sub_State,"C") & "</a>" &
              "&nbsp;&nbsp;" & Abstract_Label_Image(This_Subevolutions(1)) & "<br>" );
        Put_Line (HTML_File, "<b> the transition label satisfies the action expression</b><em> ");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(This_Form.PRef.U2ARef2);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, "</em><br><b> and in State </b> " &
             Configurations.NickName(Sub_State,"C") &
            " <b>the subformula:</b><br><em> " &
               HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:lightgreen'>");
        Put_Line(HTML_File,
--                   "<A HREF=""javascript:show('" & NickName(Sub_State,"C") & "');""" &
                     "<A HREF=""javascript:show('" & NickName(Sub_Cref,"X") & "');""" &
                     " TITLE='Explain more ...'>Is Satisfied</b></A>" );
        Put_Line (HTML_File, "</spawn>.</b><br>");
        Put_Line(HTML_File, "</div>"); 
      else
        -- TEXT MODE
        Put ("  " & Configurations.NickName(Last_State,"C"));
        Put (" --> " & Configurations.NickName(Sub_State,"C"));
        Put ("  " & Abstract_Label_Image(Sub_Evol) );
        if ThisLabel /= "" then
          Put_Line (" /* " &  ThisLabel & " */");
        end if;
        New_Line;
        Put ("the label of the last transition of the path satisfies the action expression """);
        UCTL_Utilities.Print_Action(This_Form.PRef.U2ARef2);
        Put_Line (" """);
        Put ("and in State " & Configurations.NickName(Sub_State,"C") &
           " the subformula: """ & Sub_Form.Fimage.all );
        Put_Line ("""  Is Satisfied.");
      end if;
    end;
    --
    Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,This_Form,False,HTML_File);
    --
  end Explain_EFact_True;

  procedure Explain_EF_False (RecPath: Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    ------------------------------------------
    -- At least ONE subcompuation is always present (evaluation of FORM1 in This_State)
    -- For each existing transition there is at least one additional subcomputation
    --- (variable bindings may add more subcomputations for each evolution)
    ------------------------------------------
    if This_Subcomputations'Length = 1 then
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is final
        Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
        Put_Line (HTML_File, 
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>Does NOT Hold in this State<br>");
        Put (HTML_File, "and there are no outgoing transitions from State ");
        Put_Line (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line (HTML_File, "<br>(The state is final)</b><br>");
        --
        Put_Line (HTML_File, 
          "<font color='red'>Notice: further explanations of why the subformula does not" &
          " hold in this state are omitted.</font><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put (Form.Pref.TForm.Fimage.all & " Does NOT Hold in this State ");
        Put_Line ("and there are no outgoing transitions from State");
        Put_Line (Configurations.NickName(This_State,"C"));
        Put_Line ("(The state is final)");
        --
        Put_Line ("Notice: further explanations of why the subformula does not" &
                  " hold in this state are omitted.");
      end if;
    else -- subcomputations'Length >1
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is final
        Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
        Put_Line (HTML_File, "<em>&nbsp;&nbsp;&nbsp;" & 
            HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>Does NOT Hold in this state nor in any of its descendents.</b><br>");
--        Put_Line (HTML_File, "In particular <br>");
--        declare
--          My_Iter: Evolutions_Iterator;
--        begin
--          Iterator_Initialize (My_Iter, This_State);
--          while Has_System_Transition (My_Iter) loop
--            Put_Line (HTML_File,
--                "&nbsp;&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
--                 " --&gt; " &
--                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
--                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter) & " <br>");
--             Iterator_Advance(My_Iter);
--          end loop;
--          Iterator_Finalize (My_Iter);
--        end;
--        Put_Line (HTML_File, 
--           "<b> but none of the successor states nor any of their descendents " & 
--           "satisfy the subformula:</b><br>");
--        Put_Line (HTML_File, "<em>&nbsp;&nbsp;&nbsp;" & 
--            HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
--        --
        Put_Line (HTML_File, 
         "<font color='red'>Notice: further explanations of why the subformula does "&
         "not<br> hold in this state or in any of its descendents are omitted.</font><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put_Line (Form.Pref.TForm.Fimage.all & 
           " Does NOT hold in this state in this state nor in any of its descendents.");
--        declare
--          My_Iter: Evolutions_Iterator;
--        begin
--          Iterator_Initialize (My_Iter, This_State);
--          while Has_System_Transition (My_Iter) loop
--            Put_Line (Configurations.NickName(This_State,"C") &
--                 " --> " &
--                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
--                "  " & Abstract_Action_Labels(My_Iter) );
--             Iterator_Advance(My_Iter);
--          end loop;
--          Iterator_Finalize (My_Iter);
--        end;
--        Put_Line ("none of the successor states nor any of their descendents satisfy the subformula:");
--        Put_Line (Form.Pref.TForm.Fimage.all);
        --
        Put_Line ("Notice: further explanations of why the subformula does not"); 
        Put_Line ("hold in this state or in any of its descendents are omitted.");
      end if;
    end if;
  end Explain_EF_False;



  -------------------------------------------
  --   FORM =  EF {act} $FORM1
  -- Possibilities are:
  --  1)  state is final  (subcomputations'Length=0)
  --  2)  no transitions satisfying act or 
  --      one or more transitions satifying act but not satisfying $FORM1
  --       and zero, one or more (all) transitions not recorsively satisfying FORM
  --------------------------------------------
  -- we could find all subcomputations of $FORM1 and explain their failure
  -- e.g like:
  --  s1 --> .. --> Si {act}  but Sj does not sartisfy FORM1
  --  s1 --> .. --> Sj {act}  but Sj does not sartisfy FORM1
  --  s1 --> .. --> Sk {act}  but Sj does not sartisfy FORM1
  -- of if no subevolution satisfying act exists say precisely that!
  --------------------------------------------
  procedure Explain_EFact_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Failures_Count: Natural :=0;
  begin
    --
      if HTML_Mode then
        -- HTML MODE
        Put (HTML_File,
           "<b>This happens because there is no path starting from ");
        Put_Line (HTML_File, Configurations.NickName(This_State,"C"));
        Put (HTML_File," which leads to a transition whose label satisfies </b><em>""");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
        Set_Output(Standard_Output);
        Put_Line(HTML_File,"</em>""");
        if Form.Pref.U2FormRef2 /= True_Formula then
          Put (HTML_File,
             "&nbsp;<b> and whose target state satisfies the subformula</b> &nbsp;<em>" &
             HTML_Format(Form.Pref.U2FormRef2.Fimage.all) &
             "</em> &nbsp;");
        end if;
        Put_line(HTML_File, ".<br>");
      else
        -- TEXT MODE
        Put_Line ("This happens because there is no path starting from " & 
             Configurations.NickName(This_State,"C"));
        Put (" which leads to a transition whose label satisfies """);
        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
        Put_Line("""");
        if Form.Pref.U2FormRef2 /= True_Formula then
          Put_Line (""" and whose target states satisfies the subformula """);
          Put_Line(Form.Pref.U2FormRef2.Fimage.all & """.");
        end if;
      end if;
      ---
      -- 1) This state is FINAL
      declare
         My_Iter: Evolutions_Iterator;
      begin
          Iterator_Initialize (My_Iter, This_State);
          if not  Has_System_Transition (My_Iter) then
            if HTML_Mode then
             -- HTML MODE
              Put_Line (HTML_File, "<b> In particular:</b><br>");
              Put_Line (HTML_FILE,"<b>the current state has no evolutions (is Final).</b><br>");
            else
              -- TEXT MODE
              Put_Line ("In Particular:");        
              Put_Line ("the current state has no evolutions (is Final).");
            end if;
            return;
          end if;
          Iterator_Finalize (My_Iter);
      end;
    --
    --
    return;
    --
    -- the following is not acceptable if the system is big!!
    --
    -- clarification
    --   EF {act} FF is false,  because there is no "act" or because FF never holds?
--    if HTML_Mode and (Failures_Count > 0  and Form.Pref.U2FormRef2 /= True_Formula) then
--       Put_Line (HTML_File, "<b> For Example:</b><br>");
--    else
--      Put_Line ("For Example:");
--    end if;
--    for I in All_Nodes'Range loop
--      declare
--        Those_Subs: Computations_Table := Get_Subcomputations(All_Nodes(I));
--      begin
--        for J in Those_Subs'Range loop
--          if Form /=  Get_Formula(Those_Subs(J)) then
--            Failures_Count := Failures_Count+1;
--            if HTML_Mode then
--              -- HTML MODE
--              if This_State /= Get_State(All_Nodes(I)) then
--                Put (HTML_File, Configurations.NickName(This_State,"C"));
--                Put_Line (HTML_File," &nbsp;--&gt; ... &nbsp;--&gt; &nbsp; ");
--              end if;
--              Put_Line (HTML_File,
--                "<a href='javascript:top.sendcommand(""" &
--                Configurations.NickName(Get_State(All_Nodes(I)),"C") & """)'>" &
--                Configurations.NickName(Get_State(All_Nodes(I)),"C") & "</a>");
--              Put_Line (HTML_File, "&nbsp;--&gt; &nbsp;");
--              Put_Line (HTML_File,
--                "<a href='javascript:top.sendcommand(""" &
--                Configurations.NickName(Get_State(Those_Subs(J)),"C") & """)'>" &
--                Configurations.NickName(Get_State(Those_Subs(J)),"C") & "</a>");
--              Put_Line (HTML_File, "&nbsp;&nbsp;{ <em>");
--              Set_Output(HTML_File);
--              UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
--              Set_Output(Standard_Output);
--              Put_Line(HTML_File, "</em>}<br>");
--              --
--              Put_Line (HTML_File,
--                  "&nbsp;&nbsp;&nbsp;<b> but the subformula </b>&nbsp;<em>" &
--                  HTML_Format(Get_Formula(Those_Subs(J)).Fimage.all) & "</em>&nbsp;");
--              Put (HTML_File, "is <spawn style='background-color:pink'>");
--              Put_Line (HTML_File,
--                "<b><A HREF=""javascript:show('" &
--                     NickName(Those_Subs(J)) & "');""" &
--                 " TITLE='Explain more ...'>Not Satisfied</A>" &
--                 "</spawn>&nbsp; by </b>" &
--                 Configurations.NickName(Get_State(Those_Subs(J)),"C") & "<br>");
 --           else
 --             -- TEXT MODE
 --             if This_State /= Get_State(All_Nodes(I)) then
 --               Put ("  " & Configurations.NickName(This_State,"C"));
 --               Put (" --> ... --> ");
--              end if;
--              Put (Configurations.NickName(Get_State(All_Nodes(I)),"C"));
--              Put ("  --> " & Configurations.NickName(Get_State(Those_Subs(J)),"C"));
--              Put ("  {");
--              UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
--              Put_Line(" }");
--              Put_Line ("    but the subformula """ &  Get_Formula(Those_Subs(J)).Fimage.all &
--                   """ Is Not Satisfied by " &
--                   Configurations.NickName(Get_State(Those_Subs(J)),"C"));
--            end if;
--          end if;
--        end loop;  -- Those_Subs(J)
--      end;   
--    end loop; -- All_Nodes(I)
--    if Failures_Count = 0  and then Form.Pref.U2FormRef2 /= True_Formula then
--      if HTML_Mode then
--        -- HTML MODE
--        Put_Line (HTML_File, "<b> In Particular:</b><br>");
--        Put (HTML_File,"<b> there is no path from ");
--        Put_Line (HTML_File, Configurations.NickName(This_State,"C"));
--        Put (HTML_File," which leads to a transition whose label satisfies </b>""<em>"");
--        Set_Output(HTML_File);
--        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
--        Set_Output(Standard_Output);
--        Put_Line (HTML_File,"</em>"".<br>");
--      else
--        -- TEXT MODE
--        Put_Line ("In Particular:");
--        Put_Line (" there is no path from " & Configurations.NickName(This_State,"C"));
--        Put_Line (" which leads to a transition whose label satisfies """);
--        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
--        Put_Line (""".");
--      end if;
--    end if;
--    --
--    -- Iterates Inside
----    for I in All_Nodes'Range loop
----      declare
----        Those_Subs: Computations_Table := Get_Subcomputations(All_Nodes(I));
----      begin
----        for J in Those_Subs'Range loop
----          if Form /=  Get_Formula(Those_Subs(J)) then
----            if not Already_Explored (Those_Subs(J)) then
----              if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
----              Text_Explain_Computation(RecPath,Those_Subs(J),CurDepth+1,Form,False,HTML_File);
----            end if;
----          end if;
----        end loop;
----      end;
----    end loop;
  end Explain_EFact_False;


  procedure Explain_AG_True (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
    ------------------------------------------
    --    FORM =   AG FORM1
    ------------------------------------------
    -- At least ONE subcompuation is always present (evaluation of FORM1 in This_State)
    -- For each existing transition there is at least one additional subcomputation
    --- (variable bindings may add more subcomputations for each evolution)
    ------------------------------------------
    --  IN CASE OF LOOPS    eg     s1 -> s1    we have subcomputations'Length=2
    -----------------------------------------
    --
    if This_Subcomputations'Length = 1 then
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is FINAL
        Put_Line (HTML_File, "<b>This happens because this state has no evolutions (state is final)<br>");
        Put_Line (HTML_File, "and the subformula:</b><br>");
        Put_Line (HTML_File, 
          "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
        Put (HTML_File, "<b>&nbsp;&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
        if not Already_Explored (This_SubComputations(1)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
        end if;
      else
        --  TEXT MODE
        Put_Line ("This happens because this state has no evolutions (state is final)");
        Put_Line ("and the subformula:");
        Put_Line (Form.Pref.TForm.Fimage.all & 
            " is Satisfied in this State.");
        if not Already_Explored (This_SubComputations(1)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
        end if;
      end if;
      return;
    end if;
    --
    -- subcomputations'Length >1
    --
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
        Put_Line (HTML_File, 
           "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>Is Satisfied in this state, its immediate successors, " &
            "and in all descendents states.</b><br>" & 
            " (" & Configurations.NickName(This_State,"C")  & " is not final)<br>");
--        declare
--          My_Iter: Evolutions_Iterator;
--        begin
--          Iterator_Initialize (My_Iter, This_State);
--          while Has_System_Transition (My_Iter) loop
--            Put_Line (HTML_File,
--                "&nbsp;&nbsp;&nbsp;" & Configurations.NickName(This_State,"C") &
--                 " --&gt; " &
--                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
--                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter) & " <br>");
--             Iterator_Advance(My_Iter);
--          end loop;
--          Iterator_Finalize (My_Iter);
--        end;
--        Put_Line (HTML_File, 
--            "<b> and all the successor states and all of their descendents " & 
--            "satisfy the subformula:</b><br>");
--        Put_Line (HTML_File, "<em>&nbsp;&nbsp;&nbsp;" & 
--           HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
--        --
        Put_Line (HTML_File, 
           "<font color='red'>Notice: " & 
           "further explanations of why the subformula is satisfied<br>" &
           " in this state or in all of its descendents are omitted.</font><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put_Line (Form.Pref.TForm.Fimage.all & " Is satisfied in this State, its immediate successors, ");
        Put_Line (" and in all descendent states. ");
--        Put_Line ("Moreover, ");
--        declare
--          My_Iter: Evolutions_Iterator;
--        begin
--          Iterator_Initialize (My_Iter, This_State);
--          while Has_System_Transition (My_Iter) loop
--            Put_Line (Configurations.NickName(This_State,"C") &
--                 " --> " &
--                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
--                "  " & Abstract_Action_Labels(My_Iter) );
--             Iterator_Advance(My_Iter);
--          end loop;
--          Iterator_Finalize (My_Iter);
--        end;
--        Put_Line ("and all the successor states and all of their descendents satisfy the subformula:");
--        Put_Line (Form.Pref.TForm.Fimage.all);
        ---
        Put_Line ("Notice: further explanations of why the subformula is satisfied");
        Put_Line ("in this state or in all of its descendents are omitted.");
      end if;
  end Explain_AG_True;


  procedure Explain_AG_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
   -- similar to EF False
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Form : Formula_Ref;
    Sub_State: System_Configuration;
  begin
    ------------------------------------
    -- FORM =  AG FORM1
    --   either  FORM1 is False  in "This_State" ( 1 Subcomputation)
    --   or  FORM1 is True  in "This_State"
    --           but  "AG FORM1" is False in a substate.
    --  (still 1 subcomputation because the last last success replces all others)
    ------------------------------------
    Sub_Form := Get_Formula(This_Subcomputations(1));
    if Previous /= Form and then Form /= Sub_Form  then
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:pink'> is already ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Not Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        UCTL_Utilities.Print(Sub_Form, False);
        New_Line;
        Put(" is already Not Satisfied ");
        Put (" in State ");
        Put (Configurations.NickName(This_State,"C"));
        New_Line;
      end if;
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    if Previous = Form and Form /= Sub_Form then
      --  this concludes a sequence   Si -> Sj
      if HTML_Mode then
        Put_Line (HTML_File,
               "<b>and the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:pink'> is ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Not Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
      else
        -- TEXT MODE
        Put_Line ("and the subformula:");
        UCTL_Utilities.Print(Sub_Form, False);
        Put(" is not Satisfied ");
        Put (" in State ");
        Put (Configurations.NickName(This_State,"C"));
        New_Line;
      end if;
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    -- Form = SubForm  (i.e. we are inside the recursion)
    --
    Sub_State := Get_State(This_SubComputations(1));
      if HTML_Mode then
        -- HTML MODE
        if Previous /= Form then
          Put_Line (HTML_File, "<b>This happens because </b><br>");
        end if;
        Put_Line (HTML_File,
          "&nbsp;&nbsp;&nbsp;<a href='javascript:top.sendcommand(""" & 
          Configurations.NickName(This_State,"C") & """)'>" &
          Configurations.NickName(This_State,"C") &
          "</a>");
        Put (HTML_File," &nbsp;--&gt; &nbsp; ");
        Put_Line (HTML_File,
          "<a href='javascript:top.sendcommand(""" & 
          Configurations.NickName(Sub_State,"C") & """)'>" &
          Configurations.NickName(Sub_State,"C") &
          "</a>");
        Put_Line (HTML_File, "&nbsp;&nbsp; " & Abstract_Label_Image(This_Subevolutions(1)));
        Put_Line (HTML_File, "<br>");
      else
        -- TEXT MODE
        if Previous /= Form then
          Put_Line ("This happens because");
        end if;
        Put ("  " & Configurations.NickName(This_State,"C"));
        Put (" --> " & Configurations.NickName(Sub_State,"C"));
        Put_Line ("  " & Abstract_Label_Image(This_Subevolutions(1)) );
      end if;
      if not Already_Explored (This_SubComputations(1)) then
        Explain_AG_False(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
  end Explain_AG_False;

  procedure Explain_EG_True (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    null;
    -------
    -- EG $FORM1    
    --  There is always 1 subcomputation ....
    -- FORM1 is True now and along either a finite path or a loop
    --  1)  State is FINAL 
    --  2)  There is a path leading to a final state
    --  3)  There is a loop
    --  except that in case 1) we cannot give explanations for the subformula
    --   (because during evaluation the $FORM1 evaluation explanation has been removed)
    ----------
    --
    -- 1) State is FINAL
    Iterator_Initialize (My_Iter, This_State);
    if not Has_System_Transition (My_Iter)  then
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File, 
             "<b>This happens because in this state the subformula:</b><br><em> " &
             HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
        Put_Line(HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:lightgreen'>");
        Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
             " TITLE='Explain more ...'>Is Satisfied</b></A>" );
        Put_Line (HTML_File, "</spawn>, and the state is final.</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because in this state the subformula: "&
              Get_Formula(This_SubComputations(1)).Fimage.all &
              " Is Satisfied, and the state is final.");
      end if;
      -- Iterates Inside
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      Iterator_Finalize(My_Iter);
      return;
    end if;
    Iterator_Finalize(My_Iter);
    --
    -- 2)  3) there is a loop
    if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,  
             "<b>This happens because:</b><br>");
    else
      Put_Line("This happens because:");
    end if;
    --
    --  si -> sj .. -> sk  and (sk is final)
    --  si -> sj .. -> sk  and (sk closes a loop)
    Explain_Rec_Path(RecPath,Cref, LAST_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File, 
        "<b>In all states of the above path the subformula:</b><br><em> " &
        Form.Pref.TForm.FImage.all & "</em><b> Is Satisfied.</b><br>");
--      Put_Line (HTML_File, 
--       "<font color='red'>Notice: further explanations of why the subformula is satisfied <br>" &
--       " along all the path are omitted.</font><br>");
    else
      -- TEXT MODE
      Put_line ("In all states of the above maximal path the subformula: " &
          Form.Pref.TForm.FImage.all & " Is Satisfied.");
--      Put_Line (
--       "Notice: further explanations of why the subformula is satisfied" &
--       " along all the path are omitted.");
    end if;
  end Explain_EG_True;

  
   procedure Explain_AF_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    null;
    -------
    -- EG $FORM1
    --  There is always 1 subcomputation ....
    -- FORM1 is True now and along either a finite path or a loop
    --  1)  State is FINAL
    --  2)  There is a path leading to a final state
    --  3)  There is a loop
    --  except that in case 1) we cannot give explanations for the subformula
    --   (because during evaluation the $FORM1 evaluation explanation has been removed)
    ----------
    --
    -- 1) State is FINAL 
    Iterator_Initialize (My_Iter, This_State);
    if not Has_System_Transition (My_Iter)  then
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
             "<b>This happens because in this state the subformula:</b><br><em> " &
             HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
        Put_Line(HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:color:pink'>");
        Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
             " TITLE='Explain more ...'>Is Not Satisfied</A>" );
        Put_Line (HTML_File, "</spawn>, and the state is final.</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because in this state the subformula: "&
              Get_Formula(This_SubComputations(1)).Fimage.all &
              " Is Not Satisfied, and the state is final.");
      end if;
      -- Iterates Inside
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      Iterator_Finalize(My_Iter);
      return;
    end if;
    Iterator_Finalize(My_Iter);
    --
    -- 2)  3) there is a loop
    if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
             "<b>This happens because:</b>");
    else
      Put_Line("This happens because:");
    end if;
    --
    --  si -> sj .. -> sk  and (sk is final)
    --  si -> sj .. -> sk  and (sk closes a loop)
    Explain_Rec_Path(RecPath,Cref,LAST_Cref,REC_Cref, Empty_System_Configuration_Table, HTML_File);
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File,
        "<b>In all states of the above full path the subformula:</b><br><em> " &
        HTML_Format(Form.Pref.TForm.FImage.all) & "</em><b> Is Not Satisfied.</b><br>");
      Put_Line (HTML_File,
       "<font color='red'>Notice: further explanations of why the subformula is not satisfied <br>" &
       " along all the path are omitted.</font><br>");
    else
      -- TEXT MODE
      Put_line ("In all states of the above full  path the subformula: " &
          Form.Pref.TForm.FImage.all & " Is Not Satisfied.");
--      Put_Line (
--       "Notice: further explanations of why the subformula is not satisfied" &
--       " along all the path are omitted.");
    end if;
  end Explain_AF_False;





  --------------------------------
  --  Called in the case of EG_False and AF_True
  --  We return the table of leafs of the tree (all different)
  --  whose Status is different from that of Cref.
  --   (in the final subcomputations the value is the opposite than the current one)
  --   There is at least one subcomputation for Subform, 
  --       and at least another one for descendents (unless the state is final)
  -------------------------------
  function Explain_Tree(RecPath:Computations_Table;
                        Cref: Computation_Step;   -- EG SUB    or AF SUBF
                        Root: System_Configuration;
                        Finals_Found: Computations_Table;
                        HTML_File: File_Type) return Computations_Table is
    --
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Form: Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    SubCref: Computation_Step;
    AlreadyFound: Boolean;
    Previous_Found: Computations_Table_Ref;
  begin
    if This_Subcomputations'Length =1  then
      -- this is a final subcomputation.
      AlreadyFound := False;
      for J in Finals_Found'Range loop
        if Get_State(Finals_Found(J)) = This_State then -- and 
           AlreadyFound := True;
           exit;
        end if;
      end loop;
      if not AlreadyFound then 
        -- this is a novel FINAL state (a new Tree leaf)
        return Finals_Found & This_Subcomputations(1);
      else
        return Finals_Found;
      end if;
    end if;
    --  
    -- if we have subcompuations ... check them
    Previous_Found := new Computations_Table'(Finals_Found);
    for I in 2..This_Subcomputations'last loop
      SubCref := This_Subcomputations(I);
        declare
          More_Found: Computations_Table :=  
              Explain_Tree(RecPath,SubCref, Root, Previous_Found.all,HTML_File);
        begin
          Free(Previous_Found);
          Previous_Found := new Computations_Table'(More_Found);
        end;
    end loop;
    declare
      Result: Computations_Table := Previous_Found.all;
    begin
       Free(Previous_Found);
       return Result;
    end;
  end Explain_Tree;


  -- similar to AF true: for all path exists a future state in which subform does not hold
  -- 1) Subform does not hold now. (special case)
  -- 2) We collect all target states in which Subform does not hold (without duplicates)
  --    and we show them
  --   For all paths we should print (avoiding duplications)
  --   Root -> ... ->  Si and Subform is Not Satisfied in Si
  --   Root -> ..  ->  Sj and Subform is Not Satisfied in Si
  procedure Explain_EG_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
  begin
    if This_Subcomputations'Length =1 then
      -- this is case 1)
      -- Exaclty one subcomputation  -->  state is FINAL
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, "<b>This happens because this state has no evolutions (state is final)<br>");
        Put_Line (HTML_File, "and the subformula:</b><br>");
        Put_Line (HTML_File,
          "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.TForm.Fimage.all) & "</em><br>");
        Put (HTML_File, "<b>&nbsp;&nbsp;");
        Put (HTML_File, " <spawn style='background-color:pink'> is ");
        Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
             " TITLE='Explain more ...'>Is Not Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
        if not Already_Explored (This_SubComputations(1)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
        end if;
      else
        --  TEXT MODE
        Put_Line ("This happens because this state has no evolutions (state is final)");
        Put_Line ("and the subformula:");
        Put_Line (Form.Pref.TForm.Fimage.all &
            " is Satisfied in this State.");
        if not Already_Explored (This_SubComputations(1)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
        end if;
      end if;
      return;
    end if;
    --
    -- subcomputations'Length >1
    --  this is case 2)
    --
    declare
      All_Failures: Computations_Table := 
         Explain_Tree(RecPath,Cref,This_State,(1..0 => Cref),HTML_File);
    begin
      if HTML_Mode then
        Put_Line(HTML_File, "<b>This happens because all paths from " & Configurations.NickName(This_State,"C") &
           " eventually lead to a state in which the subformula</b><br>");
        Put_Line(HTML_File, "&nbsp;<em>" & HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) &
           "</em>&nbsp; <b>does NOT hold.</b><br>");
      else
        Put_Line ("This happens because all paths from " & Configurations.NickName(This_State,"C"));
        Put (" eventually lead to a state in which the subformula ");
        Put_Line(HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & " does NOT hols.");
      end if;
      --
      if HTML_Mode then
        Put_Line(HTML_File, "In particular:<br>");
      else
        Put_Line ("In particular:");
      end if;
      for I in All_Failures'Range loop
        if HTML_Mode then
          -- HTML MODE
          Put (HTML_File, Configurations.NickName(This_State,"C"));
          Put_Line (HTML_File," &nbsp;--&gt; ... &nbsp;--&gt; &nbsp; ");
          Put_Line (HTML_File,
            "<a href='javascript:top.sendcommand(""" & 
            Configurations.NickName(Get_State(All_Failures(I)),"C") & """)'>" &
            Configurations.NickName(Get_State(All_Failures(I)),"C") & "</a><br>");
          Put_Line (HTML_File,
              "&nbsp;&nbsp;&nbsp;<b> and the subformula </b>&nbsp;<em>" & 
              HTML_Format(Get_Formula(All_Failures(I)).Fimage.all) & "</em>&nbsp;");
          Put (HTML_File, "is <spawn style='background-color:pink'>");
          Put_Line (HTML_File,
            "<b><A HREF=""javascript:show('" & 
                 NickName(All_Failures(I)) & "');""" &
             " TITLE='Explain more ...'>Not Satisfied</A>" &
             "</spawn>&nbsp; by </b>" &
             Configurations.NickName(Get_State(All_Failures(I)),"C") & "<br>");
        else
          -- TEXT MODE
          Put ("  " & Configurations.NickName(This_State,"C"));
          Put_Line (" --> ... --> " & 
              Configurations.NickName(Get_State(All_Failures(I)),"C"));
          Put_Line ("    and the subformula """ &  Get_Formula(All_Failures(I)).Fimage.all &
               """ Is Not Satisfied by " &
               Configurations.NickName(Get_State(All_Failures(I)),"C"));
        end if;
      end loop;
      --
      -- Iterates Inside
      for I in All_Failures'Range loop
        if not Already_Explored (All_Failures(I)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,All_Failures(I),CurDepth+1,Form,False,HTML_File);
        end if;
      end loop;
      return;
    end;
  end Explain_EG_False; 


 procedure Explain_AF_True (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
  begin
    if This_Subcomputations'Length =1 then
      -- this is case 1)  subformula immediately true
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
             "<b>This happens because in state " & Configurations.NickName(This_State,"C") & 
             " the subformula:</b><br><em> " &
             HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & "</em>");
        Put_Line(HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:lightgreen'>");
        Put_Line(HTML_File,
             "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
             " TITLE='Explain more ...'>Is Satisfied</b></A>" );
      else
        --  TEXT MODE
        Put_Line ("This happens because in state " & Configurations.NickName(This_State,"C") &
                  " the subformula: "& Get_Formula(This_SubComputations(1)).Fimage.all &
                  " Is Satisfied.");
      end if;
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    --  this is case 2)  subformula valid in all paths
    declare
      All_Failures: Computations_Table :=
         Explain_Tree(RecPath,Cref,This_State,(1..0 => Cref),HTML_File);
    begin
      if HTML_Mode then
        Put_Line(HTML_File, "<b>This happens because all paths from " & Configurations.NickName(This_State,"C") &
           " eventually lead to a state in which the subformula</b><br>");
        Put_Line(HTML_File, "&nbsp;<em>" & HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & 
           "</em>&nbsp; <b>holds.</b><br>");
      else
        Put_Line ("This happens because all paths from " & Configurations.NickName(This_State,"C"));
        Put (" eventually lead to a state in which the subformula ");
        Put_Line(HTML_Format(Get_Formula(This_SubComputations(1)).Fimage.all) & " holds.");
      end if;
      --
      if HTML_Mode then
        Put_Line(HTML_File, "In particular:<br>");
      else
        Put_Line ("In particular:");
      end if;
      for I in All_Failures'Range loop
        if HTML_Mode then
          -- HTML MODE
          Put (HTML_File, Configurations.NickName(This_State,"C"));
          Put_Line (HTML_File," &nbsp;--&gt; ... &nbsp;--&gt; &nbsp; ");
          Put_Line (HTML_File,
            "<a href='javascript:top.sendcommand(""" &
            Configurations.NickName(Get_State(All_Failures(I)),"C") & """)'>" &
            Configurations.NickName(Get_State(All_Failures(I)),"C") & "</a><br>");
          Put_Line (HTML_File,
              "&nbsp;&nbsp;&nbsp; and the subformula </b>&nbsp;<em>" &
              (HTML_Format(Get_Formula(All_Failures(I)).Fimage.all)) & "</em>&nbsp;");
          Put (HTML_File, "is <spawn style='background-color:lightgreen'>");
          Put_Line (HTML_File,
            "<b><A HREF=""javascript:show('" &
                 NickName(All_Failures(I)) & "');""" &
             " TITLE='Explain more ...'>Is Satisfied</b></A>" &
             "</spawn>&nbsp; by " &
             Configurations.NickName(Get_State(All_Failures(I)),"C") & "<br>");
        else
          -- TEXT MODE
          Put ("  " & Configurations.NickName(This_State,"C"));
          Put_Line (" --> ... --> " &
              Configurations.NickName(Get_State(All_Failures(I)),"C"));
          Put_Line ("    and the subformula """ &  Get_Formula(All_Failures(I)).Fimage.all &
               """ Is Satisfied by " &
               Configurations.NickName(Get_State(All_Failures(I)),"C"));
        end if;
      end loop;
      --
      -- Iterates Inside
      for I in All_Failures'Range loop
        if not Already_Explored (All_Failures(I)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,All_Failures(I),CurDepth+1,Form,False,HTML_File);
        end if;
      end loop;
      return;
    end;
  end Explain_AF_True;


  -------------------------------------------
  --   FORM =  AF {act} $FORM1
  -------------------------------------------
 procedure Explain_AFact_True (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
  begin
    ------------------------------------
    --  AF {act} FORM is a syntactic sugar for  A[ true {true} U {act} FORM1]
    ------------------------------------
    -- FORM =  AF {act}  FORM1
    --  for each possible transition t: source -> target {labels}
    --   either  labels satify act and FORM1 is True  in  target
    --   or  labels do satify act, FORM1 is false in target but AF {act} FORM1 is True in target
    --   or  labels do not satify act but AF {act} FORM1 is True in target
    ------------------------------------
    --
    --  recursive formula valid in all paths
    --
    declare
      All_Failures: Computations_Table :=
         Explain_Tree(RecPath,Cref,This_State,(1..0 => Cref),HTML_File);
    begin
      if HTML_Mode then
        Put_Line(HTML_File, 
          "<b>This happens because all paths starting from this state eventually <br>" &
            "incur in a transition whose label satisfies "" </b>");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, """ <br><b> and whose target satisfies the subformula</b> <br>""" &
                   HTML_Format(Form.Pref.U2FormRef2.Fimage.all) & """.");
      else
        Put_Line ("This happens because all paths starting from this state eventually ");
        Put ("incur in a transition whose label satisfies """);
        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2); 
        Put_Line (""""); 
        Put_Line (""" and whose target satisfies the subformula """ &
                   Form.Pref.U2FormRef2.Fimage.all & """.");
      end if;
      --
      --  possibly show the particular successfull transition found.
      --
      return;
    end;
  end Explain_AFact_True;

  -------------------------------------------
  --   FORM =  AF {act} $FORM1
  -------------------------------------------
  -- 1)  State is FINAL
  -- 2)  There is a path leading to a final state
  -- 3)  There is a loop
  ----------------------------------------------
 procedure Explain_AFact_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    My_Iter: Evolutions_Iterator;
    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    --  CASE 0)  FINAL STATE   
    --  CASE 1)  IMMEDIATE {not act} LOOP  and no Recursion
    --  CASE 2)  LONG LOOP {not act}       and no Recursion
    --  CASE 3)  IMMEDIATE RESULT {act} nor form2  and no Recursion
    --  CASE 4)  DELAYED RESULT    ( .. ... {not act} FORM --> true ) and no Recursion
    
    -- 1) State is FINAL
    Iterator_Initialize (My_Iter, This_State);
    if not Has_System_Transition (My_Iter)  then
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
             "<b>This happens because the state has no evolutions.</b><br>"); 
      else
        --  TEXT MODE
        Put_Line ("This happens because the state has no evolutions.");
      end if;
      Iterator_Finalize(My_Iter);
      return;
    end if;
    Iterator_Finalize(My_Iter);
    --
    -- 2)  3) there is a loop or a finite bad path
    if HTML_Mode and Form.Pref.U2FormRef2 /= True_Formula then
        -- HTML MODE
        Put_Line(HTML_File,
             "<b>This happens because there is at least one maximal path from </b>" &
              Configurations.NickName(This_State,"C") & 
              " <b>in which all transitions either have the label which does NOT satisfy the action axpression <i>" );
        Put(HTML_File,UCTL_Utilities.Aimage(Form.PRef.U2ARef2));
        Put_Line(HTML_File, "<i/>");
        Put_Line(HTML_File, " or have the target which does NOT satisfy the subformula <i>" &
        HTML_Format(Form.Pref.U2FormRef2.Fimage.all) & "</i>.</b><br>");
        --
    elsif (not HTML_Mode) and Form.Pref.U2FormRef2 /= True_Formula then
        -- TEXT MODE
        Put_Line("This happens because there is at least one maximal path from " &
               Configurations.NickName(This_State,"C"));
        Put_Line(" in which all transitions either have the label which does NOT satisfy the action expression <i>" &
               UCTL_Utilities.Aimage(Form.PRef.U2ARef2) & "</i>");
        Put_Line ( " or have the target which does NOT satisfy the subformula <i>" &
           Form.Pref.U2FormRef2.Fimage.all & "</i>.</b><br>" );
    elsif HTML_Mode and Form.Pref.U2FormRef2 = True_Formula then
        -- HTML MODE
        Put_Line(HTML_File,
             "<b>This happens because there is at least one maximal path from </b>" &
              Configurations.NickName(This_State,"C") &
              " <b>in which all transitions DO NOT satisfy the action expression <i>" );
        Put(HTML_File,UCTL_Utilities.Aimage(Form.PRef.U2ARef2));
        Put_Line(HTML_File, "</i>.<br>");
    else
        -- TEXT MODE
        Put_Line ("This happens because there is at least one maximal path from " &
              Configurations.NickName(This_State,"C")); 
        Put (" in which all transitions DO NOT satisfy the action expression """ );
        Put(UCTL_Utilities.Aimage(Form.PRef.U2ARef2));
        Put_Line (""".");
    end if;
    if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File, "For example:<br>");
    else
      Put_Line("For example:");
    end if;
    --
    --  si -> sj .. -> sk  and (sk is final)
    --  si -> sj .. -> sk  and (sk closes a loop)
    Explain_Rec_Path(RecPath,Cref, LAST_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File, " is one of the above mentioned failing paths.<br>");
    else
      -- TEXT MODE
      Put_Line(" is one of the above mentioned failing paths.");
    end if;
    return;
  end Explain_AFact_False;


  --  similar to EF_False
  procedure Explain_EUntil1_False  (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Status:  Computation_Status;
  begin
    ------------------------------------------------------------------
    --  EU [ Form1 {act1} U Form2]   == False
    --
    -- At least 1 subcomputation is always present
    -- If ONE subcomputation is present it can only be
    --     * Form2=False (and no successors)
    -- If TWO subcompuations are present it can be
    --     * Form2=False + Form1=False (and some successors)
    --     * Form2=False + Form1=True (and some successors, but all not satisfying Act1 )
    -- If THREE or MORE subcomputations are present it can be
    --     * Form2=False + Form1=True + Until1=False 
    --      (and some successors,  and 1 or more transitions satisfying Act1 )
    ------------------------------------------------------------------
    if This_Subcomputations'Length = 1 then
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is final
        Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef2.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>Does NOT Hold in this State<br>");
        Put (HTML_File, "and there are no outgoing transitions from State ");
        Put_Line (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line (HTML_File, "<br>(The state is final)</b><br>");
        --
        Put_Line (HTML_File,
          "<font color='red'>Notice: further explanations of why the subformula does not" &
          " hold in this state are omitted.</font><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put (Form.Pref.U1FormRef2.Fimage.all & " Does NOT Hold in this State ");
        Put_Line ("and there are no outgoing transitions from State");
        Put_Line (Configurations.NickName(This_State,"C"));
        Put_Line ("(The state is final)");
        --
        Put_Line ("Notice: further explanations of why the subformula does not" &
                  " hold in this state are omitted.");
      end if;
    end if;
    --
    Sub_Status := Get_Status(This_Subcomputations(2)); -- U1ForRef1
    if This_Subcomputations'Length = 2 and then 
        (Sub_Status=FOUND_FALSE or Sub_Status = TmP_FALSE) then
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is final
        Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef2.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>and the subformula:</b><br>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef1.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>Do Not Hold in this State<br>");
        --
        Put_Line (HTML_File,
          "<font color='red'>Notice: further explanations of why the subformulas do not" &
          " hold in this state are omitted.</font><br>");
        -- CAN BE EXPANDED
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put_Line ("    " & Form.Pref.U1FormRef2.Fimage.all); 
        Put_Line ("and the subformula:");
        Put_Line ("    " & Form.Pref.U1FormRef1.Fimage.all);
        Put_line ( " Do Not Hold in this State ");
        --
        Put_Line ("Notice: further explanations of why the subformulas do not" &
                  " hold in this state are omitted.");
        --  CAN BE EXPANDED
      end if;
    end if;
    if This_Subcomputations'Length = 2 and then
        (Sub_Status=FOUND_TRUE or Sub_Status = TMP_TRUE) then
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is final
        Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef2.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>does NOT Hold in this State<br>");
        Put_Line (HTML_File, "and even though the subformula:</b><br>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef1.Fimage.all) & "</em><br>");
        Put_Line (HTML_File, "<b>actually Holds in this State, <br>");
        Put_Line (HTML_File, "none of the possible evolutions from this state<br>");
        Put (HTML_File, "satisfies the action expression </b>"""); 
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, """.<br>");
        --
        Put_Line (HTML_File,
          "<font color='red'>Notice: further explanations of why the subformulas do not" &
          " hold in this state are omitted.</font><br>");
        -- CAN BE EXPANDED
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put_Line (Form.Pref.U1FormRef2.Fimage.all);
        Put_Line ( " does Not Hold in this State ");
        Put_Line ("and even though the subformula:");
        Put_Line (Form.Pref.U1FormRef1.Fimage.all);
        Put_Line ( " actually Holds in this State, ");
        Put_Line ( " none of the possible evolutions from this state ");
        Put ( " satisfies the action expression """);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Put_Line (""".");
        --
        Put_Line ("Notice: further explanations of why the subformulas do not" &
                  " hold in this state are omitted.");
        --  CAN BE EXPANDED
      end if;
    end if;
    --
    if This_Subcomputations'Length > 2 then
      if HTML_Mode then
        -- HTML MODE
        -- No subcomputations  -->  state is final
        Put_Line (HTML_File, "<b>This happens because the subformula:</b>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef2.Fimage.all) & "</em>");
        Put_Line (HTML_File, "<b>does NOT Hold in this State<br>");
        Put_Line (HTML_File, "<b>and even though the subformula:</b>");
        Put_Line (HTML_File,
            "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef1.Fimage.all) & "</em>");
        Put_Line (HTML_File, "<b>actually Holds in this State, <br>"); 
        Put_Line (HTML_File, " and some of the possible evolutions from this state");
        Put (HTML_File, "satisfy the action expression </b>""");      
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, """, there is no path starting from the current state");
        Put_Line (HTML_File, " which will eventually a reach a state satisfying </b><");
        Put_Line (HTML_File, 
             "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef2.Fimage.all) &  "</em><br>");
        Put_Line (HTML_File, ", <b>while all the intermediate states satisfy </b>");
        Put_Line (HTML_File, 
             "<em>&nbsp;&nbsp;&nbsp;" & HTML_Format(Form.Pref.U1FormRef1.Fimage.all) &  "</em><br>");
        Put_Line (HTML_File, " and all path transitions satisfy ");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, ".<br> ");
        --
        Put_Line (HTML_File,
          "<font color='red'>Notice: further explanations of why the subformulas do not" &
          " hold in this state are omitted.</font><br>");
        -- CAN BE EXPANDED
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        Put_Line (Form.Pref.U1FormRef2.Fimage.all);
        Put_Line ( " does NOT Hold in this State ");
        Put_Line ("and even though the subformula:");
        Put_Line (Form.Pref.U1FormRef1.Fimage.all);
        Put_Line ( " actually Holds in this State, ");
        Put_Line ( " and some of the possible evolutions from this state ");
        Put ( " satisfy the action expression "" ");
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Put_Line (""", there is no path starting from the current state");
        Put_Line (" which will eventually a reach a state satisfying ");
        Put_Line (Form.Pref.U1FormRef2.Fimage.all);
        Put_Line (", while all the intermediate states satisfy ");
        Put_Line (Form.Pref.U1FormRef1.Fimage.all);
        Put (" and all path transitions satisfy """);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Put_Line (""".");
        --
--        Put_Line ("Notice: further explanations of why the subformulas do not" &
--                  " hold in this state are omitted.");
        --  CAN BE EXPANDED
      end if;
    end if;
  end Explain_EUntil1_False;

  --  similar to EF_True
  procedure Explain_EUntil1_True  (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    Sub_Form: UCTL_Types.Formula_Ref;
    Sub_Cref: Computation_Step;
    Sub_State: System_Configuration;
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    --Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    ------------------------------------------------------------------
    --  EU [ Form1 {act1} U Form2]   == True
    --
    -- Exactly 1 subcomputation is always present
    -- It can only be
    --    1* Form2=True 
    --    2* (Form2=False) + (Form1=True) + Until1=True + transition satisfying act1
    -- Notice that the explanation of why Form1 is true CANNOT be given !!!
    ------------------------------------------------------------------
    Sub_Form := Get_Formula(This_Subcomputations(1));
    --  
    --  case 1*
    --
    if Previous /= Form and then Form /= Sub_Form  then
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        UCTL_Utilities.Print(Sub_Form, False);
        New_Line;
        Put(" is Satisfied ");
        Put (" in State ");
        Put (Configurations.NickName(This_State,"C"));
        New_Line;
      end if;
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    --  case 2*
    --
    if HTML_Mode then
      Put_Line (HTML_File, "<b>This happens because, after the path </b><br>");
    else
      Put_Line ("This happens because, afyer the path ");
    end if;
    --
    --  si -> sj .. -> sk  and (sk is ok)
    Explain_Rec_Path(RecPath,Cref, SUB_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State(Sub_Cref);
    --
      if HTML_Mode then
        Put_Line (HTML_File,
               "<b><br>the subformula:</b>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is finally ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(Sub_Cref) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(Sub_State,"C"));
        Put_Line(HTML_File, ",</b><br>");
        Put_Line (HTML_File, 
            "<b>while all the intermediate states of the path satisfy the subformula</b>");
        Put_Line (HTML_File,
             "<em>&nbsp;" & HTML_Format(Form.Pref.U1FormRef1.Fimage.all) &  "</em><br>");
        Put_Line (HTML_File, " <b>and all the transitions of the path do satisfy the action expression </b><em>");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Set_Output(Standard_Output);
        Put_Line (HTML_File, "</em>.<br> ");
        --
--        Put_Line (HTML_File,
--          "<font color='red'>Notice: further explanations of why the subformulas do not" &
--          " hold in this state are omitted.</font><br>");
        -- CAN BE EXPANDED

      else
        -- TEXT MODE
        Put_Line ("the subformula:");
        UCTL_Utilities.Print(Sub_Form, False);
        Put(" is finally Satisfied ");
        Put (" in State ");
        Put (Configurations.NickName(Sub_State,"C"));
        New_Line;
        Put_Line ("while all the intermediate states of the path satisfy the subformula");
        Put_Line ("    " & Form.Pref.U1FormRef1.Fimage.all);
        Put (" and all path transitions satisfy """);
        UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
        Put_Line (""".");
      end if;
      --
      if not Already_Explored (Sub_Cref) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,Form,False,HTML_File);
      end if;
      return;
  end Explain_EUntil1_True;


  -- similar to EUntil1_True
  procedure Explain_EWuntil1_True  (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Cref: Computation_Step;
    Sub_Form: UCTL_Types.Formula_Ref;
    Sub_State: System_Configuration;
    Rec_Cref: Computation_Step;
  begin
    ------------------------------------------------------------------
    --  EU [ Form1 {act1} W Form2]   == True
    --
    -- Exactly 1 subcomputation is always present
    -- It can only be
    --    1* Form2=True
    --    2* Form1=True (Form2=any) and no successors
    --    3* (Form2=False) + (Form1=True) + Wuntil1=True + transition satisfying act1
    -- Notice that the explanation of why Form1 is true CANNOT be given !!!
    ------------------------------------------------------------------
    Sub_Form := Get_Formula(This_Subcomputations(1));  
    -- SubForm can be either Form1, or Form2 or Form itself_
    if Sub_form=Form.PRef.U1FormRef1 then  
          -- case 2*
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in the current state </b><br>");
        Put_Line(HTML_File, "<b>and the current state is FINAL. <br>");
      else
        --  TEXT MODE
        Put("This happens because the subformula:""");
        UCTL_Utilities.Print(Sub_Form, False);
        -- New_Line;
        Put(""" is Satisfied in the current state ");
        New_Line;
        Put_line("and the current state is FINAL.");
      end if;
      --
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
      --
    if Sub_Form = Form.PRef.U1FormRef2 and 
             Get_Status(This_Subcomputations(1)) = FOUND_TRUE then   
          -- case 1*  (no recursion)
    -- TO BE ADJUSTED !!=!!=
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>");
        Put (HTML_File, " in State ");
        Put (HTML_File, Configurations.NickName(This_State,"C"));
        Put_Line(HTML_File, "</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because the subformula:");
        UCTL_Utilities.Print(Sub_Form, False);
        New_Line;
        Put(" is Satisfied ");
        Put (" in State ");
        Put (Configurations.NickName(This_State,"C"));
        New_Line;
      end if;
      --
      if not Already_Explored (This_SubComputations(1)) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    -- Form = SubForm  (i.e. we are inside the recursion)
    --    case 3*
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File, "<b>This happens because, in the path:</b><br>");
    else
      Put_Line("This happens because, in the path:");
    end if;
    --
    --  si -> sj .. -> sk 4a (sk is ok)     formula(Last_Cref) = U1FormRef2
    --  si -> sj .. -> sk 4b (sk is final)  formula(Last_Cref) = U1FormRef1
    --  si -> sj .. -> sk 4c (sk closes a loop) formula(Last_Cref) = Form
    Explain_Rec_Path(RecPath,Cref,SUB_Cref,REC_Cref, Empty_System_Configuration_Table, HTML_File);
    --  NOW WE HAVE PROBLEMS IN UNTIL DOES NOT PERFORM A RECURSIVE SET_COMPUTATION .....
    -- ???? 
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State (Sub_Cref);
    --
    --  3a   after several steps we have a state for which form2=True
    -- 
    if Sub_Form = Form.PRef.U1FormRef2 and Get_Status(Sub_Cref) = FOUND_TRUE then
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
          "<br><b>all the states of the path do satisfy the state subformula:</b><em> " &
          HTML_Format(Form.Pref.U1FormRef1.Fimage.all) & "</em>,<br>");
        Put (HTML_File, "<b>all transitions of the path do satisfy the action expression</b><em>" );
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
        Set_Output(Standard_Output);
        Put_line(HTML_File,"</em>,<br>");
        Put_Line(HTML_File, "<b>and in the last state of the path the subformula:</b><br>");
        Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
        Put (HTML_File, "<b><br>&nbsp;");
        Put (HTML_File, " <spawn style='background-color:lightgreen'> is ");
        Put_Line(HTML_File,
               "<A HREF=""javascript:show('" & NickName(Sub_Cref) & "');""" &
               " TITLE='Explain more ...'>Satisfied</A>" );
        Put_line(HTML_File, "</spawn>.<br>");
      else
        -- TEXT MODE
        New_Line;
        Put_line ("all the states of the path do satisfy the state subformula: """ &
            Form.Pref.U1FormRef1.FImage.all & """ ,");
        Put("all transitions of the path do satisfy the action expression """ );
        UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
        Put_Line (""",");
        Put_Line("and in the last state of the path the subformula: """ &
             Sub_Form.Fimage.all & """ is Satisfied.");
      end if;
      --
      if not Already_Explored (Sub_Cref) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
    if Sub_Form = Form then
      if HTML_Mode then
        -- HTML MODE
          Put_Line(HTML_File,
            "<br><b>all the states of this infinite path do satisfy the state subformula:</b><em> " &
            HTML_Format(Form.Pref.U1FormRef1.Fimage.all) & "</em>,<br>");
          Put_Line(HTML_File, "<b>and all transitions of the path do satisfy the action expression</b><em>" );
          Set_Output(HTML_File);
          UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
          Set_Output(Standard_Output);
          Put_line(HTML_File,"</em>.<br>");    
       else
         -- TEXT MODE
        Put_line ("all the states of this infinite path do satisfy the state subformula: """ &
            Form.Pref.U1FormRef1.FImage.all & """ ,");
        Put_Line("all transitions of the path do satisfy the action expression""" );
         UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
         Put_Line (""".");
       end if;
      --
      if not Already_Explored (Sub_Cref) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
     --
    if Sub_Form = Form.PRef.U1FormRef1 then
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
          "<br><b>all the states of this maximal (finite) path do satisfy the state subformula:</b><em> " &
          HTML_Format(Form.Pref.U1FormRef1.Fimage.all) & "</em>,<br>");
        Put_Line(HTML_File, "<b>and all transitions of the path do satisfy the action expression</b><em>" );
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
        Set_Output(Standard_Output);
        Put_line(HTML_File,"</em>.<br>");    
         Put_Line ("<b>and the last state of the path is FINAL.</b><br>");
       else
        -- TEXT MODE
        Put_line ("all the states of this maximal (finite) path do satisfy the state subformula:""" &
            Form.Pref.U1FormRef1.FImage.all & """,");
        Put_Line("and all transitions of the path do satisfy the action expression</b>""" );
        UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
        Put_Line (""",");
        Put_Line ("and the last state of the path is FINAL.");
      end if;
      --
      if not Already_Explored (Sub_Cref) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
  end Explain_EWuntil1_True;

  procedure Explain_EUntil2_False  (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is

    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
  begin
     -----------------
     --  A: lo stato e' finale.
     --  B: in S Form1 e' False, e non esistono transizioni che soddisfano act2
     --  C: in S Form1 e' False, e le transizioni che soddisfano act2 non soddisfano form2
     --  C: in S Form1 e' True, S non e' finale, ma nessun next state viene raggiunto con act1
     --  D: in S Form1 e' True, S non e' finale, ma in tutti i next states raggiunti con act1 
     --     L'Until e'ricorsivamente  False, o perche' ogni path in cui Form1/act1 e' true  
     --      chiude un loop o perche conduce in uno stato in cui la ricorsione non puo' essere
     --     terminata.
     -- Se form1="true" posso avere solo i casi C e D (e la formula puo' essere un EF#
     -----------------
     --  Nota: se form1="true" non viene valutata (e non compare nelle subcomputations)
     -----------------
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          if not Has_System_Transition (My_Iter)  then
            if HTML_Mode then
              -- HTML MODE
              Put_Line (HTML_File, "<b>This happens because there is no evolution <br>");
              Put_Line (HTML_File, "starting from State" &
                                  Configurations.NickName(This_State,"C") &
                                 "<br> (The state is final)</b>");
            else
              --  TEXT MODE
              Put_Line ("This happens because there is no evolution ");
              Put ("starting from State ");
              Put (Configurations.NickName(This_State,"C"));
              New_Line;
              Put ("(The state is final)");
              New_Line;
            end if;
          end if;
          Iterator_Finalize (My_Iter); 
        end;
  end Explain_EUntil2_False;

  procedure Explain_EUntil2_True  (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Form : Formula_Ref;
    Sub_State: System_Configuration;
    Sub_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    ------------------------------------
    --  EF {act} FORM is a syntactic sugar for  E[ true {true} U {act} FORM1]
    ------------------------------------
    -- FORM =  EF {act}  FORM1
    --   either  <act> FORM1 is True  in "This_State" ( 1 Subcomputation)
    --   or  <act> FORM1 is False  in "This_State"
    --           but  "EF {act} FORM1" is True in a substate.
    ------------------------------------
    --  Subcomputations'Length = 0  ==> (actleft -> False) (not posible) or else
    --  Subcomputations'Length = 0  ==>     State is FINAL (not possible)
    --  Subcomputations'Length = 1 ==>  (formleft -> False) (not posible) or else
    --1 Subcomputations'Length = 1 ==>  formleft=true (not evaluated) <act> FORM --> true  or else
    --2 Subcomputations'Length = 1 ==>  formleft=true (not evaluated)  <act> FORM --> false
    --                                       EF {act} FORM --> true for some descendent
    ------------------------------------
    --
    if Previous /= Form then    --  first call from other computation
    if HTML_Mode then
      -- HTML MODE
        if not Skip_Header then
            Put_Line (HTML_File, "<b>This happens because</b><br> ");
          end if;
    else
        if not Skip_Header then
          Put_Line ("This happens because ");
        end if;
      end if;
    end if;
    --
    --  CASE 1)  {act} FORM --> true   (immediately, recdepth=0)
    --
    Explain_Rec_Path(RecPath,Cref,SUB_Cref,REC_Cref, Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State(Sub_Cref);
    --
    if Sub_Form /=  Form then    -- last recursive call
       -- not the recursive case
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File,
          "<b> the last transition label satisfies the action expression</b><em> ");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
        Set_Output(Standard_Output);
        Put (HTML_File, "</em><br>");
        if Get_Formula(Sub_Cref) /= True_Formula then
          Put_Line (HTML_File, "<b> and in State </b> " &
               Configurations.NickName(Sub_State,"C") &
              " <b>the subformula</b>&nbsp;&nbsp;<br><em> " &
               HTML_Format(Get_Formula(Sub_Cref).Fimage.all) & "</em>");
          Put_Line (HTML_File, "&nbsp;&nbsp;<b><spawn style='background-color:lightgreen'>");
          Put_Line(HTML_File,
                     "<A HREF=""javascript:show('" & NickName(Sub_Cref) & "');""" &
                     " TITLE='Explain more ...'>Is Satisfied</b></A>" );
          Put_Line (HTML_File, "</spawn>.</b><br>");
        end if;
    else
      -- TEXT MODE
        Put ("the last transition label satisfies the action expression ");
        UCTL_Utilities.Print_Action(Form.PRef.U2ARef2);
        New_Line;
        if Get_Formula(Sub_Cref) /= True_Formula then
          Put ("and in State " & Configurations.NickName(Get_State(Sub_Cref),"C") &
             " the subformula: " & Get_Formula(Sub_Cref).Fimage.all );
          Put_Line ("  Is Satisfied.");
        end if;
      end if;
      if not Already_Explored (Sub_Cref) then
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
        Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    --
  end Explain_EUntil2_True;

  procedure Explain_EWUntil2_True  (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    This_Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Form : Formula_Ref; 
    Sub_State: System_Configuration;
    Sub_Cref : Computation_Step;
--    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    -- The Form=E[ f1 {a1} W {a2} f2]  is TRUE when:
    --
    -- 1)  f1 is true (necessarily)  and either:
    --   a) state is FINAL
    --   b) an outgoing transition directly satisfies {a2} and target state satisfies f2 
    --   c) an outgoing path fully satisfies {a1} and the last state satisfies Form (recursively)
    --      (c1) either by closing loop of {a1} or 
    --      (c2) by reaching a transition that satisfies {a2} and whose target state satisfies f2
    --
    --   E[true {ko} W {ko} false]  (TRUE when FINAL)
    --   E[p(0) {ko} W {ko} false]  (TRUE when FINAL)
    --   
    --  si -> sj .. -> sk  and (sk is ok)
    Explain_Rec_Path(RecPath,Cref, SUB_Cref,REC_Cref, Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State(Sub_Cref);
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File,
        " More Details not yet supported in this version...<br> ");
    else
      -- TEXT MODE
      if Sub_Cref = This_Subcomputations(1) then
        Put_Line ("This happens because the subformula " & 
           Sub_Form.fimage.all & " holds in state " & Configurations.NickName(This_State,"C"));
        Put (" and this state can evolve with a transition which satisfies the action expression {");
        UCTL_Utilities.Print_Action(This_Form.PRef.U2ARef2);
        Put_Line ("} into state " &  Configurations.NickName(Sub_State,"C") & ",");
        Put_Line (" and in this target state the formula " &
                   This_Form.PRef.U2FormRef2.fimage.all & " holds.");
      else
        Put_line ("More Details not yet supported in this version...");
      end if;
    end if;
    --
    if not Already_Explored (Sub_Cref) then
        if HTML_Mode then 
            Put_Line(HTML_File, "</div>"); 
        end if;
        Text_Explain_Computation(RecPath,Sub_Cref,CurDepth+1,This_Form,False,HTML_File);
    end if;
  end Explain_EWUntil2_True;

  ----------------------------------------------------
  --  chiamata da Fapply   quando viene valutato un nome ID di fixpoint 
  --    precedentemente dichiarato da max ID:  Subform
  --  Se e' True vuol dire che  o la subformula e' diventata in un certo stato definitivamente True
  --   oppure che durante la sua valutazione si e' creato un loop, anche se l'interna valutazione e' 
  --  ancora da completare  
  --  (nel caso di Tail recursion il caso di loop diventa una valore definitivamente true)
  --  E.g.    max Z: ((<true> Z)  and False)  .. se si valuta prima <true> ... Z diventa TMP_TRUE nel punto
  --  E.g.    max Z: <true> (Z and False)  .. se si valuta prima <true> ... Z diventa TMP_TRUE nel punto
  --  in cui si incontra il loop indietro, come pure <true> Z,  ma poi diventa FOUND_FALSE quando la
  --  valutazione viene terminata.
  --  Nel caso Tail recursive: 
  --  E.g.    max Z: (False and (<true> Z))  la ricorsione  non la avremmo neanche iniziata.
  --  Nel caso monotono :
  --  E.g.    max Z: ((<true> Z)  or ... ) Z puo' diventare FOUND_TRUE nel punto  di loop back.
  -----------------------------------------------------
  -- Cref  e' la computazione di una fApply "ID" (dove ID e' un identificatore di punto fisso)
  -----------------------------------------------------
  procedure Explain_FixPoint (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    Loop_State: System_Configuration;
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Status: Computation_Status := Get_Status(Cref);
    -- Sub_Form: UCTL_Types.Formula_Ref;
    Closing_Loop: Natural := 0;
  begin
     ---------------------------------------------------------
     -- PROLOGUE BY TEXT_EXPLIAN_COMPUTATION
     -- The formula XXX 
     -- which refers to the formula  max XXX:<body> evaluated in State YYY
     --  is Found True in State SSS
     ---------------------------------------------------------
    --
    -- This_Subcomputations(1)  il the computation of the corresponding max/min fix point in the
    --  same current state. Let's check if such computation is actually closing a fixpoint loop
    --    or if such computation is just a new nesting level of fixpoint.
    --
    for I in RecPath'Range loop
      if RecPath(I) = This_Subcomputations(1) then
        Closing_Loop := I;
        exit;
      end if;
    end loop;
    if Closing_Loop > 0 then
      Loop_State := Get_State(This_Subcomputations(1)); 
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, 
          "<b>This happens because the evaluation of the fixpoint variable </b> " &
          Form.Fimage.all & " <b> closes a fixpoint loop started in State </b>" &
          Configurations.NickName(Loop_State,"C") & ".<br> ");
        Put (HTML_File, "( ");
        for J in Closing_Loop..RecPath'Last loop
          Put (HTML_File, Configurations.NickName(Get_State(RecPath(J)),"C"));
          Put (HTML_File," -&gt; ");
        end loop;
        Put (HTML_File, Configurations.NickName(Loop_State,"C"));
        Put_Line (HTML_File," )");
      else
        --  TEXT MODE 
        Put_Line ("This happens because the evaluation of the fixpoint variable " &
           Form.Fimage.all); 
        Put_line (" closes a fixpoint loop started in State " & 
          Configurations.NickName(Loop_State,"C") & ".");
        Put ("( ");
        for J in Closing_Loop..RecPath'Last loop
          Put (Configurations.NickName(Get_State(RecPath(J)),"C"));
          Put (" -> ");
        end loop;
        Put (Configurations.NickName(Loop_State,"C"));
        Put_Line (" )");
      end if;
    else
      Text_Explain_Computation(RecPath,This_SubComputations(1),CurDepth+1,Form,True,HTML_File);
    end if;
    return;
  end Explain_FixPoint;

  --------------
  procedure Explain_AWuntil1_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Form : Formula_Ref; 
    Sub_State: System_Configuration;
    Sub_Cref : Computation_Step;
--    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    --
    --  si -> sj .. -> sk  and (sk is ok)
    Explain_Rec_Path(RecPath,Cref, SUB_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State(Sub_Cref);
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File,
        "More Details not yet supported in this version...<br> ");
    else
      -- TEXT MODE
      Put_line ("More Details not yet supported in this version...");
    end if;         
  end Explain_AWuntil1_False;
  
  procedure Explain_AWuntil2_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    --Sub_Form : Formula_Ref; 
    --Sub_State: System_Configuration;
    Sub_Cref : Computation_Step;
    --This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    --This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    --Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    --         form = A[ form1 {act1} W {act2} form2]   is  FALSE when
    --
    --     A)  form1 is FALSE.
    -- or  B)  form1 is TRUE and 
    --            (exists a transition that 
    --                 does not satisfy {act1}, and 
    --                  (does not satisfy act2 or the target does not satisfy form2))
    -- or  C)  form1 is TRUE and 
    --            (exists a transition that satisfies act1 (but not act2 and form2), 
    --               but recursion does not hold 
    --                because the last state of Rec_Path is in situation A or B
    --
    Explain_Rec_Path(RecPath,Cref,SUB_Cref,REC_Cref, Empty_System_Configuration_Table, HTML_File);
    -- NOTICE Sub_Cref= null when This_State is FINAL
    --
    -- Sub_Form := Get_Formula(Sub_Cref);
    -- Sub_State := Get_State(Sub_Cref);
      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
           "More Details not yet supported in this version...<br> ");
        --
      else 
        -- TEXT MODE
        if Sub_Cref = Cref or else 
              (Get_Formula(Sub_Cref) = Form.PRef.U2FormRef1 and then
               Get_State(Sub_Cref) = This_State) then 
          Put_Line ("This happens because the subformula: ");
          Put_line(Form.PRef.U2FormRef1.Fimage.all);
          Put (" is not Satisfied ");
          Put (HTML_File, " in the current state.");
          --
        elsif Get_Formula(Sub_Cref) = Form.PRef.U2FormRef1 then
          -- A[ false {aa} W {true} true] 
          Put_Line ("This happens because the subformula: ");
          Put (" is not Satisfied "); 
          Put (" in the last state of the path.");
--          Put (" and no more transitions satisfy " & 
--             Form.PREF.U2ARef1.image.all & " or " & 
--             Form.PREF.U2ARef2.image.all & " are possible.");
          --
        elsif Get_Formula(Sub_Cref) = Form.PRef.U2FormRef2 then 
          Put_Line ("This happens because the subformula: ");
          Put (" is not Satisfied "); 
          Put (" in the last state of the path.");
        end if;
-- Put_line ("More Details not yet supported in this version...");
    end if;
  end Explain_AWUntil2_False;


  procedure Explain_AUntil1_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    Sub_Form : Formula_Ref; 
    Sub_State: System_Configuration;
    --Sub_Cref : Computation_Step;
    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
    --
    My_Iter: Evolutions_Iterator;
  begin
    if HTML_Mode then
      -- HTML MODE
      Put_Line (HTML_File, "<b>This happens because</b><br>");
      else
        --  TEXT MODE
        Put_Line ("This happens because ");
    end if;
    --  si -> sj .. -> sk  and (sk is ok)
    --  Rec_Cref.State==FINAL;      Last_Cref:= Cref;      when FINAL with no subcomputations 
    --  Rec_Cref.State==FINAL;      Last_Cref.Form==Form2 when FINAL
    --  Rec_Cref.Form==Form         Last_Cref==Rec_Cref    when LOOP
    --  Rec_Cref.State!==FINAL;     Last_Cref.Form==Form1 when aborted PATH (Form1=false)
    --  Rec_Cref.State!==FINAL;     Last_Cref.Form==Form2 when aborted PATH Form1=true,(Form2=False) no more steps,
    --  Rec_Cref==initial_Cref      Last_CRef/=initial_Cref  when NO PATH
    --
    Explain_Rec_Path(RecPath,Cref, LAST_Cref,REC_Cref,Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Last_Cref);
    Sub_State := Get_State(Last_Cref);
    --
    --
    -- get_State(Cref) == FINAL
    -- 
    Iterator_Initialize (My_Iter, This_State);
    if not Has_System_Transition (My_Iter)  then
      -- (the state if is FINAL is already notified by Explain_Rec_Path)
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, " <b> and the subformula:</b><br>" &
             HTML_Format(Form.Pref.U1FormRef2.FImage.all) );
        Put_Line (HTML_File, "<spawn style='background-color:pink'> is " );
        Put_Line(HTML_File,
                     "<A HREF=""javascript:show('" & NickName(LAST_Cref) & "');""" &
                     " TITLE='Explain more ...' >Not Satisfied</A>" );
      else
         --  TEXT MODE
         Put_Line (" and the subformula: ");
         Put_Line (Form.Pref.U1FormRef2.FImage.all & " Is Not Satisfied in this state.");
      end if;
      Iterator_Finalize (My_Iter); 
      -- WE SHOULD CONTINUE WITH WHY U1FormRef2 DOES NOT HOLD HERE !!
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,Last_Cref,CurDepth+1,Form,False,HTML_File);
      return;
    end if;
    Iterator_Finalize (My_Iter); 
    
    --
    -- NO PATH FROM get_State(Cref) 
    --   because Form1 False (and Form2 False)
    --
    if Rec_Cref=Cref and then 
         Sub_Form=Form.Pref.U1FormRef1 and then 
         Get_Status(Last_Cref)=FOUND_FALSE then
       --  both  Form1 and Form2 are False in this state
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,"<b> both </b>" &  HTML_Format(Form.Pref.U1FormRef2.FImage.all) &
            " <b>and the subformula:</b><br>" & HTML_Format(Form.Pref.U1FormRef1.FImage.all) );
            Put_Line (HTML_File, "<spawn style='background-color:pink'> is " );
            Put_Line(HTML_File,
                     "<A HREF=""javascript:show('" & NickName(LAST_Cref) & "');""" &
                     " TITLE='Explain more ...' >are Not Satisfied</A>" ); 
            Put_Line(HTML_File, "</div>");
           Text_Explain_Computation(RecPath,LAST_Cref,CurDepth+1,Form,False,HTML_File);
      else
        --  TEXT MODE
        Put_Line (" both " & Form.Pref.U1FormRef1.FImage.all & " and the subformula: ");
        Put_Line (Form.Pref.U1FormRef2.FImage.all & " are Not Satisfied in this state.");
        Text_Explain_Computation(RecPath,LAST_Cref,CurDepth+1,Form,False,HTML_File);
      end if;
      return;
    end if;
    
    --
    -- NO PATH FROM get_State(Cref) 
    --   because some action not satisfying act1 even if Form1 True (and Form2 False)
    --
    --  if Rec_Cref=Cref and then 
    --     Sub_Form=Form.Pref.U1FormRef1 and then 
    --        Get_Status(Sub_Form)=FOUND_TRUE then
    --
    --  currently when some action not satisfying act1 is found, SubCref is set to Form2
    -- 
    if Rec_Cref=Cref and then Sub_Form= Form.Pref.U1FormRef2 then
      --   both  Form1 and Form2 are False in this state
      if HTML_Mode then
        -- HTML MODE
        Iterator_Initialize (My_Iter, This_State);
        while Has_System_Transition (My_Iter) loop
          Put_Line (HTML_File,
             "&nbsp;&nbsp;&nbsp;" & "<a href='javascript:top.sendcommand(""" &  
             Configurations.NickName(This_State,"C") & """)'>" & 
             Configurations.NickName(This_State,"C") & "</a>" &
             " --&gt; " &
             Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
             "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter)  & "<br>");
           Iterator_Advance(My_Iter);
        end loop;
        Iterator_Finalize (My_Iter);
        Put (HTML_File, "<b>From this state there are evolutions which do not satisfy the action expression </b><em>");
        Set_Output(HTML_File);
        UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
        Set_Output(Standard_Output);
        Put_line (HTML_File, "</em> <b>and the subformula </b><em> " &
                   HTML_Format(Form.Pref.U1FormRef2.FImage.all) & " </em> "); 
        Put_Line (HTML_File, "<b><spawn style='background-color:pink'> is " );
        Put_Line(HTML_File,
                   "<A HREF=""javascript:show('" & NickName(LAST_Cref) & "');""" &
                   " TITLE='Explain more ...' >Not Satisfied</A></b>" );
      else
        -- TEXT MODE
        Iterator_Initialize (My_Iter, This_State);
        while Has_System_Transition (My_Iter) loop
           Put ("  " & Configurations.NickName(This_State,"C") &
                " --> " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "  " & Abstract_Action_Labels(My_Iter) );
           New_Line;
        end loop;
        Iterator_Finalize (My_Iter);
        Put_Line ("From this state there are evolutions which do not satisfy the action expression ");
        UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
        Put_line (",");
        Put_line (" and the subformula: " &
                   Form.Pref.U1FormRef2.FImage.all & 
                   " Is Not Satisfied in this state.");
      end if;
      -- WE SHOULD CONTINUE WITH WHY U1FormRef2 DOES NOT HOLD HERE !!
      if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      Text_Explain_Computation(RecPath,Last_Cref,CurDepth+1,Form,False,HTML_File);
      return;
    end if;
    --
    -- REC_Cref  /= Cref  i.e. we have a path
      --
      if HTML_Mode then
        Put_Line(HTML_File,
          "<br><b>In ALL the states of the above path the subformula </b> <em> " &
          HTML_Format(Form.Pref.U1FormRef2.FImage.all) & "</em><b> Is NOT Satisfied </b><br>");
        declare
          Last_Rec_State: System_Configuration := Get_State(REC_Cref);
        begin
          -- the above path is maximal because:
          --  either last state is final (and form2 is false)(alredy notified by Explain_Rec_Path)
          --   or in last state form1 is false (and form2 is false)
          --   or in last state form2 is false (and there a transition which does not satisfy {act1})
          -- (notice maybe here is not path is laststate=current state, 
          --       i.e. REC_Cref=Cref and  LAST_CREF/=Cref)
          Iterator_Initialize (My_Iter, Last_Rec_State);
            if Has_System_Transition (My_Iter) then
               if Get_Formula(LAST_Cref)=Form.Pref.U1FormRef1 then
                 Put_Line(HTML_File,
                 "<b> and in the last state " & Configurations.NickName(Last_Rec_State,"C") &
                 " the formula </b><i>" &
                 Form.Pref.U1FormRef1.fimage.all & " </i><b>");
                 Put_Line (HTML_File, "&nbsp;&nbsp;&nbsp<b><spawn style='background-color:pink'>");
    			   Put_Line(HTML_File,
                   "<A HREF=""javascript:show('" & NickName(LAST_Cref) & "');""" &
                   " TITLE='Explain more ...'>does no longer hold</A>" );
                 Put_Line (HTML_File, "</spawn>.</b><br>");
                 --
                 -- WE COULD CONTINUE WITH WHY U1FormRef1 DOES NOT HOLD IN THE LAST STATE OF THE PATH !!
                 Put_Line(HTML_File, "</div>");
                 Text_Explain_Computation(RecPath,Last_Cref,CurDepth+1,Form,False,HTML_File);
               else
                 Put_Line(HTML_File,
                 "(and from the last state " &
                     Configurations.NickName(Last_Rec_State,"C") &
                 " is possible a further transition which does not satisfy the action expression {");
                 Set_Output(HTML_File);
                 UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
                 Set_Output(Standard_Output);
                 Put_Line(HTML_File, " })");
               end if;
            end if;
          Iterator_Finalize (My_Iter);
        end;
      else
        -- TEXT MODE
        Put_Line ("In ALL states of the above path the subformula: " &
          -- maximal {");
          -- UCTL_Utilities.Print_Action(Form.Pref.U1ARef);
          -- Put_line (" } path the subformula: " &
            Form.Pref.U1FormRef2.FImage.all & " Is NOT Satisfied,");
        declare
          Last_Rec_State: System_Configuration := Get_State(REC_Cref);
        begin
          -- the above path is maximal because:
          --  either last state is final (and form2 is false)(alredy notified by Explain_Rec_Path)
          --   or in last state form1 is false (and form2 is false)
          --   or in last state form2 is false (and there a transition which does not satisfy {act1})
          Iterator_Initialize (My_Iter, Last_Rec_State);
            if Has_System_Transition (My_Iter) then
               if Get_Formula(LAST_Cref)=Form.Pref.U1FormRef1 then
                 Put_Line(
                 "and in the last state " & Configurations.NickName(Last_Rec_State,"C") &
                 " the formula """ &
                 Form.Pref.U1FormRef1.fimage.all & """ does no longer hold.");
                 -- WE COULD CONTINUE WITH WHY U1FormRef1 DOES NOT HOLD IN THE LAST STATE OF THE PATH !!
                 -- Text_Explain_Computation(RecPath,Last_Cref,CurDepth+1,Form,False,HTML_File);
               else
                 Put(
                 "(and from the last state " &
                     Configurations.NickName(Last_Rec_State,"C") &
                 " is possible a further transition which does not satisfy the action expression {");
                 UCTL_Utilities.Print_Action(Form.PRef.U1ARef);
                 Put_Line(" })");
               end if;
            end if;
          Iterator_Finalize (My_Iter);
        end;
      end if;
      
      -- WE COULD CONTINUE WITH WHY U1FormRef2 DOES NOT HOLD IN THE LAST STATE OF THE PATH !!
      -- (but Last_Cref can be the same as Cref in the case of a loop)
      -- if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      -- Text_Explain_Computation(RecPath,Last_Cref,CurDepth+1,Form,False,HTML_File);
  end Explain_AUntil1_False;

    procedure Explain_AUntil2_False (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    Sub_Form : Formula_Ref; 
    Sub_State: System_Configuration;
    Sub_Cref : Computation_Step;
--    Last_Cref: Computation_Step;
    Rec_Cref: Computation_Step;
  begin
    --
    --  si -> sj .. -> sk  and (sk is ok)
    Explain_Rec_Path(RecPath,Cref,SUB_CRef,REC_Cref, Empty_System_Configuration_Table, HTML_File);
    Sub_Form := Get_Formula(Sub_Cref);
    Sub_State := Get_State(Sub_Cref);
    --
    if HTML_Mode then
      -- HTML MODE
      Put_Line(HTML_File,
        "More Details not yet supported in this version...<br> ");
    else
      -- TEXT MODE
      Put_line ("More Details not yet supported in this version...");
    end if;         
  end Explain_AUntil2_False;
  ----------------------------------------------------------------
  --  called both the HTML_Explain_Computation and Explain_Computation  
  --  depending on HTML mode prints the explanation of computation Cref
  --
  -- Skip_Header is TRUE  is we are inside a recursive explanation of a formula
  -- BUG   AF AG 
  --------------------
  --  Start_Exploration increases an internal Explorations_Count inside Computations_DB
  --  Already_Explored(Cref) return False the first time is called on a Cref (for this Exploration).
  --  Needed to avoid multiple repetitions of the same explanation
  ----------------------------------------------------------------
  --   RATIONALE TO BE DESCRIBED ...
  ----------------------------------------------------------------
  procedure Text_Explain_Computation (RecPath:Computations_Table;
                                 Cref: Computation_Step;
                                 CurDepth:Positive;
                                 Previous: Formula_Ref;
                                 Skip_Header: Boolean := False;
                                 HTML_File: File_Type) is
    Form: UCTL_Types.Formula_Ref := Get_Formula(Cref);
    This_Context: Computations_Table := Get_Context(Cref);
    This_State: System_Configuration := Get_State(Cref);
    This_Subcomputations: Computations_Table := Get_Subcomputations(Cref);
    This_Subevolutions: Evolutions_Table := Get_Subevolutions(Cref);
    This_Status: Computation_Status := Get_Status(Cref);
    Sub_Status: Computation_Status;
    Sub_Form: UCTL_Types.Formula_Ref;
  begin
     --
      if CurDepth > Max_Explanation_Depth  then
        if HTML_Mode  then
          Put_Line (HTML_File,
            "<b> Sorry, but more explanations are being skipped  because we have here " &
            "reached the maximum allowed explanation depth.</b><br> ");
        else
            Put_Line 
             (" Sorry, but more explanations are being skipped  because we have here " &
             "reached the maximum allowed explanation depth.");
        end if;
      end if;
    --
    --------------------------------------------------------------------------
    --  NOTE:  we need to compare IMAGES of formulas because parametric formulas
    -- are instantiated multiple times with the same values
    --    (e.g. because of bounded evaluations).
    -- all the instances have the same image and are mapped in the same computation, but the
    --   Form pointer can be be different from the original one stored in the Computations_DB.
    --
    if not Skip_Header then
      if Previous = null or else Form.Fimage.all /= Previous.Fimage.all then
        if HTML_Mode then
          ------------------------------------------------------
          Put (HTML_File, "<div id=""" & NickName(Cref) & """");
          if previous = null then
            Put_Line(HTML_File, " style='display:block'>");
          else
            Put_Line(HTML_File, " style='display:none'>");
          end if;
          Put_Line (HTML_File, "<hr>");
          Put_Line (HTML_File, "<img src=square.png onclick=""hide('" & 
			NickName(Cref) & "')""; style='cursor:pointer'></img>");
		  Put_Line (HTML_File, "<b>The formula: </b>");
		  Put_Line (HTML_File, "<br><em>" & HTML_Format(Form.Fimage.all) & "</em>");
                  -- describe CONTEXT
                  -- In the specia case of Form.Kind=fapply when we desceribe the formula
                  -- we must say to which def it refers.
                  -- E.g.   -------------------------------------------------
                  --         The formula:
                  --            X  (which denotes the fixpoint: "max X: <a>X or b<Y>" 
                  --        is  TRUE in state Sy
                  --        -------------------------------------------------
                  if Form.Kind = fapply then
                    declare
                      FullContext: Computations_Table := Get_Context(Cref);
                      Parent_CRef: Computation_Step;
                      Parent_Formula: Formula_Ref;
                      Parent_State: System_Configuration;
                    begin
                      for I in FullContext'Range loop
                        Parent_CRef := FullContext(I);
                        Parent_Formula:= Get_Formula(Parent_Cref);
                        if Parent_Formula= Form.FullDef then
                            exit;
                        end if;
                      end loop;
                      Parent_State := Get_State(Parent_CRef);
                      --
                      Put_Line (HTML_File, 
                         " &nbsp; &nbsp; ( which  denotes the fixpoint  &nbsp; &nbsp;""");
                      Put_Line (HTML_File, HTML_Format(Form.FullDef.Fimage.all) & ")" );
                       --  """ &nbsp; started in State " & Configurations.NickName(Parent_State,"C") & ")" );
                    end;  
                  end if;
		  Put (HTML_File, "<b><br>&nbsp;is ");
		  if This_Status=FOUND_TRUE or This_Status=TMP_TRUE then
		    Put (HTML_File, "<spawn style='background-color:lightgreen'>" & 
				    Computation_Status'Image(This_Status) &
				    "</spawn>");
		  elsif This_Status=FOUND_FALSE or This_Status=TMP_FALSE then
		    Put (HTML_File, "<spawn style='background-color:pink'>" & 
				    Computation_Status'Image(This_Status) &
				    "</spawn>");
		  end if;
		  Put (HTML_File, " in State ");
		  Put (HTML_File, Configurations.NickName(This_State,"C"));
          Put_Line(HTML_File, "</b><P>");
        else -- TEXT_MODE
          if Previous /= null and then
               (Form.Kind = FFalse or Form.Kind  = FTrue) then
             return;
          end if;
          Put_Line ("-------------------------------------------");
          Put_Line ("The formula: ");
--          UCTL_Utilities.Print(Form, False);
          Put_Line("  " & Form.Fimage.all);
          -- print CONTEXT
          if Form.Kind = fapply then
                    declare
                      FullContext: Computations_Table := Get_Context(Cref);
                      Parent_CRef: Computation_Step;
                      Parent_Formula: Formula_Ref;
                      Parent_State: System_Configuration;
                    begin
                      for I in FullContext'Range loop
                        Parent_CRef := FullContext(I);
                        Parent_Formula:= Get_Formula(Parent_Cref);
                        if Parent_Formula= Form.FullDef then
                            exit;
                        end if;
                      end loop;
                      Parent_State := Get_State(Parent_CRef);
                      --
                      Put_Line (" ( which denotes the fixpoint " &
                                """" & Form.FullDef.Fimage.all & """ )" );
                    end;
          else
            New_Line;
          end if;
          Put ("   is ");
          Put (Computation_Status'Image(This_Status));
          Put (" in State ");
          Put (Configurations.NickName(This_State,"C"));
          New_Line;
          New_Line;
        end if; -- TEXT_MODE
      end if;  -- Previous = null or Form.Fimage /= previous.fimage
    end if;  -- if not Skip_Header then
    --
    -----------------------------------------------------------
    --   $FORM  = TRUE
    -----------------------------------------------------------
    if Form.Kind  = FTrue then
        if HTML_Mode then
          -- HTML MODE
          Put_Line (HTML_File,
                 "<b>This happens because the formula is</b><em> by definition</em> <br>");
          Put (HTML_File, "<b>Satisfied in all States </b><br>");
        else
          --  TEXT MODE
          if Previous = null then
            Put_Line ("This happens because the formula is by definition");
            Put(" Satisfied in all States");
            New_Line;
          end if;
        end if;
       return;
    end if;

    -----------------------------------------------------------
    --   $FORM  = FALSE
    -----------------------------------------------------------
    if Form.Kind  = FFalse then
        if HTML_Mode then
          -- HTML MODE
          Put_Line (HTML_File,
                 "<b>This happens because the formula is</b><em> by definition</em> <br>");
          Put (HTML_File, "<b> NOT satisfied in any State </b><br>");
        else
          --  TEXT MODE
          if Previous = null then
            Put_Line ("This happens because the formula is by definition ");
            New_Line;
              Put(" NOT satisfied in any State");
            New_Line;
          end if;
        end if;
       return;
    end if;

    -----------------------------------------------------------
    --  $FORM =  not  $FORM1
    -----------------------------------------------------------
    if  Form.Kind = Fnot then
        Sub_Status := Get_Status(This_Subcomputations(1));
        Sub_Form := Get_Formula(This_Subcomputations(1));
        if HTML_Mode then
          -- HTML MODE
          Put_Line (HTML_File, 
                 "<b>This happens because the subformula:</b><br>");
          Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all));
          Put (HTML_File, "</em><b><br>&nbsp;is ");
          if Sub_Status=FOUND_TRUE or Sub_Status=TMP_TRUE then
           Put_Line (HTML_File, "<spawn style='background-color:lightgreen'> is ");
            Put_Line(HTML_File,
                     "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
                     " TITLE='Explain more ...' >Satisfied</A>" );
            Put_line(HTML_File, "</spawn>");
          elsif This_Status=FOUND_FALSE or This_Status=TMP_FALSE then
            Put_Line (HTML_File, "<spawn style='background-color:pink'> is " );
            Put_Line(HTML_File,
                     "<A HREF=""javascript:show('" & NickName(This_Subcomputations(1)) & "');""" &
                     " TITLE='Explain more ...' >Not Satisfied</A>" );
          end if;
          Put (HTML_File, " in State ");
          Put (HTML_File, Configurations.NickName(This_State,"C"));
          Put_Line(HTML_File, "</b><br>");
        else
          --  TEXT MODE
          Put_Line ("This happens because the subformula:");
--          UCTL_Utilities.Print(Sub_Form, False);
          Put_Line ("  " & Sub_Form.Fimage.all);
          New_Line;
          if Sub_Status=FOUND_TRUE or Sub_Status=TMP_TRUE then
            Put(" is Satisfied "); 
          elsif This_Status=FOUND_FALSE or This_Status=TMP_FALSE then
            Put(" is Not Satisfied "); 
          end if;
          Put (" in State ");
          Put (Configurations.NickName(This_State,"C"));
          New_Line;
        end if;
       if not Already_Explored (This_SubComputations(1)) then
          if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
          Text_Explain_Computation(RecPath,This_SubComputations(1),1,Form,False,HTML_File);
       end if;
       return; 
    end if; 

    -----------------------------------------------------------
    --  $FORM =   $FORM1 and $FORM2
    --  $FORM =   $FORM1 or  $FORM2
    --  $FORM =   $FORM1 implies $FORM2
    -----------------------------------------------------------
    -- maybe just one subcomputation is present, maybe there are two
    -----------------------------------------------------------
    if  Form.Kind = Fand or Form.Kind=Foor or Form.Kind=Fimply then 
      for I in This_Subcomputations'Range loop
        Sub_Status := Get_Status(This_Subcomputations(I));
        Sub_Form := Get_Formula(This_Subcomputations(I));
        if HTML_Mode then
          --  HTML MODE
          if I = 1 then
            Put_Line (HTML_File, "<b>This happens because the subformula:</b><br>");
          else
            Put_Line (HTML_File, "<br><b>And because the subformula:</b><br>");
          end if;
          Put_line(HTML_File, "<em>" & HTML_Format(Sub_Form.Fimage.all) & "</em>");
          Put (HTML_File, "<br><b>&nbsp; ");
          if Sub_Status=FOUND_TRUE or Sub_Status=TMP_TRUE then
            Put_Line (HTML_File, "<spawn style='background-color:lightgreen'> is "); 
            Put_Line(HTML_File, 
                     "<A HREF=""javascript:show('" & NickName(This_Subcomputations(I)) & "');""" &
                     " TITLE='Explain more ...' >Satisfied</A>" );
            Put_line(HTML_File, "</spawn>");
          else
           Put_Line (HTML_File, "<spawn style='background-color:pink'> is " );
            Put_Line(HTML_File, 
                     "<A HREF=""javascript:show('" & NickName(This_Subcomputations(I)) & "');""" &
                     " TITLE='Explain more ...' >Not Satisfied</A>" );
            Put_line(HTML_File, "</spawn>");
          end if;
          Put (HTML_File, " in State ");
          Put (HTML_File, Configurations.NickName(This_State,"C"));
          Put_Line(HTML_File, "</b><br>");
        else
          --  TEXT MODE
          if I = 1 then
            Put_Line ("This happens because the subformula:");
          else
            Put_Line ("And because the subformula:");
          end if;
--          UCTL_Utilities.Print(Sub_Form, False);
          Put_Line("  " & Sub_Form.Fimage.all);        
          New_Line;
          -- print CONTEXT
          Put ("   is ");
          Put (Computation_Status'Image(Sub_Status));
          Put (" in State ");
          Put (Configurations.NickName(This_State,"C"));
          New_Line;
        end if;
      end loop;
      for I in This_Subcomputations'Range loop
        Sub_Status := Get_Status(This_Subcomputations(I));
        Sub_Form := Get_Formula(This_Subcomputations(I));
        if not Already_Explored (This_SubComputations(I)) then
          if This_SubComputations'Length =1 then
            -- the following explanation is immediately visible
            if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
            Text_Explain_Computation(RecPath,This_SubComputations(I),1,Form,False,HTML_File);
          else
            -- the following explanations are visible on demand
            if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
            Text_Explain_Computation(RecPath,This_SubComputations(I),1,Form,False,HTML_File);
          end if;
        end if;
      end loop;
      return; 
    end if;

    -----------------------------------------------------------
    --  $FORM  =   EX {act} $FORM1
    -----------------------------------------------------------
    if Form.Kind=Fexist and then
       Form.PRef.Kind = Act_Next then
      if This_Status = FOUND_FALSE then
        Explain_EX_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File); 
      elsif This_Status = FOUND_TRUE then
        Explain_EX_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   AX {act} $FORM1
    -----------------------------------------------------------
    if Form.Kind=Fall and then
       Form.PRef.Kind = Act_Next then
      if This_Status = FOUND_FALSE then
        Explain_AX_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_TRUE then
        Explain_AX_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   <act> $FORM1
    -----------------------------------------------------------
    if Form.Kind = Fangle then
      if This_Status = FOUND_FALSE then
        Explain_Angle_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_TRUE then
        Explain_Angle_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   [act] $FORM1
    -----------------------------------------------------------
    if Form.Kind = FSquare then 
      if This_Status = FOUND_FALSE then
        Explain_Square_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_TRUE then
        Explain_Square_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   EF $FORM1
    -----------------------------------------------------------
    if Form.Kind = Fexist and then
       Form.PRef.Kind = Eventually then
      if This_Status = FOUND_FALSE then
        Explain_EF_False(RecPath,Cref, CurDepth ,Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_TRUE then
        Explain_EF_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   EF {act} $FORM1
    -----------------------------------------------------------
    if Form.Kind = Fexist and then
       Form.PRef.Kind = Until2 and then 
       Form.Pref.U2ARef1.Kind = Atrue and then
       Form.Pref.U2FormRef1. Kind = Ftrue then
      if This_Status = FOUND_TRUE or This_Status=TMP_TRUE then
        Explain_EFact_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_FALSE then
        Explain_EFact_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   AF {act} $FORM1
    -----------------------------------------------------------
    if Form.Kind = FAll and then
       Form.PRef.Kind = Until2 and then
       Form.Pref.U2ARef1.Kind = Atrue and then
       Form.Pref.U2FormRef1. Kind = Ftrue then
      if This_Status = FOUND_TRUE or This_Status=TMP_TRUE then
        Explain_AFact_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_FALSE then
        Explain_AFact_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   AG $FORM1
    -----------------------------------------------------------
    if Form.Kind = Fall and then
       Form.PRef.Kind = Always then
      if This_Status = FOUND_TRUE or This_Status=TMP_TRUE then
        Explain_AG_True(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_FALSE then
        Explain_AG_False(RecPath,Cref, CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   EG $FORM1
    -----------------------------------------------------------
    if Form.Kind = Fexist and then
       Form.PRef.Kind = Always then
      if This_Status = FOUND_TRUE or This_Status=TMP_TRUE then
         Explain_EG_True(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_TRUE then
         Explain_EG_False(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;
    -----------------------------------------------------------
    --  $FORM  =   AF $FORM1
    -----------------------------------------------------------
    if Form.Kind = Fall and then
       Form.PRef.Kind = Eventually then
      if This_Status = FOUND_TRUE or This_Status=TMP_TRUE then
         Explain_AF_True(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      elsif This_Status = FOUND_FALSE then
         Explain_AF_False(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      end if;
      return;
    end if;

    -----------------------------------------------------------
    --  $FORM  =   E[ F1 {act1} U F2] 
    --  $FORM  =   E[ F1 {act1} W F2] 
    -----------------------------------------------------------
      if Form.Kind = Fexist and then
         (Form.Pref.Kind= Until1 or else Form.Pref.Kind=Wuntil1)  then    
        if This_Status=FOUND_FALSE or else This_Status=TMP_FALSE then
        --
          if HTML_Mode  then
            Put_Line (HTML_File,
              "<b>because  no paths starting from here satisfy the until conditions.</b><br>");
          else
            Put_Line ("because  no paths starting from here satisfy the until conditions.");
          end if;
        return; 
       elsif This_Status = FOUND_TRUE or else This_Status=TMP_FALSE then
         if Form.Pref.Kind= Until1 then 
            Explain_EUntil1_True(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
          else
            Explain_EWuntil1_True(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
          end if;
        end if;
        return;
      end if;

    -------------------------- da aggiustare ---------------------
    -----------------------------------------------------------
    --  $FORM  =   E[ F1 {act1} U {act2} F2]
    --  $FORM  =   E[ F1 {act1} W {act2} F2]
    -----------------------------------------------------------
      if Form.Kind = Fexist and then
         (Form.Pref.Kind= Until2 or else Form.Pref.Kind=Wuntil2)  then    
        if This_Status=FOUND_FALSE or else This_Status=TMP_FALSE then
          if HTML_Mode  then
            Put_Line (HTML_File,
              "<b>because  no paths starting from here satisfy the until conditions.</b><br>");
          else
            Put_Line ("because  no paths starting from here satisfy the until conditions.");
          end if;
        elsif This_Status=FOUND_TRUE or else This_Status=TMP_TRUE then
         if Form.Pref.Kind= Until2 then
            Explain_EUntil2_True(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
          else
            Explain_EWuntil2_True(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
          end if;
        end if;
        return;
      end if;

    ----------------------------------------------------------------
    --  A[ F1 {} U F2]  is FALSE
    --  A[ F1 {} W F2]  is FALSE
    ----------------------------------------------------------------
    if Form.Kind=Fall and then
       (This_Status=FOUND_FALSE or else This_Status=TMP_FALSE) and then
       (Form.Pref.Kind= Until1 or else Form.Pref.Kind=Wuntil1) then
      if Form.Pref.Kind= Until1 then
           Explain_AUntil1_False(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      else
            Explain_AWUntil1_False(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      end if;
--      if HTML_Mode then
--        -- HTML MODE
--        Put_Line(HTML_File,
 --         "Sorry! FEATURE NOT YET SUPPORTED IN THIS VERSION ...<br> ");
--      else
--        -- TEXT MODE
--        Put_line ("Sorry! FEATURE NOT YET SUPPORTED IN THIS VERSION ...");
--      end if;
      return;
    end if;

    ----------------------------------------------------------------
    --  A[ F1 {} U {} F2]  is FALSE
    --  A[ F1 {} W {} F2]  is FALSE
    ----------------------------------------------------------------
    if Form.Kind=Fall and then
       (This_Status=FOUND_FALSE or else This_Status=TMP_FALSE) and then
       (Form.Pref.Kind= Until2 or else Form.Pref.Kind=Wuntil2) then
      if Form.Pref.Kind= Until2 then
           Explain_AUntil2_False(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      else
            Explain_AWUntil2_False(RecPath,Cref,CurDepth, Previous, Skip_Header, HTML_File);
      end if;
--      if HTML_Mode then
--        -- HTML MODE
--        Put_Line(HTML_File,
 --         "Sorry! FEATURE NOT YET SUPPORTED IN THIS VERSION ...<br> ");
--      else
--        -- TEXT MODE
--        Put_line ("Sorry! FEATURE NOT YET SUPPORTED IN THIS VERSION ...");
--      end if;
      return;
    end if;
        
    ----------------------------------------------------------------
    --  A[ F1 {} U F2]  is TRUE
    --  A[ F1 {} W F2]  is TRUE
    ----------------------------------------------------------------
    if Form.Kind=Fall and then
       (This_Status=FOUND_TRUE or else This_Status=TMP_TRUE) and then
       (Form.Pref.Kind= Until1 or else Form.Pref.Kind= Wuntil1) then
        --
        if This_Subcomputations'Length > 2 then   -- F1=true  and F2 false now
          if HTML_Mode  then
            Put_Line (HTML_File,
              "<b>because  all paths starting from here satisfy the until conditions.</b><br>");
          else
            Put_Line ("because  all paths starting from here satisfy the until conditions.");
          end if;
        end if;
        return; 
     end if;
        --
    ----------------------------------------------------------------
    --  A[ F1 {} U {} F2]  is TRUE
    --  A[ F1 {} W {} F2]  is TRUE
    ----------------------------------------------------------------
     if Form.Kind=Fall and then
        (This_Status=FOUND_TRUE or else This_Status=TMP_TRUE) and then
       (Form.Pref.Kind= Until2 or else Form.Pref.Kind=Wuntil2) then  
        --
        if This_Subcomputations'Length > 2 then   -- F1=true  and F2 false now
          if HTML_Mode  then
            Put_Line (HTML_File,
             "<b>because all paths starting from here satisfy the until conditions.</b><br>");
          else
            Put_Line ("because all paths starting from here satisfy the until conditions ");
          end if;
          return; -- stop recursive explanationation here
        end if;
      end if;
    --
    ----------------
    -- At this point, we really need to provide at least a fragment of counter example ...
    ----------------
    if Form.Kind=Fmax or Form.Kind=Fmin  then
      --   
      --  max Z: ...   min Z: ...
      --
      declare
        SubCref: Computation_Step := This_SubComputations(1);
        Sub_Formula: Formula_Ref := Get_Formula(SubCref);
        -- Sub_Context: Computations_Table := Get_Context(SubCref);
        Sub_State: System_Configuration := Get_State(SubCref);
        Sub_Status: Computation_Status := Get_Status(SubCref);
        -- ThisLabel: String := Ground_Label_Image(This_Subevolutions(1));
      begin
        Text_Explain_Computation(RecPath&Cref,This_SubComputations(1),CurDepth+1,Form,True,HTML_File);
        if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
      end;
      return;
    end if;
    --
    --
    if Form.Kind=fapply and then Form.IDen.all = "PRINT" then
      --
      --  PRINT 
      --
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the formula is</b><em> by definition</em> <br>");
        Put (HTML_File, "<b>NOT Satisfied (false) in all States </b><br>");
      else
        --  TEXT MODE
        if Previous = null then
          Put_Line ("This happens because the formula is by definition");
          Put(" NOT satisfied (false) in all States");
          New_Line;
        end if;
      end if;
      return;
    end if;

   if Form.Kind=fapply and then Form.IDen.all = "FINAL" and then This_Status=FOUND_TRUE then
      --
      --  FINAL (True)
      --
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File,
               "<b>This happens because the State " &
                   "<a href='javascript:top.sendcommand(""" &
                    Configurations.NickName(This_State,"C") & """)'>" &
                    Configurations.NickName(This_State,"C") &
                    "</a>" &
                    "  has no possible evolutions (the state if final)</b><br>");
      else
        --  TEXT MODE
        if Previous = null then
          Put_Line ("This happens because the State " &
                   Configurations.NickName(This_State,"C") &
                   "  has no possible evolutions (the state if final)");
        end if;
      end if;
     return;
   end if;

   if Form.Kind=fapply and then Form.IDen.all = "FINAL" and then This_Status=FOUND_FALSE then
      --
      --  FINAL (False)
      --
      if HTML_Mode then
        -- HTML MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          Put_Line (HTML_File, "<b>This happens because </b><br>");
          while Has_System_Transition (My_Iter) loop
            Put_Line (HTML_File,
                "&nbsp;&nbsp;&nbsp;" &
                "<a href='javascript:top.sendcommand(""" & Configurations.NickName(This_State,"C") &
                """)'>" & Configurations.NickName(This_State,"C") & "</a>" &
                " --&gt; " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "&nbsp;&nbsp;" & Abstract_Action_Labels(My_Iter)  & "<br>");
            Iterator_Advance(My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
          Put_Line (HTML_File,
             "<b> (therefore the state is NOT f9ial) </b><br>");
        end;
      else
        --  TEXT MODE
        declare
          My_Iter: Evolutions_Iterator;
        begin
          Iterator_Initialize (My_Iter, This_State);
          Put_Line ("This happens because ");
          while Has_System_Transition (My_Iter) loop
            Put ("  " & Configurations.NickName(This_State,"C") &
                 " --> " &
                Configurations.NickName(Get_Target_Configuration(My_Iter),"C") &
                "  " & Abstract_Action_Labels(My_Iter) );
            New_Line;
            Iterator_Advance(My_Iter);
          end loop;
          Iterator_Finalize (My_Iter);
          Put_Line(" (therefore the state is NOT final)");
        end;
      end if;
    return;
   end if;

   if Form.Kind=fapply and then Form.FullDef /= null then 
      Explain_Fixpoint(RecPath, Cref, CurDepth, Previous, False, HTML_File);
     return;
   end if;

   if Form.Kind=assertion and then Form.Pred.Op = NOOP then 
     --
     --  StatePredlabel(arg1,arg2,...)
     --
      if HTML_Mode then
        -- HTML MODE
        Put_Line (HTML_File, "<b>This happens because the state predicate </b><br><em>");
        Put_Line (HTML_File, HTML_Format(Form.Fimage.all) & "</em>");
        if This_Status=FOUND_TRUE then
          Put_Line (HTML_File, "<b>&nbsp; is Satisfied by the set of Abstract State Labels:</b><br>");
          Put_Line (HTML_File, "&nbsp;&nbsp;{" & AbstractLabels(Get_Abstract_State_Labels(This_State)) & "}");
        else
          Put_Line (HTML_File, "&nbsp;<b> is NOT Satisfied by the set of Abstract State Labels:</b><br>");
          Put_Line (HTML_File, "&nbsp;&nbsp;{" & AbstractLabels(Get_Abstract_State_Labels(This_State)) & "}");
        end if;
        Put_Line (HTML_File, " &nbsp;&nbsp;&nbsp;<b>in State </b> " & 
                    " <a href='javascript:top.sendcommand(""" &
                    Configurations.NickName(This_State,"C") & """)'>" &
                    Configurations.NickName(This_State,"C") &
                    "</a><br>"); 
      else
        --  TEXT MODE
        Put_Line ("This happens because the state predicate ");
        Put_Line (Form.Fimage.all);
        if This_Status=FOUND_TRUE then
          Put_Line ("  is Satisfied by the set of Abstract State Labels:");
          Put_Line ("    {" &  AbstractLabels(Get_Abstract_State_Labels(This_State)) & "}");
  
        else
          Put_Line (" is NOT Satisfied by the set of Abstract State Labels:");
          Put_Line ("    { " & AbstractLabels(Get_Abstract_State_Labels(This_State)) & "}");
          Put_Line ("  in State " & Configurations.NickName(This_State,"C")); 
        end if;
      end if;
     return;
   end if;
--
--    if Form.Kind=assertion then
        --
      --   Predlabel(predarg1,predarg2,...)
      --   id > < = /=  <= >= id PRINT_ONCE
      -- (in assertions like "%1 /= %2" all vars have already been substituted by the values)
--    end if;
   --

      if HTML_Mode then
        -- HTML MODE
        Put_Line(HTML_File,
          "Sorry! FEATURE NOT YET SUPPORTED IN THIS VERSION ...<br> ");
      else
        -- TEXT MODE
        Put_line ("Sorry! FEATURE NOT YET SUPPORTED IN THIS VERSION ...");
      end if;
      return;

--   if This_Subcomputations'Length > 0 then
--      for I in 1..This_Subcomputations'Length loop
--        declare
--          SubCref: Computation_Step := This_SubComputations(I);
--          Sub_Formula: Formula_Ref := Get_Formula(SubCref);
--          Sub_Context: Computations_Table := Get_Context(SubCref);
--          Sub_State: System_Configuration := Get_State(SubCref);
--          Sub_Status: Computation_Status := Get_Status(SubCref);
--          ThisLabel: String := Ground_Label_Image(This_Subevolutions(I));
--        begin
--          --------------------------------------
--          if Previous=null or else
--             Form.Fimage.all /= Previous.Fimage.all then
--            if I = 1 then
--              if HTML_Mode  then
--                Put_Line(HTML_File,"<br><b>This happens because </b>");
--              else
--                Put_Line(" because");
--              end if;
--            else
--              if HTML_Mode  then
--                Put_Line (HTML_File,"<b> and because</b>");
--              else
--                Put_Line ("and because");
--              end if;
--            end if;
--          end if;
--          ------------------------------------
--          --  C1  --signals->  CJ
--          --    if  C1 = CJ  and Form=AG,EG,EF,AF,A[], E[] and SubForm /= Form  omit this sect!
--          --        C1 = CJ  and Form=  F1 and F2, F1 or F2, F1 implies F2,  omit this section!
--          --
--          --  HENCE  execute this section only if:
--          --    Form = SubForm,   and CI /= CJ (i.e.  this is plain recursion)
--          --    Form /= SubForm,  CI /= CJ  (Dynamic progression in the model) ??
--          --    Form /= SubForm,  Form= EX,AX,<>,[],<<>>,[[]]   even if CI=CJ
--          -------------------------------------
--          if This_State = Sub_State and then
--               (Form.Kind=Fexist or else Form.Kind=Fall) and then
--               (Form.Pref.Kind=Eventually or else
--                Form.Pref.Kind=Always  or else
--                Form.Pref.Kind=Until1 or else
--                (Form.Pref.Kind=Until2 and then Sub_Formula /= Form.Pref.U2FormRef2) or else
--                (Form.Pref.Kind=WUntil2 and then Sub_Formula /= Form.Pref.U2FormRef2)) then
--              -- No actual state transition, just same-state subformula evaluation
--              if HTML_Mode then
--                Put_Line (HTML_File,"<br><b>and the formula:</b>");
--              else
--                Put_Line (" and the formula: ");
--              end if;
--          elsif This_State = Sub_State and then
--               (Form.Kind=Fnot or else
--                Form.Kind= Fand or else
--                Form.Kind= Foor or else
--                Form.Kind= Fimply) then
--              if HTML_Mode then
--                Put_Line (HTML_File,"<b>the formula:</b>");
--              else
--                Put_Line (" the formula: ");
--              end if;
--          else
--            if HTML_Mode then
--               -- Increment Hindex, and use it for history command
--               -- Push Element(This_State).key into History
--               Put_Line(HTML_File,"<br>&nbsp;&nbsp;&nbsp;");
--               Put (HTML_File,
--                   "<a href='javascript:top.sendcommand(""" &
--                    Configurations.NickName(This_State,"C") & """)'>" &
--                    Configurations.NickName(This_State,"C") &
--                    "</a>");
--               if ThisLabel /= "" then
--                 Put (HTML_File,"  -->  " & Configurations.NickName(Sub_State,"C"));
--                 Put (HTML_File,Abstract_Label_Image(This_Subevolutions(I)));
--                 Put(HTML_File,"<label href=""index.htm"" onmouseover=""Tip(");   -- TOOLTIP PREFIX
--                 Put (HTML_File,"' /* '+" &  HTML_Literal_Hack(ThisLabel) & "+' */'");
--                 Put(HTML_File,
--                  ")"" onmouseout=""UnTip()""><FONT color=""blue"">&nbsp;/* ... */</font></label>");  
--                 New_Line(HTML_File);
--               else
--                  Put_Line ("  -->  " & Configurations.NickName(Sub_State,"C") );
--               end if;
--               if Sub_Formula.Fimage.all /= Form.Fimage.all then
--                  Put ("<br><b>and the formula:</b> ");
--               end if;
--             else
--               Put ("  " & Configurations.NickName(This_State,"C"));
--               if ThisLabel /= "" then
--                 Put ("  -->  " & Configurations.NickName(Sub_State,"C"));
--                 Put (Abstract_Label_Image(This_Subevolutions(I)));
--                 Put (" /* " &  ThisLabel & " */");
--                 New_Line;
--               else
--                  Put_Line ("  -->  " & Configurations.NickName(Sub_State,"C"));
--               end if;
--               if Sub_Formula.Fimage.all /= Form.Fimage.all then
--                 Put (" and the formula: ");
--               end if;
--            end if;
--          end if;
--          -----------------------------------------
--          if Form.Fimage.all /= Sub_Formula.Fimage.all then
--            if HTML_Mode  then
--               if Sub_Status=FOUND_TRUE or Sub_Status=TMP_TRUE then
--                 Put_line(HTML_File,"<div style=""background-color:lightgreen;width=100%""><em> " &
--                    HTML_Format(Sub_Formula.Fimage.all) & "</em></div>");
--               else
--                 Put_line(HTML_File,"<div style=""background-color:pink;width=100%""><em> " &
 --                    HTML_Format(Sub_Formula.Fimage.all) & "</em></div>");
--               end if;
--              Put (HTML_File,"   <b>is ");
--              Put (HTML_File,Computation_Status'Image(Sub_Status));
--              Put (HTML_File," in State ");
--              Put (HTML_File,Configurations.NickName(Sub_State,"C"));
--              Put (HTML_File,"</b><br>");
--              New_Line(HTML_File);
--            else
--              UCTL_Utilities.Print(Sub_Formula, False);
--              New_line;
--              -- print CONTEXT
--              Put ("   is ");
--              Put (Computation_Status'Image(Sub_Status));
--              Put (" in State ");
--              Put (Configurations.NickName(Sub_State,"C"));
--              New_Line;
--            end if;
--          end if;
--        end;
--      end loop;
--    end if;
--
--    if This_Subcomputations'Length > 0 then
--      if CurDepth < Max_Explanation_Depth  then
--        for I in 1..This_Subcomputations'Length loop
--          declare
--            SubSubCref: Computation_Step := This_SubComputations(I);
--            SubSubCount: Natural := Get_Subcomputations(SubSubCref)'Length;
--          begin
--          if SubSubCount = 1 then
--              if not Already_Explored (This_SubComputations(I)) then
--                if (Form.Kind = Fand or else
--                   Form.Kind = Foor or else
--                   Form.Kind = Fimply)  and then
--                   This_Subcomputations'Length =2 then
--                  if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
--                  Text_Explain_Computation(RecPath,This_SubComputations(I),1,null,False,HTML_File);
--                else
--                  if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
--                  Text_Explain_Computation(RecPath,This_SubComputations(I),CurDepth+1,Form,False,HTML_File);
--                end if;
--              end if;
--          elsif SubSubCount > 1 then
--              if not Already_Explored (This_SubComputations(I)) then
--                if (Form.Kind = Fand or else
--                   Form.Kind = Foor or else
--                   Form.Kind = Fimply)  and then
--                   This_Subcomputations'Length =2 then
--                  if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
--                  Text_Explain_Computation(RecPath,This_SubComputations(I),1,null,False,HTML_File);
--                else
--                  if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
--                  Text_Explain_Computation(RecPath,This_SubComputations(I),CurDepth+1,Form,False,HTML_File);
--                end if;
--             end if;
--          end if;
--          end;
--        end loop;
--      else
--        if HTML_Mode  then
--            Put_Line (HTML_File," <b>(more explanations omitted ...)</b><br> ");
--        else
--            Put_Line (" (more explanations omitted ...) ");
--        end if;
--      end if;
--    end if; -- if This_Subcomputations'Length > 0
--
--    if HTML_Mode then Put_Line(HTML_File, "</div>"); end if;
  end Text_Explain_Computation;




  procedure HTML_ExplainEvaluation (HTML_FileName: String; Comp: Integer :=1)  is
     HTML_File: File_Type;
     Form: UCTL_Types.Formula_Ref := UCTL_Types.This_Formula;
     Current_Conf: System_Configuration := Initial_Configuration;
     Cref: Computation_Step;
     Status: Computation_Status;
     State : System_Configuration := Current_Conf;
     Tmp_Rec: Natural :=0;
  begin
--     Hindex :=0;
     Create(HTML_File, Out_File, HTML_FileName);
     Put_Line(HTML_File, "<html><body>");
     Put_line(HTML_File, "<script type=""text/javascript"" src=""wz_tooltip.js""></script>");
     Put_line(HTML_File, "<script type=""text/javascript"">");
     Put_line(HTML_File, "function show(myid)"); 
     Put_line(HTML_File, "{ var elem; "); 
     Put_line(HTML_File, "  elem = document.getElementById(myid); "); 
     Put_line(HTML_File, "  elem.style.display = ""block""; "); 
     Put_line(HTML_File, "  return; "); 
     Put_line(HTML_File, "}; "); 
     Put_line(HTML_File, "function hide(myid)");
     Put_line(HTML_File, "{ var elem; ");
     Put_line(HTML_File, "  elem = document.getElementById(myid); ");
     Put_line(HTML_File, "  elem.style.display = ""none""; ");
     Put_line(HTML_File, "  return; ");
     Put_line(HTML_File, "}; ");
     Put_line(HTML_File, "</script> "); 
     Put_Line(HTML_File,
         "<div align=left style=""background-color:white;width:95%"">");
    if Form  = null then
       Put_Line (HTML_File, "<p>  Nothing to explain! first type in an UCTL formula! <p>");
    elsif Comp =1 then
      Check_Computation (Form,Empty_Computations_Table, State, State,Cref,Status,Tmp_Rec);
      Computations_DB.Start_Exploration;
      -- Set_Output(HTML_File);
      if not Already_Explored(Cref) then
        Text_Explain_Computation (Empty_Computations_Table,Cref, 1,null,False,HTML_File);
      end if;
      Put_line(HTML_File,"<br><hr><br>");
      -- Set_Output(Standard_Output);
    else
      Cref := Computation_Step(Comp);
      Computations_DB.Start_Exploration;
      -- Set_Output(HTML_File);
      if not Already_Explored(Cref) then
        Text_Explain_Computation (Empty_Computations_Table,Cref, 1,null,False,HTML_File);
      end if;
      Put_line(HTML_File,"<br><hr><br>");
      -- Set_Output(Standard_Output);
    end if;
    Put_Line (HTML_File, "</div>");
    Put_Line (HTML_File, "</body></html>");
    Close(HTML_File);
  exception
  when Event: others =>
    Put_line (Current_Error,
            "Error in HTML_Explain");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    Set_Output(Standard_Output);
    if Is_Open(HTML_File) then Close(HTML_File); end if;
  end HTML_ExplainEvaluation;


  procedure ExplainEvaluation(Comp: Integer :=1)  is
     HTML_File: File_Type;
     Form: UCTL_Types.Formula_Ref := UCTL_Types.This_Formula;
     Current_Conf: System_Configuration := Initial_Configuration;
     Cref: Computation_Step;
     Status: Computation_Status;
     State : System_Configuration := Current_Conf;
     Tmp_Rec: Natural :=0;
  begin
--     Hindex :=0;
     if Form  = null then
       Put_Line ("  Nothing to explain! first type in an UCTL formula!");
     elsif Comp=1 then
       Check_Computation (Form,Empty_Computations_Table, State, State,Cref,Status,Tmp_Rec);
       Computations_DB.Start_Exploration;
       if not Already_Explored(Cref) then
         Text_Explain_Computation (Empty_Computations_Table,Cref, 1,null,False,HTML_File);
         Put_line("-------------------------------------------");
       end if;
     else
       Cref := Computation_Step(Comp);
       Computations_DB.Start_Exploration;
       if not Already_Explored(Cref) then
         Text_Explain_Computation (Empty_Computations_Table,Cref, 1,null,False,HTML_File);
         Put_line("-------------------------------------------");
       end if;
     end if;
  exception
  when Event: others =>
    Put_Line (Current_Error,
            "Error in ExplainEvaluation");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    Set_Output(Standard_Output);
  end ExplainEvaluation;


--???????????????????????
  procedure HTML_EvalFromFile (HTML_FileName: String; Form_FileName: String; Eval_OK: out Boolean) is
  begin
--    Subsets(1) := False;
--    Subsets(2) := False;
--    Subsets(3) := False;
     Eval_OK := False;
  end;

  procedure EvalFromFile (Form_FileName: String; Eval_OK: out Boolean) is
  begin
      UCTL_Parser.Parse(Form_FileName);
      if UCTL_Types.Formula_contains_PRINT then
          Max_LTS_Depth :=  Top_LTS_Depth;
      end if;
      Subsets(1) := False;
      Subsets(2) := False;
      Subsets(3) := False;
      if product_families and then
           Check_VactlBoxTrue(UCTL_Types.This_Formula) then 
              Subsets(1) := True; 
      end if;
      if product_families and then
           Check_VactlBoxfalse(UCTL_Types.This_Formula) then 
              Subsets(2) := True; 
      end if;
      if product_families and then
         Check_VactlBoxLiveTrue(UCTL_Types.This_Formula) then 
           Live_Model := True;
           Subsets(3) := True; 
      end if;
      for I in all_formulas.all'Range loop
        This_formula := All_formulas(I);
        UCTL_TYPES.formula_contains_print := All_Prints(I);
        EvaluateIt(Eval_OK);
        if I /= all_formulas.all'Last then
          delay(0.1);  -- just to be sure all previouos frgs are done
        end if;
      end loop;
      Eval_OK := True;
  exception
    when Parsing_Error =>
       Eval_OK := False;
    when Event: others =>
       Eval_OK := False;
       Put_line (Current_Error,"Error in EvalFromFile");
       Put_Line (Current_Error, Exception_Name(Event));
       Put_Line (Current_Error, Exception_Message(Event));
  end EvalFromFile;



  procedure EvalFromString(FormCode: String; Eval_OK: out Boolean) is
  begin
      UCTL_Parser.Parse_From_String(FormCode);
      if UCTL_Types.Formula_contains_PRINT then
          Max_LTS_Depth :=  Top_LTS_Depth;
      end if;
      Subsets(1) := False;
      Subsets(2) := False;
      Subsets(3) := False;
      if product_families and then
           Check_VactlBoxTrue(UCTL_Types.This_Formula) then 
              Subsets(1) := True; 
      end if;
      if product_families and then
           Check_VactlBoxfalse(UCTL_Types.This_Formula) then 
              Subsets(2) := True; 
      end if;
      if product_families and then
         Check_VactlBoxLiveTrue(UCTL_Types.This_Formula) then 
           Subsets(3) := True; 
           Live_Model := True;
      end if;
      EvaluateIt(Eval_OK);
      Eval_OK := True;
  exception
    when Parsing_Error =>
       Eval_OK := False;
    when Event: others =>
       Eval_OK := False;
       Put_line (Current_Error,"Error in EvalFromString");
       Put_Line (Current_Error, Exception_Name(Event));
       Put_Line (Current_Error, Exception_Message(Event));
  end EvalFromString;


  procedure Parse_Formula (Formula_FileName: String; Parse_OK: out Boolean) is
  begin
    Parse_OK := True;
    UCTL_Parser.Parse(Formula_FileName);
    if All_Formulas= null then
       Parse_OK := False;
       return;
    end if;
    All_Subsets := new Subsets_Table(All_Formulas.all'Range);
    for I in All_Formulas.all'Range loop
      All_Subsets(I)(1) := False;
      All_Subsets(I)(2) := False;
      All_Subsets(I)(3) := False;
      if product_families and then
         Check_VactlBoxTrue(All_Formulas(I)) then All_Subsets(I)(1) := True; end if;
      if product_families and then
         Check_VactlBoxfalse(All_Formulas(I)) then All_Subsets(I)(2) := True; end if;
      if product_families and then
         Check_VactlBoxLiveTrue(All_Formulas(I)) then All_Subsets(I)(3) := True; end if;
     end loop;
  exception
    when others => Parse_OK := False;
  end Parse_Formula;


  -------------------------------------------------------------
  --     ACTION FORMULE    <action>    (#  or not #  ??)
  --     a,   b,   c,  a or b,   a and b,  not c   => return False
  --   aa and not may,   not may,  = return True
  -------------------------------------------------------------
  function isBOX(Action: Action_Ref) return boolean is
  begin
    if Action.Kind = Anot and then
         Action.Anot.Kind = Aid and then
          Action.Anot.AidPred.Kind = Aid and then
           Action.Anot.AidPred.Labels /= null and then
            Action.Anot.AidPred.Labels(1).all = "may" then
        --  not may
      return True;
    elsif Action.Kind = Aand  and then
        Action.Aref2.Kind = Anot and then
         Action.Aref2.Anot.Kind = Aid and then
          Action.Aref2.Anot.AidPred.Kind = Aid and then
           Action.Aref2.Anot.AidPred.Labels /= null and then
            Action.Aref2.Anot.AidPred.Labels(1).all = "may" then
        --  act and not may
      return True;
    else
        --  act
      return False;
    end if;
  end isBOX;
  ---------------------------------------------
  --  VACTL-BOX-TRUE:  Formula True valid for all products 
  -- VACTL-BOX-TRUE =  POS + not NEG
  -- POS = true, false, max, min, and, or, <>#, [], EF#, EF#{}, AF#, AF#{}, AG, AG#
  -- NEG = true, false, max, min, and, or, <>, EF, AF, AF[]
  --------------------------------------
  function Check_VactlBoxTrue  (F:Formula_Ref) return Boolean is
    B1, B2 : Boolean;
  begin
    case F.Kind is
       when Ftrue | Ffalse | FApply | Assertion =>  
         --  beware!!    FINAL is Ok for VACT-TRUE!!
         return True;
       when  Fand | Foor =>
         B1 := Check_VactlBoxTrue(F.LeftRef);
         B2 := Check_VactlBoxTrue(F.Rightref);        
         return B1 and B2;
       when FNot =>
         B1 := Check_VactlBoxFalse(F.NotRef);
         return B1;
       when  Fimply =>
         B1 := Check_VactlBoxFalse(F.LeftRef);
         B2 := Check_VactlBoxTrue(F.Rightref);
         return B1 and B2;
       when Fmax | Fmin =>
         B1 :=  Check_VactlBoxTrue(F.Fdef);
         return B1;
       when FWangle |FWsquare =>     -- ?? boh?
          return False;
       when FAngle  =>  
         if isBOX(F.Aref) then     -- <>
            -- <>#
            return Check_VactlBoxTrue(F.FormRef);      
         else
            -- <>
            return False;
         end if;
       when Fsquare =>  
         if isBOX(F.Aref) then 
            -- []
            return False;  
         else
            --[]#   ko
            B1 :=  Check_VactlBoxTrue(F.FormRef);
            return B1;
         end if;
       when Fexist | Fall =>
         case F.Pref.Kind is
           when Eventually =>   --  EF AF
              return False;
           when Always =>
             if F.Kind = Fall then
                return Check_VactlBoxTrue(F.Pref.Tform);    -- AG
             else
                return False;   --  EG
             end if;
           when  Act_Next =>    -- EX, AX
              if isBOX(F.Pref.AAref) then    -- EX#   AX#
                return Check_VactlBoxTrue(F.Pref.AFormRef);
              else 
                return False;    -- EX  AX
              end if;
           when  Until1 | WUntil1 =>    -- EF#  AF#  AG#
              if isBOX(F.Pref.U1ARef) and then
                 not Check_VactlBoxTrue(F.Pref.U1FormRef1) then
                return Check_VactlBoxTrue(F.Pref.U1FormRef2);
              else 
                return False;
              end if;
           when  Until2 | WUntil2 =>    -- EF{} AF{} e versioni #
              if isBOX(F.Pref.U2ARef1) and then
                 isBOX(F.Pref.U2ARef2) and then
                 Check_VactlBoxTrue (F.Pref.U2FormRef1) then
                return Check_VactlBoxTrue(F.Pref.U2FormRef2);
              else 
                return False;
              end if;              
         end case;
    end case;
  end Check_VactlBoxTrue;

  function mayfree (A: Action_ref) return Boolean is
  begin
    case A.Kind is
      when Atrue | Afalse   => 
         return True;
      when Anot  =>   
         return mayfree(A.Anot);
      when Aand | Aor   =>
         return mayfree(A.Aref1) and then mayfree(A.Aref2);
      when Aid =>
         if  A.AidPred /= null and then
             A.AidPred.Labels /= null and then
             A.AidPred.labels(1).all /= "may" then
              return True;
          else
            return False; 
         end if;
      when Aas =>
         return True;
    end case;
  end mayfree;

  --------------------------------------
  --  VACTL-BOX-FALSE:  Formula false remain false for all products
  -- VACTL-BOX-FALSE =  NEG + not POS
  -- POS = true, false, max, min, and, or, <>#, [], EF#, EF#{}, AF#, AF#{}, AG, AG#
  -- NEG = true, false, max, min, and, or, <>, EF, AF{}
  ---------------------------------------------
  function Check_VactlBoxFalse  (F:Formula_Ref) return Boolean is
     B1, B2: Boolean;
  begin
    case F.Kind is
       when Ftrue | Ffalse | FApply | Assertion =>
         if F.Kind=fapply and then F.IDen.all = "FINAL" then
           return False;
         else
           return True;
        end if;
       when  Fand | Foor =>
         B1 := Check_VactlBoxFalse(F.LeftRef);
         B2 := Check_VactlBoxFalse(F.Rightref);
         return B1 and B2;
       when FNot =>
         B1 := Check_VactlBoxTrue(F.NotRef);
         return B1; 
       when  Fimply =>
         B1 := Check_VactlBoxTrue(F.LeftRef);
         B2 := Check_VactlBoxFalse(F.Rightref);
         return B1 and B2;
       when Fmax | Fmin =>
         B1 :=  Check_VactlBoxFalse(F.Fdef);
         return B1;
       when FWangle |FWsquare =>     -- ?? boh?
          return False;
       when FAngle  =>                    
         if mayfree(F.Aref) then     -- <>
            return Check_VactlBoxFalse(F.FormRef);
         else
            return False;         -- <>#
         end if;
       when Fsquare =>                    
            if isBox(F.Aref) then
                return Check_VactlBoxFalse(F.FormRef);
            else
              return False;         --[],
            end if;
       when Fexist | Fall =>
         case F.Pref.Kind is
           when Eventually =>   --  EF
              return Check_VactlBoxFalse(F.Pref.Tform);
           when Always =>
                return False;   --  EG
           when  Act_Next =>    -- EX, AX
              if F.Kind = Fexist and then
                 mayfree(F.Pref.AARef) then
                return Check_VactlBoxFalse(F.Pref.AFormRef);    -- EX  
              else
                return False;    -- EX  AX
              end if;
           when  Until1 | WUntil1 =>    -- EF#  AF#  AG#
              if mayfree(F.Pref.U1ARef) and then
                 Check_VactlBoxFalse(F.Pref.U1FormRef1) then
                return  Check_VactlBoxFalse(F.Pref.U1FormRef2);
              else
                return False;
              end if;
           when  Until2 | WUntil2 =>    -- EF{} AF{} e versioni #
              if mayfree(F.Pref.U2ARef1) and then
                 mayfree(F.Pref.U2ARef2) and then
                 Check_VactlBoxFalse(F.Pref.U2FormRef1)  then
                   return  Check_VactlBoxFalse(F.Pref.U2FormRef2);
              else
                return False;
              end if;
         end case;
    end case;
  end Check_VactlBoxFalse;

  ---------------------------------------------------
  -- VACTL_BOX_LIVE_TRUE  =  VACTL_BOX_TRUE + AF + AF{}
  ---------------------------------------------------
  function Check_VactlBoxLiveTrue  (F:Formula_Ref) return Boolean is
    B1, B2 : Boolean;
  begin
    case F.Kind is
       when Ftrue | Ffalse | FApply | Assertion =>
         return True;
       when  Fand | Foor =>
         B1 := Check_VactlBoxLiveTrue(F.LeftRef);
         B2 := Check_VactlBoxTrue(F.Rightref);
         return B1 and B2;
       when FNot =>
         B1 := Check_VactlBoxFalse(F.NotRef);
         return B1;
       when  Fimply =>
         B1 := Check_VactlBoxFalse(F.LeftRef);
         B2 := Check_VactlBoxLiveTrue(F.Rightref);
         return B1 and B2;
       when Fmax | Fmin =>
         B1 :=  Check_VactlBoxLiveTrue(F.Fdef);
         return B1;
       when FWangle |FWsquare =>  
            return False;
       when FAngle  =>
         if isBOX(F.Aref) then 
            -- <>#
            return Check_VactlBoxLiveTrue(F.FormRef);         
         else
            -- <>
            return False;
         end if;
       when Fsquare =>
         if isBOX(F.Aref) then  
            --[]#
            return False;      
         else
            -- []
            B1 :=  Check_VactlBoxLiveTrue(F.FormRef);
            return B1;
         end if;
       when Fexist | Fall =>
         case F.Pref.Kind is
           when Eventually =>   --  EF AF
              if F.Kind= Fexist then
                return False;
              else
                return Check_VactlBoxLiveTrue(F.Pref.TForm);
              end if;
           when Always =>
             if F.Kind = Fexist then
                return False;   --  EG
             else
                return Check_VactlBoxLiveTrue(F.Pref.TForm);    -- AG
             end if;
           when  Act_Next => 
              -- EX#  AX#
              if isBOX(F.Pref.AAref) then
                return Check_VactlBoxLiveTrue(F.Pref.AFormRef);
              end if;
              if F.Kind= Fall then   
                 -- AX
                 return Check_VactlBoxLiveTrue(F.Pref.AFormRef);
              else
                 return False;
              end if;
           when  Until1 | WUntil1 =>    -- EF#  AF#  AG#
              if isBOX(F.Pref.U1ARef) and then
                  Check_VactlBoxLiveTrue(F.Pref.U1FormRef1) then
                return Check_VactlBoxTrue(F.Pref.U1FormRef2);
              end if;
              if F.Kind= Fall and then       -- AF
                   Check_VactlBoxLiveTrue(F.Pref.U1FormRef1) then
                return Check_VactlBoxLiveTrue(F.Pref.U1FormRef2);
              end if;
              return False;
           when  Until2 | WUntil2 =>    -- EF#{} AF#{} 
              if isBOX(F.Pref.U2ARef1) and then
                   isBOX(F.Pref.U2ARef2)  then
                return Check_VactlBoxTrue(F.Pref.U2FormRef2);
              end if;
              if F.Kind= Fall  and then    --  AF{}
                 Check_VactlBoxLiveTrue (F.Pref.U2FormRef1) then 
                return Check_VactlBoxTrue(F.Pref.U2FormRef2);
              end if;
              return False;
         end case;
    end case;
  end Check_VactlBoxLiveTrue;
  
-- type Property_Status is (True, False, Aborted, Unchecked);
-- type State_Property is record
--   Status: Property_Status;
--   Image: String_Ref;
--   CompRef: Integer;
-- end record;
-- type State_Properties ia array (Positive range <>) of Property_Status;
-- type State_Properties_Ref is access type State_Properties;
--

  function Get_State_Properties (The_State: MyConfigurations.Kernel.System_Configuration) return State_Properties is
    These_Computations: Computations_Table := Get_State_Computations(The_State);
    These_Properties: State_Properties(These_Computations'Range);
    Status: Computation_Status;
  begin
    for K in These_Properties'Range loop
       Status := Get_Status(These_Computations(K));
       if Status = Found_True or Status = Tmp_True then 
          These_Properties(K).Status := True;
       elsif Status = Found_False or Status = Tmp_False then 
          These_Properties(K).Status := False;
       elsif Status = Aborted then 
          These_Properties(K).Status := Aborted;
       else 
          These_Properties(K).Status := Unchecked;
       end if;
       These_Properties(K).PImage := Get_Formula(These_Computations(K)).Fimage;
       These_Properties(K).CompRef := Integer(These_Computations(K));
    end loop;
    return These_Properties;
  end Get_State_Properties;
  
  
end UCTL;
