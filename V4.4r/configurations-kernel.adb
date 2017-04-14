with Interfaces;
with Flags; use Flags;
with Dyn_Store;
with NickNames;
with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash; 
with Ada.Containers.Hashed_Sets; 
with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Conversion;
--with GNAT.Semaphores; use GNAT.semaphores;
--with SYSTEM; use System;
separate(Configurations)
package body Kernel is
--
--  cosa ci vorrebbe :  ...
--
------------------------  KERNEL PRIVATE PART -----------------
--  private;
--  type System_Configuration_Data;
--  type System_Configuration is access System_Configuration_Data;
--
--  type Evolutions_Iterator is record
--     The_Conf: System_Configuration;
--       Current_Evolution: Natural :=0;
--  end record;
------------------------  END PRIVATE SPEC -------------
--
--  type System_Configurations_Table is array(Positive range <>) of System_Configuration;
--  type System_Configurations_Table_Ref is access System_Configurations_Table;  
--
--  type String_Tables_Vector_Array is array(Positive range <>) of String_Tables_Vector_Ref;
--  type String_Tables_Vector_Array_Ref is access String_Tables_Vector_Array;
--
--  type System_Configuration_Data is record
--     NickNum: Natural;
--     ORCIDS_Table: Int64_Table_Ref;
--     System_Evolutions: System_Configurations_Table_Ref;
--     Abstract_Trasition_Labels: String_Tables_Vector_Array_Ref;
--     Abstract_State_Predicates: String_Tables_Vector_Ref;
--  end record;
--
--
-- function Matching (SCD: System_Configuration_Data; KD: Int_Table_Ref) is 
-- begin
--  return SCD.Objects_Table.all = Kd.all;
-- end Matching;
--     
--
--  function Hashit (This_Table: Int_Table; prefix: String := "") return Natural;
--
--  function Mk_Key(This_Table: Int_Table;
--        Cache_Max: Natural := Key_Module) return Positive is
--    Result: Integer := 0 ;
--  begin
--    Result := HashIt(This_Table);
--    return Positive (Result +1);
--  end Mk_Key;
--  
--  function Initial_Element(Key: Int_Table_Ref) return System_Configuration_Data is
--    The_Conf: System_Configuration_Data;
--  begin
--    The_
--    return The_Conf;
--  end;

--  procedure Set_Progressive(N:positive; ER: Element_Ref) is
--  begin
--    ER.NickNum := N;
--  end;
--
--
--  package System_Conf_DB is 
--    new My_Hash(System_ Configuration_Data, System_ Configuration,
--    Int_Table_Ref,Matching,Mk_key,Initial_Element,Set_Progressive);


--
-- MEMORY FRIENDLY:  
--     NOTE:  function Get_Ground_Action_Labels: String_Table_Ref  Should be Freed by Caller!
--     NOTE:  function Get_Abstract_Action_Labels: String_Table_Ref  Should NOT be Freed by Caller!
--     NOTE:  function Get_Ground_Action_Labels: String_Table - its ELEMENTS should be Freed by caller!
--
--  OPTIMIZATION:   if "self is never used inside a class" we can avoid to initialize the corresponding 
--   field in the ORC structure, using the same data for all identical object components.
--  (not much essential in the railway case since tracks have differnt attributes in any case)
--
--  OPTIMIZATIONS: SRC_DB  insteak of full explicit int64_Tabble we could save a
--     previous nicknum and a table of changes. Saveing memory when MANY active objects are present. 
--
--  OPTIMIZATION.  Keeping queues outside ORCs wuould allow to recycle the same evolutions when
--      further events are queued in the object whose evolutions have already been compouted
--      (NOn clear if this would be really a gain)

--  OPTIMIZATIONS: chekcing objects priorities from TOP to botton to avoid generation of
--    evolutions of objects which cannot evolve.
--
--  MEMORY_SAVING:  Not Using SCI_DB:
---------------------------  SPECIFICATION -------------------------------------------
-- package Kernel is
--   -- -- -- --
--   type System_Configuration is private;
--   --
--   function "=" (left: System_Configuration; right: System_Configuration) return Boolean;
--   function Initial_Configuration return System_Configuration;
--   function Undefined_Configuration return System_Configuration;
--   --
--   function Get_Abstract_State_Labels(Current_Conf: System_Configuration) return String_Tables_Vector;
--   function Get_Ground_State_Label_Images(Current_Conf: System_Configuration) return String_Table;
--   function Progressive (This_Conf: System_Configuration) return Integer;
--   function Configuration_Satisfies(This_SRC: System_Configuration;
--                                    State_predicate: String_Table) return Boolean;
--   -- -- -- -- 
--   type Evolutions_Iterator is private;
--   --
--   procedure Iterator_Initialize ( It: in out Evolutions_Iterator;
--                                   This_Conf: System_Configuration;
--                                   Active_Object: Natural :=0);
--   procedure Iterator_Restart (It: in out Evolutions_Iterator);
--   procedure Iterator_Finalize (It: in out Evolutions_Iterator);
--   function  Has_System_Transition (It: Evolutions_Iterator) return Boolean;
--   procedure Iterator_Advance (It: in out Evolutions_Iterator);
--   function Get_Source_Configuration (It: Evolutions_Iterator) return System_Configuration;
--   function Get_Target_Configuration (It: Evolutions_Iterator) return System_Configuration;
-- --   function Get_Abstract_Action_Labels (It: Evolutions_Iterator) return String_Tables_Vector_Ref;
--   function Get_Abstract_Action_Labels (It: Evolutions_Iterator) return String_Tables_Vector;
-- --   function Get_Ground_Action_Labels (It: Evolutions_Iterator) return String_Table_Ref;
--   function Get_Ground_Action_Labels (It: Evolutions_Iterator) return String_Table;
--   -- -- -- --
--   function StatesSpace_Size return Int64;
--   function StatesSpace_Stats return String;
--   function FindFromKey(Element_Key: String_Table_Ref) return System_Configuration;
--   function GetUniqueKey(This_Conf: System_Configuration) return String_Table_Ref;
--   function GetProgressive(Num: Integer) return System_Configuration;
--   --
--   procedure Load_Model (File_Name: String);
--   procedure Print_Configuration(This_Conf: System_Configuration);
--   procedure HTML_Print_Configuration(HTML_File: File_Type; This_Conf: System_Configuration);
--   procedure HTML_Print_Model(HTML_File: File_Type);
--   procedure HTML_Print_System_Structure(HTML_File: File_Type; This_Conf: System_Configuration);
--   procedure HTML_Print_Possible_Evolutions (HTML_File: File_Type; This_Conf: System_Configuration);
--   --  VMC liveness check-- -- --
--   procedure Check_Liveness(This_Conf: System_Configuration);
--   -- --  debugging -- -- --
--   procedure Dump_States;
--   --
-- private
--   type System_Configuration is new Integer;
--   --
--   type XXX_Iterator;
--   type Evolutions_Iterator is access XXX_Iterator;
-- end Kernel;
-- use Kernel;
-------------------------- STRUCTURE  ---------------------------------
--  package Global_Env;    -- defined Nicknames / Dyn_Store
--  package UML_Types;
--  package System_Model
--  package UML_Parser     -- procedure Parse 
--  package UML_Configurations
--  package UML_Explore
------------------------------------------------------------------------
--  --------------------------   MORE   BASIC TYPES -------------------------
--  type Bool_Table is array (Positive range <>) of Boolean;
--  Empty_Bool_Table: Bool_Table(1..0);
--  type Bool_Table_Ref is access Bool_Table;
--  --
--  type Int_Table is array (Positive range <>) of Integer;
--  Empty_Int_Table: Int_Table(1..0);
--  type Int_Table_Ref is access Int_Table;
--  procedure Free is new Ada.Unchecked_Deallocation(Int_Table,Int_Table_Ref);
--  --
--  type Int64_Table is array (Positive range <>) of Int64;
--  Empty_Int64_Table: Int64_Table(1..0);
--  type Int64_Table_Ref is access Int64_Table;
--  type Int64_List_Table is array (Positive range <>) of Int64_Table_Ref;
--  procedure Free is new
--       Ada.Unchecked_Deallocation(Int64_Table,Int64_Table_Ref);
--  --
--  type List_Table is array (Positive range <>) of Num_Table_Ref;
--  Empty_List_Table: List_Table(1..0);
--  type List_Table_Ref is access List_Table;
-- --
--  type Array_Table is array (Positive range <>) of Int_Table_Ref;
--  Empty_Array_Table: Array_Table(1..0);
--  type Array_Table_Ref is access Array_Table;
--  ----------------------------------------------------------------------------


use Global_Env;

--####################################################################################
package UML_Types is
--
-- pragma Elaborate_Body;
--

type Bool_Ref is access Boolean;
type BoolMat is array  (Positive range <>, Positive range <>) of Bool_Ref;
type BoolMat_Ref is access BoolMat;

StructBase: constant Integer := - 1_000_000_000;
ObjectBase: constant Integer := - 2_000_000_000;
--
IntFalse: constant Integer  := Integer'First;  -- was 0 (0 when used as index)
IntTrue: constant Integer  := Integer'Last;    -- was 1 (1 when used as index)
IntNullObject: constant Integer := ObjectBase;    -- was 0 
IntOUTObject: constant Integer := ObjectBase-1;    -- was 0 
IntEmptyStruct: constant Integer := StructBase - 1;   --was 0; (must be item #1 in Vectors_DB)
--
--UndefinedStructure: Constant Integer := StructBase;
--
Is_Active: constant String_Ref := new String'("Is_Active");
--
-- entita' dichiarate tramite: Vars  x,y,z,w
--

type Value_Kind is (SpecialToken, umlString, Undefined, Composite,
                    Object, Bool, Number,
                    Objvector, BoolVector, Numvector, Vector,
                    Objmatrix,Boolmatrix, Nummatrix, Matrix,
                    Objcube,Boolcube,Numcube, Cube);

--
subtype  TypeInfo is Natural;  -- for obj types, if positive, is the Class index
--
type Basic_Value is record
  Code:Integer :=0;
  Kind: Value_Kind := Undefined;
end record;
--
type SystemVar;
type SystemVar_Ref is access SystemVar;
type Vars_Table is array (Positive range <>) of SystemVar_Ref;
type Vars_Table_Ref is access Vars_Table;
No_Vars: Vars_Table := (1..0 => null);

--
-- entita' dichiarate tramite:  Events  a(x), b(y),e(z)
--
-- event names e events vars devono essere diverse da system vars
type EventVar;
type EventVar_Ref is access EventVar;
type EventVars_Table is array (Positive range <>) of EventVar_Ref;
type EventVars_Table_Ref is access EventVars_Table;
--
-- The type of Trigger or Transition variables costituting the Transition Env
type EventVar is record  --
  Name: String_Ref;    -- the name of the var
  Num_Key: Positive;   -- the index in the Transition Env
  Kind: Value_Kind := Undefined;  
  TypeInfo: Natural :=0;  -- Kind=Object -> TypeInfo=Class index in All_Classes 
end record;
No_EventVars: constant EventVars_Table := (1..0 => null);


type IntExpr;
type IntExpr_Ref is access IntExpr;
type IntExpr_Table;
type IntExpr_Table_Ref is access IntExpr_Table;

--  SIMPLE INTEGER EXPRESSIONS
-- expressione che compare in guards, assignments, e output actions
--  e.g.  x,y,z, 40 
--  OCCHIO   che   objvar.selection  e' permessa e indica una function call  !!!
--
type SimpleIntExpr;
type SimpleIntExpr_Ref is access SimpleIntExpr;
type SimpleIntExpr_Table is array (Positive range <>) of SimpleIntExpr_Ref;
type SimpleIntExpr_Table_Ref is access SimpleIntExpr_Table;
--
type SimpleIntExpr is record
  Literal_Value: Integer :=0;
  Image: String_Ref;
  Local_Variable: Natural :=0 ;   -- ChartVars(E.Local_variable) = V
  Event_Variable: EventVar_Ref;
  Remote_Variable: Natural :=0;     ---  localvar.remotevar,  eventvar.remotevar
  Special_Token: String_Ref;        -- Head, Tail, Length, Null, Self, This, ...
  Kind: Value_Kind := Number;
  Is_Vector: SimpleIntExpr_Table_Ref;
  Is_Indexing: IntExpr_Table_Ref;     -- this is the index expression -- should become IntExpr_Table_Ref
  Head_Tail_Data : SimpleIntExpr_Ref;   --- added 30-06-2016
end record;

OUTObject: constant SimpleIntExpr_Ref := 
           new SimpleIntExpr'(IntOUTObject, new String'("OUT"), 0, null, 0, null, Object,null,null,null);

-- INTEGER EXPRESSIONS
-- e.g.  x+y, y+3, 4-3
type IntOp is (No_Op, Plus , Minus, Times, Div, Modulus, Enclosed, Join);
type IntExpr is record
 Left: IntExpr_Ref;
 Op: IntOp := No_Op;
 Right: IntExpr_Ref;
 Simple: SimpleIntExpr_Ref;  
end record;

Zero: constant IntExpr_Ref :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(0, new String'("0"), 0, null, 0, null, Number,null,null,null));
Uno: constant IntExpr_Ref := 
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(1, new String'("1"), 0, null,0,  null, Number,null,null,null));

BoolTrue: constant IntExpr_Ref  :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntTrue, new String'("true"), 0, null, 0, null, Bool,null,null,null));
BoolFalse:constant IntExpr_Ref  :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntFalse, new String'("false"), 0, null, 0, null, Bool,null,null,null));

-- further initialized by UML_Types.adb
NullStruct: IntExpr_Ref := 
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntEmptyStruct, new String'("[]"), 0, null, 0, null, Composite,null,null,null));
pragma Volatile(NullStruct);

-- further initialized by UML_Types.adb
NullObject: IntExpr_Ref := 
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntNullObject, new String'("null"), 0, null, 0, null, Object,null,null,null));
pragma Volatile(NullObject);

type IntExpr_Table is array (Positive range <>) of IntExpr_Ref;
Empty_IntExpr_Table: constant IntExpr_Table := (1..0 => null);
Empty_IntExpr_Table_Ref: constant IntExpr_Table_Ref := new IntExpr_Table'(Empty_IntExpr_Table);

-- INTEGER TO BOOLEAN EXPRESSIONS
--  e.g.   x<4, y+1>x-1, z+y>0
type IntBoolOp is ( LT, GT, EQ, NE, LE, GE);

type IntBoolExpr is record
  Left: IntExpr_Ref;
  Op: IntBoolOp;
  Right: IntExpr_Ref;
end record;

-- BOOLEAN TO BOOLEAN EXPRESSIONS
--  e.g.  (z+y>0  & x<4 )
type BoolBoolOp is (NoOp, AndOp, OrOp, NotOp);
-- TRANSITION GUARDS
type BoolBoolExpr;
type BoolBoolExpr_Ref is access BoolBoolExpr;
subtype Guard_Ref is BoolBoolExpr_Ref;

type BoolBoolExpr (Kind: BoolBoolOp := NoOp) is record
  case Kind is
    when NoOp => 
       Compare: IntBoolExpr;
    when others =>
       Left: Guard_Ref;
       Right: Guard_Ref;
  end case;
end record;

type  umlExpr is record
   umlInt: IntExpr_Ref;      -- integer expression, obj, literals, 
   umlBool: BoolBoolExpr_Ref;  -- composite boolean expressions
end record;

type umlExpr_Ref is access umlExpr;
type umlExpr_Table is array (Positive range <>) of umlExpr_Ref;
type umlExpr_Table_Ref is access umlExpr_Table;
Empty_umlExpr_Table: umlExpr_Table := (1..0 => null);

-- il Kind della systemvar e' definito da Systemvar-initial.kind
type SystemVar is record
  Name: String_Ref;
  Global_Num_Key: Positive;   --    All_Vars(V.Num_Mey) = V    --- UNUSED !!!!!!!
  Local_Num_Key: Positive;   
  Chart: Natural :=0;          --  index in All_Charts
  Observed: Boolean := False;
  Initial: SimpleIntExpr_Ref;    --- constant integer literal or chartname  
  Kind: Value_Kind := Undefined;
  TypeInfo: Natural :=0;  -- Kind=Object -> TypeInfo=Class index in All_Classes  
end record;

-- STATES:  SIMPLE, COMPOSITE, CONCURRENT, REGION, 
type State;
type State_Ref is access State;
type States_Table is  array (Positive range <>) of State_Ref;
type States_Table_Ref is access States_Table;

-- SYSTEM EVENTS: as declared by: SystemEvents  r2,a2,e1,e2,f1,r1,e(x), a(x) 
type Event;
type Event_Ref is access Event;
type Events_Table is array (Positive range <>) of Event_Ref;
type Events_Table_Ref is access Events_Table;
No_Events: constant Events_Table := (1..0 => null);

-- STATE TRANSITIONS
type Transition;
type Transition_Ref is access Transition;
type Transitions_Table is array (Positive range <>) of Transition_Ref;
type Transitions_Table_Ref is access Transitions_Table;

-- IL CATALOGO DI TUTTE LE STATE TRANSITIONS CLASSIFICATE PER TRIGGERING EVENT
type Triggered_Transitions is record
   This_Event: Event_Ref;                   -- Null event in case of completion
   These_Transitions: Transitions_Table_Ref;
end record;
type TransitionsCatalogue is 
     array (Positive range <>) of Triggered_Transitions;
type TransitionsCatalogue_Ref is access TransitionsCatalogue;

type Event_Kind is (Signal, Operation, Undefined);  --  NEEDED  UNDEFINED
-- SYSTEM EVENTS come da appropriata dichiarazione,  riferiti 
--  all'interno delle transitions come parte del trigger e della action
type Event is record
   Name: String_Ref;
   Num_Key: Natural := 1;     -- indice dell'evento nella tabella All_Events
   Params: EventVars_Table_Ref;  -- a 0 or positively sized table.
   Observed: Boolean := True;   -- TOBEMOVED
   Return_Type: Value_Kind := Undefined; 
   Kind: Event_Kind := Undefined;
-- TypeInfo:Natural :=0  -- TO BE ADDED
end record;
--
-- Each (active =declared) Chart has  ChartEvents(1) = Null_Event
--
Null_Event: constant Event_Ref :=  
     new Event'(Name => new String'("-"), 
--              Chart => 0,
                Num_Key => 1, 
                Params => new EventVars_Table'(No_EventVars),
                Observed => False,
                Return_Type => Undefined,
                Kind => Signal);
--
--  OUTChart.ChartEvents(1) = Null_Event
--  ERRChart.ChartEvents(2) = Lost_Event
--  OUTChart.ChartEvents(3) = Assign_Event
--
Lost_Event: constant Event_Ref :=
     new Event'(Name=> new String'("lostevent"),
                Num_Key => 2,
                Params => new EventVars_Table'(1..1 => 
                        new EventVar'(new String'("id"),1,Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
Assign_Event: constant Event_Ref :=
     new Event'(Name=> new String'("assign"),
                Num_Key => 3,
                Params => new EventVars_Table'(
                        new EventVar'(new String'("var"),1,Undefined,0),
                        new EventVar'(new String'("ind"),2,Undefined,0),
                        new EventVar'(new String'("val"),3,Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
Runtime_Error_Event: constant Event_Ref :=
     new Event'(Name=> new String'("Runtime_Error"),
                Num_Key => 4,
                Params => new EventVars_Table'(
                        new EventVar'(new String'("val1"),1,Undefined,0),
                        new EventVar'(new String'("val2"),2,Undefined,0)),
--                Params => new EventVars_Table(1..0),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturn_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 5,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnInt_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 6,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Number,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObj_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 7,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Object,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBool_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 8,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Bool,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnIntV_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 9,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Numvector,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObjV_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 10,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Objvector,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBoolV_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 11,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Boolvector,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnIntM_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 12,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Nummatrix,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObjM_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 13,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Objmatrix,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBoolM_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 14,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Boolmatrix,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnIntC_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 15,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Numcube,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObjC_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 16,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Objcube,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBoolC_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 17,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Boolcube,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);

-- EVENTUALLY AN EVENT_INSTANCE MIGHT REFER TO A TARGET OBJECT.
type Event_Instance is record
 The_Event: Event_Ref;
 The_Target: SimpleIntExpr_Ref;  
 The_Args: umlExpr_Table_Ref;  -- a 0 or positively sized table
end record;
--
type Events_Instance_Table is array (Positive range <>) of Event_Instance;
type Events_Instance_Table_Ref is access Events_Instance_Table;
Empty_Events_Instance_Table: Events_Instance_Table(1..0);
  ------ := (1..0 => (null,0,null));

-- TRANSITION GUARDS
--subtype Guard_Expr is BoolBoolExpr;
--type Guard_Ref is access Guard_Expr;

-- TRANSITION ACTION(S) 
-- come appare in   ... -(../event(x+1))-> ...
type Action;
type Action_Ref is access Action;
type Actions_Table is array (Positive range <>) of Action_Ref;
type Actions_Table_Ref is access Actions_Table;
No_Actions: constant Actions_Table := (1..0 => null);
--
--   Action_Kind = Call  is EVER USED?????? or opcalls are just mapped in Signals?
type Action_Kind is (Signal, Call, Function_Call, Assignment, OpReturn, 
                      Whileloop, Exitloop, Forloop, VarDecl, Conditional);
type Action is record
   Kind: Action_Kind := Signal;
   Signalled_Event: Event_Instance;    -- return, signal, opcall, funcall,
    -- Assignment_Left is the local reference to the SystemVar. (index in localvars) 
    --                references to Eventvars have a negative value (index in tenv)
   Assignment_Left: Integer;     -- positive values and indexes of Class Vars
                                 -- negative values are indexes of Transition Vars
   Assignment_Left_Image: string_ref;
--   Assignment_Left_Index: IntExpr_Ref;
   Assignment_Left_Indexes: umlExpr_Table_Ref := new umlExpr_Table'(Empty_umlExpr_Table);
   Assignment_Right: umlExpr_Ref;
   LoopCond: BoolBoolExpr_Ref;    -- while loop
   LoopBody: Actions_Table_Ref;   -- while loop,  for loop
   TVar: EventVar_Ref;    -- Kind = Vardecl
   TValue: umlExpr_Ref;   -- Kind = Vardecl
   IfCond: BoolBoolExpr_Ref;   -- Kind = Conditional
   ThenBody:  Actions_Table_Ref;  -- Kind = Conditional
   ElseBody: Actions_Table_Ref;  -- Kind = Conditional
   For_Var: EventVar_Ref;  -- for loop
   For_VarMin: EventVar_Ref;  -- for loop
   For_VarMax: EventVar_Ref;  -- for loop
   For_Min: IntExpr_Ref; -- for loop
   For_Max: IntExpr_Ref; -- for loop
   Env_Depth: Natural :=0;
end record;

--  FULL STATES DEFINITION
-- stateref are relocatable !!!  I.e.  ChartStates can be simple copied !!!(
type State_Kind is (Parallel, Composite, Simple);
type State is record
  Kind: State_Kind := Simple;
  SubStates: States_Table_Ref := new States_Table(1..0);
  LocalTransitions: Transitions_Table_Ref
      := new Transitions_Table(1..0);  -- only for Composite States
  OutgoingTransitions: TransitionsCatalogue_Ref := 
          new TransitionsCatalogue(1..0);
  FullName: String_Ref;
  Parent: State_Ref;
  FirstRuntimePosition: Natural :=0;  -- set by uml_model  LOCAL to the chart
  LastRuntimePosition: Natural :=0;   -- set by uml_model  LOCAL to the chart
  Depth: Positive := 1;   --  Parallel states are considered for the depth
  Priority: Positive := 1;--  Parallel States are NOT considered 
  Ancestors: States_Table_Ref;    -- set by Set_Ancestors  (relocatable)
  Num_Key: Positive;    -- local ref
--  Chart: Natural :=0;    
  Finals: States_Table_Ref;  -- set by Set_Finals for composite states
  Deferred: Events_Table_Ref := new Events_Table(1..0);   -- list of deferred events 
  EntryActions: Actions_Table_Ref;
  ExitActions: Actions_Table_Ref;
end record;


type Transition_Kind is (Simple, Internal, Synch, Join, Fork);
type Transition is record
  Label: String_Ref;       
  Trigger: Event_Ref;     
  Guard:  Guard_Ref;      
  Actions: Actions_Table_Ref;
  Source: States_Table_Ref; 
  Target: States_Table_Ref;
  Kind: Transition_Kind := Simple; 
  Owner: State_Ref;      
  Mask:  States_Table_Ref;    -- non dovrebbe essere globale, se valida per ogni
  Num_Key: Positive;
end record;

--  
-- il tipo  "Chart" e' utilizzato (in modo ambiguo) per descrivere sia le classi che gli
-- oggetti (istanze delle classi) del sistema.
-- Variabili di tipo "Chart" sono memorizzate sia in "All_Classes" (tabella delle classi)
--   sia in All_Charts (tabella degli oggetti sia passivi che attivi).
-- La lista degli oggetti "attivi" (dei loro indici in All_Charts) e' memorizzata in Active_Objects
-- Una dichiarazione di "Chart" crea sia una entri come class, che una come object (quindi due istanze
--  del tipo Chart)
--
type Chart is record
  Name: String_Ref;            -- relocatable
  Chart_Position: Natural := 0;  -- index in Active_Charts
  Top_State_Num: Integer := -1; -- set to 1 for active charts, (i.e. charts with vars or states)
      --  Top_State_Num=0 indica una classe passiva (senza transizioni), 
      --  eventualmente una classe con solo attributi pubblicamente accessibili,
      --  che e' eventualmente possibile estendere in un tempo successivo con un body.
      --  (che puo aggiungere nuovi attributi e nuovo behavior)
      -- Top_State_Num=1 indica una classe di cui e' stato definito il body
      --   e quindi la cui definizione e' congelata.
      --Top_State_Num=-1  indica un placeholder di class (ancora da definire)
  ChartParent: Natural :=0;    -- the parent (or class) definition for the child
  ChartVars: Vars_Table_Ref := new Vars_Table'(No_Vars);    -- SPECIFIC
  ChartEvents: Events_Table_Ref := new Events_Table(1..0);  -- relocatable
  ObservedEvents: Bool_Table_Ref := new Bool_Table(1..0);  -- SPECIFIC
  ChartStates: States_Table_Ref:= new States_Table(1..0);   -- relocatable
  ChartTransitions: Transitions_Table_Ref := new Transitions_Table(1..0);
  ChartStates_Base: Natural := 0;   -- set by Clone ..
  ChartInterferences: BoolMat_Ref;    -- shared among all objects of the class
  NiceParallelism: Boolean := False;
end record;

type Chart_Table is array (Positive range <>) of Chart;
type Chart_Table_Ref is access Chart_Table;
--
-- Initialized by the parser (Parse)
--

 type Binding is record
    Attribute: String_Ref;   --  ChartVars(The_Var).Name
    Value_Image: String_Ref;     --  Inital_Expr.Image
 end record;

 type Bindings_Table is array (Positive range <>) of Binding;
 type Bindings_Table_Ref is access Bindings_Table;

 type UML_Object is record
   The_Object: Natural :=0;  -- index in All_Charts
   Bindings: Bindings_Table_Ref := new Bindings_Table (1..0);
 end record;

 type Objects_Table is array (Positive range <>) of UML_Object;
 type Objects_Table_Ref is access Objects_Table;

 -- updated by UML_parser
 UML_Configuration: Objects_Table_Ref := new Objects_Table(1..0);
 pragma Volatile(UML_Configuration);

--  The top-level state (the first declared)
-- This_UML_Object: State_Ref;
--
--  All Charts are all the chart explicilty declared by Chart daclarations
--  and (called active charts) and all charts mentioned of prefix of signals
--  (however still not being var names).  E.g.  OUT,  ERR of just the names
--  of other entities not currently packaged inside the system.
--
OutChart: constant Chart  := (Name => new String'("OUT"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     Chartparent => 1,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0) ,
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );
ErrChart: constant Chart   := (Name => new String'("ERR"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 2,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );
TokensChart: constant Chart   := (Name => new String'("Token"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );
OBJ_UnspecifiedError: constant Chart   := (Name => new String'("UnspecifiedError"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

OBJ_InvalidSpecialToken: constant Chart   := (Name => new String'("InvalidSpecialToken"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

OBJ_InvalidIndex: constant Chart   := (Name => new String'("InvalidIndex"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

OBJ_CodingBug: constant Chart   := (Name => new String'("CodingBug"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

OBJ_InvalidAbstraction: constant Chart   := (Name => new String'("InvalidAbstraction"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

OBJ_InvalidTarget: constant Chart   := (Name => new String'("InvalidTarget"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

OBJ_InvalidStructure: constant Chart   := (Name => new String'("InvalidStructure"),
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False
                    );

-- updated by UML_Parser
All_Classes: Chart_Table_Ref  :=  new Chart_Table'(OutChart, ErrChart, TokensChart);
pragma Volatile (All_Classes);

All_Charts: Chart_Table_Ref  := 
      new Chart_Table'
        (OutChart,ErrChart,TokensChart,OBJ_UnspecifiedError,OBJ_InvalidSpecialToken,OBJ_InvalidIndex,
         OBJ_CodingBug,OBJ_InvalidAbstraction,OBJ_InvalidTarget,OBJ_InvalidStructure);
pragma Volatile (All_Charts);

Predefined_Charts_Count : constant Natural := All_Charts.all'Length;
--
-- Active Charts are only those explicitly declared by "Chart" declarations
-- (for which an explicit Top State has been declared)
--  (it is table of Chart indexes, extended with each new Current_Chart)
--
Active_Charts: Num_Table_Ref := new Num_Table(1..0);                          --!!!!
pragma Volatile(Active_Charts);

function Is_Active_Chart(The_Chart: Natural) return Boolean;

-- The initial systemevent Queue;
-- This_Initial_Queue: Events_Instance_Table_Ref :=  -- a 0 or >0 sized table
--      new Events_Instance_Table'(Empty_Events_Instance_Table);

--  type Evolution_Data is record
--     Source: Int64;    -- Nick in SRC_DB
--     Selector: Natural;  -- index in All_Charts
--     Actions: Int64;   -- Nick in Signals_DB
--     Target: Int64;  -- Nick in SRC_DB
--     AbstractLabels: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
--  end record;
--  No_Evolution: Evolution_Data := (0,0,0,0,null);
--  type Evolutions_Table is array (Positive range <>) of Evolution_Data;
--  Empty_Evolutions_Table : Evolutions_Table := (1..0 => No_Evolution);
--  type Evolutions_Table_Ref is access Evolutions_Table;
-- procedure Free is new Ada.Unchecked_Deallocation(Evolutions_Table,Evolutions_Table_Ref);

-------------------  GLOBAL DATA -----------------------
-- The table of all the Chart Variables definition
--  further initialized by Parse_System_Vars
--
--  They exactly correspond to the Runtime vars vector
--  Their Num_Key is their runtime position in the runtime confuguration
--
-- All_Vars_Count: Natural :=0;
--All_Vars: Vars_Table_Ref := new Vars_Table'(No_Vars);


-- The table of all system Events (parametric) definitions
--  further initialized bu Parse_Events
--
--  All_Events e' una lista GLOBALE a tutte le classi di
--   "nomi" di eventi (senza altri dati). 
--  Quando viene elaborato un signal di una classe, il corrispondente evento
--  viene aggiunto alla lista (se non c'e' gia'), inizializzato con il suo nome
--  ed il numero dei suoi parametri.
--  Quando viene elaborata una dichiarazione di classe, il corrispondento 
--  evento viene aggiunto alla lista. Se c'e' gia' viene verificata la
--  corrispondenza con il suo precedente profilo, che se incompleto (e.e.
--  nomi dei parametri mancanti) viene completato.
--  In questo caso la dichiarazione completa viene anche aggiunta alla
--  lista locale degli eventi della classe.
--
--  Il campo Num_Key e' l'indice dell'evento nella tabella All_Events.
--
All_Events: Events_Table_Ref := 
   new Events_Table'(Null_Event, Lost_Event, Assign_Event, Runtime_Error_Event, 
                     OpReturn_Event,
                     OpReturnInt_Event,OpReturnObj_Event,OpReturnBool_Event,
                     OpReturnIntV_Event,OpReturnObjV_Event,OpReturnBoolV_Event,
                     OpReturnIntM_Event,OpReturnObjM_Event,OpReturnBoolM_Event,
                     OpReturnIntC_Event,OpReturnObjC_Event,OpReturnBoolC_Event); 
        --   Null_Event = "-";  
        --   Lost_event= "ERR.lostevent(id)"
        --   Assign_event= "OUT.assign(var,index, val)
pragma Volatile(All_Events);

-------------------- observation utilities ----------------------
 --
 function Eval_Literal (Id: String) return Integer;
 --
-- Observed_Objects: Num_Table_Ref := new Num_Table(1..0); -- indexes in All_Charts
 --
-- procedure Reset_Observations;
-- procedure Set_Observations (Mode: Observation_Kind; Args: String);
 --
-- function Is_Observable(Current_Chart: Natural; 
--                     This_Action: Action_Ref) return Boolean;
-- function Is_Observable(Target, Event_key: Natural) return Boolean;

--procedure HTML_Settings;
--procedure TXT_Settings;

--procedure Set_Parameter(Input_Line: String);

---------------------------------------------------------------------------
--           OBSERVATIONS 
--           
type AbstractionKind is (StateKind, ActionKind, BothKinds);
--
type BinOp is (NOOP, EQ, GT, LT, LE, GE, NE); 
--
type Rule_Left_Elem is record
  LTerm: String_Ref;                                  -- src
  LPartner: String_Ref;                               -- target
  LOp: String_Ref;                                    -- event /obj (inState clause)
  LMode: String_Ref;
  LArgs: String_Table_Ref;                            -- [arg1,arg2]/ (Top.S1.s2)
  ----
  IdsL: String_Table_Ref := Empty_String_Table_Ref;   -- [obj1,var]/$v/literal/
  LeftOp: BinOp :=NOOP;                               --  EQ/NE/LT/GT/LE/GE
  IdsR: String_Table_Ref :=  Empty_String_Table_Ref;  --  [obj2,var]/$v/literal
end record;

type Rule_Left is array (Positive range <>) of Rule_Left_Elem;
type Rule_Left_Ref is access Rule_Left;

type Observation_Rule is record
  Kind: AbstractionKind := BothKinds;                 -- State/Action
  Left: Rule_Left_Ref;
  RLabels: String_Table_Ref := Empty_String_Table_Ref;--  [mainlabel,aarg1,aarg2]
end record;
 --
 -- State:   is_active(obj,state) and obj.var=3  ->  mainlabel(aarg1,aarg2)
 -- State:   inState(obj.state) and obj.var=$v  ->  mainlabel(aarg1,$v)
 -- State:   obj.var1 > obj.var2  ->  label(op,op,1)
 -- Action:  src:target.event<arg1,arg2> -> mainlabel(aarg1,aarg2)
 -- Action:  src:target.event!<$1,$2,*,a>   ->  mainlabel(event,$2,$1)
 -- Action:  $1:$2.$3!<>   ->  mainlabel($3,$2,$1)

 type Abstractdata is record
  Labels: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
 end record;

Emptydata: Abstractdata;
Tau: Constant Abstractdata := Emptydata;

type Observations_Table is array (Positive Range <>) of Observation_Rule;
type Observations_Table_Ref is access Observations_Table;
Default_Rules: constant Observations_Table(1..1)  :=
 (1..1 =>   -- $1($*) -> $1($*)
                (ActionKind,
                 new Rule_Left'( 1 => 
                     (null,null,new String'("$1"),null,
                      new String_Table'(1..1 => new String'("$*")),
                      null, noop,null)),
                 new String_Table'(1 => new String'("$1"),
                                  2 => new String'("$*"))));

All_Observations: Observations_Table_Ref := new Observations_Table(1..0);
pragma Volatile(All_Observations);

--------------------------------------------
  function Has_Random_Queues(Index:Natural) return Boolean;

 --------------   Print  utilities ----------------------------

 function Value_Image(This_Var:SystemVar_Ref; 
                      This_Value:Integer) return String;
 function Value_Image (Code: Integer; Kind: Value_kind) return String;
 function Signal_Image (The_Signal: Int_Table; 
                          Target_Included:Boolean := True) return String;
 function Event_Image (The_Signal: Int_Table) return String;
 function Target_Image (The_Signal: Int_Table) return String;
 function Arg_Image (The_Signal: Int_Table; 
                       ArgNum: Positive; Chart:Natural) return String;
 function Args_Count (The_Signal: Int_Table) return Natural;
 function Normalized_Literal(Source: String) return String;
 function Trigger_Image (The_Signal: Int_Table) return String;
 function Actions_Image (These_Actions: Actions_Table) return String;
 function BoolBoolExpr_Image (This_BoolBoolExpr: BoolBoolExpr) return String;
 function IntExpr_Image (This_IntExpr: IntExpr) return String;
 function IntExpr_Kind (This_IntExpr: IntExpr) return Value_Kind;
 function umlExpr_Kind (This_umlExpr: umlExpr) return Value_Kind;
 function SignalAct_Image (This_Instance: Event_Instance) return String;
 function Transition_Image (This: Transition_Ref;
                              Label_Included: Boolean := True) return String;
 function Transition_DotImage (This: Transition_Ref;
                              Label_Included: Boolean := True) return String;
 function Transition_Label (This: Transition_Ref) return String;
 function Transitions_Image (This_Sequence:Transitions_Table) return String;

  function Is_Final (This: State_Ref) return Boolean ;
  function Is_Default_Initial (This: State_Ref) return Boolean;
end UML_Types;
--use UML_Types;
--######################################################################################
--
--  SRC: Int64_Table(1..Active_Charts.all'Length);
--          --  SRC(I) := abs(ORC_DB.NickNum(ORC));
--  Initial_SRC: Int64 := abs(SRC_DB.NickNum(SRC));
--  Initial_Conf: System_Configuration := System_Configuration(Initial_SRC);
    
-- System_Configuration e' strutturalmente un Integer.
-- Esso donota un indice nel dabase SRC_DB i cui elementi sono  vettori di interi.
-- Ogni elemento del vettore denota un indice nel database ORC_DB i cui elementi denotano
-- i possiibili stati dei singoli oggetti che costituiscono il sistema.
-- (se il system ha 3 oggetti, la SRC sara il vettore degli indici dei loro tre stati)
--------------------------------------------------------------------------------------
--  Gli element della SCR_DB dovrebbero essere coppie <Object_state, Additional_Queue>
--  Se Object_State ha gia delle evoluzioni nuovi elementi da incodare vanno nella 
--  additional_queue senza dover ricalcolare il tutto.
--  Se le evoluzioni delle Object_State non sono ancora stata calcolate, essor viene
--  esteso con gli elementi della additional_queue e le sue evoluzioni ricalcolate
--  Object_State non ha possibili evoluzione, viene esteso prendendo in considerazione
--  gli elementi della additional_queue e le sue evoluzioni ricalcolate.
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
--  Nota bene  SCI_DB  e' un Dyn_STORE. 
--  L'indice degli elementi e' dato dai  NickNum del SCR_DB.
--
--  IL DB PORTEBBE ESSERE DI PUNTATORI A SYSTEM_COMNFIGURATION INFO
--  IN  MODO CHE MAN MANO CHE VIENE DEFINITO LA MEDESIMA STRUTTURA VIENE INCREMENTATA.
--  COSI COME E' ORA AD OGNI UPDATE VIENE FATTO IL FREE DELLA
--  SCI PRECEDENTE E ALLOCATA EX NOVO LA SCI ATTUALE.
-------------------------------------------------------------------------------------
--
--  SRC_DB = [ Element   | Element  | ... ]
--               ||        Elements are tables of Object_Runitime_Configurations
--               ||
--  Int64_Table [ orc1, orc2, ... ]   indexed by Active_Charts'Range
--                      ||  Elements are Nicknums of Object_Runtime_Configuration provided by ORC_DB
--                      ||
--  Object_Runtime_Configuration [ ....]   
--
-- - - - - - - - - - - - - - - - - - - - - - - - -
--  SE Active_Charts'Length >20 si puo usare un rappresentazione implicita dello stato:
--  Int64_Table [ orc1, orc2, ... ] 
--     if orc1 > 0    orci  are all NickNums provided by ORC_DB
--     if orc1 <0     -orc1  is the NickNum inside SRC_DB of a "base" SRC system configuration
--     the subsequent pairs  orci, orc1+1 are as follows:
--        orci an index inside the "base" SRC configuration
--        orc1i+1  an a new orc value to be sustituted at the previousÃ²ly given index.
--     I.e. if  -orc1 is the NickNum of the Element "srcbase"
--     Actual Current System Configuration =  currentbase where
--     currentbase[i] = srcbase[i]  if is different from all current orci  [even i]
--     currentbase[i] = orcj+1      if j =  orci for one i
--   Oni tanto (e.g. ogni 5 transizioni, si rimette lo stato completo)
--   Per poter fare cio' e' necessaria una versione modificare di "="  
--   due src elements sono uguali se donotano la medesima configurazione attuale, non se
--   e' uguale la loro rappresentazione implicita.
-------------------------------------------------------------------------------------
--                   NO MORE USED !!!!!!!!
--   SCI_DB  =   [ Element |  Element | ... ]  Indexed by Nicknums of System_Configurations from SRC_DB 
--                                 ||  Elements are pointers to a System_Evolutions_Table.
--                                 ||
--   System_Configuration_Info  [ System_Evolutions ]      
--                                  |
--                                  |
--                                  V
--    System_Evolutions_Table   [  |  |  |  |  |  |  ]   indexed by Active_Charts'Range
--                                |
--                                |  
--                                V
--   Partial_Evolutions_Table   [  |  |  |  |  |  |  ]  indexd by object evolutions in a system state
--                                |
--                                |
--                                V
--   System_Runtime_Evolution  [ Target_System | (Nick in SRC_DB) 
--                               Object_Evolution ] 
--                                      ||
--                                      ||
--   Object_Runtime_Evolution    [ Evolving_Object |  (Natural)
--                                         Trigger |  (Nick of SIGNALS_DB)
--                                     Transitions |  (Nick of TRANSITIONS_DB)
--                                         Signals |  (Nick in SIGNALS_DB)
--                                          Target |  (Nick od ORC_DB)
--                               TransitionsSetKey |  (Natural) 
--                                    AbstractInfo ]
--                                           ||
--                                           ||
--   AbstractData [ Labels] (String_Tables_Vector_Ref) 
--
-------------------------------------------------------------------------------------
--
--  ORC_DB = [ Element   | Element  | ... ]
--               ||  Elements are Object_Runitime_Configurations
--               ||
--  Object_Runtime_Configuration [                   SSize | (Natural)
--                                                   VSize | (Natural)
--                                                    Self | (Natural) 
--                                Current_States(1..SSize) | (States_Table)
--                                  Current_Vars(1..Vsize) | (Int_Table)
--                                          Current_Queue  | (Nick in Signals_DB)
--                                      Sustended_Sequence | (Nick in Transitions_DB)
--                              Suspended_Transition_Index |  (Natural)
--                                       Suspended_Actions | (Nick in Vectors_DB)
--                                         Suspended_Call  | (Nick in Vectors_DB)
--                                          Transition_Env | (Nick in Vectors_DB)
--                                           Trigger_Args  ] (Nick in Vectors_DB)
--
-------------------------------------------------------------------------------------
--
--  OCI_DB = [ Element   | Element  ...]   indexed by NickNum of ORC_DB
--               ||
--   Object_Configuration_Info [ Object_Evolutions]
--                                   |
--                                   |
--                                   V
--  Object_Runtime_Evolutions [ Object_Runtime_Evolution | ... | ...]
--
-------------------------------------------------------------------------------------

  type Abstractdata is record
   Labels: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
  end record;

  type Object_Runtime_Evolution is record
    Evolving_Object: Natural;  --  index in Active_Charts'Range
    Trigger: Int64;  -- nick in SIGNALS_DB  -- 0=completion -1=deadlock
    Transitions: Int64;  -- nick in TRANSITIONS_DB    =  sequence
    Signals: Int64;  -- nick in SIGNALS_DB: all events, propri+altrui+change
    Target: Int64;   -- nick in ORC_DB
    TransitionsSetKey: Natural :=0;
    AbstractInfo: Abstractdata;
  end record;
  type Object_Runtime_Evolution_Ref is access Object_Runtime_Evolution;

   type System_Runtime_Evolution  is record
     Object_Evolution: Object_Runtime_Evolution;
     Target_System: Int64;     -- nick in SRC_DB
   end record;

--X  type Partial_Evolutions_Table is array (positive range <>) of System_Runtime_Evolution;
  -- was Partial_System_Runtime_Evolutions

--X  type Partial_Evolutions_Table_Ref is access Partial_Evolutions_Table;
--X  -- was Partial_System_Runtime_Evolutions_Ref

  -- -- -- --
--X  type System_Evolutions_Table is array (positive range <>) of Partial_Evolutions_Table_Ref;
--X  type System_Evolutions_Table_Ref is access System_Evolutions_Table;

--  type System_Configuration_Info is record
--    System_Evolutions: System_Evolutions_Table_Ref;
--  end record;
--X  subtype System_Configuration_Info is System_Evolutions_Table;

  -- -- --
  type Object_Runtime_Evolutions is array (Positive range <>) of Object_Runtime_Evolution;
  type Object_Runtime_Evolutions_Ref is access Object_Runtime_Evolutions;
  No_Evolutions: Object_Runtime_Evolutions(1..0);
  No_Evolutions_Ref: constant Object_Runtime_Evolutions_Ref := new Object_Runtime_Evolutions(1..0);
  type Object_Runtime_Evolutions_Table is array (Positive range <>) of Object_Runtime_Evolutions_Ref;
  type Object_Runtime_Evolutions_Table_Ref is access Object_Runtime_Evolutions_Table;

  --  il vettore delle possibili evoluzioni di un oggetto e' la
  --  principale informazione associata ad una ORC
  --
  type Object_Configuration_Info is record
    Object_Evolutions:  Object_Runtime_Evolutions_Ref;  -- null if undefined
  end record;

  -- -- -- --
--X  type System_Runtime_Evolutions is                -- in configurations-system_model  body
--X     array (positive range <>) of Partial_Evolutions_Table_Ref;
--X  type System_Runtime_Evolutions_Ref is access System_Runtime_Evolutions;
  
--X   type Partial_System_Runtime_Evolutions is           -- in UML_Configurations spec
--X      array (positive range <>) of System_Runtime_Evolution; 
      
  type Object_Runtime_Configuration (SSize: Natural;    -- in object_model spec
             VSize: Natural) is record
     Self: Natural :=0;
     Current_States: UML_Types.States_Table (1..SSize);
     Current_Vars: Int_Table (1..VSize);
     Current_Queue:  Int64;    ---  nick in Signals_DB
     Suspended_Sequence: Int64 := 0;    -- nick in Transitions_DB
     Suspended_Transition_Index: Natural :=0;
     Suspended_Actions: Natural := 0; --nick in Vectors_DB (the suspended actions table)
     Suspended_Call: Natural :=0;  -- index in All-Events
     Transition_Env: Int64 := 1;  -- nick in Vectors_DB (the current transition env.)
         --  1=NullVector          -- the call event in which the object is suspended
     TriggerArgs: Int64 := 1; --  1=NullVector;
--     AbstractInfo: Abstractdata;
  end record;
pragma Volatile(Object_Runtime_Configuration);

--------------------------------------------------------------------------------------
------------------------------------- DATA-BASES -------------------------------------
  type BitVector is array(1..32) of Boolean;
  Pragma Pack(Bitvector);
  for BitVector'Size use 32;
  Function Tobits is new Unchecked_Conversion (Integer, BitVector);
  Function ToInt is new Unchecked_Conversion (BitVector,Integer);
  subtype String4 is String(1..4);
  function tostring4 is new Unchecked_Conversion (Integer, String4);
  --------------------------------
  --   Hashit((value1,value2,value3)) becomes:
  --   Ada.Strings.Hash(" value1 value2 value3");
  --------------------------------
  function Hashit (This_Table: Int_Table; prefix: String := "") return Natural is
      use Interfaces;
      thestring: String((This_Table'First-1)*4+1..This_Table'Length * 4);   
      thehash: Ada.Containers.Hash_Type;
      U32: Interfaces.Unsigned_32;
      MASK: Interfaces.Unsigned_32 := 2**26-1;
    begin
      for I in This_Table'Range loop
        thestring(I*4-3 .. I*4) := tostring4(This_Table(I));
      end loop;
      thehash :=  Ada.Strings.Hash(thestring);
      U32 := Interfaces.Unsigned_32(thehash);
      U32 := U32 and MASK;
      return Natural(U32);
--      return Natural(thehash mod Ada.Containers.Hash_Type(Key_Module));
  end Hashit;

------------------------------------------------------------------------

  function Null_Int_Table return Int_Table;
  function Mk_Key(This_Table: Int_Table;
        Cache_Max: Natural := Key_Module) return Positive;

   package Vectors_DB is new NickNames
   (  Int_Table,
      Null_Int_Table,
      Mk_Key,
      "=");

  function MK_Nullvector return Int64 is
  begin
     return Vectors_DB.NickNum(Empty_Int_Table);
  end MK_Nullvector;
  --
  NullVector: Integer := MK_Nullvector;

  function Null_Int64_Table return Int64_Table;

--package Signals_DB renames Vectors_DB;
  package Signals_DB is new NickNames   -- in UML_CONFIGURATIONS spec
     (Int64_Table,
      Null_Int64_Table,
      Mk_Key,
      "=");

  -- --
  function Object_Null_Configuration return Object_Runtime_Configuration;
  function Mk_Key (This_Conf: Object_Runtime_Configuration;
              Cache_Max: Natural := Key_Module) return Positive;
  package ORC_DB is new NickNames       -- in UML_CONFIGURATIONS spec
     (Object_Runtime_Configuration,
      Object_Null_Configuration,
      Mk_Key,
      "=");

  function Null_Info return Object_Configuration_Info;
  package OCI_DB is new Dyn_Store (Object_Configuration_Info, Null_Info);  -- Object_Model spec

  -- 
  package SRC_DB is  new
    NickNames (Int64_Table,                -- in Configurations.Global_Env ??
      Null_Int64_Table,
      Mk_Key,
      "=");

  -- 
  function Null_Int_Table return Int_Table is
  begin
     return Empty_Int_Table;
  end Null_Int_Table;

  function Mk_Key(This_Table: Int_Table;
        Cache_Max: Natural := Key_Module) return Positive is
    Result: Integer := 0 ;
  begin
    Result := HashIt(This_Table);
    return Positive (Result +1);
  end Mk_Key;

  function Null_Int64_Table return Int64_Table is
  begin
     return Empty_Int64_Table;
  end Null_Int64_Table;

  function Object_Null_Configuration return Object_Runtime_Configuration is
    Tmp: Object_Runtime_Configuration(0,0);
  begin
    Tmp.Current_Queue :=0;
    return Tmp;
  end Object_Null_Configuration;

  function Mk_Key (This_Conf: Object_Runtime_Configuration;
              Cache_Max: Natural := Key_Module) return Positive is
    TheTable: Int_Table (1.. 8+This_Conf.Vsize + This_Conf.SSize);
    Result: Integer := 0 ;
  begin
    TheTable(1) := Integer(This_Conf.Self);
    TheTable(2) := Integer(This_Conf.Suspended_Sequence);
    TheTable(3) := Integer(This_Conf.Suspended_Transition_Index);
    TheTable(4) := Integer(This_Conf.Suspended_Actions);
    TheTable(5) := Integer(This_Conf.Suspended_Call);
    TheTable(6) := Integer(This_Conf.Transition_Env);
    TheTable(7) := Integer(This_Conf.TriggerArgs);
    TheTable(8) := Integer(This_Conf.Current_Queue);
    for I in 1.. This_Conf.Vsize loop
      TheTable(8+I) := Integer(This_Conf.Current_Vars(I));
    end loop;
    for I in 1.. This_Conf.Ssize loop
      if UML_Types."/="(This_Conf.Current_States(I),null) then
        TheTable(8+This_Conf.Vsize + I) := Integer(This_Conf.Current_States(I).Num_Key);
      else 
        TheTable(8+This_Conf.Vsize + I) :=0;
      end if;
    end loop;
    --
    Result :=  HashIt(TheTable);
    Result :=  Natural (Result +1);
-- DEBUG
--    Ada.Text_IO.Put_line(Integer'Image(Result));
    return Result;
    --
  end Mk_Key;

   function Null_Info return Object_Configuration_Info is
     Tmp: Object_Configuration_Info;
  begin
     return Tmp;
  end Null_Info;

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

--------------------------------------------------
  Undefined_Var: exception;
  Loading_Error: exception;
  Tab_Error: exception;
  UML_Error: exception;
  --
  UnspecifiedError: Integer := UML_Types.ObjectBase - 4;
  InvalidSpecialToken: Integer := UML_Types.ObjectBase - 5;
  InvalidIndex: Integer := UML_Types.ObjectBase - 6;
  CodingBug: Integer := UML_Types.ObjectBase - 7;
  InvalidAbstraction: Integer := UML_Types.ObjectBase - 8;
  InvalidTarget: Integer := UML_Types.ObjectBase - 9;
  InvalidStructure: Integer := UML_Types.ObjectBase - 10;

  Runtime_Errors_Enabled: Boolean := True;
  Runtime_Errors_Count: Natural :=0;
  pragma Volatile(Runtime_Errors_Count);
  Runtime_Error_Num: Integer := UnspecifiedError;
  pragma Volatile(Runtime_Error_Num);
  Runtime_Error_Val: Integer := 0; 
  pragma Volatile(Runtime_Error_Val);
  Runtime_Error_Msg: String_Ref;
  pragma Volatile(Runtime_Error_Msg);
 

  ----------------------------  UML SETTINGS ------------------------------
  Priorities_Enabled: Boolean := True;
  Priorities_Defined:Boolean := False;
  Max_Evolutions_Depth: Positive := 10;
  ---------------------------- UML STATE STATS --------------------------
  MAXQUEUE: Integer :=0;
  pragma Volatile(MAXQUEUE);
  MAXVECT: Integer :=0;
  pragma Volatile(MAXVECT);
  INTMIN: Integer :=0;
  pragma Volatile(INTMIN);
  INTMAX: Integer :=0;
  pragma Volatile(INTMAX);
  -----------------------------------------------------------------------
  Show_Params: Boolean := True;
  Show_Target:Boolean := True;
  ShowStuttering: Boolean := True;        -- obj:ERR.lostevent(event)
  ObserveReturns: Boolean := False;
  Show_Assignments: Boolean := False;    -- for CUSTOM  specialization
  Explicit_Tau_Requested: Boolean := False;  --  "obj:"  ->  "tau"
  -- even when Explicit_Tau_Requested=False the label obj: satisfies tau!
  ShowEvolvingObject: Boolean := True;      --  default for GRAY_BOX
--  -- used by umc2tab
--  Observed_Vars: Num_Table_Ref := new Num_Table'(Empty_Num_Table);


  -------------  ADDITIONAL NUMERIC AND STRING UTILITIES --------------
 function Append (To: String_Ref; What: String) return String_Ref;

--######################################################################################
package Object_Model is
  --
  -- data la configurazione attuale di un oggetto, restituisce la
  -- lista delle sue possibili evolutions.
  --   (Active_Object serve per riconoscere i segnali locali all'oggetto)
  function Get_Object_Evolutions (
          Current_Conf: Int64;
          Active_Object: Natural) return Object_Runtime_Evolutions_Ref;

  -- La funzione "Get_Initial_Configuration" restituisce la configurazione
  -- iniziale del Active_object risultante dal parsing del file sorgente uml.

  function Get_Initial_Configuration (Active_Object:Natural) return Int64;

       ------------- query functions ---------------
  function Get_Signal (Nick: Int64) return Int_Table;
  function Get_Signals (Nick:Int64) return Int64_Table;
  function Get_Transitions_Sequence(Nick: Int64) return UML_Types.Transitions_Table;

  function Get_Active_States (Current_Conf: Int64) return UML_Types.States_Table;
  function Get_Var_Value(Current_Conf: Int64;
                     The_Variable:Positive) return Integer;
  function Get_Var_Values(Current_Conf: Int64) return Int_Table;
  function QueueSize(Current_Conf: Int64) return Natural;
  function Get_Queue_Items(Current_Queue: Int64) return Int64_Table;

--
--  function Get_Trigger_Signal (Current_Conf: Int64;
--                              Active_Object:Natural ) return Int64;

  function Is_Suspended(Current_Conf: Int64) return Boolean;
  function Get_Suspended_Op(Current_Conf: Int64) return Natural;

 ---------------  initialization utilities -----------------
 procedure Initialize_Object_Model;

end Object_Model;
--######################################################################################




--######################################################################################
package System_Model is

-------------------------------------------------------------------------
-- iniziale risultante dal parsing del file sorgente uml.
-------------------------------------------------------------------------
  function Get_System_Initial_Configuration return Int64;           -- !!!
  procedure Set_System_Initial_Configuration;

 function Compute_System_Evolutions
             (Current_Configuration: Int64) return Object_Runtime_Evolutions_Table_Ref;

-- function Compute_Object_Evolutions
--             (Current_Configuration: Int64;   -- System Configuration
--              Current_Object: Natural) return Object_Runtime_Evolutions_Ref;

  function Compute_Target_Configuration(
             Current_Configuration: Int64;  -- System Configuration
             Active_Object: Natural;        -- index in Active_Charts
             Current_Object_Evolution: Natural) return Int64;  -- System Configuration
-------------------------------------------------------------------------
-- la funzione Get_System_Evolutions restituisce le evoluzioni
--  parziali (cioe' indotte da un particolare oggetto) di una 
--  system configuration.
-- A seconda dei casi tali evoluzioni sono calcolate o prese da una cache.
-------------------------------------------------------------------------
--  function Get_System_Evolutions (Current_Configuration: Int64;     -- !!!
--        Current_Object: Natural) return Partial_Evolutions_Table_Ref;

  function  Configurations_Count return Int64;

    ------------- query functions ---------------
 function SRC_Count return Int64;
 function Get_Object_Conf(NickNum: Int64; Obj:Natural) return Int64;
 function Exists (NickNum: Int64) return Boolean;
 function MaxQueueSize (NickNum: Int64) return Natural;
 procedure Initialize_System_Model;

 -- NOTA :
 -- data una System_Configuration   le sue possibili evoluzioni NON
 --  sono calcolate tutte assieme appena la configurazione viene incontrata
 --  (iterator.initialize) ma esse sono calcolate, partizionate per oggetto, 
 --  via via che sono richieste  (iterator.advance)
 -- Cio' causa una dispersione delle informazioni sulle possibili evoluzioni
 --  di una configurazione, (penalizzante nel caso di attraverso completo del modello,
 --  e penalizzante nel caso di attraversamento BREADTH FIRST)
 --  che e' vantaggiosa solo nel caso di attraversamenti DEPTH first PARZIALI.
 --  (FORSE E' IL CASO DI RIVEDERE QUESTA SCELTA)
end System_Model;
--######################################################################################


--######################################################################################
package UML_Parser is
  --
  -- Parses the uml file indentified by the given name and creates 
  -- the corresponding uml object in "This_UML_Object"
  --
  procedure Parse (Uml_File_Name: String);
  --
  Max_Line_Length: Positive := 10000;
  Max_Empty_lines: Natural := 500;
  Max_Comment_lines: Natural := 500;
  Max_Components: Positive := 500;
  Max_Synchronizations: Positive := 10000;
  --
end UML_Parser;
--######################################################################################



--######################################################################################
package UML_Configurations is


  ---------------- Iteration Utilities --------------
--  procedure Iterator_Initialize ( It: in out Evolutions_Iterator;
--                                  This_Conf: System_Configuration;
--                                  Active_Object: Natural :=0);
--  procedure Iterator_Restart (It: in out Evolutions_Iterator);
--  procedure Iterator_Finalize (It: in out Evolutions_Iterator);
--  function  Has_System_Transition (It: Evolutions_Iterator) return Boolean;
--  procedure Iterator_Advance (It: in out Evolutions_Iterator);
--  function Get_Target_Configuration (It: Evolutions_Iterator) return System_Configuration;
--  function Get_Source_Configuration (It: Evolutions_Iterator) return System_Configuration;
--
--  procedure Free is new Ada.Unchecked_Deallocation(Evolutions_Table,Evolutions_Table_Ref);
--
--  --------------  Ititialization Utilities --------------------
  procedure Initialize_Configurations;
  function Initial_Configuration return System_Configuration;
--

--  TO BE MOVED INTO UML_TYPES ????
  ------------------ Info Utilities ------------------------------

  function Has_Fireable_Transitions(This_Conf: System_Configuration;
                         Active_Object:Positive) return Boolean;
--  function Get_AbstractStateLabels(This_Conf: System_Configuration) return String_Tables_Vector;

  ----------------  Display Utilitites --------------------------
--  function Display_AbstractStateLabels(This_Conf: System_Configuration) return String;
--  function Display_AbstractLabels(It: Evolutions_Iterator) return String;
  function  Configurations_Count return Int64;
  function Get_Sequence (It: Evolutions_Iterator) return UML_Types.Transitions_Table;
  function Get_Active_States(This_Conf:System_Configuration;
                             Active_Object:Positive) return UML_Types.States_Table;
  function Display_States (This_Conf: System_Configuration;
                         Active_Object:Positive) return String;
  function Display_Vars(This_Conf: System_Configuration;
                         Active_Object:Positive) return String;
  function Display_Queue (This_Conf: System_Configuration;
                         Active_Object:Positive) return String;
  function Display_Fireable_Sequences (This_Conf: System_Configuration;
                         Active_Object:Positive) return String;
--  function Display_Trigger (This_Conf: System_Configuration;
--                         Active_Object:Positive :=1)  return String;
  function Display_Sequence (It: Evolutions_Iterator) return String;
  function Display_Actions (It: Evolutions_Iterator) return String;
--  function Display_Signals (Evolution: Evolution_Data;
--                          Ground_Zero:Boolean := False) return String;
  function Display_Chart_Name(It: Evolutions_Iterator) return String;
  function Get_Chart_Name(It: Evolutions_Iterator) return String_Ref;
  function Display_Trigger(It: Evolutions_Iterator ) return String;
  --
  procedure HTML_Print_Fireables (The_File:FIle_Type;
                             This_Conf:System_Configuration;
                             Active_Object: Natural);
  procedure Print_Fireables (The_File:FIle_Type;
                             This_Conf:System_Configuration;
                             Active_Object: Natural);
--  function Display_Variable (This_Conf: System_Configuration;
----      The_Variable: Positive) return String;
  function Display_Variable (This_Conf: System_Configuration;
      TheObj: String; TheVar:String)  return String;
  function Eval_Attribute (This_Conf: System_Configuration;
                           Ids: String_Table) return Integer;
  function HTML_Transition_Format (Source: String) return String;
  function Is_Suspended(This_Conf: System_Configuration;
                         Active_Object:Positive) return Boolean;
end UML_Configurations;



  function HTML_Signals_Format (Source:String) return String is
   Result:String(1..10000);
   OUTC: Natural := 1;
   htmlspace: String := "&nbsp;";
   indent: natural :=5;
  begin
   for I in Source'Range loop
     if Source(I)='<' then
       Result(OUTC..OUTC+3) := "&lt;";
       OUTC := OUTC+4;
       --
     elsif Source(I)='>' then
       Result(OUTC..OUTC+3) := "&gt;";
       OUTC := OUTC+4;
       --
     elsif Source(I)=';' and then I /= Source'Last then
       Result(OUTC) := ';';
       OUTC := OUTC+1;
       Result(OUTC..OUTC+3) := "<br>";
       OUTC := OUTC+4;
       for K in 1..indent loop
         Result(OUTC..OUTC+4) := "&nbsp";
         OUTC := OUTC+5;
       end loop;
     else
       Result(OUTC) := Source(I);
       OUTC := OUTC+1;
     end if;
    end loop;
    return Result(1..OUTC-1);
  end HTML_Signals_Format;

package UML_Explore is
  use UML_Configurations;

  procedure Save_As_HTML (HTML_File: File_Type; Current_Conf: System_Configuration);
  procedure Print_Info (Current_Conf: System_Configuration);
  procedure Print_Chart_Info (Current_Conf: System_Configuration;
                        Active_Object:Positive :=1);
  procedure Print_Stats;
  procedure Dump_HTML_Model(HTML_File: File_Type);
  procedure Dump_Class_Statechart(The_Class: Natural);
  function HTML_Class_Tooltip (HTML_File:File_Type; The_Class: Positive) return String;
end UML_Explore;

--######################################################################################



--######################################################################################
--####################      INTERFACE   IMPLEMENTATIONS  ###############################
--######################################################################################
-- uses
--
--  System_Model.Get_System_Initial_Configuration)
--  System_Model.Get_Object_Conf
--  UML_Types.Signal_Image
--  UML_Types.Trigger_Image
--  Object_Model.Get_Signals
--  Object_Model.Get_Signal
--  Object_Model.Get_Trigger_Signal
--  Object_Model.QueueSize

--  UML_Types.All_Observations
--  UML_Types.Active_Charts
--  UML_Types.All_Charts

   type XXX_Iterator is record
     Current_Configuration: Int64;  -- nick in SRC_DB
     Chart_Locked : Natural  := 0;
     Active_Object: Natural := 0;
--     Object_Evolutions: Object_Runtime_Evolutions_Ref; -- retrieved from in OCI_DB
     Current_Object_Evolution: Natural :=0;   -- index in list of partial evol.
     Objects_Evolutions: Object_Runtime_Evolutions_Table_Ref; 
   end record;

   ----------------------
   function "=" (left: System_Configuration; right: System_Configuration) return Boolean is
   begin
     return Integer(left) = Integer(right);
   end;

   ----------------------
   function Undefined_Configuration return System_Configuration is
   begin
      return System_Configuration(0);
   end Undefined_Configuration;


   ---------------------
   function Initial_Configuration return System_Configuration is
     N: System_Configuration;
   begin
     N := System_Configuration(System_Model.Get_System_Initial_Configuration);
     return N;
   end Initial_Configuration;

   procedure  Eval_State_Expr (This_Conf: System_Configuration;
                           Ids: String_Table; 
                           DollarValue: Integer; 
                           EV: out Integer; 
                           Failed: out Boolean);
   ---------------------
   function Get_Abstract_State_Labels(Current_Conf: System_Configuration) 
        return String_Tables_Vector is
     use UML_Types;
     --
     LeftValue: Integer;
     LV,RV: Integer;
     Ids: String_Table_Ref;
     Thelabels: String_Table_Ref := Empty_String_Table_Ref;
     Result: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
     Prev: String_Tables_Vector_Ref;
     -- Observation rule =  (Kind => StateKind, LTerm=null, LOp=obj, LArgs=activestate)
     EV: Boolean;
     Tmp: Boolean;
     DollarNames: array(1..10) of String_Ref; -- extracted from sintactic model (no dynamic creation)
     DollarValueImages: array(1..10) of String_Ref;-- dynamically created and passed to the caller ...
     DollarValues: array(1..10) of Integer;        -- dynamically created 
     DollarCount: Natural :=0;
     Failed: Boolean;
   begin
    if UML_Types.All_Observations.all'Length=0 then 
        return Result.all; 
    end if;
    for I in UML_Types.All_Observations.all'Range loop
      --
      --  for all observation rules ...
      --  Le State Observations sono sequenze di condizioni del tipo:
      --       
      --   inState(obj.top.S1.c1)
      --   obj.attr[index, index] = expr
      --   obj.attr[index, index] = $i       -- dove $i viene qui definita
      --   obj.attr[index, index] = $j       -- dove $j e' stata definita in precedenza
      --   $i > $j         -- dove $i, $j sono state definite in precedenza
      --   obj1.attr1 /=  obj2.attr2[index]
      --   obj1.attr <= expr
      --    obj.attr[$1] = []
      --    obj.attr = [obj1,*] ???
      --    obj.attr.length = 0 
      --    obj.attr.head =   obj1
      --    obj.attr.tail.head = obj2
      --    $1.tail.[$2]  = expr  ???
      --   nota che sintatticamente e' possibile scrivere:
      --     obj1 < Obj2,   Obj1 > [1,2,3]  ,  [] < [1,2,3]  anche se cio' ha poco senso!!
      --   e cio' e' difficile da controllare staticamente perche' posso avere:
      --    obj1.attr1 = $1  and   obj2.attr2=$2   and  $1 < $2
      --   
      --  avrebbe anche senso poter scrivere $1 + $2 = $3,   $1.head = $2, $1.length=0
      --   ma cio richiederebb una valutazione piu' complessa ... 
      --
      --  Le condizioni vengono valutate una alla volta definendo man mano i $i incontrati
      --  Come primo passo della valutazione di una condizione vengono espansi i $i gia definiti
      --  Tali $i sono stringhe memorizzate in DollarValues (e questo puo' essere un problema!!!!)
      --  Quindi se il valore di un $i e' un valore composito "[1,2,3]" esso e' una singola stringa
      --
      Tmp := True;
      DOLLARCOUNT := 0;
      DollarValues(1) := 0; -- just to avoid compiler warnings
      -- Tmp is set to False as soon as a rule is seen as not matching
      --
        -- we consider here only "State" observations ...
        --
      if UML_Types.All_Observations(I).Kind = UML_Types.StateKind then
        -- 
        for L in UML_Types.All_Observations(I).Left.all'Range loop
          -- 
          -- for each left-side condition ...
          -- which has the structure:  
          --     [LOp, LArgs,    -- used only for inState rules
          --      IdsL,LeftOP,IdsR ]  -- used for all kind of relations/equations
          --
          --
         if UML_Types.All_Observations(I).Left(L).IdsL /= null and then
             UML_Types.All_Observations(I).Left(L).IdsL(1).all = "final" then 
            -- 
            declare
              It: Evolutions_Iterator;
            begin
               Iterator_Initialize(It,Current_Conf);
               if Has_System_Transition(It) then
                 Tmp := False;
               end if;
               Iterator_Finalize(It);
            end;
         elsif UML_Types.All_Observations(I).Left(L).LOp /= null then
            --
            --  CASO 1)  inState condition
            --
            --  inState(obj.top.S1.c1)   -- NO VARS IN THESE RULE ELEMENTS
            --
            Ids := new String_Table'( Is_Active &
                                      UML_Types.All_Observations(I).Left(L).LOp &
                                      UML_Types.All_Observations(I).Left(L).LArgs.all);
            Failed := False; 
            Eval_State_Expr(Current_Conf, Ids.all, DollarValues(1), LeftValue, Failed);
            Free(Ids);   -- no more needed
            if Failed or  LeftValue /= UML_Types.IntTrue then
              -- questa regola NON e' applicabile
              Tmp := False;
              exit;
            end if;
            --
          else
            --
            -- LOp = null -->  THIS IS A RELATION/EQUATION OR A VAR DEFINITION
            -- replace already known dollarvalues
            --
            -- Qui LVids e RVids sono macro espansioni sintattiche degli IdsL e IdsR
            -- dove le $1 sono espanse con le IMMAGINI dei corrispondenti valori.
            -- cio ovviamente crea complicazioni nel caso di valore composti perche'
            --  non saro' piu' in grado di applicarci indexing, tail, head > < 
            --  i.e  nel caso in cui  $1 > $2  o  $1.head= 12, $1=[1,*,3]  ... 
            --  sono tuttavia ancora in grado di gestire i casi piu' semplici
            --  $1 = [] ,  $1 != [1,2]   infatti questi sono gli unici casi permessi
            --
            declare 
              LVids: String_Table := UML_Types.All_Observations(I).Left(L).IdsL.all;
              RVids: String_Table := UML_Types.All_Observations(I).Left(L).IdsR.all;
            begin
              --
              -- MACROESPANDO GLI IDS DI QUESTA Left CONDITION  
              -- TALE MACROESPANSIONE PUO' CONTENERE VALORI COMPOSTI "indigeribili" per un
              -- confronto numerico
              --  
              -- Se rimangono $vars e' perche' non sono state definite.  L'unico contesto
              -- in cui e' ammessa una $var e' nella RVids come unico elemento.
              -- Se non e' cosi si salta la valutazione della regola.
              --
              for LVI in LVids'Range loop
                 if LVids(LVI)(1) = '$' then
                    for DV in 1 .. DOLLARCOUNT loop
                       if LVids(LVI).all = DollarNames(DV).all then
                          LVids(LVI) := DollarValueImages(DV);
                       end if;
                    end loop;
                    if LVids(LVI)(1) = '$' then
                      -- i $i non possono comparire piu'
                      Tmp := False;
                      exit;
                    end if;
                    -- composite values possono comparire solo nella prima posizione
                    if LVI > 1 and then LVids(LVI)(1) = '[' then
                      Tmp := False;
                      exit;
                    end if;
                 end if;
              end loop;
              for RVI in RVids'Range loop
                if RVids(RVI)(1) = '$' then
                   for DV in 1 .. DOLLARCOUNT loop
                       if RVids(RVI).all = DollarNames(DV).all then
                          RVids(RVI) := DollarValueImages(DV);
                       end if;
                   end loop;
                   if RVI > 1 and then RVids(RVI)(1) = '$' then
                      -- i $i non possono comparire ad un indice > 1
                      Tmp := False;
                      exit;
                   end if;
                    -- composite values possono comparire solo nella prima posizione
                    if RVI > 1 and then RVids(RVI)(1) = '[' then
                      Tmp := False;
                      exit;
                    end if;
                end if;
              end loop;
              --
              -- A questo punto le macroespansioni delle variabili gia' definite' e' stata fatta.
              --
              -- Si fanno un po di controlli, (alcuni omittibili perche fatti anche dal parser)
              -- in un left side non ci possono essere, neppure nella prima posizione, composite literals
              -- NON derivati da istanza di variabile,  (OMETTIBILE)
              if LVids'Last >= 1 and then  LVids(1)(1) = '[' and then 
                   UML_Types.All_Observations(I).Left(L).IdsL(1)(1) /= '$'  then
                Tmp := False;
                exit; 
              end if;
              -- in un right side se un composite value compare nella prima posizione e NON e' una
              -- istanza di variabile, non devono esistere altre componenti (OMETTIBILE)
              if RVids'Last >1 and then  RVids(1)(1) = '[' and then 
                   UML_Types.All_Observations(I).Left(L).IdsR(1)(1) /= '$' then
                Tmp := False;
                exit;
              end if;
              if not Tmp then 
                exit;  -- interrompe la valutazione delle regola, ignorando le successive condizioni
              end if; 
              --
              -- Se RVids(1) e' una variabile  l'operatore DEVE essere una uguaglianza. (OMETTIBILE)
              --
              if RVids(1)(1) = '$' and then 
                 UML_Types.All_Observations(I).Left(L).LeftOp /= UML_Types.EQ then
                 Tmp := False;
                 exit;   -- ignora questa regola e le sue successive condizioni
              end if;
              -- -----------------------------------------------------------
              -- A questo punto devo considereare tre possibili situazioni:
              -- 1) La relazione e' una var definition
              -- 2) La relazione ha nella parte destra una literal composito esplicito "[1,2,3]"
              -- 3) La relazione NON ha nella parte destra una literal composito esplicito "[1,2,3]"
              -- 
              --  1) La relazione e' una var definition
              --
              if UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.EQ and then
                    RVids'Length = 1 and then RVids(1)(1) = '$' then
                --
                -- Nel caso di variable definition  posso valutare la parte sinistra con Eval_State_Expr
                --
                --  obj.attr = $i
                --  obj.attr[ind,indx2] = $i
                --  maxqueusize = $1
                --  obj.queuesize = $1)
                --
                --
                DOLLARCOUNT := DOLLARCOUNT+1;
                DOLLARNAMES(DOLLARCOUNT) := RVids(1);
                declare
                  TheDynValueImage: String_Ref;  -- DOLLARVALUEIMAGE
                begin
                  --
                  if LVids'Length >= 2 and then
                     LVids(2).all /= "queuesize" then
                    -- obj.localvar
                    Failed := False;
                    Eval_State_Expr(Current_Conf, LVids, DollarValues(1), LV, Failed);
                    if Failed then Tmp:= False; exit; end if;
                    TheDynValueImage := new String'(Value_Image(LV, Undefined));
                      DOLLARVALUEIMAGES(DOLLARCOUNT) := TheDynValueImage;
                    DOLLARVALUES(DOLLARCOUNT) := LV;
                  else
                    -- maxqueuesize,
                    -- obj.queuesize
                    Failed := False;
                    Eval_State_Expr(Current_Conf, LVids, DollarValues(1), LV, Failed);
                    if Failed then Tmp:= False; exit; end if;
                    declare
                      STR: String := Integer'Image(LV);
                    begin
                      if STR(1) = ' ' then
                        TheDynValueImage := new String(1..STR'Length-1);
                        TheDynValueImage.all := STR(STR'First+1..STR'Last);
                      else
                        TheDynValueImage := new String'(STR);
                      end if;
                      DOLLARVALUEIMAGES(DOLLARCOUNT) := TheDynValueImage;
                      DOLLARVALUES(DOLLARCOUNT) := LV;
                      end;
                  end if;
                end;
              -- 
              -- 2) La relazione ha nella parte destra una literal composito esplicito "[1,2,3]" 
              --
              elsif RVids'Length = 1 and then RVids(1)(1) = '[' and then
                      UML_Types.All_Observations(I).Left(L).IdsR(1)(1) /= '$' then
                --
                -- NON E' UN VARIABLE DEFINITION MA IL RIGHT SIDE E' UN SINGOLO COMPOSITE LITERAL !!
                --  NON DERIVATO DA UNA $1
                -- valuto il left, side, lo trasformo in stringa, e confronto per uguaglianza le immagini
                -- e le l'operatore relazionale e' < o > abbandono la regola.
                --
                if  UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.GT or else
                    UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.GE or else
                    UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.LT or else
                    UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.LE then
                  Tmp := False;
                  exit;
                end if;
                Failed := False;
                    Eval_State_Expr(Current_Conf, LVids, DollarValues(1), LV, Failed);
                if Failed then Tmp:= False; exit; end if;
                declare
                  TheDynValueImage: String := Value_Image(LV,Undefined);
                begin
                  if UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.EQ and then
                      TheDynValueImage /= RVids(1).all then
                    Tmp := False;
                    exit;
                  end if;
                  if UML_Types.All_Observations(I).Left(L).LeftOp = UML_Types.NE and then
                      TheDynValueImage = RVids(1).all then
                    Tmp := False;
                    exit;
                  end if;
                end;
                --
                --  3) tutti gli altri casi, elborati chiamando Eval_State_Expr sui due termini della relazione
                -- 
              else
                --
                EV := True;
                --
                Failed := False;
                Eval_State_Expr(Current_Conf, LVids, DollarValues(1),LV, Failed);
                if Failed then Tmp:= False; exit; end if;
                Eval_State_Expr(Current_Conf, RVids, DollarValues(1),RV, Failed);
                if Failed then Tmp:= False; exit; end if;
                --
                case UML_Types.All_Observations(I).Left(L).LeftOp is
                  when UML_Types.GT => 
                      if LV > StructBase and then RV > StructBase then
                        EV := LV > RV;  -- solo se integers!!!
                      else
                        Tmp := False;
                        exit;
                      end if; 
                  when UML_Types.GE => 
                      if LV > StructBase and then RV > StructBase then
                        EV := LV >= RV;  -- solo se integers!!!
                      else
                        Tmp := False;
                        exit;
                      end if; 
                  when UML_Types.LT => 
                      if LV > StructBase and then RV > StructBase then
                        EV := LV < RV;   -- solo se integers!!!
                      else
                        Tmp := False;
                        exit;
                      end if; 
                  when UML_Types.LE => 
                      if LV > StructBase and then RV > StructBase then
                        EV := LV <= RV;  -- solo se integers!!!
                      else
                        Tmp := False;
                        exit;
                      end if; 
                  when UML_Types.EQ =>  
                        EV := LV = RV;
                  when UML_Types.NE =>  
                        EV := LV /= RV;
                  when UML_Types.NOOP => 
                       EV := False;
                end case;
                if not EV then
                   Tmp := False;
                   exit;  -- ignora questa regola e le sue successive condizioni
                end if;
              end if;
              --
            end;  -- declare LVids RVids
          end if;  -- inState or relation  
          --
        end loop;  -- CYCLE OBSERVING LEFT_ELEMENTRS AND COLLEECTING VARS
        --
        if Tmp then   -- This is MATCHING RULE
          Thelabels := new String_Table'(UML_Types.All_Observations(I).Rlabels.all);
          --
          for V in 1.. DOLLARCOUNT loop   -- for each var defined
            for K in Thelabels'Range loop
              if Thelabels(K).all = DOLLARNAMES(V).all then  -- do the substitution
                Thelabels(K) := DOLLARVALUEIMAGES(V);
              end if;
            end loop;
          end loop;  -- for each var
          --  FREE MEMORY ...  if not empty
          Prev := Result;
          Result := new String_Tables_Vector'(Result.all & Thelabels);
          if Prev /= Empty_String_Tables_Vector_Ref then Free(Prev); end if;
        else
          for V in  1.. DOLLARCOUNT loop
            Free(DOLLARVALUEIMAGES(DOLLARCOUNT));
          end loop;
        end if;   -- This is MATCHING RULE
        --
      end if;  -- ruleKind=action
     end loop;   -- CYCLE FOR ALL RULES
     --
     --  FREE MEMORY ..... if not empty case
     declare
       ToBeReturned: String_Tables_Vector := SortUnique(Result).all;
--       ToBeReturned: String_Tables_Vector := Result.all;
     begin
--        if Result /= Empty_String_Tables_Vector_Ref then Free(Result); end if;
        return ToBeReturned;
     end;
   end Get_Abstract_State_Labels;

   -------------------------------------
   ---   NOT ACTUALLY NEEDED !!!!!   Ground labels never observed in UMC
   -------------------------------------
   function Get_Ground_State_Label_Images(Current_Conf: System_Configuration) return String_Table is
     Result: String_Table(1..0);
   begin
     return Result;
   end Get_Ground_State_Label_Images;

   ------------------------
   function Progressive (This_Conf: System_Configuration) return Integer is
   begin
       return Integer(This_Conf);
   end Progressive;

  -------------------------
  procedure Iterator_Initialize ( It: in out Evolutions_Iterator;
                                  This_Conf: System_Configuration;
                                  Active_Object: Natural :=0) is
    This_SRC: Int64 := Int64(This_Conf);
    Chart_First: Positive :=1;
    Chart_Last:Positive := UML_Types.Active_Charts.all'Length;
    OEV: Object_Runtime_Evolutions_Ref;
  begin
     if It = null then
        It := new XXX_Iterator;
     end if;
    --
    It.Current_Configuration := This_SRC;
    It.Chart_Locked :=  Active_Object;
    if It.Chart_Locked /= 0 then
      Chart_First := It.Chart_Locked;
      Chart_Last := It.Chart_Locked;
    end if;
    It.Active_Object := 0;
    It.Current_Object_Evolution := 0;
    --
    --  initialize the list of evolutions
    --
    It.Objects_Evolutions := System_Model.Compute_System_Evolutions(This_SRC); 
    for I in Chart_First..Chart_Last loop
      It.Active_Object := I;
      OEV := It.Objects_Evolutions.all(I);
      if OEV.all'Length >0 then
        It.Current_Object_Evolution := 1;
        exit;
      elsif I = Chart_Last then
         It.Active_Object := 0;
         It.Current_Object_Evolution := 0;
      end if;
    end loop;
  end Iterator_Initialize;

  ------------------------------------------------------------
  -- Similar to Iterator_Initialize,
  -- only preserves the existing All_Fireables and the Current_Configuration
  -- Appena inizializzato l'iteratore si posiziona sulla prima
  --  evoluzione possibile (se c'e');
  ----------------------------------------------------------
  procedure Iterator_Restart (It: in out Evolutions_Iterator) is
    Chart_First: Positive :=1;
    Chart_Last:Positive := UML_Types.Active_Charts.all'Length;
    This_SRC: Int64;
  begin
    --
    This_SRC:= It.Current_Configuration;
    if It.Chart_Locked /= 0 then
      Chart_First := It.Chart_Locked;
      Chart_Last := It.Chart_Locked;
    end if;
    --
    --  find first evolving object
    --
    for I in Chart_First..Chart_Last loop
      It.Active_Object := I;
      if It.Objects_Evolutions(I).all'Length >0 then
        It.Current_Object_Evolution := 1;
        exit;
      elsif I = Chart_Last then
         It.Active_Object := 0;
         It.Current_Object_Evolution := 0;
      end if;
    end loop;
  end Iterator_Restart;


  ------------------------
  function Are_The_Same(First: String_Table; Second: String_Table) return Boolean is
  begin
    if First'Length /= Second'Length then return False; end if;
    for I in First'Range loop
      if First(I).all /= Second(I).all then return False; end if;
    end loop;
    return True;
  end Are_The_Same;
  

  ------------------------
  function Are_The_Same(First: String_Tables_Vector; 
                        Second: String_Tables_Vector)return Boolean is
  begin
    if First'Length /= Second'Length then return False; end if;
    for I in First'Range loop
      if not Are_The_Same (First(I).all, Second(I).all) then return False; end if;
    end loop;
    return True;
  end Are_The_Same;

  ------------------------
  function Is_Redundant(Evolution_Index: Natural;
                         It: Evolutions_Iterator) return boolean is
  begin
    for I in 1..Evolution_Index-1 loop
      if It.Objects_Evolutions(It.Active_Object)(I).Target =
            It.Objects_Evolutions(It.Active_Object)(Evolution_Index).Target 
         and then
           Are_The_Same(
            It.Objects_Evolutions(It.Active_Object)(I).AbstractInfo.Labels.all,
            It.Objects_Evolutions(It.Active_Object)(Evolution_Index).AbstractInfo.Labels.all)
      then
          return True;
      end if;
    end loop;      
    return False;     
--  MAYBE JUST TARGET AND SIGNALS ARE SUFFICIENT!!!! cannot remember why I added TransitionsSetKey!
--        It.Objects_Evolutions(It.Active_Object)(I).TransitionsSetKey =
--           It.Objects_Evolutions(It.Active_Object)(Evolution_Index).TransitionsSetKey and then

-- now we compare ther labels, not  the signals!!  (12/12/2016)
--        It.Objects_Evolutions(It.Active_Object)(I).Signals =
--           It.Objects_Evolutions(It.Active_Object)(Evolution_Index).Signals then


  end Is_Redundant;

  ----------------------------------------------------------------------
  -- Appena inizializzato l'iteratore si posiziona sulla prima
  --  evoluzione possibile (se c'e');
  ----------------------------------------------------------------------
  procedure Iterator_Advance (It: in out Evolutions_Iterator) is
    Chart_First: Positive :=1;
    Chart_Last:Positive := UML_Types.Active_Charts.all'Length;
    This_SRC: Int64;
  begin
    --
    This_SRC:= It.Current_Configuration;
    if It.Chart_Locked /= 0 then
      Chart_First := It.Chart_Locked;
      Chart_Last := It.Chart_Locked;
    end if;
      while It.Current_Object_Evolution < It.Objects_Evolutions(It.Active_Object).all'Length loop
        It.Current_Object_Evolution := It.Current_Object_Evolution+1;
          if not Is_Redundant(It.Current_Object_Evolution,It) then
         return;
        end if;
      end loop;
    --
      Chart_First := It.Active_Object+1;
      for I in Chart_First..Chart_Last loop
        It.Active_Object := I;
        if It.Objects_Evolutions(I).all'Length >0 then
          It.Current_Object_Evolution := 1;
          exit;
        elsif I = Chart_Last then
          It.Active_Object :=0;
          It.Current_Object_Evolution := 0;
        end if;
      end loop;
      if Chart_First > Chart_Last then
          It.Active_Object :=0;
          It.Current_Object_Evolution := 0;
      end if;
    --
  end Iterator_Advance;

  --------------------------- 
  function  Has_System_Transition (It: Evolutions_Iterator) return Boolean is
  begin
     return It.Current_Object_Evolution  /= 0;
  end  Has_System_Transition;

  ---------------------------
  procedure Free is new Ada.Unchecked_Deallocation(XXX_Iterator,Evolutions_Iterator);
  procedure Free is new Ada.Unchecked_Deallocation(Object_Runtime_Evolutions_Table,
                                                     Object_Runtime_Evolutions_Table_Ref);
  ---------------------------
  procedure Iterator_Finalize (It: in out Evolutions_Iterator) is
  begin
    if It /= null then
       if It.Objects_Evolutions /= null then
           Free(It.Objects_Evolutions);
       end if;
       Free(It);
    end if;
  end Iterator_Finalize;

  --------------------------
  function Get_Source_Configuration (It: Evolutions_Iterator) return System_Configuration is
  begin
    return System_Configuration(It.Current_Configuration);
  end Get_Source_Configuration;

  --------------------------
  function Get_Target_Configuration (It: Evolutions_Iterator) return System_Configuration is
  begin
    if It.Current_Object_Evolution=0 then
      raise UML_Error;
    end if;
    return System_Configuration(
            System_Model.Compute_Target_Configuration(
                 It.Current_Configuration, 
                 It.Active_Object,
                 It.Current_Object_Evolution));
  end Get_Target_Configuration;

   ---------------------------
   function Get_Abstract_Action_Labels (It: Evolutions_Iterator) return String_Tables_Vector_Ref is
     Result: String_Tables_Vector_Ref;
   begin
    if It.Current_Object_Evolution=0 then
      raise UML_Error;
    end if;
     Result :=
       It.Objects_Evolutions(It.Active_Object)(It.Current_Object_Evolution).AbstractInfo.Labels;
     return Result;
   end Get_Abstract_Action_Labels;
   ---------------------------
   function Get_Abstract_Action_Labels (It: Evolutions_Iterator) return String_Tables_Vector is
   begin
     return Get_Abstract_Action_Labels(It).all;
   end Get_Abstract_Action_Labels;

   --------------------------------
   function Display_Signals (It: Evolutions_Iterator) return String;
   function Display_Chart_Name (It: Evolutions_Iterator) return String;
   function Display_Trigger (It: Evolutions_Iterator) return String;
   --------------------------------
   function Get_Ground_Action_Labels (It: Evolutions_Iterator) return String_Table is
     Full: String_Table(1..2);
   begin
     --
     --    accept(trigger,arg1.arg2); signal1(arg1.arg2); assign(var,value)
     --    accept("-"); signal(arg1)
     --    lostevent(signal,arg1,arg2)
     --
     if It.Current_Object_Evolution=0 then
       raise UML_Error;
     end if;
     --
     Full(1) :=  new String'(
         Display_Chart_Name(It)  & ":: " & Display_Trigger(It) & " /");
     Full(2) := new String'(Display_Signals(It));
     return Full;
   end Get_Ground_Action_Labels;
   ---------------------------------
   function Get_Ground_Action_Labels (It: Evolutions_Iterator) return String_Table_Ref is
   begin
     return new String_Table'(Get_Ground_Action_Labels(It));
   end Get_Ground_Action_Labels;

   ----------------------------------
   --  MAXQUEUE= max objectr queue
   --  MAXVECT = max size returned by + of vectors
   --  INTRANGE= max and min values returned by artimethic ops
   ----------------------------------
   procedure Print_StatesSpace_Stats is
      All_States: Int64 := StatesSpace_Size;
   begin
      Put_line ("Stats:" &
              "  STATES =" & Int64'Image(All_States) & 
              "  MaxQUEUE =" &  Integer'Image(MAXQUEUE) &
              "  IntRANGE =" & Integer'Image(INTMIN) & " .." & Integer'Image(INTMAX) &
              "  MaxVectLEN =" & Integer'Image(MAXVECT) & 
              "  VectorsCOUNT =" & Integer'Image(VECTORS_DB.Current_Count));
      if Flags.Debug then
        Put_Line ("DBStats:  Vec=" & 
                  Integer'Image(Vectors_DB.Current_Count) &
                  "(" & Integer'Image(Vectors_DB.Max_Key_Conflicts) & 
                  " ), Sig=" & Integer'Image(Signals_DB.Current_Count) &
                  "(" & Integer'Image(Signals_DB.Max_Key_Conflicts) &
                  " ), Orc=" & Integer'Image(ORC_DB.Current_Count) &
                  "(" & Integer'Image(ORC_DB.Max_Key_Conflicts) &
                  " ), Src=" & Integer'Image(SRC_DB.Current_Count) &
                  "(" & Integer'Image(SRC_DB.Max_Key_Conflicts) & " )"); 
      end if;
   end print_StatesSpace_Stats;
 
   ----------------------------------
   function StatesSpace_Size return Int64 is
   begin
    return Int64(System_Model.Configurations_Count);
   end StatesSpace_Size;

   -----------------------------------
   function FindFromKey(Element_Key: String_Table_Ref) return System_Configuration is
   begin
     return System_Configuration(Integer'Value(Element_Key(1).all));
   end;

   -----------------------------------
   -- MEMORY:  we could save this info in the configuration so as to be recycle it
   --
   function GetUniqueKey(This_Conf: System_Configuration) return String_Table_Ref is
     SS: String_Ref := new String'(Integer'Image(Integer(This_Conf)));
   begin
     return new String_table'(1..1=> SS);
   end;

   ----------------------------------
   function GetProgressive(Num: Integer) return System_Configuration is
   begin
      if Num > 0 and then Num <= StatesSpace_Size then
       return System_Configuration(Num);
      else
        return 1;
      end if;
   end GetProgressive;
  
   ----------------------------------
   procedure Load_Model (File_Name: String) is
      HTML_File: File_Type;
   begin
     UML_Parser.Parse(File_Name);
     UML_Configurations.Initialize_Configurations;
   end  Load_Model;

   ------------------------
   procedure Print_Configuration(This_Conf: System_Configuration) is
   begin
      UML_Explore.Print_Info(This_Conf);
   end Print_Configuration;

   -----------------------------------------------------------------------------------------
   -- HTML_File is an already existing and opened file, created by HTML_Configuration_Info,
   --  Adds to this file an html description of the current configuration.
   -----------------------------------------------------------------------------------------
   procedure HTML_Print_Configuration(HTML_File: File_Type; This_Conf: System_Configuration) is
   begin
      UML_Explore.Save_As_HTML(HTML_File,This_Conf);
   end HTML_Print_Configuration;

   -----------------------------
   procedure HTML_Print_Model(HTML_File: File_Type) is
   begin
      UML_Explore.Dump_HTML_Model(HTML_File);
   end HTML_Print_Model;

   procedure HTML_Print_Abstractions (HTML_File: File_Type); 
   ------------------------------
     procedure HTML_Print_System_Structure(
            HTML_File: File_Type;
            This_Conf: System_Configuration) is
        i: Natural :=0;
     begin
       --  Display Link to HTML class descriptions
       --
       Put_Line(HTML_File, "<table><tr><td><br><B>The Active System Classes are:</B><td>");
       Put_Line(HTML_File, "<table style='text-align:center;'>");
       for The_Class in UML_Types.TokensChart.ChartParent+1 .. UML_Types.All_Classes.all'Length loop
        if UML_Types.All_Classes(The_Class).Top_State_Num >0 then 
          if i mod 6 = 0 then
            Put_Line(HTML_File, "<tr>");
          end if;
          i := i+1;
          Put_Line(HTML_File, "<td style='text-align:center;'><table><tr><td >" &
          "<a href=""javascript:top.ViewStatechart('" & 
              UML_Types.All_Classes(The_Class).name.all & "');""" &
              " title=""Draw the Statechart"">" & 
              "<img src='chart.png'></img></a>&nbsp;"); 
          Put_Line (HTML_File, "<tr><td>");
          Put_Line (HTML_File, "<span style=""background-color:orange"" ");
          Put(HTML_File,  " onmouseover=""Tip('"); 
          Put(HTML_File, UML_Explore.HTML_Class_Tooltip(HTML_File,The_Class));
          Put_Line(HTML_File, "')"" ");
          Put_Line(HTML_File, " onmouseout=""UnTip()"" " & " >");
          Put(HTML_File, UML_Types.All_Classes(The_Class).name.all);
          Put_Line(HTML_File, "</span></table>");
          New_Line(HTML_File);
          --
          UML_Explore.Dump_Class_Statechart(The_Class);
          --
         end if;
       end loop;
       Put_Line(HTML_File, "</table>");
       Put_Line(HTML_File, "</table>");
       --
       Put (HTML_File,"<table><tr><td><B>The Active System Objects are:</B>&nbsp;<td>");
       Put_Line(HTML_File, "<table>");
       for The_Class in UML_Types.TokensChart.ChartParent+1 .. UML_Types.All_Classes.all'Length loop
         if UML_Types.All_Classes(The_Class).Top_State_Num >0 then
           Put_Line(HTML_File, "<table><tr>");
           for K in UML_Types.Active_Charts.all'Range loop
             if UML_Types.All_Charts(UML_Types.Active_Charts(K)).ChartParent =  The_Class then
               Put_Line(HTML_File, 
                 "<td" &
                 " onmouseover=""Tip('<b>EventsQueue:</b><br>" &
                 UML_Configurations.Display_Queue(This_Conf,K)  &
                 "<br><br><b>Local Vars:</b><br>" &
                 UML_Configurations.HTML_Transition_Format(UML_Configurations.Display_Vars(This_Conf,K)) &
                 "')"" ");
               Put (HTML_File, 
                 " onmouseout=""UnTip()"" " &
                 "> <span id='" & 
                 UML_Types.All_Charts(UML_Types.Active_Charts(K)).Name.all);
                if UML_Configurations.Is_Suspended(This_Conf,K) then
                  Put(HTML_File,
                   "' style='border:1px solid black;margin:3px;padding:2px;background-color:orange'>");
                else
                  Put(HTML_File,
                   "' style='border:1px solid black;margin:3px;padding:2px;background-color:white'>");
                end if;
                Put_Line (HTML_File,
                     UML_Types.All_Charts(UML_Types.Active_Charts(K)).Name.all & "</span>");
             end if;
           end loop;
           Put_Line(HTML_File, " :" & UML_Types.All_Classes(The_Class).Name.all);
           Put_Line(HTML_File, "</table>");
         end if;
       end loop;
       --
       Put_Line(HTML_File, "</table>");
       --
       HTML_Print_Abstractions (HTML_File);
     end HTML_Print_System_Structure;

   ---------------------------------------
   procedure Dump_States is
   begin
     null;
   end Dump_States;


  -----------------------------------------------------------------
  --  Chiamata  da Eval_State_Abstract_Labels
  --  E.g. Ids= true    (false)
  --  E.g. Ids= null
  --  E.g. Ids= 123
  --  E.g. Ids= maxqueuesize   (the longest object queue)
  --  E.g. Ids= queuedevents   (the total number of queued events)
  --  E.g. Ids= ObjectName
  --  E.g. Ids= OBJ.ATTR.ATTR...ATTR
  --  E.g. Ids= ATTR.ATTR...ATTR  
  --       (implycitly applied to Active_objects(1) quando unico obj)
  --          ATTR puo' essere una variabile_locale:OBJ  oppure "queuesize"
  --  
  --  Ids e' una vettore di Stringhe: i.e. (OBJ,ATTR,ATTR,...,ATTR)
  --      
  --      x[0]      ===   ("x","0")
  --      x.length  ===   ("x","length")
  --      [0,1]    ===   ("#", "0", "1")
  --      IsActive(C1.top.S1)    __  IsActive.C1.top.S1
  --      [1,2,3].tail.tail.head[2]
  -----------------------------------------------------------------
  --  TO BE CHECKED since ASSERT no longer exists!!!
  -----------------------------------------------------------------
  --
  --  PROBABILMNETE  DOVREBBE RITORNARE UN STRING  NON UN INT!!!
  procedure Eval_State_Expr (This_Conf: System_Configuration;
                           Ids: String_Table;
                           DollarValue: Integer;
                           EV: out Integer;
                           Failed : out Boolean) is
    --
    Sys: Int64 := Int64(This_Conf);
    Tmp_Var: UML_Types.SystemVar_Ref;
    This_Var: Natural :=0;
    This_Chart: Natural := 0;
    Default_Chart: Natural := 0; 
    This_Pos: Natural;
    Result: Integer :=0;
    Index: Integer;
  begin 
    Failed := False;
    --  global assertion
    --  this is just a literal, or maxqueuesize or IsActive
    --    
    --   True, False,  Null, maxqueuesuze,  123, -123
    --   maybe we should add  "empty" for "[]" 
    --  
    if ids'Length =0 then
         EV :=  UML_Types.IntEmptyStruct; 
         return;
       --
    elsif ids(1).all="false" or
          ids(1).all="FALSE" or
            ids(1).all="False" then
        EV := UML_Types.IntFalse;
        return;
     --  
    elsif ids(1).all="true" or
          ids(1).all="TRUE" or
            ids(1).all="True" then
        EV := UML_Types.IntTrue;
        return;
    --        
    elsif ids(1).all="null" or
          ids(1).all="NULL" or 
            ids(1).all="Null" then
        EV :=  UML_Types.IntNullObject;
        return;
      --
    elsif ids(1)(1) in '0'..'9' then
         EV := Integer'Value(ids(1).all);
         return;
      --
    elsif ids(1).all'length >1 and then
           ids(1)(1) = '-' and then
            ids(1)(2) in '0'..'9' then
          EV := Integer'Value(ids(1).all);
         return; 
      --
    elsif ids(1).all = "maxqueuesize" then
            EV :=  System_Model.MaxQueueSize(Sys);
           return; 
      --
    elsif ids(1).all = "confid" then  --??????????????  -- make observable state num
           EV := Integer(Sys);        -- ????????????
           return;
      --
    elsif ids(1).all = "Is_Active" then
      for I in UML_Types.Active_Charts.all'Range loop
        if UML_Types.All_Charts(UML_Types.Active_Charts(I)).Name.all = Ids(2).all then
          This_Chart:= UML_Types.Active_Charts(I);
          This_Pos := 3;
          exit;
        end if;
      end loop;
      if This_Chart =0 then
        -- if only 1 active chart allow the omission of the chart name prefix
        if UML_Types.Active_Charts.all'Length=1 then
          This_Chart := UML_Types.Active_Charts(1);
          This_Pos := 2;
        else
           Put_Line (Current_Error,
              "Error in Abstraction expression: missing object prefix");
           Runtime_Errors_Count := Runtime_Errors_Count +1;
           Runtime_Error_Num :=  InvalidAbstraction;  
           Runtime_Error_Val :=0;
           raise UML_Error;
        end if;
      end if;
      -- 
      --
      declare
        ThisName: String_Ref;
        Prev: String_Ref;
        TheChart: UML_Types.Chart renames UML_Types.All_Charts(This_Chart);
        TheObjectConf: Int64:= System_Model.Get_Object_Conf(Sys,TheChart.Chart_Position);
        Indent: Integer :=0;
        Active_States: UML_Types.States_Table := Object_Model.Get_Active_States(TheObjectConf);
      begin
        ThisName := Ids(This_Pos);  -- 2 or 3
        for J in This_Pos+1 .. Ids'length loop
           -- MEMORY:
           Prev := ThisName;
           ThisName := new String'(ThisName.all & "." & Ids(J).all);
           if Prev /= Ids(This_Pos) then
             -- MEMORY
             Free(Prev);
           end if;
        end loop;
        for  K in Active_States'Range loop
          Indent := Active_States(K).FullName.all'Length - ThisName.all'Length;
          for L in 1.. Indent +1 loop
            -- the name is a fullname fragment
            if Active_States(K).FullName(L..L+ThisName.all'Length-1)=ThisName.all and then
              -- the name is surrounded by "."
              (L=1 or else Active_States(K).FullName(L-1) = '.') and then
              (L=Indent+1 or else Active_States(K).FullName(L+ThisName.all'Length)='.') then
              -- MEMORY
              if  ThisName /= Ids(This_Pos) then
                Free(ThisName);
              end if;
              --return UML_Types.IntTrue;
              EV := UML_Types.IntTrue;
              return;
            end if;
          end loop;
       end loop;
        -- MEMORY
        if  ThisName /= Ids(This_Pos) then
          Free(ThisName);
        end if;
        EV := UML_Types.IntFalse;
        return;
        -- return UML_Types.IntFalse;
      end;
    end if;
    --
    -- OBJ  must be an active chart
    --  (i.e. one DEFINED by the model and containing at least a var or state)
    --
    -- This is the classical  obj.attribute[selector]Ã[selector2]   case
    --
    --
    -- Expr: object.attr[index].head.tail.length
    -- Expr: [1,2,3].head.attr[index].head.tail.length  
    -- dove  "DollarValue" e' il valore numerico di [1,2,3]
    --
    This_Pos := 1;   ---  the position of the identifier being evaluated
    if Ids(1)(1) /= '[' then
      --
      -- Expr:  object.attr ....
      --
      This_Chart := 0;  -- initially undefined. Must be explicitly defined
      for I in UML_Types.All_Charts.all'Range loop
        if UML_Types.All_Charts(I).Name.all = Ids(1).all then
          This_Chart:= I;
          Result := UML_Types.ObjectBase - I;
          if Ids'Length=1 then
             EV := Result;
            return;
          end if;
          exit;
        end if;
      end loop;
      if This_Chart=0 then
         Failed := True;
         return;
      end if;
      --
      --  Eval the Attribute
      --
--        --   obj.queuesize
--        if Ids(This_Pos).all= "queuesize" and then
--            (This_Pos > 1 or else UML_Types.Active_Charts.all'Length=1) then
--          --   ...  queuesize
--          --   queuesize
--          EV := Object_Model.QueueSize(System_Model.Get_Object_Conf(
--                  Sys,UML_Types.All_Charts(This_Chart).Chart_Position));
--          return;
--        end if;
--        --  obj.var
--        This_Var := 0;
--        for I in UML_Types.All_Charts(This_Chart).ChartVars.all'Range loop
--          Tmp_Var := UML_Types.All_Charts(This_Chart).ChartVars(I);
--          if Tmp_Var.Name.all = Ids(This_Pos).all then
--             This_Var := I;
--             Result := Object_Model.Get_Var_Value(
--                         System_Model.Get_Object_Conf(
--                            Sys, UML_Types.All_Charts(This_Chart).Chart_Position),I);
--             exit;
--          end if;
--        end loop;
--        if This_Var = 0 then
--          Failed := True;
--          return;
--        end if;
      --
    else  
      --
      -- composite initial value   "[1,2,3]" 
      --
      Result := DollarValue;
    end if;
    --
    ----------
    -- at this point we have in Result a Value. 
    -- we can return it or a modification of it depending on the further manipulations requested
    --
    This_Pos := This_Pos +1;
    --
    while This_Pos in Ids'Range loop
      --
      --  <..>.length
      --
      if Ids(This_Pos).all="Length" or else Ids(This_Pos).all="length"  then
        if Result < UML_Types.StructBase and then Result > UML_Types.ObjectBase then
          declare
            The_vect: int_table := Vectors_DB.retrieve(UML_Types.StructBase - Result);
          begin
            EV := The_vect'Length;
            return;
          end;
        else
          EV := 0;
          return;
        end if;
      --
      --  <..>.head
      --
      elsif Ids(This_Pos).all="Head" or else Ids(This_Pos).all="head"  then
        if Result < UML_Types.StructBase and then Result > UML_Types.ObjectBase then
          declare
            The_vect: int_table := Vectors_DB.retrieve(UML_Types.StructBase - Result);
          begin
            if The_vect'Length >0 then 
              EV := The_vect(1);
            else
              EV := 0;
            end if;
            return;
          end;
        else
          Failed := True;
          -- or EV := 0; ??
          -- obj.head = 0  o obj[0] = null?  or  Failed??
          return;
        end if;
      --
      --   Ids(This_Pos).all="Tail" or else Ids(This_Pos).all="tail"  then
      --  Handle Tail operator
      --
      --  <..>.tail
      --
      elsif Ids(This_Pos).all="Tail" or else Ids(This_Pos).all="tail"  then
        if Result < UML_Types.StructBase and then Result > UML_Types.ObjectBase then
          declare
            The_vect: int_table := Vectors_DB.retrieve(UML_Types.StructBase - Result);
            thetail: int_table(1..The_vect'Length-1) :=
                 The_vect(The_vect'First+1 ..The_vect'Last);
          begin
            EV :=  UML_Types.StructBase - abs(Vectors_DB.CheckNum(thetail));
            return;
          end;
        else
          Failed := True;
          return;
        end if;

      --
      --  <..>[Index]
      --
      elsif Ids(This_Pos)(1) in '0'..'9' or else Ids(This_Pos)(1)= '-' then
        --
        if Result= UML_Types.intTrue then
          -- true[13]
          -- EV := intFalse;
          Failed := True;
          return;
        elsif Result > UML_Types.StructBase then
          -- 1234[3] = 0
          -- EV := 0;
          Failed := True;
          return;
        elsif Result =  UML_Types.intFalse  then
          -- EV := intFalse;
          Failed := True;
          return;
        elsif Result < UML_Types.Objectbase then
          -- EV := intNullObject;
          Failed := True;
          return;
        end if;
        --
        Index := Integer'Value(ids(This_Pos).all);
         --  BISOGNA AGGIUSTARE LA SEMANTICA DEI VALORI DEFAULT E DEGLI OPERATORI MISTI
         --  considerare null=[]=0=false ???
         -- [1,2,3][9] = 0
         -- [][9] = 0
         -- [null,Obj,Obj][9] =null 
        declare
           The_vect: int_table := Vectors_DB.retrieve(UML_Types.StructBase - Result);
        begin
          if Index+1 not in The_vect'Range then
            EV := 0;
            --  or Failed := True;
            return;
          else
            Result := The_vect(Index+1);
            This_Pos := This_Pos +1;
          end if;
        end;
      --
      --  <..>.queuesize
      --
      elsif Ids(This_Pos).all= "queuesize" and then
            (This_Pos > 1 or else UML_Types.Active_Charts.all'Length=1) then
          --   ...  queuesize
          --   queuesize
          if  Result >= UML_Types.ObjectBase then
            Failed := True;
            return;
          end if;
          This_Chart := UML_Types.ObjectBase - Result;
          EV := Object_Model.QueueSize(System_Model.Get_Object_Conf(
                  Sys,UML_Types.All_Charts(This_Chart).Chart_Position));
          return;
      --
      --  <..>.attribute
      --
      else
        -- 1) Guarda se Result identifica un oggetto 
        -- 2) Guarda se attribute e' un attributo valido
        -- 3) se tutto ok restituisci il valore altrimenti setta Failed
        --
        if Result >= UML_Types.Objectbase then
          Failed := True;
          return;
        end if;
        --
        This_Chart := UML_Types.ObjectBase - Result;
        This_Var := 0;
        for I in UML_Types.All_Charts(This_Chart).ChartVars.all'Range loop
          Tmp_Var := UML_Types.All_Charts(This_Chart).ChartVars(I);
          if Tmp_Var.Name.all = Ids(This_Pos).all then
            This_Var := I;
            exit;
          end if;
        end loop;
        if This_Var = 0 then
          Failed := True;
          return;
        else
          Result := Object_Model.Get_Var_Value(
                       System_Model.Get_Object_Conf(
                        Sys, UML_Types.All_Charts(This_Chart).Chart_Position),This_Var);
          This_Pos := This_Pos +1;
        end if;
        --
      end if; -- initial composite or object name
    end loop; -- for all Ids of the expr
    --
    EV := Result;
    return;
  end Eval_State_Expr;

   -----------------------------------------------------------------------
   function Display_Signals (It: Evolutions_Iterator) return String is
    Tmp: String_Ref;
    Result: String_Ref := new String'("");
    First: Boolean := True;
  begin
    --  no signals case
    if It.Current_Object_Evolution=0 then
      raise UML_Error;
    end if;
    declare
      Encoded_Actions: Int64 := 
         It.Objects_Evolutions(It.Active_Object)(It.Current_Object_Evolution).Signals;
      These_Signals: Int64_Table :=  Object_Model.Get_Signals(Encoded_Actions);
    begin
      for I in These_Signals'Range loop
        declare
          This_Sig: Int_Table := Object_Model.Get_Signal(These_Signals(I));
        begin
          if First then
            First := False;
          else
             Tmp := Result;
             Result := new String'(Result.all & "; ");
             Free(Tmp);
          end if;
          Tmp := Result;
          Result := new String'(Result.all & UML_Types.Signal_Image(This_Sig));
          Free(Tmp);
        end;
      end loop;
    end;
    declare
      Static: String := Result.all;
    begin
      Free(Result);
      return Static;
    end;
  end Display_Signals;

  ----------------------
  function Display_Chart_Name(It: Evolutions_Iterator) return String is
  begin
    if It.Active_Object  /= 0 then
      return UML_Types.All_Charts(UML_Types.Active_Charts(It.Active_Object)).name.all;
    else
       return "";
    end if;
  end Display_Chart_Name;

  -----------------------------
  function Display_Trigger(It: Evolutions_Iterator) return String is
    Sys: Int64 := It.Current_Configuration;
    --  BUG BUG SHOULD take :
    --   It.objects_evolutions(it.active_object)(it.current_object_evolution).trigger
    --
    Trigger_Signal: Int64 :=
      It.objects_evolutions(it.active_object)(it.current_object_evolution).trigger;
--   WAS:
--       Object_Model.Get_Trigger_Signal(
--           System_Model.Get_Object_Conf(Sys,It.Active_Object), It.Active_Object);
  begin
    if Trigger_Signal = 0 then
      --  completion event
      return "-";
    elsif Trigger_Signal > 0 then
      -- stuttering or transiton activation
      return UML_Types.Trigger_Image(Vectors_DB.Retrieve(Trigger_Signal));
    else
      return "";
    end if;
  end Display_Trigger;

 function Append (To: String_Ref; What: String) return String_Ref is
    Tmp: String_Ref := To;
    Result: String_Ref;
 begin
    if To = null then
      Result := new String'(What);
    else
      Result := new String'(To.all & What);
      Free(Tmp);
    end if;
    return Result;
 end Append;

procedure HTML_Display_Ground_Info (HTML_File: File_Type;  My_Iter: Evolutions_Iterator) is
  Sigs: String := Display_Signals(My_Iter);
begin
  Put_Line (HTML_File,
            "<font size=-1>" & Uml_Configurations.Display_Chart_Name(My_Iter)  & "::"  &
            "<br>&nbsp;&nbsp;&nbsp; { &nbsp;" & Display_Trigger(My_Iter));
      --  WAS:      "<br>&nbsp;&nbsp;&nbsp; { &nbsp;" & Uml_Configurations.Display_Trigger(My_Iter));
  if Sigs /= "" then
    Put_Line(HTML_File,
            " &nbsp;/ <br>&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;" &
            HTML_Signals_Format(Display_Signals(My_Iter)) &
            "<br>&nbsp;&nbsp;&nbsp; } </font> ");
  else
     Put_line(HTML_File, "}</font>");
  end if;
  Put_Line (HTML_File, "<script>");
  Put_Line (HTML_File, "document.getElementById('" &
            Uml_Configurations.Display_Chart_Name(My_Iter) & 
            "').style.backgroundColor='lightgreen'; ");
  Put_Line (HTML_File, "</script>");
end HTML_Display_Ground_Info;


procedure HTML_Print_Possible_Evolutions (
                     HTML_File: File_Type; 
                     This_Conf: System_Configuration) is
  Index: Natural :=0;
  My_Iter: Evolutions_Iterator;
begin
   Put_Line (HTML_File,"<p><b>The Possible Evolutions from Configuration " &
                NickName(This_Conf,"C") & " are:<b><P>");
   --
   -- the table of possible evolutions
   -- first column : the source
   --
   Put_Line(HTML_File, 
      "<table border='1'><tr><td align=center><b>" &
      NickName(This_Conf,"C")  & 
      "</b><br>");
   declare
      Props: String_Table := Get_Abstract_State_Label_Images(This_Conf);
   begin
    if Props'Length /= 0 then
      Put(HTML_File, "{ ");
      for I in Props'Range loop
         Put(HTML_File, HTML_Format(Props(I).all));
         if I < Props'last then Put (HTML_File, ", "); end if;
      end loop;
      Put_Line(HTML_File, " }");
    end if;
   end; 
   --
   -- second column: the table of actual evolutions
   --   
   Put_Line(HTML_File, "<td>");
   Put_Line(HTML_File, "<table cellpadding='10' border='1'>");
   for K in UML_TYpes.Active_Charts.all'Range loop
     Iterator_Initialize (My_Iter,This_Conf,K);
     while Has_System_Transition(My_Iter) loop
        Index := Index+1;
        --
        Put_Line (HTML_File, "<tr><td>");
        Put (HTML_File, NickName(This_Conf,"C") &
              " --> "  &
              NickName(Get_Target_Configuration(My_Iter),"C") );
        declare
           Abstract_Labels: String_Table :=
              Get_Abstract_Action_Label_Images(My_Iter);
        begin
          Put (HTML_File, "&nbsp;&nbsp;&nbsp; { ");
          for I in Abstract_Labels'Range loop
            Put (HTML_File, HTML_Format(Abstract_Labels(I).all));
            if I < Abstract_Labels'last then Put(HTML_File, ","); end if;
          end loop;
          Put_Line (HTML_File, " }<td>");
        end;
       HTML_Display_Ground_Info (HTML_File, My_Iter);
       --
       Put (HTML_File, 
              "<td align=center><b><a href='javascript:top.sendcommand(""" &
              Integer'Image(Index)(2..Integer'Image(Index)'Length) &
               """)'>");
       Put_Line(HTML_File, NickName(Get_Target_Configuration(My_Iter),"C") );
       Put (HTML_File,"</a> &nbsp;");
       Put_Line(HTML_File, "</b><br>");
       declare
          Props: String_Table := 
             Get_Abstract_State_Label_Images(Get_Target_Configuration(My_Iter));
       begin
        if Props'Length /= 0 then
          Put(HTML_File, " { ");
          for I in Props'Range loop
             Put(HTML_File, HTML_Format(Props(I).all));
             if I < Props'last then Put (HTML_File, ", "); end if;
          end loop;
          Put_Line(HTML_File, " }");
        end if;
       end;
       ---
       Iterator_Advance(My_Iter);
     end loop;  -- Has_System_Transition
   end loop;  -- K in Active_Charts.all'Range
   Put_Line(HTML_File,"</table>");
   --
   Put_Line(HTML_File,"</table>");
   --
   if Index=0 then
      New_Line(HTML_File);
      Put_Line (HTML_File,"<i>No evolutions possible from Configuration <i> " & 
                NickName(This_Conf,"C"));
   end if;
   Put_Line(HTML_File, "<hr>");
end HTML_Print_Possible_Evolutions;


  function HTML_Observation_Rule (OO:UML_Types.Observation_Rule) return String is
    use UML_Types;
    res: String_Ref := new String'("");
    tmp: String_Ref;
    LR: Rule_Left_Elem;
  begin
    if OO.Kind= StateKind then
       tmp := res;
       res := new String'(res.all & "<b>State</b>&nbsp;&nbsp;");
       Free(tmp);
    else
       tmp := res;
       res := new String'(res.all & "<b>Action</b>&nbsp;&nbsp;");
       Free(tmp);
    end if;
    for I in OO.Left.all'Range loop
      if OO.Left(I).LeftOP /= NOOP then
        -- obj.var1 > obj.var2 
        for J in OO.Left(I).IdsL.all'Range loop
           tmp := res;
           res := new String'(res.all & OO.Left(I).IdsL(J).all);
           Free(tmp);
           if J /= OO.Left(I).IdsL.all'Last then
             tmp := res;
             res := new String'(res.all & ".");
             Free(tmp);
           end if;
        end loop;  -- IdsL
        tmp := res;
        case OO.Left(I).LeftOP is
          when EQ =>  res := new String'(res.all & "=");
          when GT =>  res := new String'(res.all & "&gt;");
          when LT =>  res := new String'(res.all & "&lt;");
          when LE =>  res := new String'(res.all & "&lt;=");
          when GE =>  res := new String'(res.all & "&gt;=");
          when NE =>  res := new String'(res.all & "/=");
          when others => null;
        end case; 
        Free(tmp);
        for J in OO.Left(I).IdsR.all'Range loop
           tmp := res;
           res := new String'(res.all & OO.Left(I).IdsR(J).all);
           Free(tmp);
           if J /= OO.Left(I).IdsR.all'Last then
             tmp := res;
             res := new String'(res.all & ".");
             Free(tmp);
           end if;
        end loop;  -- IdsR
        --
      elsif  OO.Kind= StateKind and then OO.Left(I).Lop /= null then 
        -- inState(obj.state)  ... 
        tmp := res;
        res := new String'(res.all & "inState(");
        Free(tmp);           
        for J in OO.Left(I).LArgs.all'Range loop
          tmp := res;
          res := new String'(res.all & OO.Left(I).LArgs(J).all);
          Free(tmp);
          if J /= OO.Left(I).LArgs.all'Last then
            tmp := res;
            res := new String'(res.all & ".");
            Free(tmp);
          end if;
        end loop;  -- LArgs            
        --
      else -- Kind=ActionKind
        if OO.Left(I).LTerm /= null then
          tmp := res;
          res := new String'(res.all & OO.Left(I).LTerm.all & ":" );
          Free(tmp);
        end if;
        if OO.Left(I).LPartner /= null then
          tmp := res;
          res := new String'(res.all & OO.Left(I).LPartner.all & "." );
          Free(tmp);    
        end if;
        if OO.Left(I).LOp /= null then
          tmp := res;
          res := new String'(res.all & OO.Left(I).LOp.all);
          Free(tmp);    
        end if;
        if OO.Left(I).LArgs /= null then
          tmp := res;
          res := new String'(res.all & "(" );
          Free(tmp);    
          for J in OO.Left(I).LArgs.all'Range loop
            tmp := res;
            res := new String'(res.all & OO.Left(I).LArgs(J).all);
            Free(tmp);
            if J /= OO.Left(I).LArgs.all'Last then
              tmp := res;
              res := new String'(res.all & ",");
              Free(tmp);
            end if;
          end loop;  -- LArgs
          tmp := res;
          res := new String'(res.all & ")" );
          Free(tmp);
        end if;
      end if;
      --
      if I /= OO.Left.all'Last then
        tmp := res;
        res := new String'(res.all & "<br>&nbsp;&nbsp;and&nbsp;&nbsp;");
        Free(tmp);
      else   -- last left_elem
        --
        tmp := res;
        res := new String'(res.all & "&nbsp;-&gt;&nbsp;");
        Free(tmp);
        --
        for J in OO.RLabels.all'Range loop
          tmp := res;
          res := new String'(res.all & OO.RLabels(J).all);
          Free(tmp);
          if J /= OO.RLabels'Last then
            if J = 1 then
              tmp := res;
              res := new String'(res.all & "(");
              Free(tmp);
            else
              tmp := res;
              res := new String'(res.all & ",");
              Free(tmp);
            end if;
          elsif J > 1 then
              tmp := res;
              res := new String'(res.all & ")");
              Free(tmp);
          end if;
        end loop;  -- RLabels
      end if;
    end loop;  -- OO.Left
    --
    tmp := res;
    res := new String'(res.all & "<br> ");
    Free(tmp);
    --
    declare
      Result : String := res.all;
    begin
      Free(res);
      return Result;
    end;
  end HTML_Observation_Rule;

  function HTML_Abstraction_Rules(Kind: UML_Types.AbstractionKind) return String is
    use UML_Types;
    res: String_Ref := new String'("");
    tmp: String_Ref;
  begin
   for I in All_Observations.all'Range loop
      if All_Observations(I).Kind=Kind then
        tmp := res;
        res := new String'(res.all & HTML_Observation_Rule(UML_Types.All_Observations(I)) & " ");
        Free(tmp);
      end if;
   end loop;
    --
    if res.all = "" then
      tmp := res;
      res := new String'(res.all & "&nbsp;<i>none</i>&nbsp; ");
      Free(tmp);
    end if;
    declare
      Result : String := res.all;
    begin
      Free(res);
      return Result;
    end;
  end HTML_Abstraction_Rules;

 procedure HTML_Print_Abstractions (HTML_File: File_Type) is
   use UML_Types;
  begin
   Put_Line (HTML_File, "<table ><tr><td><b> The System Abstractions are: &nbsp;</b>");
   --
   Put_Line (HTML_File, "<td valign=top>");
   Put_Line (HTML_File, 
               "<table border=1> <tr><td bgcolor=white " &
               " onmouseover=""Tip('" & HTML_Abstraction_Rules(StateKind) &
               "')"" onmouseout=""UnTip()"" " & " >");
   Put_Line (HTML_File, "<b>State Abstractions</b>");
   Put_Line (HTML_File, "</table>");
   --
   Put_Line (HTML_File, "<td valign=top>");
   Put_Line (HTML_File, "<table border=1> <tr> <td bgcolor=white " &
               " onmouseover=""Tip('" & HTML_Abstraction_Rules(ActionKind) &
               "')"" onmouseout=""UnTip()"" " & " >");
   Put_Line (HTML_File, "<b>Action Abstractions</b>");
   Put_Line (HTML_File, "</table>");
   --
   Put_Line (HTML_File, "</table>");
   Put_Line (HTML_File, "</table><br>");
 end HTML_Print_Abstractions;

  package body UML_Types is separate;
  package body UML_Configurations is separate;
  package body Object_Model is separate;
  package body System_Model is separate;
  package body UML_Parser is separate;
  package body UML_Explore is separate;

  -- called by UCTL_Logics
  procedure Check_Liveness(This_Conf: System_Configuration) is
  begin
      null;
  end  Check_Liveness;

  --------------------------------------
  function Are_Matching(Item1: String; Item2:String) return Boolean is
  begin
   if Item1'length /= Item2'Length then return False; end if;
   --
   if Item1= "True" and then Item2= "true" then return True; end if;
   if Item1= "true" and then Item2= "True" then return True; end if;
   if Item1= "False" and then Item2= "false" then return True; end if;
   if Item1= "false" and then Item2= "False" then return True; end if;
   --
   return Item1=Item2;
  end Are_Matching;
  
  function Configuration_Satisfies(This_SRC: System_Configuration;
                                      State_predicate: String_Table) return Boolean is
    Result:Boolean;
    Thislabels: String_Tables_Vector := Get_Abstract_State_Labels(This_SRC);
  begin
    for I in Thislabels'Range loop  -- look inside all model lables
      if State_predicate'Length=1 and then
         Thislabels(I) /= null and then
         State_predicate(1).all = Thislabels(I)(1).all then 
          return True;
          -- do not look inside the arguments, if only main key is present.
          --  we do NOT distinguish between  "pred" and "pred()"
          -- does this hold also for ection expressions???
      end if;
      if Thislabels(I) /= null and then 
           State_predicate'length = Thislabels(I).all'Length then
         Result := True;
         for J in State_predicate'Range loop   -- look inside assertion ids
           if State_predicate(J).all /= "*" and then
              not Are_Matching(State_predicate(J).all, Thislabels(I)(J).all) then
             Result := False;
             exit;
           end if;
         end loop;
         if Result=True then return True; end if;
      end if;
    end loop;
    return False;
  end Configuration_Satisfies;
                                      
end Kernel;

