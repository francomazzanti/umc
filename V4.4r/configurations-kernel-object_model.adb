with Ada.Unchecked_Deallocation;
with Text_IO; use Text_IO;
--with GNAT.Semaphores; use GNAT.semaphores;
--with SYSTEM; use System;
separate (Configurations.Kernel)
package body Object_Model is

--OMSEM: Binary_Semaphore(true, Default_ceiling);
--pragma Volatile(OMSEM);

-------------------------------------------------------------------
-- -- data la configurazione attuale di un oggetto, restituisce la
-- -- lista delle sue possibili evolutions.
-- --   (Active_Object serve per riconoscere i segnali locali all'oggetto)
--   function Get_Object_Evolutions (
--           Current_Conf: Int64;
--           Active_Object: Natural) return Object_Runtime_Evolutions_Ref;
-- 
-- -- La funzione "Get_Initial_Configuration" restituisce la configurazione
-- -- iniziale del Active_object risultante dal parsing del file sorgente uml.
-- 
--   function Get_Initial_Configuration (Active_Object:Natural) return Int64;
-- 
--        ------------- query functions ---------------
--   function Get_Signal (Nick: Int64) return Int_Table;
 --  function Get_Signals (Nick:Int64) return Int64_Table;
--   function Get_Transitions_Sequence(Nick: Int64) return UML_Types.Transitions_Table;
-- 
--   function Get_Active_States (Current_Conf: Int64) return UML_Types.States_Table;
--   function Get_Var_Value(Current_Conf: Int64;
--                      The_Variable:Positive) return Integer;
--   function Get_Var_Values(Current_Conf: Int64) return Int_Table;
--   function QueueSize(Current_Conf: Int64) return Natural;
--   function Get_Queue_Items(Current_Queue: Int64) return Int64_Table;
-- 
--   function Get_Trigger_Signal (Current_Conf: Int64;
--                               Active_Object:Natural ) return Int64;
-- 
--   function Is_Suspended(Current_Conf: Int64) return Boolean;
--   function Get_Suspended_Op(Current_Conf: Int64) return Natural;
-- 
--  ---------------  initialization utilities -----------------
--  procedure ReInitialize_Object_Model;
-------------------------------------------------------------------

use Global_Env;
use UML_Types;

------------------------------------------------------------------------
procedure Free is new Ada.Unchecked_Deallocation(Transitions_Table,Transitions_Table_Ref);
procedure Free is new Ada.Unchecked_Deallocation(
            Object_Runtime_Evolutions,Object_Runtime_Evolutions_Ref);
------------------------------------------------------------------------

package Uml_Model is
  --
  function Compute_Object_Evolutions(
      Current_Conf: Int64;
      Active_Object: Natural) return Object_Runtime_Evolutions_Ref;
 --
 procedure ReInitialize_Model;
end Uml_Model;
use UML_Model;

-----------------------------------------------------------------
  function Null_Transitions_Table return Transitions_Table;
  function Mk_Key (This_Table: Transitions_Table;
            Cache_Max: Natural := Flags.Key_Module) return Positive;
  package Transitions_DB is new NickNames  -- in UML_CONFIGURATIONS spec
     (Transitions_Table,         --  the transitions local num_key
      Null_Transitions_Table,
      Mk_Key,
      "=");
-----------------------------------------------------------------
  function Null_Transitions_Table return Transitions_Table is
     Result: Transitions_Table(1..0);
  begin
     return  Result;
  end Null_Transitions_Table;

  function Mk_Key (This_Table: Transitions_Table;
            Cache_Max: Natural := Flags.Key_Module) return Positive is
    Shift: Int32 := 1;
    Result: Int32 := 0 ;
    Tmp: Int32 := 0;
    Tmp64: Long_Long_Integer := 0;
  begin
       for I in This_Table'Range loop
       Tmp := Int32(This_Table(I).Num_Key) * Shift;
       Tmp64 := Long_Long_Integer(This_Table(I).Num_Key) * Long_Long_Integer(Shift);
       if Long_Long_Integer(Tmp) = Tmp64  then
         Result := Result xor Tmp;
         Shift := Shift * 2;
       else
         Tmp := Int32(This_Table(I).Num_Key);
         Result := Result xor Tmp;
         Shift := 1;
       end if;
    end loop;
    return Natural (Result mod Int32(Cache_Max) ) +1;
  end Mk_Key;
-----------------------------------------------------------------


-----------------------------------------------------------------
--  type Object_Runtime_Configuration (SSize: Natural;
--                             VSize: Natural) is record
--     Current_States: States_Table (1..SSize);
--     Current_Vars: Int_Table (1..VSize);
--     Current_Queue:  Int64 := 0;    ---  nick in Signals_DB
--  end record;
-----------------------------------------------------------------

 ----------------------------------------------------------------------
  -- Current Chart is the index in All_Charts
  -- Evals the args table of the current trigger
  -- used by Get_Initial_Configuration  during the evaluation of the
  --  initial values of the variables (e.g. self)
  ----------------------------------------------------------------------

  function Eval_SimpleIntExpr (
       Current_Chart: Natural;
       This_Expr: SimpleIntExpr_Ref;
       Current_Conf: Object_Runtime_Configuration;
       Current_EVars: Int_Table) return Integer is
    TheVar: SystemVar_Ref;
  begin
    if This_Expr.Local_Variable /= 0 then
      TheVar := All_Charts(Current_Chart).ChartVars(This_Expr.Local_Variable);
      return Current_Conf.Current_Vars(TheVar.all.Local_Num_Key);
    elsif This_Expr.Event_Variable /= null then
      return Current_EVars(This_Expr.Event_Variable.all.Num_Key);
    elsif This_Expr.Special_Token /= null then
        if This_Expr.Special_Token.all = "emptyqueue" then
           if Signals_DB.Retrieve(Current_Conf.Current_Queue)'Length =0 then
              return IntTrue;
           else
              return IntFalse;
           end if;
        elsif This_Expr.Special_Token.all = "queuesize" then
          This_Expr.Kind := Number;
          return Signals_DB.Retrieve(Current_Conf.Current_Queue)'Length;
        elsif This_Expr.Special_Token.all = "true" then
            This_Expr.Kind := Bool;
            return IntTrue;
        elsif This_Expr.Special_Token.all = "false" then
            This_Expr.Kind := Bool;
            return IntFalse;
        elsif This_Expr.Special_Token.all = "self" then
            This_Expr.Kind := Object;
            return ObjectBase - Current_Chart;
        elsif This_Expr.Special_Token.all = "this" then
            This_Expr.Kind := Object;
            return ObjectBase - Current_Chart;
        elsif This_Expr.Special_Token.all = "null" then
            This_Expr.Kind := Object;
            return NullObject.Simple.Literal_Value;
        else
            Put_Line(Current_Error,"Unexpected token as expression");
            Runtime_Errors_Count := Runtime_Errors_Count +1;
            Runtime_Error_Num := InvalidSpecialToken;
            Runtime_Error_Val :=0;
            raise UML_Error;
        end if;
    elsif This_Expr.Is_Vector /= null then
       declare
          tmpv: Int_Table(This_Expr.Is_Vector.all'Range);
          reduced: Natural := tmpv'Length;
       begin
         for I in tmpv'Range loop
           tmpv(I) := Eval_SimpleIntExpr(Current_Chart, This_Expr.Is_Vector(I),
                                          Current_Conf, Current_EVars);
         end loop;
         while reduced >0 and then tmpv(reduced) =0 loop
              reduced := reduced-1;
         end loop;
         return StructBase - Vectors_DB.NickNum(tmpv(1..reduced)); 
         --   [0,0,0] = [];
       end;
    else
      return This_Expr.Literal_Value;
    end if;
  end Eval_SimpleIntExpr;

  function IntExpr_Kind (
       Current_Chart: Natural;
       This_IntExpr: IntExpr) return Value_Kind is
    Result: Value_Kind;
    BaseKind: Value_Kind;
  begin
    if This_IntExpr.Left = null then
       Result := This_IntExpr.Simple.Kind;
       if Result = Undefined then
         if This_IntExpr.Simple.Local_Variable >0 then
           Result := All_Charts(Current_Chart).ChartVars(This_IntExpr.Simple.Local_Variable).Kind;
         elsif This_IntExpr.Simple.Event_Variable /= null then
           Result := This_IntExpr.Simple.Event_Variable.Kind;
         end if;
         if This_IntExpr.Simple.Is_Indexing /= null then
             BaseKind := Result;
             if This_IntExpr.Simple.Is_Indexing.all'Length=1 then
               case BaseKind is 
                 when Objvector => Result := Object;
                 when Objmatrix => Result := Objvector;
                 when Objcube => Result := Objmatrix;
                 when Numcube => Result := Nummatrix;
                 when Nummatrix => Result := Numvector;
                 when Numvector => Result := Number;
                 when Boolcube => Result := Boolmatrix;
                 when Boolvector => Result := Bool;
                 when Boolmatrix => Result := Boolvector;
                 when others => null;
               end case;
             elsif This_IntExpr.Simple.Is_Indexing.all'Length=2 then
               case BaseKind is
                 when Objmatrix => Result := Object;
                 when Objcube => Result := Objvector;
                 when Numcube => Result := Numvector;
                 when Nummatrix => Result := Number;
                 when Boolcube => Result := Boolvector;
                 when Boolmatrix => Result := Bool;
                 when others => null;
               end case;
             else  -- This_IntExpr.Simple.Is_Indexing.all'Length=3
               case BaseKind is
                 when Objcube => Result := Object;
                 when Numcube => Result := Number;
                 when Boolcube => Result := Bool;
                 when others => null;
               end case;
             end if;  -- Is_Indexing.all'Length\
         end if; -- Is_Indexing /= null
         --
         -- special case v[i].length
         --
         if This_IntExpr.Simple.Special_Token /= null and then
             This_IntExpr.Simple.Special_Token.all = "Length" then
           Result := Number;
         end if;
       end if; -- Undefined
    else
       return (IntExpr_Kind(Current_Chart, This_IntExpr.Left.all));
    end if;
    return Result;
  end IntExpr_Kind ;

  function umlExpr_Kind (
       Current_Chart: Natural;
       This_umlExpr: umlExpr) return Value_Kind is
  begin
    if This_umlExpr.umlInt /= null then
          return IntExpr_Kind(Current_Chart, This_umlExpr.umlInt.all);
    else
       return Bool;
    end if;
  end umlExpr_Kind ;


  procedure Set_IntExpr_Kind (
       Current_Chart: Natural;
       This_IntExpr: IntExpr_Ref; 
       Kind: Value_Kind) is
    Result: Value_kind;
  begin
    if This_IntExpr.Left = null then
      Result := This_IntExpr.Simple.Kind;
      if This_IntExpr.Simple.Local_Variable >0 then
         All_Charts(Current_Chart).ChartVars(This_IntExpr.Simple.Local_Variable).Kind := Kind;
      elsif This_IntExpr.Simple.Event_Variable /= null then
         This_IntExpr.Simple.Event_Variable.Kind := Kind;
      end if;
    end if;
  end Set_IntExpr_Kind ;

  procedure Set_umlExpr_Kind (
       Current_Chart: Natural;
       This_umlExpr: umlExpr_Ref; 
       Kind: Value_Kind) is
  begin
     if This_umlExpr.umlInt /= null then
         Set_IntExpr_Kind(Current_Chart, This_umlExpr.umlInt, Kind);
     end if;
  end Set_umlExpr_Kind;

------------------    iteration  functions -----------------------

--------------------------------------------------------------------
--  called by Get_Initial_Configuration.
--  updates This_RState with the list of active states belongin to Current_State
--
-- Given the pointer "This_RState", and the state "Current_State",
-- updates the table pointed by This_RState by setting the reference
--  to the Current_State in the correct position in the table.
-- If the Current State is a composite state, navigates all its substates
--  setting the reference to currently active substates in the appropriate
--  positions of the table.
--------------------------------------------------------------------
 procedure Assign (Current_State: State_Ref; 
                    This_RState: in out States_Table) is
   --
   outgoing: Transition_Ref;
   target: State_Ref;
 begin
    case Current_State.Kind is
     when Simple =>
       if Is_Default_Initial(Current_State) then
         -- ACTUALLY THERE IS NO DEFAULT INITIAL STATE  ... 
         outgoing := Current_State.OutgoingTransitions(1).These_Transitions(1);
         target := outgoing.Target(1);
         Assign(target,This_RState);
       else
         This_RState(Current_State.FirstRuntimePosition) :=
            Current_State;
       end if;
     when Composite =>
       Assign(Current_State.Substates(1),This_RState);
     when Parallel =>
       for I in Current_State.SubStates.all'Range loop
         Assign(Current_State.Substates(I),This_RState);
       end loop;
   end case;
 end Assign;

-- La funzione "Get_Initial_Configuration" restituisce la configurazione
-- iniziale risultante dal parsing del file sorgente uml.
--
--  Non abbiamo bisogna di salvare la object initial configuration dal
--  momento che viene gia' salvata la system initial conf.
--
  function Get_Initial_Configuration (Active_Object:Natural) return Int64 is
    This_Chart_Index : natural := Active_Charts(Active_Object);
    States_Count: Natural;
    Vars_Count: Natural;
  begin
    States_Count :=
      All_Charts(This_Chart_Index).ChartStates(1).LastRuntimePosition;
    Vars_Count := All_Charts(This_Chart_Index).ChartVars.all'Length;
    declare
      This_ORC: Object_Runtime_Configuration(States_Count,Vars_Count);
    begin
      This_ORC.Self := This_Chart_Index;
      This_ORC.Current_Vars := (others => 0);
      for I in 1..Vars_Count loop
        if All_Charts(This_Chart_Index).ChartVars(I).Initial /= null then
          This_ORC.Current_Vars(I) :=
             Eval_SimpleIntExpr (This_Chart_Index,
                All_Charts(This_Chart_Index).ChartVars(I).Initial,
                This_ORC,
                Empty_Int_Table);
        elsif All_Charts(This_Chart_Index).ChartVars(I).Kind=Numvector or else
              All_Charts(This_Chart_Index).ChartVars(I).Kind=Objvector or else
              All_Charts(This_Chart_Index).ChartVars(I).Kind=Boolvector or else
              All_Charts(This_Chart_Index).ChartVars(I).Kind=Composite then
           This_ORC.Current_Vars(I) := Nullvector;
        else
           This_ORC.Current_Vars(I) := 0;
        end if;
      end loop; 
      --
      This_ORC.Current_States := (others => null);
      Assign (All_Charts(This_Chart_Index).ChartStates(1),
              This_ORC.Current_States);
      --
      This_ORC.Current_Queue := abs(Signals_DB.NickNum(Null_Int64_Table));
      --
--declare
--  NN: Integer := ORC_DB.JustCheckNum(This_ORC);
--begin
--  if NN = 0 then Put_line("NEWORC: " & Integer'Image(Mk_Key(This_ORC))); end if;
--end;
      return abs(ORC_DB.NickNum(This_Orc));
    end;
  end Get_Initial_Configuration;


-- data la configurazione attuale di un oggetto, restituisce la
-- lista delle sue possibili evolutions.
--   (Active_Object serve per riconoscere i segnali locali all'oggetto)
--
  function Get_Object_Evolutions (
      Current_Conf: Int64;
      Active_Object: Natural) return Object_Runtime_Evolutions_Ref is
    --
    OCI: Object_Configuration_Info;
  begin
    --
    -- if already known, return the already computer result
    --   otherwise compute it (and save it)
    --
    OCI := OCI_DB.Retrieve(Current_Conf); 
    if OCI.Object_Evolutions = null then
      OCI.Object_Evolutions :=
         Compute_Object_Evolutions(Current_Conf,Active_Object);
      OCI_DB.Store(OCI,Current_Conf);
    end if;
    return OCI.Object_Evolutions;
  end Get_Object_Evolutions;

  ------------------ query functions --------------------

  function Get_Signal (Nick: Int64) return Int_Table is
  begin
     return Vectors_DB.Retrieve(Nick);
  end Get_Signal;


  function Get_Signals (Nick:Int64) return Int64_Table is
  begin
     return Signals_DB.Retrieve(Nick);
  end Get_Signals;

  function Get_Queue_Items (Current_Queue:Int64) return Int64_Table is
  begin
     return Signals_DB.Retrieve(Current_Queue);
  end Get_Queue_Items;

  function Get_Trigger_Signal (Current_Conf: Int64;
                               Active_Object:Natural ) return Int64 is
    OCI: Object_Configuration_Info;
  begin 
    OCI := OCI_DB.Retrieve(Current_Conf);
    if OCI.Object_Evolutions = null then
       OCI.Object_Evolutions := 
          Compute_Object_Evolutions(Current_Conf,Active_Object);
       OCI_DB.Store(OCI,Current_Conf);
    end if;
    if OCI.Object_Evolutions.all'Length =0 then
          --  no evOLUtions
      return -1;
    else
      return OCI.Object_Evolutions(1).Trigger;
    end if;
  end Get_Trigger_Signal; 


  function Get_Transitions_Sequence(Nick: Int64) return Transitions_Table is
    No_Transitions: Transitions_Table(1..0);
  begin 
    if Nick > 0 then
      return Transitions_DB.Retrieve(Nick);
    else
      return No_Transitions;
    end if;
  end Get_Transitions_Sequence;

  function Get_Suspended_Op(Current_Conf: Int64) return Natural is
     This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf);
--    --
--    This_Sequence: Transitions_Table :=
--            Transitions_DB.Retrieve(This_ORC.Suspended_Sequence); 
--    Suspended_Action: Action_Ref := 
--          This_Sequence(This_ORC.Suspended_Transition_Index).
--              Actions(This_ORC.Suspended_Action_Index);
  begin
--    return Suspended_Action.Signalled_Event.The_Event.Num_Key;
    return  This_ORC.Suspended_Call;
  end Get_Suspended_Op;

  --  an object is suspended after the execution
  --  of a call operation, but only until the return signal is returned.
  --
  function Is_Suspended(Current_Conf: Int64) return Boolean is
     This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf);
  begin
    if This_ORC.Suspended_Transition_Index = 0 then
       return False;
    end if; 
    declare
      This_Queue : Int64_Table := Signals_DB.Retrieve(This_ORC.Current_Queue);
    begin
      if This_Queue'Length = 0 then
        return True;
      end if;
      declare
        Top: Int_Table := Vectors_DB.Retrieve(This_Queue(1));
      begin
         if Top(1) in OpReturn_Event.Num_Key .. OpReturnBoolC_Event.Num_Key  then
           return False;
         else
           return True;
         end if;
      end;
    end;
  end Is_Suspended;


  function Get_Var_Value(Current_Conf: Int64;
                     The_Variable:Positive) return Integer is
    This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf);
  begin
    return This_ORC.Current_Vars(The_Variable);
  end Get_Var_Value;

  function Get_Var_Values(Current_Conf: Int64) return Int_Table is
    This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf);
  begin
    return This_ORC.Current_Vars;
  end Get_Var_Values;

  function QueueSize(Current_Conf: Int64) return Natural is
    This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf);
    These_Signals: Int64_Table := Signals_DB.Retrieve(This_ORC.Current_Queue);
  begin 
    return These_Signals'Length;
  end QueueSize;


  function IsNested (s1,s2: State) return boolean is
  begin
    if s1.FullName.all'Length > s2.FullName.all'Length and then
     s1.FullName.all(1..s2.FullName.all'Length) = s2.FullName.all and then
     s1.FullName.all(s2.FullName.all'Length+1) = '.' then
       return True;
    else
       return False;
    end if;
  end IsNested;

  function Get_Active_States(Current_Conf:Int64) return States_Table is
    This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf);
    Count: Natural :=0;
    TmpResult: States_Table(1..This_ORC.Current_States'Length);
  begin
    for I in This_ORC.Current_States'Range loop
      if This_ORC.Current_States(I) /= null then
        if This_ORC.Suspended_Transition_Index =0 then
          Count := Count+1;
          TmpResult(Count) := This_ORC.Current_States(I);
        else
          declare
            This_Sequence: Transitions_Table :=
                 Transitions_DB.Retrieve(This_ORC.Suspended_Sequence);
            This_Transition: Transition_Ref := 
                 This_Sequence(This_ORC.Suspended_Transition_Index);
            This_State: State_Ref := This_ORC.Current_States(I);
            Exited: Boolean := False;
          begin
            for J in This_Transition.Source.all'Range loop
               if This_Transition.Source(J).FullName.all = This_State.FullName.all or else
                 IsNested(This_Transition.Source(J).all, This_State.all) then
                 Exited := True;
               end if;
            end loop;
            if not Exited then
              Count := Count+1;
              TmpResult(Count) := This_ORC.Current_States(I);
            end if;
          end; 
        end if;
      end if;
    end loop;
    declare
      Result: States_Table(1..Count);       
    begin
      Result :=  TmpResult(1..Count);
      return Result;
    end;
  end Get_Active_States; 

   ---------------  initialization utilities -----------------

   procedure Initialize_Object_Model is
   begin
      UML_model.ReInitialize_Model;
      ORC_DB.ReInitialize_DB; --QQQ
      Signals_DB.ReInitialize_DB;
      Transitions_DB.ReInitialize_DB;
     OCI_DB.ReInitialize_DB;
   end Initialize_Object_Model;

-------------------------------------------------------------------
-------------------------------------------------------------------
package body Uml_Model is
use System_Model;
--------------------------- object model.ads ---------------------------------
--
--  type Object_Runtime_Configuration (SSize: Natural;
--                             VSize: Natural) is record
--     Current_States: States_Table (1..SSize);
--     Current_Vars: Int_Table (1..VSize);
--     Current_Queue:  Int64;    ---  nick in Signals_DB
--  end record;
--
--  type Object_Runtime_Evolution is record
--    Evolving_Object: Natural;  --  index in Active_Charts'Range
--    Trigger: Int64;  -- nick in signals_db  -- 1 signal
--    Transitions: Int64;  -- nick in transitions_db
--    Signals: Int64;  -- nick in signals_db: all events, propri+altrui+change
--    Target: Int64;   -- nick in orc_db
--  end record;
--
--  type Object_Runtime_Evolutions is array
--     (Positive range <>) of Object_Runtime_Evolution;
--
--  type Object_Runtime_Evolutions_Ref is access Object_Runtime_Evolutions;
--
--  type Object_Configuration_Info is record
--    Object_Evolutions:  Object_Runtime_Evolutions_Ref;
--  end record;

-- package ORC_DB is new NickNames
-- package Vectors_DB is new NickNames
-- package Signals_DB is new NickNames
-- package Transitions_DB is new NickNames
-- package OCI_DB is new Dyn_Store
--

--------------------------  uml_model.ads -------------------------------
--
--  function Compute_Object_Evolutions(
--      Current_Conf: Int64;
--      Active_Object: Natural) return Object_Runtime_Evolutions_Ref
--
--------------------------------------------------------------------------
--

subtype Action_ref is UML_Types.Action_ref;

  type Transitions_Tables_Vector is
     array (Positive Range <>) of Transitions_Table_Ref;
  type Transitions_Tables_Vector_Ref is access Transitions_Tables_Vector;
  Empty_Vector: Transitions_Tables_Vector(1..0);

  function Eval_IntExpr (
     Current_Chart: Natural;
     This_Expr:IntExpr;
     Current_RVars: Int_Table;
     Current_RQueue: Int64_Table;
     Current_EVars: Int_Table)    return Integer;

-------------------------------------------------------------------
 function Eval_SimpleIntExpr (
         Current_Chart: Natural;
         This_Expr:SimpleIntExpr_Ref;
         Current_RVars: Int_Table;
         Current_RQueue: Int64_Table;
         Current_EVars: Int_Table) return Integer is
    --
    Result: Integer;
  begin
    --
    if This_Expr.Local_Variable /= 0 and then This_Expr.Remote_Variable =0  then
      --  evaluation of class variable
      Result :=  Current_RVars(This_Expr.Local_Variable);
      return Result;
    --
    elsif This_Expr.Event_Variable /= null and then This_Expr.Remote_Variable =0  then
       -- evaluation of parameter of trigger
      Result :=  Current_EVars(This_Expr.Event_Variable.all.Num_Key);
      return Result;
      --
    elsif This_Expr.Is_Indexing /= null and then
            This_Expr.Is_Indexing.all'Length >0  then
         Result := Eval_SimpleIntExpr (Current_Chart,This_Expr.Head_Tail_Data,
                           Current_RVars,Current_RQueue,Current_EVars);
         -- we have one or more indexes
         for K in This_Expr.Is_Indexing.all'Range loop
           -- for each index of the structure
           if Result in ObjectBase+1 .. StructBase then
             declare
               myvect: Int_Table := Vectors_DB.Retrieve(StructBase - Result);
               myindex: Integer := 
                   Eval_IntExpr(Current_Chart, This_Expr.Is_Indexing(K).all,
                                    Current_RVars,Current_RQueue,Current_EVars);
             begin
               if myindex = IntFalse then myindex := 0; end if;
               if myindex = IntTrue then myindex := 1; end if;
               if myindex <= ObjectBase then
                    myindex := ObjectBase - myindex; 
               end if;
               if myindex < 0 then myindex := -myindex; end if;
               if myindex+1 in  myvect'range then
                 Result := myvect(myindex+1);
                  if K= This_Expr.Is_Indexing.all'last then
                     exit;
                  end if;
               else 
                 Result := 0;
                 exit;
               end if;
             end;
           else
             Result := 0;
             exit;
           end if;
        end loop;
        return Result;
        --
    -- special  keywords
    elsif This_Expr.Special_Token /= null then
        --  emptyqueue
        if This_Expr.Special_Token.all = "emptyqueue" then
           if  Current_RQueue'Length=0 then
              return IntTrue;
           else
              return IntFalse;
           end if;
        -- queuesize
        elsif This_Expr.Special_Token.all = "queuesize" then
          return Current_RQueue'Length;
        -- true
        elsif This_Expr.Special_Token.all = "true" then
            return IntTrue;
        -- false
        elsif This_Expr.Special_Token.all = "false" then
            return IntFalse;
        -- self
        elsif This_Expr.Special_Token.all = "self" then
            return ObjectBase - Current_Chart;
        -- this
        elsif This_Expr.Special_Token.all = "this" then
            return ObjectBase - Current_Chart;
        -- null
        elsif This_Expr.Special_Token.all = "null" then
            return NullObject.Simple.Literal_Value;
        --
      elsif (This_Expr.Special_Token.all = "Length" or
             This_Expr.Special_Token.all = "length") then
          Result := Eval_SimpleIntExpr (Current_Chart,This_Expr.Head_Tail_Data,
                           Current_RVars,Current_RQueue,Current_EVars);
          if Result in ObjectBase+1 .. StructBase then
            declare
              myvect: Int_Table := Vectors_DB.Retrieve(StructBase - Result);
            begin
              return myvect'Length;
            end;
          else
            return 0;
          end if;
         --
      elsif (This_Expr.Special_Token.all = "Head" or
             This_Expr.Special_Token.all = "head") then
          Result := Eval_SimpleIntExpr (Current_Chart,This_Expr.Head_Tail_Data,
                           Current_RVars,Current_RQueue,Current_EVars);
          if Result in ObjectBase+1 .. StructBase then
            declare
                myvect: Int_Table := Vectors_DB.Retrieve(StructBase - Result);
            begin
              if myvect'length > 0 then
                Result := myvect(1);
                return Result;
              else
                 return 0;
              end if;
            end;
          else
            return 0;
          end if;
         --
      elsif (This_Expr.Special_Token.all = "Tail" or
             This_Expr.Special_Token.all = "tail") then
          Result := Eval_SimpleIntExpr (Current_Chart,This_Expr.Head_Tail_Data,
                           Current_RVars,Current_RQueue,Current_EVars);
          if Result in ObjectBase+1 .. StructBase then
            declare
              myvect: Int_Table := Vectors_DB.Retrieve(StructBase - Result);
              mynextvect: Int_Table(1..myvect'length-1) := myvect(2..myvect'length);
            begin
              Result :=  StructBase - Vectors_DB.NickNum(mynextvect);
              return Result;
            end;
          else
            return 0;
          end if;
        --
        else  -- unknown Special_Token
            --Put_Line(Current_Error,"Unexpected token as expression");
            Put_Line("-- RUNTIME_ERROR: Unexpected token as expression");
            Runtime_Errors_Count := Runtime_Errors_Count +1;
            Runtime_Error_Num := InvalidSpecialToken;
            Runtime_Error_Val :=0;
            raise UML_Error;
        end if;
     --
     -- evaluation of a vector
     elsif This_Expr.Is_Vector /= null then
       declare
          tmpv: Int_Table(This_Expr.Is_Vector.all'Range);
       begin
         for I in tmpv'Range loop
           tmpv(I) := Eval_SimpleIntExpr(Current_Chart, This_Expr.Is_Vector(I),
                                          Current_RVars, Current_RQueue, Current_EVars); 
         end loop;
         return StructBase - Vectors_DB.NickNum(tmpv);
       end;
    else
      --  numeric literals or static (global) object names
      return This_Expr.Literal_Value;
    end if;
  end Eval_SimpleIntExpr;

  function Eval_BoolBoolExpr (
     Current_Chart: Natural;
     This_Expr: BoolBoolExpr;
     Current_RVars: Int_Table;
     Current_RQueue: Int64_Table;
     Current_EVars: Int_Table) return Boolean;
 
  function Eval_IntExpr (
           Current_Chart: Natural;
           This_Expr:IntExpr;
           Current_RVars: Int_Table;
           Current_RQueue: Int64_Table;
           Current_EVars: Int_Table)    return Integer is
    This_Left: Integer;
    This_Right: Integer;
    Result: Integer;
  begin
    if This_Expr.Op = No_Op and then This_Expr.Left = null then
      return Eval_SimpleIntExpr(Current_Chart, This_Expr.Simple,
                            Current_RVars,Current_RQueue,Current_EVars);
    elsif This_Expr.Op = No_Op and then This_Expr.Left /= null  then
      return Eval_IntExpr(Current_Chart, This_Expr.Left.all,
                            Current_RVars,Current_RQueue,Current_EVars);
    end if;
    --
    This_Left :=
      Eval_IntExpr(Current_Chart, This_Expr.Left.all,
                            Current_RVars,Current_RQueue,Current_EVars);
    This_Right :=
      Eval_IntExpr(Current_Chart, This_Expr.Right.all,
                            Current_RVars,Current_RQueue,Current_EVars);
    --
    case This_Expr.Op is
      when No_Op => null;
      when Enclosed => null;
      when Plus | Join => 
         if This_Left > StructBase+1 and then
             This_Right > StructBase+1 then
           Result := This_Left + This_Right;
           if Result > Configurations.Kernel.INTMAX then 
               INTMAX := Result; 
           end if;
           if Result < Configurations.Kernel.INTMIN then 
               INTMIN := Result; 
           end if;
           --
         elsif This_Left = 0 then
              return This_Right;
              -- joining or summing 0 (undefined)
         elsif This_Right = 0 then
              -- joining or summing 0 (undefined)
              return This_Left;
           --
         elsif This_Left in ObjectBase+1 .. StructBase and then
            This_Right in ObjectBase+1 .. StructBase then
            -- join of vectors
           declare
             v1: Int_Table := Vectors_DB.Retrieve(StructBase - This_Left);
             v2: Int_Table := Vectors_DB.Retrieve(StructBase - This_Right);
             v3: Int_Table(1..v1'Length+v2'Length) := v1 & v2;
           begin
             if V3'Length > Configurations.Kernel.MAXVECT then MAXVECT := v3'length; end if;
             return StructBase - Vectors_DB.NickNum(v3);
           end;
           --
         else 
           -- sum of objects / boolean 
           -- mixed sum of number with object or structure or boolean
           return 0;
         end if;
      when Minus => 
         if This_Left > StructBase+1 and then
             This_Right > StructBase+1 then
           -- actually integer operation!!
           Result := This_Left - This_Right;
           if Result > Configurations.Kernel.INTMAX then 
                 INTMAX := Result; 
           end if;
           if Result < Configurations.Kernel.INTMIN then 
              INTMIN := Result; 
           end if;
         else
           return 0;
         end if;
      when Times => 
         if This_Left > StructBase+1 and then
             This_Right > StructBase+1 then
           -- actually integer operation!!
           Result := This_Left * This_Right;
           if Result > Configurations.Kernel.INTMAX then INTMAX := Result; end if;
           if Result < Configurations.Kernel.INTMIN then INTMIN := Result; end if;
         else
           return 0;
         end if;
      when Div => 
         if This_Left > StructBase+1 and then
             This_Right > StructBase+1 and then
              This_Right /= 0 then
           Result := This_Left / This_Right;
         else
           return 0;
         end if;
      when Modulus => 
         if  This_Left > StructBase+1 and then
             This_Right > StructBase+1 and then
             This_Right /= 0 then
           Result := This_Left mod This_Right;
         else
           return 0;
         end if;
    end case;
    return Result;
  end Eval_IntExpr;


  ---------------------------------------------------------------------------
  -- Since simplexp can be references to global variables  their evaluation
  --  can no-longer be simply local to the object!!! we need acces to all the
  --  system configuration!!!
  ---------------------------------------------------------------------------
  function Eval_umlExpr (
     Current_Chart: Natural;
     This_Expr:umlExpr;
     Current_RVars: Int_Table;
     Current_RQueue: Int64_Table;
     Current_EVars: Int_Table)    return Integer is
  begin
    if This_Expr.umlInt /= null then
      return Eval_IntExpr
        (Current_Chart, This_Expr.umlInt.all, 
           Current_RVars,Current_RQueue,Current_EVars);
    else
      if Eval_BoolBoolExpr (Current_Chart, This_Expr.umlBool.all, 
           Current_RVars,Current_RQueue,Current_EVars) then
        return IntTrue;
      else
        return IntFalse;
      end if;
    end if;
  end Eval_umlExpr;

  function Eval_IntBoolExpr (
     Current_Chart: Natural;
     This_Expr: IntBoolExpr;
     Current_RVars: Int_Table;
     Current_RQueue: Int64_Table;
     Current_EVars: Int_Table)
  return Boolean is
     This_Left: Integer :=
       Eval_IntExpr(Current_Chart, This_Expr.Left.all,
                       Current_RVars,Current_RQueue,Current_EVars);
     This_Right: Integer :=
        Eval_IntExpr(Current_Chart, This_Expr.Right.all,
                       Current_RVars,Current_RQueue,Current_EVars);
  begin
    case This_Expr.Op is
       when LT =>
           return (This_Left < This_Right);
       when GT =>
           return (This_Left > This_Right);
       when EQ =>
           --  0  EQ  IntFalse  -> True   (0 converted into IntFalse)
           if This_Left =0 and This_Right=IntFalse then return True; end if;
           if This_Right =0 and This_Left=IntFalse then return True; end if;
           if This_Left =0 and This_Right=IntTrue then return False; end if;
           if This_Right =0 and This_Left=IntTrue then return False; end if;
           return (This_Left = This_Right);
       when NE =>
           --  0  NE  IntTrue  -> False   (0 converted into IntFalse)
           if This_Left =0 and This_Right=IntFalse then return False; end if;
           if This_Right =0 and This_Left=IntFalse then return False; end if;
           if This_Left =0 and This_Right=IntTrue then return True; end if;
           if This_Right =0 and This_Left=IntTrue then return True; end if;
           return (This_Left /= This_Right);
       when LE =>
           return (This_Left <= This_Right);
       when GE =>
           return (This_Left >= This_Right);
    end case;
  end Eval_IntBoolExpr;

  function Eval_BoolBoolExpr (
     Current_Chart: Natural;
     This_Expr: BoolBoolExpr;
     Current_RVars: Int_Table;
     Current_RQueue: Int64_Table;
     Current_EVars: Int_Table) return Boolean is
   Tmp: Boolean;
  begin
    case This_Expr.Kind is
       when NoOp =>
           return Eval_IntBoolExpr (Current_Chart, This_Expr.Compare,
                                   Current_RVars,Current_RQueue,Current_EVars);
       when NotOp =>
          return  not Eval_BoolBoolExpr (Current_Chart, This_Expr.Left.all,
                                  Current_RVars,CUrrent_RQueue,Current_EVars);
       when AndOp =>
         Tmp := 
           Eval_BoolBoolExpr (Current_Chart, This_Expr.Left.all,
                                  Current_RVars,CUrrent_RQueue,Current_EVars);
         if Tmp = False then return False; end if;
         return
            Eval_BoolBoolExpr (Current_Chart, This_Expr.Right.all,
                               Current_RVars,Current_RQueue,Current_EVars);
       when OrOp =>
          Tmp := Eval_BoolBoolExpr (Current_Chart, This_Expr.Left.all,
                                 Current_RVars,Current_RQueue,Current_EVars);
          if Tmp then return True; end if;
          return 
              Eval_BoolBoolExpr (Current_Chart, This_Expr.Right.all,
                                 Current_RVars,Current_RQueue,Current_EVars);
    end case;
  end Eval_BoolBoolExpr;

  function Satisfies_Guard (
    Current_Chart: Natural;
    This_Guard: Guard_Ref;
    Current_RVars: Int_Table;
    Current_RQueue: Int64_Table;
    Current_EVars: Int_Table)
  return Boolean is
  begin
    if This_Guard = null then
      return True;
    else
      return Eval_BoolBoolExpr(Current_Chart, This_Guard.all,
                             Current_RVars, Current_RQueue, Current_EVars);
    end if;
  end Satisfies_Guard;


  -----------------------------------------------------------
  --  Actions_Table e' una tabella degli indici che identifica il posizionamento
  --  "The_Action" nella transizione attualmente in esecuzione
  -- Suspended_Actions e' il Vectors_DB.Nicknum della Actions_Table qualora la
  --  "The_Action" sia una operation-call o una function-assignment.
  -- Se la "The_Action" e' una azione composta (e.g. loop)   la "Suspended_Actions"
  --  identifica il posizionamento della basic action annidata causa della sospensione.
  -- "Suspended_Call" e' l'indice (in All_Events) dell'evento (call-operation)
  --   che ha causato la sospensione.
  -- "Evars" e' l'ambiente "locale" della transizione, costituito inizialmente
  -- dai paramteri del trigger che ha attivato la transizione.
  -- New_vars e' il vettore dello stato degli attributi dell'oggetto.
  -----------------------------------------------------------
  procedure Apply_Action(Actions_Table: Int_Table;
                         Current_Chart: in Natural;
                         New_Vars: in out Int_Table;
                         Current_States: in States_Table;
                         Current_Queue : in Int64_Table;
                         Tenv: in Natural;
                         All_Signals: in out Int64_Table;
                         All_Signals_Count: in out Natural;
                         The_Action: Action_Ref;
                         Suspended_Actions: out Integer;
                         Suspended_Call: out Natural;
                         Next_TEnv: out Natural;
                         Exited: in out Boolean);

  -------------------------------------------------------------------
  --  called by Compute_Object_Evolutions  when  building the oject
  --  evolution corresponding to a sequence of transitions.
  --  (in the case of completion transitions and triggered transitions)
  -------------------------------------------------------------------
  function Apply_Transitions_Sequence (
     Active_Object: Positive;    -- the current active object  (self)
     Current_Vars: Int_Table;    -- vars data;
     Current_Queue: Int64_Table; -- queue data
     Current_States: States_Table;  -- states data
     Trigger: Int_Table;            -- trigger data
     Trigger_Pos: Natural;   -- 0 = completion event
     This_Sequence_Nick: Int64) -- nick in Transitions_DB
          return Object_Runtime_Evolution;

  function Apply_Stuttering(
     Active_Object: Positive;    -- the current active object  (self)
     Current_Vars: Int_Table;    -- vars data;
     Current_Queue: Int64_Table; -- queue data
     Current_States: States_Table;  -- states data
     Trigger: Int_Table;            -- trigger data
     Trigger_Pos: Natural           --  it is always > 0
     ) return Object_Runtime_Evolution;

  function Get_Fireable_Sets (
          Current_Conf: Object_Runtime_Configuration;
          Active_Chart: Natural;
          Current_Signal: Int64 --  nick in Vectors_DB or 0
            ) return Transitions_Tables_Vector;

  function Compute_Evolutions (Current_Chart: Natural;
                               This_Trigger: Event_Ref;
                               Current_Conf: Object_Runtime_Configuration;
                               Current_EVars: Int_Table;
                               Current_State: State_Ref;
                                Outer_Transitions: Transitions_Table :=
                                     (1..0 => null) )
            return Transitions_Tables_Vector;

  function Select_Locally_Fireable  (
          Current_Chart: Natural;
          This_Trigger: Event_Ref;
          Current_Conf: Object_Runtime_Configuration;
          Current_EVars: Int_Table;
          Current_State: State_Ref;
          Outer_Transitions: Transitions_Table)
              return Transitions_Table; 

 function Merge_Transitions (
    Internal_Evolutions:Transitions_Tables_Vector;
    Local_Transitions: Transitions_Table)  
  return Transitions_Tables_Vector;
            
 function Compose_Evolutions (
    One: Transitions_Tables_Vector;
    Two: Transitions_Tables_Vector) 
 return Transitions_Tables_Vector;
              
  function Join_Transitions (Current_Chart: Natural;
                             This_Trigger: Event_Ref;
                             Current_Conf: Object_Runtime_Configuration;
                             Current_EVars: Int_Table;
                             Joined: Transitions_Tables_Vector;
                             Regions: States_Table;
                             Outer_Transitions: Transitions_Table)
                                     return Transitions_Tables_Vector;

  ------   descrizione -------  
  -- Get_Fireable_Sets  ricerca gli  insiemi di fireable transitions
  -- invocando Compute_Evolutions sul TOP state del modello.
  --
  -- Dato uno stato (top, composto o simple) Compute_Evolutions, in 
  -- generale, restituisce la lista delle possibili evoluzioni interne dello 
  -- stato (una evoluzione interna = una lista di fireable transitions).
  -- Per fare cio' ha bisogno anche di sapere quali sono le altre possibili
  -- fireable transitions che richiedono l'uscita dagli stati di livello
  -- piu' altro al fine di valutare la compatibilita' delle transizioni 
  -- interne dal punto di vista della priorita'.
  -- Se lo stato e' "Simple" la lista delle internal evolutions e' vuota.
  -- Se lo stato e' un "Composite Sequential" state, la lista delle internal
  -- evolutions e' data dalla lista delle internal evolutions del sottostato
  -- attualmente attivo (Compute_Evolutions) composte con la lista delle 
  -- local transitions (Select_Locally_Fireable). Tale composizione deve 
  -- ovviamente tenere conto di tutte le priorita'.
  -- Se lo stato e' un "Parallel" state, la lista delle internal evolutions
  -- degli active substates di tutte le regioni (Compute_Evolutions) sono 
  -- composte in parallelo (Join_Transitions) in modo incrementale 
  -- (Compose_Evolutions) tenendo ovviamente conto di tutte le priorita'.
  --
  -- Mentre i vincoli di priorita' sono verificati in due tempi (in parte
  -- top down da Merge_Transitions e in parte botton up da 
  -- Select_Locally_Fireable), i vincoli di conflittualita' sono verificati
  -- per il tipo di induzione strutturale effettuata (durante l'operazione 
  -- Merge_Transitions le local transitions sono sempre incompatibili fra di 
  -- loro e incompatibili con qualsiasi altra internal transition dei 
  -- susbstates) (i.e. vengono aggiunte come singletons alla lista delle 
  -- interal evolutions).
  --
  --  Una volto attenuto l'insieme dei gruppi di transizioni attivabili
  --   abbiamo tre possibilita':
  --   1) insieme e' vuoto:  nells evoluzione elimina il trigger dalla coda
  --   2) insieme a senza trigger: si applica senza tagliare la coda
  --   3) insiema non vuoto con trigger: si applica e si taglia la coda 
  --  Notab che in questa implementazione dei completion events viene
  --  evitata la generazione esplicita nella coda di completion events.
 
--
--  function Apply_Evolution (
--    This_Evolution: Transitions_Table_Ref;
--    From_RConf: Runtime_Configuration)
--  return Runtime_Configuration;
--

  ------- descrizione ------------
  -- Data la configurazione attuale, ed una lista di transizioni da eseguire
  -- in sequenza, applica le necessarie trasformazioni ai valori delle variabili,
  -- ed allo stato (una trasformazione per ogni transizione).
  -- Alla fine delle trasformazioni, il "triggering event" viene rimosso dalla
  -- coda, e la sequenza di signals locali generati aggiunti alla medesima.
  -- 
  -- Eventuali signals "esterni" dovrebbero essere "ricarcolati in un seconda
  -- ri-esecuzione????
           

  ------- descrizione ------------
  -- Get_Fireable_Sets restituisce gli insiemi di fireable transitions.
  -- Dato un insieme di fireable transitions, tuttavia, in genere da questo
  -- si possono derivare diverse possibili "evoluzioni" del sistema, 
  -- definite dal particolare ordine in cui le varie azioni indicate nelle
  -- transizioni sono eseguite. In particolare l'ordine in cui sono effettuati
  -- gli assegnamenti a variabili globali influenzano lo configurazione
  -- target, e l'ordine in cui sono inviati signals possono influenzare sia 
  -- la configurazione target che il behaviour visibile del sistema.
  -- 
  -- Il modo in cui, dato un insieme fireable, da questo si derivano
  -- le varie possibili evoluzioni si suppone che venga risolto ad un livello
  -- di astrazione piu' elevato (e.g. non in uqesto modulo).
  -- Qui si suppone soltanto che, una volta fissata una specifica evoluzione,
  -- essa sia ben rappresentabile da una sequenza di transizioni.
  -- NOTA: cio' vuol dire, ad esempio, che supponiamo che le azioni di una 
  -- transizione semplice, qualora siano azioni composte (cio' una sequenza 
  -- di azioni semplici) siano eseguite come un unica AZIONE ATOMICA.
  --
  -- La funzione Apply_Evolution, data la configurazione attuale, e data una
  -- specifica evoluzione, restituisce la configurazione target raggiunta.
                                 
          
 ---------------  other utilities -----------------             

 type PriorityResult is (Higher, Lower, Uncomparable);
 
 function Compare (t1, t2: Transition) 
 return PriorityResult;
 
 function IsNested (s1,s2: State) 
 return boolean;
  
 function Currently_Active_Substate (
   This_State: State_Ref;
    Current_RState: States_Table ) 
 return State_Ref;
              
  function Is_Currently_Active (The_State: State_Ref;
                                 Current_States: States_table)
                                  return Boolean;
                                  
 function Satisfies_Trigger (
    This_Trigger: Event_Ref; 
    This_Event: Event_Ref) 
  return Boolean;

          
  function Is_Completed (
   The_State: State_Ref;
   Current_RState: States_table)
 return Boolean;

  ---------------  Print and dump  utilities ----------------- 
  
 procedure Print_Evolution (
    This_Evolution: Transitions_Table;
    Count: Positive);
    
--------------------------------------------------------------------

  ----------------------------------------------------------------------
  -- Given a currently active state, the function returns a table
  -- of possibile local firaeable transitions from this state.
  -- Here "local"  means "internal to current state" but "outgoing"
  -- from the currently active SubState. 
  --   (I.e. the Currently active SubState is exited).
  -- "Fireable"  means "in agree with the current Event (if any), and
  --  with the active participation of all the source partners.
  ----------------------------------------------------------------------
  ----------------------------------------------------------------------
  -- given  Outer_transitions = []
  -- given  Current-Trigger = e
  -- given Current_State.LocalTransitions = 
  --  [ [...] , [ e, [t1, t2, t3, t4] ],  [ ...]
  -- given   Guard(t1), Guard(t2), Guard(3) = true, Guard(t4)=False
  -- given  active(t1), active(t2), active(t3)
  -- given      t1 higher t2   ,  t4 higher t3
  -- returns  [ t1, t3 ]
  --
  --  if Outer_transitions = [t6, t7] , where  t6 higher t3
  --  returns  [t1]
  --      
  -- Current_Trigger can be:
  --     Null_Event,  
  --     Chartevents(J)  ,  J >= 2
  --     null Event_Ref pointer -   matching any event
  ----------------------------------------------------------------------
  function Select_Locally_Fireable  (
          Current_Chart: Natural;
          This_Trigger: Event_Ref;
          Current_Conf: Object_Runtime_Configuration;
          Current_EVars: Int_Table;
          Current_State: State_Ref;
          Outer_Transitions: Transitions_Table)
              return Transitions_Table is
    --
    This_Substate: State_Ref :=
       Currently_Active_Substate (Current_State, Current_Conf.Current_States);
    Max_Fireable: Natural;
    --
  begin
    --
    -- navigates the catalog
    --
    Max_Fireable := 0;
    for I in This_Substate.OutgoingTransitions.all'Range loop
      Max_Fireable :=
         Max_Fireable + 
          This_Substate.OutgoingTransitions(I).These_Transitions.all'Length;
    end loop;

    declare
      All_Fireables: Transitions_Table(1..Max_Fireable);
      Fireables_Count: Natural := 0;
      Final_Fireables: Transitions_Table(1..Max_Fireable);
      --
      This: Transition_Ref;
      PRR: PriorityResult;
      --
      Is_Active: Boolean;
      Guard_OK: Boolean;
      To_Be_Added : Boolean;
      Completion_OK: Boolean;
    begin
      --
      -- for each set of transitions TRIGGERED BY THE SAME EVENT
      --
      for I in This_Substate.OutgoingTransitions.all'Range loop
        --
        if Satisfies_Trigger (This_trigger,
                This_Substate.OutgoingTransitions(I).This_Event) then
         -- 
         -- for each TRANSITION triggered by this trigger
         --
         for J in 
          This_Substate.OutgoingTransitions(I).These_Transitions.all'Range 
                   loop
            --
            This := This_Substate.OutgoingTransitions(I).These_Transitions(J);
            --
            -- check if the transition is really active
            -- (might have source in a non active-sub-substate)
            --
            Is_Active := True;
            for K in 1..This.Source.all'Length loop
              if not Is_Currently_Active(This.Source(K),
                                          Current_Conf.Current_States) then
                Is_Active := False;
                exit;
              end if;
            end loop;
            --
            Guard_OK := Satisfies_Guard (Current_Chart, This.Guard,
                       Current_Conf.Current_Vars,
                       Get_Queue_Items(Current_Conf.Current_Queue),
                       Current_EVars);
            --
            Completion_OK := True;
            if This_trigger = null or This_trigger = Null_Event then
            for K in 1..This.Source.all'Length loop
              if not Is_Completed(This.Source(K),Current_Conf.Current_States) then
                Completion_OK  := False;
                exit;
              end if;
            end loop;
            end if;
            --
            if Completion_OK and Guard_OK and Is_Active then
              --
              -- compare the priority of this transition with
              -- the priority of outer transitions and
              -- the priority of all the other locally fireables:
              -- if this priority is lower then any in the outer or local
              --   table then discard this transition
              -- if this priority is higher then some in the local table
              --   then remove from the table the lowers and add this.
              -- if this priority is always uncomparable then
              --   add this transition to the all_fireables table.
              --
              To_Be_Added := True;
              for L in Outer_Transitions'Range loop
                PRR := Compare(This.all,Outer_Transitions(L).all);
                if PRR = Lower then
                    To_Be_Added:= False;
                    exit;
                end if;
              end loop;
              --
              for L in 1..Fireables_Count loop
                if All_Fireables(L) /= null then
                  PRR := Compare(This.all,All_Fireables(L).all);
                  if PRR = Lower then
                    To_Be_Added:= False;
                    exit;
                  elsif PRR = Higher then
                    All_Fireables(L) := null;
                  end if;
                end if;
              end loop;   -- previous All_Fireables
              --
              if To_Be_Added then
                Fireables_Count := Fireables_Count +1; 
                All_Fireables(Fireables_Count) := This;
              end if;
              --
            end if;
            --
          end loop;    -- This transition in groups range
        end if;
        --
      end loop;    -- group in outgoing catalogue
      --
      -- Make final clean-up removing the possible holes in the table;
      --
      Fireables_Count := 0;
      for I in All_Fireables'Range loop
        if All_Fireables(I) /= null then
           Fireables_Count :=  Fireables_Count +1;
           Final_Fireables(Fireables_Count) := All_Fireables(I);
        end if;
      end loop;
      return Final_Fireables(1..Fireables_Count);
    end;
    --
  end Select_Locally_Fireable;


  ----------------------------------------------------------------------
  --  given:   [ [t1,t2,t6], [t3, t7]  ]   and  [ t4, t5 ]
  --  where:       t4 lower t1  
  --                      
  --  returns: [ [t1,t2,t6], [t3, t7], [t5] ]
  --
  -- i.e.  singletons with non-lower priority are added to the groups.
  ----------------------------------------------------------------------
  -- Notice that t1 , t2 and t6 are "Uncomparable" 
  --   because belonging to different orthogonal regions
  --
  -- If ti and tj are two transitions belonging to two different evolutions
  -- (sets of transitions) then either ti and tj belong to different 
  --  othogonal regions, or they have exactly the same source 
  -- (i.e. there still no priority ordering beween them). 
  --
  -- Internal_Evolutions (as returned by Compute_Evolutions) can already be
  -- trusted to have a priority not lower than any intrnal, local or outer 
  -- transition. 
  -- (Compute_Evolutions discards internal transitions with lower priority).
  --
  -- Local_Transitions (as returned by Select_Locally_Fireable) can be trusted
  -- to have a priority not lower than any other local or outer transition. 
  -- (Select_Locally_Fireable discards local transitions with lower priority).
  --
  -- The merging must still take into account the PRIORITY
  -- in the sense that from the Local_Transitions we must
  -- exclude all those transitions whose priority is lower than a
  -- transition in any of the internal evolutions.
  --
  ----------------------------------------------------------------
  function Merge_Transitions (Internal_Evolutions:Transitions_Tables_Vector;
                             Local_Transitions: Transitions_Table)  
            return Transitions_Tables_Vector is
    --
    Max_Evolutions: Natural := Local_Transitions'Length +
                                Internal_Evolutions'Length;
    All_Evolutions: Transitions_Tables_Vector(1..Max_Evolutions);
    Evolutions_Count: Natural;
    To_Be_Added: Boolean;
    PRR: PriorityResult;
    --
  begin
    Evolutions_Count := Internal_Evolutions'Length;
    All_Evolutions(1..Evolutions_Count) := Internal_Evolutions;
    for I in Local_Transitions'Range loop
       To_Be_Added := True;
       for J in Internal_Evolutions'Range loop
         for K in Internal_Evolutions(J).all'Range loop
            PRR := Compare (Local_Transitions(I).all,All_Evolutions(J)(K).all);
            if PRR = Lower then
               To_Be_Added := False;
               exit;
            end if;
         end loop;  -- for all transitions in each internal evolutions.
         if not To_Be_Added then
           exit;
         end if;
       end loop; -- for all internal evolutions
       --
       if To_Be_Added then
         Evolutions_Count := Evolutions_Count +1;
         All_Evolutions(Evolutions_Count) :=
                 new Transitions_Table'(1..1 => Local_Transitions(I));
         -- This "new" allocation will be freed either by an internal
         --  "Compose_Evolution" or by the caller of "Compute_Evolutions".
       end if;
    end loop;   -- for all all local transitions
    --
    return All_Evolutions(1..Evolutions_Count);
    --
  end Merge_Transitions;


  ----------------------------------------------------------------------
  --   given  [ [t1,t2], [t3] ]  and  [ [t4] , [t5,t6] ]    
  --                       returns
  --   [ [t1,t2,t4], [t1,t2,t5,t6], [t3,t4], [t3,t5,t6] ]
  --
  -- Notice the priorites do not need to be taken into account because
  -- the transions to be joined are orthogonal
  ----------------------------------------------------------------------
  function Compose_Evolutions (One: Transitions_Tables_Vector;
                               Two: Transitions_Tables_Vector) 
              return Transitions_Tables_Vector is 
     
    Result: Transitions_Tables_Vector(1.. One'Length * Two'Length);
    FREE_One: Transitions_Tables_Vector := One;
    FREE_Two: Transitions_Tables_Vector := Two;
  begin
    -- One'Length  is supposed to be > 0 
    -- One'First = Two'First = 1;
    --
    if  One'Length = 0 then 
       return Two;
    end if;
    --
    if  Two'Length = 0 then 
       return One;
    end if;
    --
    for I in One'Range loop 
      for J in Two'Range loop
        Result((I-1)* (Two'Length) + J) := 
            new Transitions_Table'(One(I).all & Two(J).all);
      end loop;
    end loop;
    --
    for I in FREE_One'Range loop
        Free (FREE_One(I));
    end loop;
    for I in FREE_Two'Range loop
        Free (FREE_Two(I));
    end loop;
    --
    return Result;
    --
  end Compose_Evolutions;

  ----------------------------------------------------------------------
  ----------------------------------------------------------------------
  function Join_Transitions (Current_Chart: Natural;
                             This_Trigger: Event_Ref;
                             Current_Conf: Object_Runtime_Configuration;
                             Current_EVars: Int_Table; 
                             Joined: Transitions_Tables_Vector;
                             Regions: States_Table;
                             Outer_Transitions: Transitions_Table)
                                     return Transitions_Tables_Vector is
  begin
    if Joined'Length = 0 and Regions'Length = 0 then
      return (1..0 => null);
      --
    elsif Joined'Length = 0 and  Regions'Length > 0 then
      --
      declare
        --
        This_Region: State_Ref := Regions(Regions'First);
        --
        Internal_Evolutions: Transitions_Tables_Vector :=
           Compute_Evolutions(
              Current_Chart,This_Trigger, Current_Conf, 
              Current_EVars,This_Region, Outer_Transitions);
        --
      begin
        --
        return Join_Transitions (Current_Chart,This_Trigger,  Current_Conf,
                                Current_EVars,Internal_Evolutions,
                                Regions(Regions'First+1.. Regions'Last),
                                Outer_Transitions);
      end;
      --
    elsif Joined'Length  > 0 and Regions'Length = 0 then
      return Joined;
      --
    else   -- Joined'Length  > 0 and Regions'Length > 0 then
      declare
        This_Region: State_Ref := Regions(Regions'First);
        Internal_Evolutions: Transitions_Tables_Vector :=
          Compute_Evolutions(Current_Chart, This_Trigger, Current_Conf,
                           Current_EVars,This_Region, Outer_Transitions);
        Composed_Evolutions: Transitions_Tables_Vector  :=
          Compose_Evolutions(Joined,Internal_Evolutions);
      begin
        return Join_Transitions (
           Current_Chart,This_Trigger, Current_Conf,
           Current_EVars, Composed_Evolutions,
           Regions(Regions'First+1.. Regions'Last),
           Outer_Transitions);
      end;
      --
    end if;
  end Join_Transitions;


  ----------------------------------------------------------------------
  -- Given the a currently active State, the
  -- function returns the table of possible state evolutions 
  -- considering all the rules whith do not require the exiting from
  -- the state, and considering also the priority of all the other 
  -- "outer" applicable rules which do require ad exiting from the state.
  --
  -- In other words, this function returns all the fireable INTERNAL 
  -- evolutions with a priority non in contrast with that of all the 
  --  fireable non-internal transitions.
  --
  -- Each possible evolution is represented, in general by a 
  -- table of transitions.
  -- These evolution will be later merger (function Merge) with the 
  --  transitions which do exit from this state.
  --
  --  Se il trigger e "null", allora le transizioni da prendere in 
  --  considerazione sono quelle senza trigger esplicito.
  ----------------------------------------------------------------------
  --  NOTICE The access elements returned MUST be freed by the caller
  ----------------------------------------------------------------------
  -- 
  function Compute_Evolutions (Current_Chart: Natural;
                               This_Trigger: Event_Ref;
                               Current_Conf: Object_Runtime_Configuration;
                               Current_EVars: Int_Table;
                               Current_State: State_Ref;
                                Outer_Transitions: Transitions_Table :=
                                     (1..0 => null) ) 
            return Transitions_Tables_Vector is
  begin
    case Current_State.Kind is
    when Simple =>
      -- return an empty vector of evolutions
      return (1..0 => null);
      --
    when Composite  =>
      declare
        This_Substate: State_Ref :=
          Currently_Active_Substate (Current_State,Current_Conf.Current_States);
        --
        -- consider trigger&guards, and joins
        -- Notice: Enabled_Local_Transitions are all not GROUPS but SINGLES
        --
        Local_Transitions: Transitions_Table :=
            Select_Locally_Fireable (Current_Chart, This_Trigger, Current_Conf,
                     Current_EVars, Current_State, Outer_Transitions);   
       --
        Local_And_Outer_Transitions: Transitions_Table :=
                                  Outer_Transitions & Local_Transitions;
        --
        Internal_Evolutions: Transitions_Tables_Vector  :=
           Compute_Evolutions (Current_Chart, This_Trigger,  Current_Conf,
                               Current_EVars,This_Substate, 
                               Local_And_Outer_Transitions);
        --
      begin
        return Merge_Transitions (Internal_Evolutions, 
                                    Local_Transitions);
        --
        -- Internal_Evolutions are known to have a priority
        -- not lower than any local or outer transition.
        --
        -- Local_Transitions are known to have a priority not lower
        --   than any local or outer transition.
        --
        -- the merging must still take into account the PRIORITY
        -- in the sense that from the Local_Transitions we must  
        -- exclude all those transitions whose priority is lower than a 
        -- transition in any of the internal evolutions.
        --
      end;
      --
    when Parallel =>
      --  
      -- A Parallel state has NO local transitions, but just a set
      -- of regions, each one with an active substate.
      --
      --  Maybe a Parallel might have local transitions if we consider
      --   the case of synchronous transitions between parallel regions.
      --
      --  The substates of a parallel State are its regions.
      --
      return Join_Transitions (Current_Chart, This_Trigger,  Current_Conf,
                               Current_EVars,(1..0 => null),
                               Current_State.Substates.all,
                               Outer_Transitions);
      --
      --  
      --   currenlty there are no local transitions ...
      -- Enabled_Local_Transitions
      --    := Select_Fireable (Current_State);
      --  return Merge_Transitions (Enabled_Internal_Evolutions,
      --                         Enabled_Local_Transitions);
      --
    end case;
  end Compute_Evolutions;
  

  ------------------------------------------------------------------------
  -- All states have two components:  localtransitions and outgoingtransitions.
  -- These components are initialized by the UML parser.
  -- state.localtranitions contains a table of transitions. 
  --
  -- The procedure Parse_Transitions adds the current transition to the table of
  -- localtransitions of the state which OWNS the transition (i.e. the least
  -- common ancestor of all sources and targets)
  --
  -- The outgoingtransitions catalogue is built by the Load_Outgoing_Catalogues
  -- procedure and by Parse_Transition.
  -- Parse_Transition adds the current transition to the outgoingtransitions table
  -- of the direct substate of the owner state which includes the FIRST source state of the
  -- the transition.  
  -- I.e.  ogni transition in state.localtransitions e' ANCHE presente in un
  --  state.substate(i).outgoingtransitions per il substate in cui e' inclusa la source(1)
  -- Dopo aver analizzato tutte le transizioni di una classe viene chiamata
  --   Load_Outgoing_Catalogues  che RIFA LA MEDESIMA COSA!!!
  ------------------------------------------------------------------------
  
  ------------------------------------------------------------------------
  -- return the list of fireable-sets of transitions  trigger by a given
  --  signal or completion event.
  ------------------------------------------------------------------------
  function Get_Fireable_Sets (
          Current_Conf: Object_Runtime_Configuration;
          Active_Chart: Natural;
          Current_Signal: Int64 --  nick in Vectors_DB or 0
            ) return Transitions_Tables_Vector  is 
--Z          )   return Int64_Table is   -- nick in Transitions_DB
--     Found: Boolean;
--     Current_Trigger: Event_Ref;
--     Event_Index: Positive;
--     Signal_Target: Natural;
--     EVars_Count: Natural;
     Current_Chart:Positive := Active_Charts(Active_Chart);
  begin
    if Current_Signal = 0 then
      --
      -- Even without looking at the system queue, first check
      --  whether there is some executable COMPLETION TRANSITION
      --   (i.e. transitions without explcit trigger)
      --
      declare
         CurrentState: State_Ref := All_Charts(Current_Chart).ChartStates(1);
         Completions: Transitions_Tables_Vector  :=
              Compute_Evolutions ( Current_Chart, null, Current_Conf,
                  (1..0 => 0), CurrentState);
--Z         Nicks : Int64_Table(Completions'Range);
      begin
           -- if some completion is possible return them
           --  (they will be applied without removing the top of the queue)
--Z           for I in Completions'Range loop
--Z              Nicks(I) := abs(Transitions_DB.NickNum(Completions(I).all));
--Z           end loop;
--Z           return Nicks;
         return Completions;
      end;
    end if;
    --
    --
    --
    -- if  one trigger is available  set the Event_Env (for Event_Vars)
    --   and look for possible evlutions
    --
    declare
      CurrentState: State_Ref := All_Charts(Current_Chart).ChartStates(1);
      Trigger_Signal: Int_Table := Vectors_DB.Retrieve(Current_Signal);
      Signal_Target: Natural  := Trigger_Signal(2);
      Current_Trigger: Event_Ref  := All_Events (Trigger_Signal(1));
      EVars_Count: Natural  := Current_Trigger.Params.all'Length;
      Current_EVars: Int_Table (1..EVars_Count):= 
         Trigger_Signal(3.. 2 + EVars_Count);
      Completions: Transitions_Tables_Vector  :=
          Compute_Evolutions (Current_Chart, Current_Trigger, Current_Conf,
             Current_EVars, CurrentState );
--Z      Nicks : Int64_Table(Completions'Range);
    begin
--Z      for I in Completions'Range loop
--Z        Nicks(I) := abs(Transitions_DB.NickNum(Completions(I).all));
 --Z     end loop;
--Z      return Nicks;
       return Completions;
    end;
  end Get_Fireable_Sets;
  

 -- 
 --  Old_Struct_Value = 1  Indexes=[1]  Orig=dd  =>  [0,dd]
 --  Old_Struct_Value = [aa,bb,cc]  Indexes=[1]  Orig=dd  =>  [aa,dd,cc]
 --  Old_Struct_Value = [aa]  Indexes=[1]  Orig=dd  =>  [aa,dd]
 --
 --   NEW TOBEDONE Dic17
 --  Old_Struct_Value = [0,0,1]  Indexes=[2] Orig=0   =>   []
 --
 function Make_New_Value (Old_Struct_Value: Integer; 
                          The_Indexes: Int_Table;
                          Orig_Value: Integer) return Integer is
   The_Index: Integer;
   Tmp_Value: Integer; 
   Default_Value: Integer :=0;
 begin
   -- e.g.   x:int[] := [aa];
   --
   if The_Indexes'Length = 0 then return Orig_Value; end if;
   --
   The_Index := The_Indexes(The_Indexes'First);
   --
   if Old_Struct_Value not in ObjectBase+1 ..StructBase then
     if The_Index >=  0 then
       --  index is 5, -- add [0,0,0,0,0]  -- or [null, null, null ...]  ?????
       if Old_Struct_Value <= IntFalse then 
          Default_Value := IntFalse;
       elsif Old_Struct_Value <= IntNullObject then
          Default_Value := IntNullObject;
       end if;
       --
       declare
         Bigvector: Int_table(1..The_index+1) := (others => Default_Value);
         reduced: Natural := Bigvector'Length;
       begin
         Tmp_Value := Make_New_Value(Default_Value,
                                  The_Indexes(The_Indexes'First+1..The_Indexes'Last),
                                  Orig_Value);
         Bigvector(The_Index+1) := Tmp_Value;
         if Tmp_Value =0 and The_Index = Bigvector'Last-1 then
           while reduced >0 and then Bigvector(reduced) =0 loop
             reduced := reduced -1;
           end loop;
         end if;
         return StructBase - Vectors_DB.NickNum(Bigvector(1..reduced));
       end;
     else
       -- not allowed negative indexes  ????
       --  why nor modelling symmetric vectors  vext(i)=vect(-i)?? TOBEDONE
       -- and maybe stiòll generating the Runtime_Error signal !!!
       -- WHAT ABOUT  INDEX<ObjectBase???  cannot build million-sized vector!!!
       --
       --Put_Line(Current_Error,"-- Index out of bounds");
       Put_Line("-- RUNTIME_ERROR: Index out of bounds");
       Runtime_Errors_Count := Runtime_Errors_Count +1;
       Runtime_Error_Num :=  InvalidIndex;
       Runtime_Error_Val :=  The_Index;
       raise UML_Error;
     end if;
     --
   else
     --
     declare
       Thevector: Int_Table := Vectors_DB.Retrieve(StructBase - Old_Struct_Value);
       reduced: Natural;
     begin
       for K in Thevector'Range loop
         if Thevector(K) <= IntFalse then
            Default_Value := IntFalse;
            exit;
         elsif Thevector(K) <= IntNullObject then
           Default_Value := IntNullObject;
           exit;
         end if;
       end loop;
       if The_Index+1 >=  1 and then
           The_Index+1 <=  Thevector'Last then
          -- index is in range
          Tmp_Value := Make_New_Value(Thevector(The_Index+1), 
                                      The_Indexes(The_Indexes'First+1..The_Indexes'Last),
                                      Orig_Value);
          Thevector(The_Index+1) := Tmp_Value;
          reduced := Thevector'Length;
         if Tmp_Value =0 and The_Index = Thevector'Length-1 then
           while reduced >0 and then Thevector(reduced) =0 loop
             reduced := reduced -1;
           end loop;
         end if;            
          return StructBase - Vectors_DB.NickNum(Thevector(1..reduced));
          --
       elsif The_Index+1 >=  1 then
           --  vector is 1..10  index is 15, -- add [0,0,0,0,0]
           declare
             extension: Integer := The_Index+1 - Thevector'Last;
             Bigvector: Int_table := Thevector & (1..extension => Default_Value);
           begin
             Tmp_Value := Make_New_Value(0,
                                      The_Indexes(The_Indexes'First+1..The_Indexes'Last),
                                      Orig_Value);
             Bigvector(The_Index+1) := Tmp_Value;
             if Tmp_Value = 0 then 
               return Old_Struct_Value;
             else
               return StructBase - Vectors_DB.NickNum(Bigvector);
             end if;
           end;
       else
          --Put_Line(Current_Error,"-- Index out of bounds");
          Put_Line("-- RUNTIME_ERROR: Index out of bounds");
          Runtime_Errors_Count := Runtime_Errors_Count +1;
          Runtime_Error_Num :=  InvalidIndex;
          Runtime_Error_Val :=  The_Index;
          raise UML_Error;
       end if;
     end;
   end if;
 end Make_New_Value;


  ------------------------------------------------------------ 
  --  called when an action suspended on call operation is
  --   resumed because a return signal has arrived.
  -- In case of call_action ... nothing to do
  -- In case of assignement with function call  do the
  --  assignment and generate the assign event
  ------------------------------------------------------------ 
  procedure Resume_Action (Full_Suspended_Actions_Table: Int_Table;
                         Actions_Table: Int_Table;
                         Current_Chart: in Natural;
                         New_Vars: in out Int_Table;
                         Current_States: in States_Table;
                         Current_Queue : in Int64_Table;
                         TEnv: Natural;
                         Return_Vars: in Int_Table;
                         All_Signals: in out Int64_Table;
                         All_Signals_Count: in out Natural;
                         The_Action: Action_Ref;
                         Further_Suspended_Actions: out Integer;
                         Further_Suspended_Call: out Natural;
                         Next_TEnv: out Natural;
                         Exited: in out Boolean) is
    Tmp_Value: Integer;
    Tmp_Signal: Event_Instance;
    ChartVars: Vars_Table_Ref;
--    Simple: SimpleIntExpr_Ref;
    Local_Var: SystemVar_Ref;
    The_Index: Integer;
    Evars: Int_Table := Vectors_DB.Retrieve(TEnv);
    TopCall: Boolean;
  begin
    Further_Suspended_Actions :=0;
    Further_Suspended_Call :=0;
    Next_TEnv := TEnv;
    --
    --   BASIC TOP_LEVEL OPERATION CALL
    --
    TopCall := (The_Action.Kind = Call) or else
       (The_Action.Kind=Signal and then
              The_Action.Signalled_Event.the_event.kind = operation);
    --
    if  TopCall then
         --  nothing to do ..   Op-Calls are included in Signal actions (???)
      null;
      return;
      --
    --
    --   BASIC TOP_LEVEL FUNCTION CALL     
    --
    elsif The_Action.Kind = Function_Call then
      Tmp_Value := Return_Vars(1);
      --
      if The_Action.Assignment_Left_Indexes.all'Length = 0 then
        --
        if The_Action.Assignment_Left >0 then
          --This is an assignment to a Class Var
          --
          ChartVars := All_Charts(Current_Chart).ChartVars;
          Local_Var := ChartVars(The_Action.Assignment_Left);
          --
          -- update the New_Vars vector
          --
          New_Vars(Local_Var.Local_Num_Key) := Tmp_Value;
          -- generate the assign signal is the variable is observed or the mode is interactiove
          if Local_Var.Observed or else 
              NonInteractive = False or else
                Ground_Action_Labels_Needed = True then
            declare
              Assign_Signal: Int_Table(1..5);
            begin
              Assign_Signal(1) := Assign_Event.Num_Key;               -- assign event
              Assign_Signal(2) := Current_Chart;   -- target
              Assign_Signal(3) := Local_Var.Local_Num_Key; -- arg1  (the attribute)
--              Assign_Signal(4) := 0;                       -- (the index, if composite)
              Assign_Signal(4) := IntEmptyStruct;                       -- (the index, if composite)
              Assign_Signal(5) := Tmp_Value;               -- the value
              All_Signals_Count := All_Signals_Count+1;
              All_Signals(All_Signals_Count) :=
                  abs(Vectors_DB.NickNum(Assign_Signal));
             end;
          end if;
        else
          -- this an assignent to a Transition Var (negative index)
          declare
             Transition_Env: Int_Table := Vectors_DB.Retrieve(Tenv);
          begin
             Transition_Env(- The_Action.Assignment_Left) := Tmp_Value;
             Next_TEnv := Vectors_DB.NickNum(Transition_Env);
          end;  
        end if;
        --
      else   -- The_Action.Assignment_Left_Indexes.all'Length /= 0 
        declare
          The_Indexes: Int_Table(1..The_Action.Assignment_Left_Indexes.all'Length);
          Old_Struct_Value: Integer;
          New_Struct_Value: Integer;
          Assign_Signal: Int_Table(1..5);
        begin
          --
          for K in The_Indexes'Range loop
            The_Index := 
               Eval_umlExpr(Current_Chart, The_Action.Assignment_Left_Indexes(K).all,
                       New_Vars, Current_Queue, EVars);   
            if The_Index = IntFalse then The_Index :=0; end if;
            if The_Index = IntTrue then The_Index :=1; end if;
            if The_Index <=  ObjectBase then
               The_Index := ObjectBase - The_Index;
            end if;
            The_Indexes(K) := The_Index;
          end loop;
          --
          if The_Action.Assignment_Left >0 then
            --This is an assignment to a Class Var
            -- 
            ChartVars := All_Charts(Current_Chart).ChartVars;
            Local_Var := ChartVars(The_Action.Assignment_Left);
            Old_Struct_Value := New_Vars(Local_Var.Local_Num_Key);
            New_Struct_Value := Make_New_Value(Old_Struct_Value, The_Indexes,Tmp_Value);
            New_Vars(Local_Var.Local_Num_Key) := New_Struct_Value;
            --
--           declare
--            Thevector: Int_Table :=
--               Vectors_DB.Retrieve(StructBase - New_Vars(Local_Var.Local_Num_Key));
--          begin
--            if The_Index+1 >=  1 and then
--               The_Index+1 <=  Thevector'Last then
--              Thevector(The_Index+1) := Tmp_Value;
--              New_Vars(Local_Var.Local_Num_Key) := StructBase - Vectors_DB.NickNum(Thevector);
--            elsif The_Index+1 >=  1 then
--               --  vector is 1..10  index is 15, -- add [0,0,0,0,0]
--               declare
--                 extension: Integer := The_Index+1 - Thevector'Last;
--                 Bigvector: Int_table := Thevector & (1..extension => 0);
--               begin
--                 Bigvector(The_Index+1) := Tmp_Value;
--                 New_Vars(Local_Var.Local_Num_Key) := Vectors_DB.NickNum(Bigvector);
--               end;
--            else
--              --Put_Line(Current_Error,"-- Index out of bounds");
--              Put_Line("-- RUNTIME_ERROR: Index out of bounds");
--              Runtime_Errors_Count := Runtime_Errors_Count +1;
--              Runtime_Error_Num :=  InvalidIndex;
--              Runtime_Error_Val :=  The_Index;
----              raise UML_Error;
--            end if;
--          end;         
            --
          -- generate the assign signal is the variable is observed or the mode is interactiove
          if Local_Var.Observed or else 
              NonInteractive = False or else
                Ground_Action_Labels_Needed = True then
              Assign_Signal(1) := Assign_Event.Num_Key;  -- assign signal
              Assign_Signal(2) := Current_Chart;
              Assign_Signal(3) := Local_Var.Local_Num_Key;
  --            Assign_Signal(4) := The_Indexes(1)+1;
              Assign_Signal(4) := StructBase - abs(Vectors_DB.NickNum(The_Indexes));
              Assign_Signal(5) := Tmp_Value;
              All_Signals_Count := All_Signals_Count+1;
              All_Signals(All_Signals_Count) :=
                  abs(Vectors_DB.NickNum(Assign_Signal));
            end if;
            --
          else  -- NOT The_Action.Assignment_Left >0
            -- this an assignent to a Transition Var (negative index)
            --
            declare
               Transition_Env: Int_Table := Vectors_DB.Retrieve(Tenv);
            begin
              Old_Struct_Value := Transition_Env(-The_Action.Assignment_Left);
              New_Struct_Value := Make_New_Value (Old_Struct_Value, The_Indexes,Tmp_Value);
              Transition_Env(-The_Action.Assignment_Left)  := New_Struct_Value;
              Next_TEnv := Vectors_DB.NickNum(Transition_Env);
--
--
--            if The_Index+1 in Thevector'Range then
--              Thevector(The_Index+1) := Tmp_Value;
--              Transition_Env(-The_Action.Assignment_Left) :=
--                    StructBase - Vectors_DB.NickNum(Thevector);
--            elsif The_Index+1 >=  1 then
--               --  vector is 1..10  index is 15, -- add [0,0,0,0,0]
--               declare
--                 extension: Integer := The_Index+1 - Thevector'Last;
--                 Bigvector: Int_table := Thevector & (1..extension => 0);
--               begin
--                 Bigvector(The_Index+1) := Tmp_Value;
--                 Transition_Env(-The_Action.Assignment_Left) :=
--                      StructBase - Vectors_DB.NickNum(Bigvector);
--               end;
--            else
--              --Put_Line(Current_Error,"-- Index out of bounds");
--              Put_Line("-- RUNTIME_ERROR: Index out of bounds");
--              Runtime_Errors_Count := Runtime_Errors_Count +1;
--              Runtime_Error_Num := InvalidIndex;
--              Runtime_Error_Val := The_Index;
--              raise UML_Error;
--            end if;
--            Next_TEnv := Vectors_DB.NickNum(Transition_Env);
            end;
          --
          end if;
        end;
      end if;  --The_Action.Assignment_Left_Indexes.all'Length
      --
    -----
    ----   suspended   WHILE LOOP
    ----
    elsif  The_Action.Kind = Whileloop then
      --    
      --  identify suspended subaction
      declare
         Exited: Boolean := False;
         Condition: Boolean := True;
         Suspended_Actions : Integer :=0;
         Current_Actions_Table: Int_Table(1..Actions_Table'Length+1) := 
               Actions_Table & 0;
         Suspended: Natural := 
                 Full_Suspended_Actions_Table(Current_Actions_Table'Length);
      begin
        --
        --  resume it
           Resume_Action(Full_Suspended_Actions_Table, Current_Actions_Table,
                Current_Chart,New_Vars,Current_States, Current_Queue,
                TEnv, Return_Vars, All_Signals,All_Signals_Count,
                The_Action.LoopBody(Suspended), 
                Further_Suspended_Actions, Further_Suspended_Call,
                Next_TEnv, Exited);         
        if  further_Suspended_Actions > 0 then
           return;  --  WAS EXIT  ----  WAS WRONG????
        end if;
        --
        --  execute the other actions of the loop (with the current Nextenv)
        if not Exited then
        for J in Suspended+1 .. The_Action.LoopBody.all'Length loop
           Current_Actions_Table(Current_Actions_Table'Length) := J;
           Apply_Action(Current_Actions_Table, Current_Chart,
                  New_Vars,Current_States, Current_Queue,
                   Next_TEnv, All_Signals,All_Signals_Count,
                   The_Action.LoopBody(J),
                   Further_Suspended_Actions, Further_Suspended_Call,
                   Next_TEnv, Exited);
           if Exited then 
             exit;
           end if;
           if   Further_Suspended_Actions > 0 then
             return;  --  WAS EXIT  ----  WAS WRONG????
           end if;
        end loop;
        end if;
        --
        --  restore the env size
        --
        if not Exited then
          EVars(1..The_Action.Env_Depth) := 
                  Vectors_DB.Retrieve(Next_TEnv)(1..The_Action.Env_Depth);
          Condition :=
              Eval_BoolBoolExpr (Current_Chart, The_Action.LoopCond.all,
                New_Vars, Current_Queue, EVars(1..The_Action.Env_Depth));
          Next_TEnv := Vectors_DB.NickNum(EVars(1..The_Action.Env_Depth));
        end if;
        --
        -- maybe restart new loops
        --
        while not exited and Condition and Further_Suspended_Actions = 0 loop
          for J in 1 .. The_Action.LoopBody.all'Length loop
             Current_Actions_Table(Current_Actions_Table'Length) := J;
             Apply_Action(Current_Actions_Table, Current_Chart,
                    New_Vars,Current_States, Current_Queue,
                     Next_TEnv, All_Signals,All_Signals_Count,
                     The_Action.LoopBody(J), 
                     Further_Suspended_Actions, Further_Suspended_Call,
                     Next_TEnv, Exited);
             if Exited then 
               exit;
             end if;
             if  Further_Suspended_Actions > 0 then
               return;  --  WAS EXIT  ----  WAS WRONG????
             end if;
          end loop;
          if Exited then
            exit;
          end if;
          --
          EVars(1..The_Action.Env_Depth) :=  
                  Vectors_DB.Retrieve(Next_TEnv)(1..The_Action.Env_Depth);
          Condition :=
              Eval_BoolBoolExpr (Current_Chart, The_Action.LoopCond.all,
                New_Vars, Current_Queue, EVars(1..The_Action.Env_Depth));
          Next_TEnv := Vectors_DB.NickNum(EVars(1..The_Action.Env_Depth));
          Suspended := 0;   -- restart the loop from the beginning now
        end loop;
        --
      end;
    -----
    ----   suspended   FOR LOOP
    ----
    elsif  The_Action.Kind = ForLoop then
      --
      --  identify suspended subaction
      declare
         Condition: Boolean := True;
         Suspended_Actions : Integer :=0;
         Current_Actions_Table: Int_Table(1..Actions_Table'Length+1) := 
                 Actions_Table & 0;
         Suspended: Natural := 
                 Full_Suspended_Actions_Table(Current_Actions_Table'Length);
        forvar: integer;
        formax: integer;
        forvars: Int_Table(1..Evars'length);
        Exited: Boolean := False;
      begin
        --
        --  resume it
        Resume_Action(Full_Suspended_Actions_Table, Current_Actions_Table,
                Current_Chart,New_Vars,Current_States, Current_Queue,
                TEnv, Return_Vars, All_Signals,All_Signals_Count,
                The_Action.LoopBody(Suspended),
                Further_Suspended_Actions, Further_Suspended_Call,
                Next_TEnv, Exited);
        --
        --  execute the other actions of the loop
        --
        if   Further_Suspended_Actions > 0 then
          return;  -- was  EXIT ???? ISN'T IT WRONG ?
        end if;
        --
        if not Exited then
        for J in Suspended+1 .. The_Action.LoopBody.all'Length loop
          Current_Actions_Table(Current_Actions_Table'Length) := J;
          Apply_Action(Current_Actions_Table, Current_Chart,
                    New_Vars,Current_States, Current_Queue,
                     Next_TEnv, All_Signals,All_Signals_Count,
                     The_Action.LoopBody(J),
                     Further_Suspended_Actions, Further_Suspended_Call,
                     Next_TEnv,Exited);
          if Exited then
            exit;
          end if;
          if  Further_Suspended_Actions > 0 then
             return;  -- was  EXIT ???? ISN'T IT WRONG ?
          end if;
        end loop;
        end if;
        --
        --  maybe execute more loops
        --
        if not Exited then
          forvars(1..The_Action.Env_Depth+3) := 
               Vectors_DB.Retrieve(Next_TEnv)(1..The_Action.Env_Depth+3);
          formax := forvars(The_Action.Env_Depth+3);
          forvar := forvars(The_Action.Env_Depth+1) + 1;
          forvars(The_Action.Env_Depth+1) := forvar;
        end if;
        --
        while not Exited and forvar <= formax and Further_Suspended_Actions = 0 loop
          --
          Next_TEnv := Vectors_DB.NickNum(forvars(1..The_Action.Env_Depth+3));
          --
          for J in 1 .. The_Action.LoopBody.all'Length loop
             Current_Actions_Table(Current_Actions_Table'Length) := J;
             Apply_Action(Current_Actions_Table, Current_Chart,
                    New_Vars,Current_States, Current_Queue,
                     Next_TEnv, All_Signals,All_Signals_Count,
                     The_Action.LoopBody(J),
                     Further_Suspended_Actions, Further_Suspended_Call,
                     Next_TEnv,Exited);
             if Exited then
                exit;
             end if;
             if   Further_Suspended_Actions > 0 then
               return;  -- was  EXIT ???? ISN'T IT WRONG ?
             end if;
          end loop;
          if Exited then
            exit;
          end if;
          --
          forvars(1..The_Action.Env_Depth+3) := 
               Vectors_DB.Retrieve(Next_TEnv)(1..The_Action.Env_Depth+3);
          formax := forvars(The_Action.Env_Depth+3);
          forvar := forvars(The_Action.Env_Depth+1) + 1;  -- loop index increment
          forvars(The_Action.Env_Depth+1) := forvar;
          --
        end loop;

        -- restore next tenv without vars
        Next_TEnv :=  Vectors_DB.NickNum(forvars(1..The_Action.Env_Depth));
        --
      end;
    ----
    ----  SUSPENDED  IF 
    ----
    elsif The_Action.Kind = Conditional then
      declare
         Suspended_Actions : Integer :=0;
         Current_Actions_Table: Int_Table(1..Actions_Table'Length+1) := 
              Actions_Table & 0;
         Suspended: Natural := 
                Full_Suspended_Actions_Table(Current_Actions_Table'Length);
      begin
        if Suspended in The_Action.ThenBody.all'Range then
           Resume_Action(Full_Suspended_Actions_Table, Current_Actions_Table,
                Current_Chart,New_Vars,Current_States, Current_Queue,
                TEnv, Return_Vars, All_Signals,All_Signals_Count,
                The_Action.ThenBody(Suspended),
                Further_Suspended_Actions, Further_Suspended_Call,
                Next_TEnv,Exited);
          if Exited then
            -- env restored by outer loop
            return;
          end if;
          --
          --  execute the other actions of the then branch
          --
          for J in Suspended+1 .. The_Action.ThenBody.all'Length loop
               Current_Actions_Table(Current_Actions_Table'Length) := J;
               Apply_Action(Current_Actions_Table, Current_Chart,
                      New_Vars,Current_States, Current_Queue,
                       Next_TEnv, All_Signals,All_Signals_Count,
                       The_Action.ThenBody(J),
                       Further_Suspended_Actions, Further_Suspended_Call,
                       Next_TEnv,Exited);
               if Exited or Further_Suspended_Actions > 0 then
                 return;
               end if;
            end loop;
            --
        elsif Suspended - The_Action.ThenBody.all'Length in The_Action.ElseBody.all'Range then
           Resume_Action(Full_Suspended_Actions_Table, Current_Actions_Table,
                Current_Chart,New_Vars,Current_States, Current_Queue,
                TEnv, Return_Vars, All_Signals,All_Signals_Count,
                The_Action.ElseBody(Suspended - The_Action.ThenBody.all'Length),
                Further_Suspended_Actions, Further_Suspended_Call,
                Next_TEnv, Exited);
            if Exited then
            -- env restored by outer loop
              return;
            end if;
          --
          --  execute the other actions of the else branch
          --
          for J in Suspended + 1 .. 
                     The_Action.ThenBody.all'Length + The_Action.ElseBody.all'Length loop
               Current_Actions_Table(Current_Actions_Table'Length) := J;
               Apply_Action(Current_Actions_Table, Current_Chart,
                      New_Vars,Current_States, Current_Queue,
                       Next_TEnv, All_Signals,All_Signals_Count,
                       The_Action.ElseBody(J - The_Action.ThenBody.all'Length),
                       Further_Suspended_Actions, Further_Suspended_Call,
                       Next_TEnv, Exited);
               if Exited or Further_Suspended_Actions > 0 then
                 -- env restored by outer loop
                 return;
               end if;
          end loop;
          --
        else
          --Put_Line(Current_Error,"severe runtime inconsistency ...");
          Put_Line("-- RUNTIME_ERROR: severe runtime inconsistency ...");
            Runtime_Errors_Count := Runtime_Errors_Count +1;
            Runtime_Error_Num := CodingBug;
            Runtime_Error_Val := 0;
          raise UML_Error;
        end if;
        -- restore the nextenv size
        --
        EVars(1..The_Action.Env_Depth) := 
           Vectors_DB.Retrieve(Next_TEnv)(1..The_Action.Env_Depth);
        Next_TEnv := Vectors_DB.NickNum(Evars(1..The_Action.Env_Depth));
      end; 
    ----
    ----
    else
      --Put_Line(Current_Error, "Unexpected suspended action");
      Put_Line("-- RUNTIME_ERROR: Unexpected suspended action");
            Runtime_Errors_Count := Runtime_Errors_Count +1;
            Runtime_Error_Num := CodingBug;
            Runtime_Error_Val := 1;
      raise UML_Error;
    end if;
  end Resume_Action;

  -----------------------------------------------------------
  --  Actions_Table e' una tabella degli indici che identifica il posizionamento
  --  "The_Action" nella transizione attualmente in esecuzione
  -- Suspended_Actions e' il Vectors_DB.Nicknum della Actions_Table qualora la
  --  "The_Action" sia una operation-call o una function-assignment.
  -- Se la "The_Action" e' una azione composta (e.g. loop)   la "Suspended_Actions"
  --  identifica il posizionamento della basic action annidata causa della sospensione.
  -- "Suspended_Call" e' l'indice (in All_Events) dell'evento (call-operation)
  --   che ha causato la sospensione.
  -- "Evars" e' l'ambiente "locale" della transizione, costituito inizialmente
  -- dai paramteri del trigger che ha attivato la transizione.
  -- New_vars e' il vettore dello stato degli attributi dell'oggetto.
  -----------------------------------------------------------
  procedure Apply_Action(Actions_Table: Int_Table;
                         Current_Chart: in Natural;
                         New_Vars: in out Int_Table;
                         Current_States: in States_Table;
                         Current_Queue : in Int64_Table;
                         Tenv: in Natural;
                         All_Signals: in out Int64_Table;
                         All_Signals_Count: in out Natural;
                         The_Action: Action_Ref;
                         Suspended_Actions: out Integer;
                         Suspended_Call: out Natural;
                         Next_TEnv: out Natural;
                         Exited: in out Boolean) is
    Tmp_Value: Integer;
    Tmp_Signal: Event_Instance;
    ChartVars: Vars_Table_Ref := All_Charts(Current_Chart).ChartVars;
    Simple: SimpleIntExpr_Ref;
    Local_Var: SystemVar_Ref;
    Signal_Target: Integer;
    The_Index: Integer;
    Evars: Int_Table := Vectors_DB.Retrieve(TEnv);
    Assign_Signal: Int_Table(1..5);
  begin
    Suspended_Actions := 0;
    Suspended_Call :=0;
    Next_TEnv := Tenv;
      ----
      ----    EXIT LOOP
      ----
    if The_Action.Kind = Exitloop then
       Exited := True;
      return;
      ----
      ----     SIMPLE ASSIGNMENTS 
      ----
    elsif The_Action.Kind = Assignment then
      --
      if The_Action.Assignment_Left_Indexes.all'Length = 0 then
        --
        --   NO INDEXING CASE 
        --
        Tmp_Value := 
        Eval_umlExpr(Current_Chart, The_Action.Assignment_Right.all, New_Vars, Current_Queue, EVars);
        --
        if The_Action.Assignment_Left >0 then        
          Local_Var := ChartVars(The_Action.Assignment_Left);
          --
          -- update the New_Vars vector
          New_Vars(Local_Var.Local_Num_Key) := Tmp_Value;
          --
          if Local_Var.Kind = Undefined then
             Local_Var.Kind := umlExpr_Kind(Current_Chart, The_Action.Assignment_Right.all);
          elsif umlExpr_Kind(Current_Chart, The_Action.Assignment_Right.all) = Undefined then
             Set_umlExpr_Kind(Current_Chart,The_Action.Assignment_Right, Local_Var.Kind);
          end if;
          --
          -- generate the assign signal is the variable is observed or the mode is interactive
          if Local_Var.Observed or else 
              NonInteractive = False or else
                Ground_Action_Labels_Needed = True then
             Assign_Signal(1) := Assign_Event.Num_Key;  -- assign signal
             Assign_Signal(2) := Current_Chart;    -- change event
             Assign_Signal(3) := Local_Var.Local_Num_Key;
--             Assign_Signal(4) := 0;
             Assign_Signal(4) := IntEmptyStruct;
             Assign_Signal(5) := Tmp_Value;
             All_Signals_Count := All_Signals_Count+1;
             All_Signals(All_Signals_Count) := 
                 abs(Vectors_DB.NickNum(Assign_Signal));
          end if;
        else
          -- this an assignent to a Transition Var (negative index) 
          declare
             Transition_Env: Int_Table := Vectors_DB.Retrieve(Tenv);
          begin
             Transition_Env(- The_Action.Assignment_Left) := Tmp_Value;
             Next_TEnv := Vectors_DB.NickNum(Transition_Env);
             --
             -- generate the assign signal is the variable is observed or the mode is interactive
             if -- Local_Var.Observed or else 
               NonInteractive = False or else
                Ground_Action_Labels_Needed = True then
                Assign_Signal(1) := Assign_Event.Num_Key;  -- assign signal
                Assign_Signal(2) := Current_Chart;    -- change event
                Assign_Signal(3) := The_Action.Assignment_Left;  -- NEGATIVE index
                Assign_Signal(4) := IntEmptyStruct;
                Assign_Signal(5) := Tmp_Value;
                All_Signals_Count := All_Signals_Count+1;
                All_Signals(All_Signals_Count) := 
                    abs(Vectors_DB.NickNum(Assign_Signal));
             end if;
          end;
        end if;
      else -- The_Action.Assignment_Left_Indexes.all'Length /= 0
        --
        -- THIS IS THE INDEXING CASE
        --
--        The_Index := Eval_IntExpr(Current_Chart, The_Action.Assignment_Left_Index.all,
--                             New_Vars, Current_Queue, EVars);
--        if The_Index = IntFalse then The_Index :=0; end if;
--        if The_Index = IntTrue then The_Index :=1; end if;
--        if The_Index <=  ObjectBase then
--           The_Index := ObjectBase - The_Index;
--        end if;
        declare
          The_Indexes: Int_Table(1..The_Action.Assignment_Left_Indexes.all'Length);
          Old_Struct_Value: Integer;
          New_Struct_Value: Integer;
        begin
          --
          for K in The_Indexes'Range loop
            The_Index :=
               Eval_umlExpr(Current_Chart, The_Action.Assignment_Left_Indexes(K).all,
                       New_Vars, Current_Queue, EVars);
            if The_Index = IntFalse then The_Index :=0; end if;
            if The_Index = IntTrue then The_Index :=1; end if;
            if The_Index <=  ObjectBase then
               The_Index := ObjectBase - The_Index;
            end if;
            The_Indexes(K) := The_Index;
          end loop;
        --
        Tmp_Value := Eval_umlExpr(Current_Chart, The_Action.Assignment_Right.all,
                             New_Vars, Current_Queue, EVars);
        -- set_kind(The_Index, Int); if needed (its ok by default)
        --
        if The_Action.Assignment_Left > 0 then  
          --    This is an assignment to a Class Var
          --
          Local_Var := ChartVars(The_Action.Assignment_Left);
          if Local_Var.Kind = Undefined then
             case UmlExpr_Kind(Current_Chart, The_Action.Assignment_Right.all)  is
               when Object => Local_Var.Kind := ObjVector;
               when Bool => Local_Var.Kind := BoolVector;
               when Number => Local_Var.Kind := NumVector;
               when Others => Local_Var.Kind := Composite;
             end case;
          end if;
          Old_Struct_Value := New_Vars(Local_Var.Local_Num_Key);
          New_Struct_Value := Make_New_Value (Old_Struct_Value, The_Indexes,Tmp_Value);
          New_Vars(Local_Var.Local_Num_Key) := New_Struct_Value;
          -- generate the assign signal is the variable is observed or the mode is interactiove
          if Local_Var.Observed or else 
              NonInteractive = False or else
                Ground_Action_Labels_Needed = True then
              Assign_Signal(1) := Assign_Event.Num_Key;  -- assign signal
              Assign_Signal(2) := Current_Chart;    -- change event
              Assign_Signal(3) := Local_Var.Local_Num_Key;
              Assign_Signal(4) := StructBase - abs(Vectors_DB.NickNum(The_Indexes));
              Assign_Signal(5) := Tmp_Value;
              All_Signals_Count := All_Signals_Count+1;
              All_Signals(All_Signals_Count) :=
                abs(Vectors_DB.NickNum(Assign_Signal));
          end if;
          --
        else
          -- this an assignent to a Transition Var (negative index)
          declare 
             Transition_Env: Int_Table := Vectors_DB.Retrieve(Tenv);
          begin
            --
            Old_Struct_Value := Transition_Env(-The_Action.Assignment_Left);
            New_Struct_Value := Make_New_Value (Old_Struct_Value, The_Indexes,Tmp_Value);
            Transition_Env(-The_Action.Assignment_Left)  := New_Struct_Value;
            Next_TEnv := Vectors_DB.NickNum(Transition_Env);
            if NonInteractive = False or else
                Ground_Action_Labels_Needed = True then
              Assign_Signal(1) := Assign_Event.Num_Key;  -- assign signal
              Assign_Signal(2) := Current_Chart;    -- change event
              Assign_Signal(3) := The_Action.Assignment_Left;
              Assign_Signal(4) := StructBase - abs(Vectors_DB.NickNum(The_Indexes));
              Assign_Signal(5) := Tmp_Value;
              All_Signals_Count := All_Signals_Count+1;
              All_Signals(All_Signals_Count) :=
                abs(Vectors_DB.NickNum(Assign_Signal));
            end if;
          end;
        end if;
        --
        end;
      end if;   --The_Action.Assignment_Left_Indexes.all'Length
      --
    ----
    ----    RETURN SIGNAL
    ----
    elsif The_Action.Kind = OpReturn then
      -- 
      -- This is a return;  or  return(v);  action
      --
       Signal_Target  := ObjectBase - Evars(1);  -- (implicit "caller" operations arg)
       Tmp_Signal := The_Action.Signalled_Event;
       declare
         The_Signal: Int_Table(1..3);
       begin
         The_Signal(1) :=  Tmp_Signal.The_Event.Num_Key;
         The_Signal(2) :=  Signal_Target;
         if Tmp_Signal.The_Args(1) = null then
            -- void
            The_Signal(3) := 0;
         else
            The_Signal(3) := 
                Eval_umlExpr(Current_Chart, Tmp_Signal.The_Args(1).all, 
                     New_Vars, Current_Queue, EVars);
         end if;
         All_Signals_Count := All_Signals_Count+1;
         All_Signals(All_Signals_Count) := abs(Vectors_DB.NickNum(The_Signal));
       end;
       --
    ----
    ----   FUNCTION or OPERATION CALL
    ----
    elsif The_Action.Kind = Call or else The_Action.Kind = Function_Call 
       or else (The_Action.Kind=Signal and then 
              The_Action.Signalled_Event.the_event.kind = operation) then
         --  THERE IS SOME GARBAGE HERE!!!!  
      --
      -- this is  a FUNCTION CALL action
      --
      Suspended_Actions := Vectors_DB.NickNum(Actions_Table);
      -- first step: establish the real target of the signal
      -- in case of a dynamic signal, get the real target from the vars
      --
      Simple := The_Action.Signalled_Event.The_Target;
      Suspended_Call := The_Action.Signalled_Event.The_Event.Num_Key;
      Signal_Target :=
         ObjectBase -
          Eval_SimpleIntExpr( Current_Chart,Simple, New_Vars, Current_Queue, EVars);
      -- if Target is not active no problem .... we deadlock  ..
      if Signal_Target not in All_Charts.all'Range then
         --  1 = "OUT"
         --  2 = "ERR"  
         Runtime_Errors_Count := Runtime_Errors_Count+1;
         Runtime_Error_Num := InvalidTarget;
         Runtime_Error_Val := ObjectBase - Signal_Target;
         Signal_Target := 2;      -- ErrChart
         raise UML_Error;

     end if;
      --
      --  get the signal reference to be enqueued
      --
      Tmp_Signal := The_Action.Signalled_Event;
      -- 
      --  update the all_signals queue with the signals data
      --
      declare
         -- Th_args are  the explicitly provided args
        The_Signal: Int_Table(1..3+Tmp_Signal.The_Args.all'Length);
      begin
        The_Signal(1) :=  Tmp_Signal.The_Event.Num_Key;
        The_Signal(2) :=  Signal_Target;
        The_Signal(3) :=  ObjectBase - Current_Chart; --- the caller implicit arg
        for J in 1 .. Tmp_Signal.The_Args.all'Length loop
          The_Signal(3+J) :=
              Eval_umlExpr(Current_Chart, Tmp_Signal.The_Args(J).all, New_Vars, 
                         Current_Queue, EVars);
          if All_Events(The_Signal(1)).Params(J).Kind = Undefined then
             All_Events(The_Signal(1)).Params(J).Kind :=
                umlExpr_Kind(Current_Chart, Tmp_Signal.The_Args(J).all);
          elsif umlExpr_Kind(Current_Chart, Tmp_Signal.The_Args(J).all) = Undefined then
             Set_umlExpr_Kind(Current_Chart,Tmp_Signal.The_Args(J),
                                All_Events(The_Signal(1)).Params(J).Kind);
          end if;
        end loop;
        All_Signals_Count := All_Signals_Count+1;
        All_Signals(All_Signals_Count) := abs(Vectors_DB.NickNum(The_Signal));
      end;
      --
    ----
    ----     PLAIN SIGNAL
    ----
    elsif The_Action.Kind = Signal then
      --
      -- this is  a SIGNAL or OPERATION CALL  action 
      --
      -- first step: establish the real target of the signal
      -- in case of a dynamic signal, get the real target from the vars
      --
      Simple := The_Action.Signalled_Event.The_Target;
      --  Object values are in the range  below 2_000_000_000 (ObjectBase)
      Signal_Target :=
         ObjectBase -
           Eval_SimpleIntExpr(
             Current_Chart,Simple, New_Vars, Current_Queue, EVars);
      if Signal_Target not in All_Charts.all'Range then
         --  1 = "OUT"
         --  2 = "ERR" 
         Runtime_Errors_Count := Runtime_Errors_Count+1;
         Runtime_Error_Num := InvalidTarget;
         Runtime_Error_Val := ObjectBase - Signal_Target;
         Signal_Target := 2;      -- ErrChart
         raise UML_Error;
     end if;
      --
      --
      --  get the signal reference to be enqueued
      --
      Tmp_Signal := The_Action.Signalled_Event;
      --
      --  update the all_signals queue with the signals data
      --
      declare
        The_Signal: Int_Table(1..2+Tmp_Signal.The_Args.all'Length+1);
        Signal_Size:Natural  := 2+Tmp_Signal.The_Args.all'Length;
      begin
        The_Signal(1) :=  Tmp_Signal.The_Event.Num_Key;
        The_Signal(2) :=  Signal_Target;
        -- NOTICE THAT Tmp_Signal.The_Event may be WRONG w.r.t Kind
        if All_Events(The_Signal(1)).Kind = Signal or
           All_Events(The_Signal(1)).Kind = Undefined then -- for OUT-ERR signals
          -- it is an asynchronous  SIGNAL
          for J in 1 .. Tmp_Signal.The_Args.all'Length loop
            The_Signal(2+J) :=
              Eval_umlExpr(Current_Chart, Tmp_Signal.The_Args(J).all, New_Vars,
                           Current_Queue, EVars);
             if All_Events(The_Signal(1)).Params(J).Kind = Undefined then
               All_Events(The_Signal(1)).Params(J).Kind := 
                  umlExpr_Kind(Current_Chart, Tmp_Signal.The_Args(J).all);
             elsif umlExpr_Kind(Current_Chart, Tmp_Signal.The_Args(J).all) = Undefined then
               Set_umlExpr_Kind(Current_Chart,Tmp_Signal.The_Args(J), 
                                  All_Events(The_Signal(1)).Params(J).Kind);
             end if;
          end loop;
        else   
          -- it is an OPERATION CALL
          Signal_Size := Signal_Size +1;
          The_Signal(3) := Current_Chart;
          for J in 1 .. Tmp_Signal.The_Args.all'Length loop
            The_Signal(3+J) :=
              Eval_umlExpr(Current_Chart, Tmp_Signal.The_Args(J).all, New_Vars,
                           Current_Queue, EVars);
             if All_Events(The_Signal(1)).Params(J).Kind = Undefined then
               All_Events(The_Signal(1)).Params(J).Kind :=
                  umlExpr_Kind(Current_Chart, Tmp_Signal.The_Args(J).all);
             elsif umlExpr_Kind(Current_Chart, Tmp_Signal.The_Args(J).all) = Undefined then
               Set_umlExpr_Kind(Current_Chart,Tmp_Signal.The_Args(J), 
                                  All_Events(The_Signal(1)).Params(J).Kind);
             end if;
          end loop;
          Suspended_Actions := Vectors_DB.NickNum(Actions_Table);
          Suspended_Call := The_Action.Signalled_Event.The_Event.Num_Key;
        end if;
        All_Signals_Count := All_Signals_Count+1;
        All_Signals(All_Signals_Count) := 
               abs(Vectors_DB.NickNum(The_Signal(1..Signal_Size)));
      end;
    ----
    ----   WHILE LOOP
    ----
    elsif The_Action.Kind = Whileloop  then
      --
      --  valuta la condizione
      --  estendi Actions_table
      ---   richiama Apply_Actions sulla sottosequenza di azioni.
      ---  
      declare
        Exited: Boolean := False;
        Condition: Boolean;
        Current_Actions_Table: Int_Table(1..Actions_Table'Length+1) := Actions_Table & 0;
      begin
         Condition := 
             Eval_BoolBoolExpr (Current_Chart, The_Action.LoopCond.all,
                New_Vars, Current_Queue, EVars);
  wcycle:while Condition and Suspended_Actions = 0 loop
           --
          for J in The_Action.LoopBody.all'Range loop
             Current_Actions_Table(Current_Actions_Table'Length) := J;
             Apply_Action(Current_Actions_Table, Current_Chart,
                           New_Vars,Current_States, Current_Queue,
                           Next_TEnv, All_Signals,All_Signals_Count,
                           The_Action.LoopBody(J), Suspended_Actions,Suspended_Call,
                           Next_TEnv, Exited);
             if Exited then
                exit wcycle;
             end if;
             if Suspended_Actions > 0 then
               return;
             end if;
          end loop;
          --
          EVars := Vectors_DB.Retrieve(Next_TEnv)(1..EVars'Length);
          Condition :=
                Eval_BoolBoolExpr (Current_Chart, The_Action.LoopCond.all,
                  New_Vars, Current_Queue, EVars); 
         end loop wcycle; 
      end;
      --
    ----
    ----   CONDITIONAL
    ----
    elsif The_Action.Kind = Conditional  then
      declare
        Condition: Boolean;
        Current_Actions_Table: Int_Table(1..Actions_Table'Length+1) := Actions_Table & 0;
      begin 
         Condition :=
             Eval_BoolBoolExpr (Current_Chart, The_Action.IfCond.all,
                New_Vars, Current_Queue, EVars);
         if Condition then
           for J in The_Action.ThenBody.all'Range loop
             Current_Actions_Table(Current_Actions_Table'Length) := J;
             Apply_Action(Current_Actions_Table, Current_Chart,
                           New_Vars,Current_States, Current_Queue,
                           Next_TEnv, All_Signals,All_Signals_Count,
                           The_Action.ThenBody(J), Suspended_Actions,Suspended_Call,
                           Next_TEnv,Exited);
             if Suspended_Actions > 0 or Exited then
               return;
             end if;
           end loop;
         elsif The_Action.ElseBody /= null then
           for J in The_Action.ElseBody.all'Range loop
             Current_Actions_Table(Current_Actions_Table'Length) := 
                      The_Action.ThenBody.all'Length + J;
             Apply_Action(Current_Actions_Table, Current_Chart,
                           New_Vars,Current_States, Current_Queue,
                           Next_TEnv, All_Signals,All_Signals_Count,
                           The_Action.ElseBody(J), Suspended_Actions,Suspended_Call,
                           Next_TEnv, Exited);
             if Suspended_Actions > 0 or exited then
               return;
             end if;
           end loop;
         end if;
         EVars := Vectors_DB.Retrieve(Next_TEnv)(1..EVars'Length);
         Next_TEnv := Vectors_DB.NickNum(Evars);
      end;
      --
    elsif The_Action.Kind = VarDecl then
      --
      -- this is  a transition VARIABLE DECLARATION
      --
      declare
        Extended: Int_Table(1..Evars'Length+1) :=  Evars & 0;
        InitValue: Integer; 
      begin
        if The_Action.TValue /= null then
          InitValue := Eval_umlExpr(Current_Chart, The_Action.TValue.all,
                             New_Vars, Current_Queue, EVars);
          Extended(Evars'Length+1) := InitValue;
        end if;
        Next_TEnv := Vectors_DB.NickNum(Extended);
      end;
      --
    elsif The_Action.Kind = Forloop then
      --
      --           FORLOOP
      --
      -- declare the three localvars:   ForIndex, formin formax  and initialize them
      -- if index in range execute the body  
      --  increment the var, and if in range continue to execute the body.
    
      declare
        Exited: Boolean := False;
        forvar: integer;
        formin: integer;
        formax: integer;
        Current_Actions_Table: Int_Table(1..Actions_Table'Length+1) := Actions_Table & 0;
        forvars: Int_Table(1..Evars'length+3); 
      begin
         formin :=  Eval_IntExpr(Current_Chart, The_Action.For_Min.all,
                             New_Vars, Current_Queue, EVars);
         formax :=  Eval_IntExpr(Current_Chart, The_Action.For_Max.all,
                             New_Vars, Current_Queue, EVars);
         forvar := formin;
         forvars := EVars & (forvar, formin, formax);
         --
  fcycle:while forvar <= formax and Suspended_Actions = 0 loop
           Next_TEnv := Vectors_DB.NickNum(EVars & (forvar, formin, formax));
           for J in The_Action.LoopBody.all'Range loop
              Current_Actions_Table(Current_Actions_Table'Length) := J;
              Apply_Action(Current_Actions_Table, Current_Chart,
                            New_Vars,Current_States, Current_Queue,
                            Next_TEnv, All_Signals,All_Signals_Count,
                            The_Action.LoopBody(J), Suspended_Actions,Suspended_Call,
                            Next_TEnv, Exited);
             if Exited then
                exit fcycle;
             end if;
             if Suspended_Actions > 0 then
                return;
             end if;
           end loop;
           forvars := Vectors_DB.Retrieve(Next_TEnv)(1..forvars'Length);
           formax := forvars(EVars'Length+3);
           formin := forvars(EVars'Length+2);
           forvar := forvars(EVars'Length+1)+1;
           EVars := Vectors_DB.Retrieve(Next_TEnv)(1..EVars'Length);
         end loop fcycle;
         EVars := Vectors_DB.Retrieve(Next_TEnv)(1..EVars'Length);
         Next_TEnv := Vectors_DB.NickNum(Evars);
      end;
    --
    end if;
  end Apply_Action;
  
  function AbstractEvolvingView (Current_Chart: Natural)  return String_Tables_Vector_Ref;

  function AbstractTriggerView (TheTrigger: Int64;
                         Current_Chart: Natural)  return String_Tables_Vector_Ref;

  function AbstractView (TheSignals: Int64_Table;
                         Current_Chart: Natural)  return String_Tables_Vector_Ref;

--
-- La funzione "Apply_Transitions_Sequence", data la attuale 
--  configurazione del sistema, e data una particolare sequenza di 
--  transizioni da eseguire, (eventualmente una sequenza nulla in caso 
--  di stuttering) restituisce la nuova evoluzione del sistema costituita 
--  dalla coppia
--  <sequenza di signals generati, nuova configurazione raggiunta>
--
--  N.B. Da quando i target sono dinamici  (= -1) non e' piu' possibile
--  distinguere gli internal signals dagli external signals prima di aver
--  eseguito uttte le azioni preceddenti la valutazione del signal stesso
--   e.g.  x:=y; x.event(z)  tali signal cono considerati come INTERNI
--   (i.e da mettere in qualche coda)
--
-- N.B.  tutte le transizioni della sequenza hanno medesimo trigger.
--  Se il trigger e' null o Null_Event allora si tratta di 
--  completion transitions che vanno applicate senza rimuovere il
--  trigger dalla coda, perche' non c'e'.
--
-- if trigger_pos =0 (completion event), trigger =(0,0)
-- Transition_Sequence'Length > 0  (Apply_Stuttering isn otherwise called)
--
-- Se l'esecuzione di una sequenza di transizioni pORTA alla esecuzione
-- di una call operation, la sequenza di azioni viene interrotta e la
-- ORC va nello stato:
-- (last-vars,orig-queue+local-signals,orig-states,
--  suspended-sequence, suspended-trans,suspended-action,suspended-trigger-pos)
--  da cui puo' essere resumata quando ritorna il return signal.
--
  -------------------------------------------------------------------
  --  called by Compute_Object_Evolutions  when  building the oject
  --  evolution corresponding to a sequence of transitions.
  --  (ONLY in the case of completion transitions and  triggered transitions).
  -------------------------------------------------------------------
  function Apply_Transitions_Sequence (
       Active_Object: Positive;    -- the current active object  (self)
       Current_Vars: Int_Table;    -- vars data;
       Current_Queue: Int64_Table; -- queue data
       Current_States: States_Table;  -- states data
       Trigger: Int_Table;            -- trigger data
       Trigger_Pos: Natural;   -- 0 = completion event
       This_Sequence_Nick: Int64)   -- nick in Transitions_DB
           return Object_Runtime_Evolution is 
    --
    New_Vars: Int_Table := Current_Vars;
    Current_Chart: Natural := Active_Charts(Active_Object);
    Evars_Count :Natural := Trigger'Length -2;
    Evars: Int_Table(1..Evars_Count) := Trigger(3..Trigger'Length);
    Signals_Count: Natural :=0;
    This_Sequence: Transitions_Table :=
            Transitions_DB.Retrieve(This_Sequence_Nick);
    Trimmed_Size: Natural;
    New_Queue: Int64;
    Exited: State_Ref;
    New_States: States_Table := Current_States;
    --
    Result: Object_Runtime_Evolution;
    New_ORC: Object_Runtime_Configuration(Current_States'Length, 
                                         Current_Vars'Length);
    Suspended_Call: Natural :=0;
    Transition_Env: Natural := Nullvector;
    TEnv: Natural := Vectors_DB.NickNum(Evars);
    Next_TEnv: Natural :=0;
--    Abstract_TriggerInfo: String_Tables_Vector_Ref;
    RANDOMQUEUE: Boolean := Has_Random_Queues(Current_Chart);
  begin
    -- Nota che il trigger viene rimosso anche se l'evoluzione
    -- potra' essere sospesa. In quest'ultimo caso il trigger
    -- andra salvato in ORC.Trimmed_trigger
    if Trigger_Pos > 0 then   -- triggered transitions
      Trimmed_Size := Current_Queue'Length -1;
    else                      -- completion transitions
      Trimmed_Size := Current_Queue'Length;
    end if;
    --
    -- in realta' i segnali generati potranno essere di meno,
    --  soprattutto se ci saranno sospensioni su call .. 
    --  OPPURE MOLTI DI PIU' SE CI SONO LOOP....
--    for I in This_Sequence'Range loop
--      Signals_Count := Signals_Count + This_Sequence(I).Actions.all'Length;
--    end loop;
    --
    declare
      Trimmed_Queue: Int64_Table(1..Trimmed_Size);
--      All_Signals: Int64_Table(1..Signals_Count) := (others => 0);  
      All_Signals: Int64_Table(1..1000); 
      All_Signals_Count: Natural := 0;
      Suspended_Actions: Natural := 0; --nick in Vectors_DB (the suspended actions table)
      Current_Transition_Index: Natural := 0;
      Actions_Table: Int_Table := (1..1 =>0);
      IsExited: Boolean := False;
    begin
      for I in This_Sequence'Range loop
        Current_Transition_Index := I;
        Next_TEnv := TEnv;
        for J in This_Sequence(I).Actions.all'Range loop
           Actions_Table(1) :=J;
           Apply_Action(Actions_Table, Current_Chart,
                         New_Vars,Current_States, Current_Queue,
                         Next_TEnv, All_Signals,All_Signals_Count,
                         This_Sequence(I).Actions(J), Suspended_Actions,Suspended_Call,
                         Next_TEnv,IsExited);
           if Suspended_Actions > 0 then
             exit;
           end if;
        end loop;
        if Suspended_Actions > 0 then
          exit;
        end if;
      end loop;
      -- New_VARS  and ALL_SIGNALS have been updated

      -- 1) in the case of NOT completions trans. remove the trigger from the q
      --    BEWARE that also in case of CALLS we remove it.
      --
      if Trigger_Pos > 0 then
        for I in 1..Trigger_Pos-1 loop
          Trimmed_Queue(I) := Current_Queue(I);
        end loop; 
        for I in Trigger_Pos+1 .. Current_Queue'Length loop
          Trimmed_Queue(I-1) := Current_Queue(I);
        end loop; 
      else
        Trimmed_Queue := Current_Queue;
      end if;

      -- 2) filter non visible signals (if any)  (e.g. non observable assign)
      --
      --  OUT.LOSTEVENT signals are generated only by stuttering.
      --
      --  ASSIGN events are always generated, evenr if not observable.
      --  ASSIGN events are Displayed only if observable.
      --  ASSIGN events are not enqueued.
--      All_Signals_Count := 0;
--      for I in All_Signals'Range loop
--        if ... then
--          All_Signals_Count := All_Signals_Count+1;
--          All_Signals(All_Signals_Count) := All_Signals(I);
--        end if;
--      end loop;

      -- 3) update the local queue with local signals,
      --
      declare
        New_Queue_Items : Int64_Table(1..Trimmed_Size+All_Signals_Count); 
        New_Queue_Size: Natural := Trimmed_Size;
      begin
         New_Queue_Items(1..Trimmed_Size) := Trimmed_Queue(1..Trimmed_Size);
         for I in 1.. All_Signals_Count loop 
           declare
             This_Signal: Int_Table := Vectors_DB.Retrieve(All_Signals(I));
           begin
             if This_Signal(2) = Current_Chart and then
                 This_Signal(1) /= Assign_Event.Num_Key then
               --
               -- IN CASE OF RANDOM QUEUE INSERT THE SIGNAL KEEPING THE QUEUE SORTED
               --
               if RANDOMQUEUE then
                  New_Queue_Size := New_Queue_Size +1;
                  New_Queue_Items(1..New_Queue_Size) := 
                     InsertSorted(All_Signals(I), New_Queue_Items(1..New_Queue_Size-1));
               else
                  New_Queue_Size := New_Queue_Size +1;
                  New_Queue_Items(New_Queue_Size) := All_Signals(I);
               end if;
               --  assign signals are  now local events
             end if;
           end;
         end loop;
         New_Queue := 
           abs(Signals_DB.NickNum(New_Queue_Items(1..New_Queue_Size)));
         if New_Queue_Size > Configurations.Kernel.MAXQUEUE then
            MAXQUEUE := New_Queue_Size;
         end if;
      end;

      -- 4) update the currently active states,
      --
      if Suspended_Actions > 0 then
        -- in case of suspension ..
        New_States := Current_States;
      else
        New_States := Current_States; 
        for I in This_Sequence'Range loop
          -- for all sources of this transition, clear the source
          Exited :=
            Currently_Active_Substate (This_Sequence(I).Owner,Current_States);
          for J in Exited.FirstRuntimePosition .. Exited.LastRuntimePosition loop
               New_States(J) := null;
          end loop;                           --  JUST ADDED  11-01-2006
          -- Apply the transition mask
          for K in This_Sequence(I).Mask.all'Range loop
            if This_Sequence(I).Mask(K) /= null then    
              New_States(K) :=  This_Sequence(I).Mask(K);
            end if;
          end loop;
        end loop;
      end if;
      
      -- 5) build the target and cache its nick
      --
      New_ORC.Self := Current_Chart;
      if Suspended_Actions > 0 then
        New_ORC.Suspended_Transition_Index := Current_Transition_Index;
        New_ORC.Suspended_Actions := Suspended_Actions;
        New_ORC.Suspended_Call := Suspended_Call;
        New_ORC.Transition_Env := Next_TEnv;
        New_ORC.TriggerArgs := TEnv;
--        if Trigger_Pos =0  then
--           New_ORC.Current_Trigger := 0;
--        else
--          New_ORC.Current_Trigger := Current_Queue(Trigger_Pos);
--        end if;
        New_ORC.Suspended_Sequence := This_Sequence_Nick;
      end if;
      New_ORC.Current_States := New_States;
      New_ORC.Current_Vars := New_Vars;
      New_ORC.Current_Queue := New_Queue;

      -- 6) build and return the ore
      --
      Result.Evolving_Object := Active_Object;
      Result.Transitions :=  abs(Transitions_DB.NickNum(This_Sequence));
      -- Result.Trigger .. already adjusted
      if Trigger_Pos > 0 then
        Result.Trigger := Current_Queue(Trigger_Pos);
--        Abstract_TriggerInfo := AbstractTriggerView(Result.Trigger,Current_Chart);
      else
        Result.Trigger := 0;
      end if;  
      Result.Signals  := 
          abs(Signals_DB.NickNum(All_Signals(1..All_Signals_Count)));
      -- Result.Transitions .. already adjusted
--declare
--  NN: Integer := ORC_DB.JustCheckNum(New_ORC);
--  S: String_Ref;
--begin
--if (NN = 0) then 
--  S := new String'("NEWORC: " & Integer'Image(ORC_DB.Max_key_Conflicts) &
--        " KEY=" & Integer'Image(Mk_Key(New_ORC)) &  
--         " VARS=" & Integer'Image(Mk_Key(New_ORC.Current_Vars)));
--    Result.Target :=  abs(ORC_DB.NickNum(New_ORC));
--  Put_line(S.all & " -> " & Integer'Image(Result.Target));
--else
--    Result.Target :=  abs(ORC_DB.NickNum(New_ORC));
--end if;
--end;
--OMSEM.seize;
      Result.Target :=  abs(ORC_DB.NickNum(New_ORC));
--OMSEM.release;
      declare
        Tmp1: String_Tables_Vector_Ref; -- AbstractTriggerView
        Tmp2: String_Tables_Vector_Ref; -- AbstractEvolvingView
        Tmp3: String_Tables_Vector_Ref; -- Tmp1 + Tmp2
      begin
        Tmp2 := AbstractEvolvingView(Current_Chart);
        if Result.Trigger  >0 then
          Tmp1 := AbstractTriggerView(Result.Trigger,Current_Chart);
        end if;
        if Tmp1 = null or else Tmp1.all'Length =0 then
           Tmp3 := Tmp2;
           Tmp2 := null;
        else
          Tmp3 := new String_Tables_Vector'(Tmp1.all & Tmp2.all);
        end if;
        if Tmp1 /= null and then Tmp1.all'Length >0 then
           Free(Tmp1);
        end if;
        if Tmp2 /= null and then Tmp2.all'Length >0 then
           Free(Tmp2);
        end if;
        Tmp1 := Tmp3;
        Tmp2 := AbstractView(All_Signals(1..All_Signals_Count), Current_Chart);
        if Tmp1 = null or else Tmp1.all'Length =0 then
           Tmp3 := Tmp2;
           Tmp2 := null;
        else
          Tmp3 := new String_Tables_Vector'(Tmp1.all & Tmp2.all);
        end if;        
        if Tmp1 /= null and then Tmp1.all'Length >0 then
           Free(Tmp1);
        end if;
        if Tmp2 /= null and then Tmp2.all'Length >0 then
           Free(Tmp2);
        end if;
        Result.AbstractInfo.Labels := SortUnique(Tmp3);
      end;
      --
      return Result;
    end;
  end Apply_Transitions_Sequence;

  --------------------------------------------------------------
  --  Given a list of Int64 keys, each one denoting a communicated signal 
  --  returns a list of string_tables corresponding to the abstract view of them.
  -- The abstract view is defined by the UML Abstraction Rules,
  --  IF THE SIGNAL IS ACTUALLY AN OPCALL EVENT  IT HAS AN ADDITIONAL
  --   _CALLER PARAMETER WHICH IS SKIPPED
  -- 
  -- SEE ALSO  ABSTRACTTRIGGERVIEW AbstractLostEventView
  --  
  -- La abstractview di un insieme di signals e' data dalla unione delle abstractview
  -- dei singoli signal. Questo signiofica chew una abstractview non puo'm dipendere
  --  da condizioni su signals multipli
  --------------------------------------------------------------
  function AbstractView (TheSignals: Int64_Table;
                         Current_Chart: Natural)  return String_Tables_Vector_Ref is
    --
    TablesResult: String_Tables_Vector_Ref :=  Empty_String_Tables_Vector_Ref;
    Prev: String_Tables_Vector_Ref;
  begin
    if TheSignals'Length =0  or else
       All_Observations.all'Length = 0 then
       return Empty_String_Tables_Vector_Ref;
    end if;
    -- 
    for SIG in TheSignals'Range loop
      declare
      --
      -- type Rule_Left_Elem is record
      --   LTerm: String_Ref;                                  -- src
      --   LPartner: String_Ref;                               -- target
      --   LOp: String_Ref;                                    -- event /obj (inState clause)
      --   LMode: String_Ref;
      --   LArgs: String_Table_Ref;                            -- [arg1,arg2]/ (Top.S1.s2)
      --   --------  additional "and" part  ------------------
      --   IdsL: String_Table_Ref := Empty_String_Table_Ref;   -- [obj1,var]/$v/literal/
      --   LeftOp: BinOp;                                      --  EQ/NE/LT/GT/LE/GE
      --   IdsR: String_Table_Ref :=  Empty_String_Table_Ref;  --  [obj2,var]/$v/literal
      -- end record:
      --
      -- type Rule_Left is array (Positive range <>) of Rule_Left_Elem;
      --
      -- Observation  is record
      --     Kind: AbstractionKind := BothKinds;           -- State/Action
      --     Left: Rule_Left;
      --     RLabels: String_Table_Ref := Empty_String_Table_Ref; -- [mainlabel,aarg1,aarg2]
      -- ennd record
      --  
      -- State:   is_active(obj,state) 
      --            and obj.var=3  
      --            and obj2.var=4  ->  mainlabel(aarg1,aarg2)
      -- State:   inState(obj.state) and obj.var=$v  ->  mainlabel(aarg1,$v)
      -- State:   obj.var1 > obj.var2  ->  label(op,op,1)
      -- Action:  src:target.event<arg1,arg2> -> mainlabel(aarg1,aarg2)
      -- Action:  src:target.event!<$1,$2,*,a>   ->  mainlabel(event,$2,$1)
      -- Action:  $1:$2.$3!<>   ->  mainlabel($3,$2,$1)
      --
      --            MATCHING GROUND ACTIONS PARAMETERS
      --    GROUND ACTION         IS MATCHED BY              IS NOT MATCHED BY
      --      foo                 foo(*),foo()                foo(a),foo($v)
      --      foo(a)              foo,foo(a),foo(*),foo(a,*)  foo(),foo(a,b),foo(a,$v)
      --      foo(a,b)            foo(*),foo(a,*),foo(a,b,*)  foo(),foo(a),foo(a,b,c)
      -- abstract parameters must be the same number of the ground parametrs
      --  or they can be LESS than ground parameters if the last one is "*"
      --  or they can be ONE MORE than ground parameters if the last one is "*"
      --
      --
      RulePrev: String_Table_Ref;
      RuleResult: String_Table_Ref;
      DollarNames: String_Table(1..10);
      DollarValues: String_Table(1..10);
      DollarVectors: String_Tables_Vector(1..10);
      DollarCount: Natural;
      RuleSize: Natural; 
      II: Natural :=1;
      MatchFound: Natural :=0;
      --
      The_Signal: Int_Table := Vectors_DB.Retrieve(TheSignals(SIG));
      --
      -- (1=> event, 2 => target, 3... args)
    begin 
      --
      for I in All_Observations.all'Range loop  -- CYCLE ON ALL (ACTION) RULES
        --
        -- ACTION RULES have the most generic form:
        --   $1:$2.$3($4,$5) and $1 = $2  and $3 /= [123] and $4 < $5   ->      $1($2,$4)
        --   |||||||||||||||  solo questa parte definisce le variabili usabili in tutto il resto della regola
        --
        MatchFound :=I;
        -- MatchFound is reset to 0 if  for some reason the ruleAll_Observations(I) does not match
        --
        DollarCount :=0;
        --
        if All_Observations(I).Kind /= ActionKind then
          -- Skip STATE rules
          MatchFound :=0;   -- we give up
          DollarCount :=0;          
        end if;
        --
        for L in All_Observations(I).Left'Range loop -- CYCLE ON LEFT COMPONENTS OF RULE
          --
          if L = 1 then
            --
            -- CHECK SOURCE OBJECT
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LTerm /= null and then
               All_Observations(I).Left(L).LTerm(1) /= '$' and then
               All_Observations(I).Left(L).LTerm(1) /= '*' and then
               All_Observations(I).Left(L).LTerm.all /= 
                    All_Charts(Current_Chart).Name.all then
               MatchFound :=0;  -- we give up
            end if;
            -- SAVE MATCHING OBJECT IN VARS 
            if MatchFound >0 and then   -- not already given up
                All_Observations(I).Left(L).LTerm /= null  and then
                All_Observations(I).Left(L).LTerm(1) = '$' then
              DollarCount := DollarCount+1;
              Dollarnames(DollarCount) := All_Observations(I).Left(L).LTerm;
              Dollarvalues(DollarCount) := new String'(All_Charts(Current_Chart).Name.all);
            end if;
            --
            -- CHECK TARGET OBJECT
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LPartner /= null and then
               All_Observations(I).Left(L).LPartner(1) /= '$' and then
               All_Observations(I).Left(L).LPartner(1) /= '*' and then
               All_Observations(I).Left(L).LPartner.all /= 
                   All_Charts(The_Signal(2)).Name.all then
               MatchFound :=0;  -- we give up
            end if;
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LPartner = null and then
               All_Observations(I).Left(L).LOp = null then
               MatchFound :=0;  -- we give up
            end if;
            -- SAVE TARGET OBJECT IN VARS
            if MatchFound >0 and then   -- not already given up
                All_Observations(I).Left(L).LPartner /= null  and then
                All_Observations(I).Left(L).LPartner(1) = '$' then
              DollarCount := DollarCount+1;
              Dollarnames(DollarCount) := All_Observations(I).Left(L).LPartner;
              Dollarvalues(DollarCount) := new String'(All_Charts(The_Signal(2)).Name.all);
            end if;
            --
            -- CHECK EVENTNAME
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LOp /= null and then
               All_Observations(I).Left(L).LOp(1) /= '$' and then
                 All_Observations(I).Left(L).LOp(1) /= '*' and then
               All_Observations(I).Left(L).LOp.all /= 
                    All_Events(The_Signal(1)).Name.all then
               MatchFound :=0;  -- we give up
            end if;
            -- SAVE EVENTNAME IN VARS (UNLESS assign/return/lostevent,accept)
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LOp /= null and then
               (All_Observations(I).Left(L).LOp(1) = '$' or else
                 All_Observations(I).Left(L).LOp(1) = '*') and then
               (All_Events(The_Signal(1)).Name.all = "accept" or else  --  ?? do we have it??
                 All_Events(The_Signal(1)).Name.all = "assign" or else
                 All_Events(The_Signal(1)).Name.all = "return" or else
                 All_Events(The_Signal(1)).Name.all = "lostevent" ) then
               MatchFound :=0;  -- we give up
            end if;
            if MatchFound >0 and then   -- not already given up
                All_Observations(I).Left(L).LOp /= null  and then
                All_Observations(I).Left(L).LOp(1) = '$' then
              DollarCount := DollarCount+1;
              Dollarnames(DollarCount) := All_Observations(I).Left(L).LOp;
              Dollarvalues(DollarCount) := new String'(All_Events(The_Signal(1)).Name.all);
            end if;
            --            MATCHING GROUND ACTIONS PARAMETERS
            --    GROUND ACTION         MATCHES                   DOES NOT MATCH
            --      foo                 foo(*),foo()                foo(a),foo($v)
            --      foo(a)              foo,foo(a),foo(*),foo(a,*)  foo(),foo(a,b),foo(a,$v)
            --      foo(a,b)            foo(*),foo(a,*),foo(a,b,*)  foo(),foo(a),foo(a,b,c)
            --
             --  abstract args count                      ground args count
             --  All_Observations(I).Left(L).LArgs.all'Length     (The_Signal'Length-2) -3
            --                                            Args_Count(The_Signal)
            if MatchFound >0 and then
               All_Observations(I).Left(L).LOp /= null and then
               All_Observations(I).Left(L).LOp.all = "assign" and then
               The_Signal(1)=Assign_Event.Num_Key and then 
               The_Signal(3) in All_Charts(Current_Chart).ChartVars.all'Range and then
               All_Charts(Current_Chart).ChartVars(The_Signal(3)).name.all =
                    All_Observations(I).Left(L).LArgs(1).all then
               --
               -- ********* MISSING CHECK ON VAR INDEX!!! ********
               --
               -- SPECIAL ASSIGN CASE:
               --
               -- OBS: ARGS=[var,index,index,value]  from assign(var,[index,index], value)
               -- OBS: ARGS=[var,value]  from assign(var,value)
               --   warning  notice the confusion with  assign (var, [index,index,value]) !!! 
               -- SIG:  [assign,tgt,var_key,indexstruct,value]    sig'length=5  sempre
               --                   arg1      arg2      arg3
               --
               --
               if All_Observations(I).Left(L).LArgs = null or else
                  All_Observations(I).Left(L).LArgs.all'length < 2 then
                  MatchFound :=0;  -- we give up
                  --
               elsif All_Observations(I).Left(L).LArgs.all'length = 2 then
                  --  OBS.LARGS=(var,value)
                  --   Action assign(var,$val)  -> ...
                  if The_Signal(4) /= IntEmptyStruct then
                    MatchFound :=0;  -- we give up
                  elsif All_Observations(I).Left(L).LArgs(2)(1) = '$' then
                    --  OBS:LARGS=(var,$val)
                    DollarCount := DollarCount+1;
                    DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(2);
                    DollarValues(DollarCount) := new String'(Arg_Image(The_Signal,3,Current_Chart));
                  elsif All_Observations(I).Left(L).LArgs(L)(1) /= '*' and then
                      Arg_Image(The_Signal,3,Current_Chart) /=
                      Normalized_Literal(All_Observations(I).Left(L).LArgs(2).all) then
                    MatchFound :=0;  -- we give up 
                  end if;
               else
                  -- OBSERV=         LOP    LArgs(1) Largs(2)  Largs(3) Largs(4).. 
                  -- OBSERV=  Action assign(var,[index1,index2],value)  -> ...   CHANGED INTO NEXT
                  -- OBSERV=  Action assign(var,index1,index2,value)  -> ...
                  -- SIGNAL= [Assign_Event.Num_Key, Obj.ChartIndex, LocalVar.Num_Key, Indexes, Value];
                  --
                 declare
                   The_Indexes: Int_Table :=   Vectors_DB.Retrieve(StructBase - The_Signal(4));
                 begin
                   if The_Indexes'Length /= All_Observations(I).Left(L).LArgs.all'length -2 then
                     --  iignal_Indexes'Length must be Largs'Length - 1(varcomponent) - 1(value component)
                     MatchFound :=0;  -- we give up
                   else
                     --   signal v[1]:= ..  does not match rule  v[2] := ...
                     for II in The_Indexes'Range loop
                      if All_Observations(I).Left(L).LArgs(II+1)(1) /= '*' and then
                         All_Observations(I).Left(L).LArgs(II+1)(1) /= '$' and then
                         Value_Image(The_Indexes(II),number) /=
                           Normalized_Literal(All_Observations(I).Left(L).LArgs(II+1).all) then
                         MatchFound :=0;  -- we give up
                       end if;
                     end loop;
                     --  signal v :=  10  does not match  rule  v := 12
                     if MatchFound >0 and then
                        All_Observations(I).Left(L).LArgs(The_Indexes'Length+2)(1) /= '$' and then
                        All_Observations(I).Left(L).LArgs(The_Indexes'Length+2)(1) /= '*' and then
                        --  
                        Arg_Image(The_Signal,3,Current_Chart) /=
                          Normalized_Literal(All_Observations(I).Left(L).LArgs(The_Indexes'Length+2).all) then
                          MatchFound :=0;  -- we give up
                     end if;
                     -----
                     if MatchFound >0 then
                       -- signal  v[$i] := 3  matches rule   v[5] := 3  
                       --  if $i was not yet defined, now becomes defined
                       -- notice that "assign" is ONLY the first clause, so its vars are undefined
                       --   unless we allow  v[$i,$i] := $i as valid clause ...
                       for II in The_Indexes'Range loop
                         if All_Observations(I).Left(L).LArgs(II+1)(1) = '$' then
                           DollarCount := DollarCount+1;
                           DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(II+1);
                           DollarValues(DollarCount) := new String'(Value_Image(The_Indexes(II),number));
                         end if;
                       end loop;
                       -- signal  v := 17  matches rule   v:= $v  
                       --  if $v was not yet defined, now becomes defined
                       if All_Observations(I).Left(L).LArgs(The_Indexes'Length+2)(1) = '$' then
                         DollarCount := DollarCount+1;
                         DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(The_Indexes'Length+2);
                         DollarValues(DollarCount) := new String'(Arg_Image(The_Signal,3,Current_Chart));
                       end if;
                     end if;
                     ------------
                     -- TO BE CHECKED THE CASE  Action assign(v,$i,$i,$i) as valid clause ...
                     ------------
                   end if;
                 end;
               end if;
            --
            else  -- not the ASSIGN  CASE
            --  CHECK JUST ONE MORE TEMPLATE PARAM ONLY IF LAST IS '*'  or '$*'
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LArgs /= null  and then
               All_Observations(I).Left(L).LArgs.all'Length = Args_Count(The_Signal) +1 and then
               All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "*" and  then
               All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "$*" then
               --
               -- abstract args count = ground args count +1  and last abstract arg /= '*'
               --   abstract foo(a,b) doeas not match ground foo(a) 
               -- 
               MatchFound :=0;  -- we give up
            end if;
            --
            -- CHECK NOT SURELY FAR TOO MANY TEMPLATE PARAMS
            if  MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LArgs /= null  and then
               All_Observations(I).Left(L).LArgs.all'Length > Args_Count(The_Signal) +1 then
               --  abstract args count >  ground args count +1
               --    abstract foo(a,*)  does not match  ground foo
               --    abstract foo(a,b)  does not match  ground foo
               MatchFound :=0;  -- we give up
            end if;
            --
            --  CHECK NOT TOO FEW TERMPLATE PARAMS, UNLESS LAST IS '*'  (or assign)
            if  MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LArgs /= null  and then
               All_Observations(I).Left(L).LArgs.all'Length < Args_Count(The_Signal) and then
               (All_Observations(I).Left(L).LArgs.all'Length = 0 or else
               All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "*" ) and then
               (All_Observations(I).Left(L).LArgs.all'Length = 0 or else
               All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "$*") then
               --
               --  abstract args count <  ground args count and last abstract arg /= "*" and /= "$*"
               --    abstract foo(a)  does not match  ground foo(a,b)
               --    abstract foo()  does not match  ground foo(a)
               MatchFound :=0;  -- we give up
            end if;
            --
            -- CHECK ACTUAL MATCHING FOR TEMPLATE PARAMS, SAVING MATCHING VARS
            if MatchFound >0 and then   -- not already given up
               All_Observations(I).Left(L).LArgs /= null  then
              for J in 1 .. All_Observations(I).Left(L).LArgs.all'Last loop
                if J = All_Observations(I).Left(L).LArgs.all'Last and then
                   All_Observations(I).Left(L).LArgs(J) /= null  and then
                   All_Observations(I).Left(L).LArgs(J).all'Length >1 and then
                    All_Observations(I).Left(L).LArgs(J)(1..2) = "$*" then
                  DollarCount := DollarCount+1;
                  DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(J);
                  declare
                    ST: String_Table(1..Args_Count(The_Signal)-J+1);
                  begin
                    for K in 1..ST'Last loop
                      ST(K) := new String'(Arg_Image(The_Signal,J-1+K,Current_Chart)); 
                    end loop;
                    DollarVectors(DollarCount) := new String_Table'(ST);
                  end;
                 elsif All_Observations(I).Left(L).LArgs(J) /= null  and then
                   All_Observations(I).Left(L).LArgs(J).all'Length >0 and then
                    All_Observations(I).Left(L).LArgs(J)(1) = '$' then
                  DollarCount := DollarCount+1;
                  DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(J);
                  DollarValues(DollarCount) := 
                       new String'(Arg_Image(The_Signal,J,Current_Chart));
                elsif All_Observations(I).Left(L).LArgs(J) /= null  and then
                    All_Observations(I).Left(L).LArgs(J)(1) /= '$' and then
                    All_Observations(I).Left(L).LArgs(J)(1) /= '*' and then
                    Arg_Image(The_Signal,J,Current_Chart) /= 
                       Normalized_Literal(All_Observations(I).Left(L).LArgs(J).all) then
                  MatchFound :=0;     --  we give up
                  exit;
                end if;
              end loop;
            end if;
            --
            end if;  -- ASSIGN CASE OR NOT
            --
          else  -- L  /= 1
          --
          --  This is the case of a leftside successive to the first one, which
          --    can only have the form of a relation
          --  The appearing dollarnames have already a value taken from the evaluation of the FIRST leftside
          --
          --    CHECK    $i = $j  or   $i /= step
          --
          if MatchFound >0 and then
              All_Observations(I).Left(L).LeftOp /= NOOP then
            declare
              str1, str2: String_Ref;
              N : Natural := 0;
            begin
              if All_Observations(I).Left(L).IdsL(1)(1)='$' then
                for J in 1..DollarCount loop
                   if DollarNames(J).all = All_Observations(I).Left(L).IdsL(1).all then
                      N := J;
                      exit;
                   end if;
                end loop;
                if N = 0 then 
                   Runtime_Errors_Count := Runtime_Errors_Count +1;
                   Runtime_Error_Num := InvalidAbstraction;
                   Runtime_Error_Val :=0;
                   raise UML_Error; 
                end if;
                str1 := DollarValues(N);
              else
                str1 := All_Observations(I).Left(L).IdsL(1);
              end if;
              N :=0;
              if All_Observations(I).Left(L).IdsR(1)(1)='$' then
                for J in 1..DollarCount loop
                   if DollarNames(J).all = All_Observations(I).Left(L).IdsR(1).all then
                      N := J;
                      exit;
                   end if;
                end loop;
                if N = 0 then 
                   Runtime_Errors_Count := Runtime_Errors_Count +1;
                   Runtime_Error_Num := InvalidAbstraction;
                   Runtime_Error_Val :=0;
                  raise UML_Error; 
                end if;
                str2 := DollarValues(N);
              else
                str2 := All_Observations(I).Left(L).IdsR(1);
              end if;
              if All_Observations(I).Left(L).LeftOp = EQ and then
                                        str1.all /= str2.all then
                 MatchFound := 0;
              elsif All_Observations(I).Left(L).LeftOp = NE and then
                                           str1.all = str2.all then
                 MatchFound := 0;
              elsif All_Observations(I).Left(L).LeftOp = GT and then
                   (str1(1) not in '0'..'9'  or else
                    str2(1) not in '0'..'9'  or else
                    Integer'Value(str1.all) <= Integer'Value(str2.all)) then
                 -- we can have    Obj > Obj   , num > num,  num > [strruct]
                 -- we ONLY match the case of num > num
                 MatchFound := 0;
              elsif All_Observations(I).Left(L).LeftOp = GE and then
                    (str1(1) not in '0'..'9'  or else
                     str2(1) not in '0'..'9'  or else
                     Integer'Value(str1.all) < Integer'Value(str2.all)) then
                 -- we ONLY match the case of num >= num
                 MatchFound := 0;
              elsif All_Observations(I).Left(L).LeftOp = LE and then
                   (str1(1) not in '0'..'9'  or else
                    str2(1) not in '0'..'9'  or else
                    Integer'Value(str1.all) > Integer'Value(str2.all)) then
                 -- we ONLY match the case of num <= num
                 MatchFound := 0;
              elsif All_Observations(I).Left(L).LeftOp = LT and then
                   (str1(1) not in '0'..'9'  or else
                    str2(1) not in '0'..'9'  or else
                    Integer'Value(str1.all) >= Integer'Value(str2.all)) then
                 -- we ONLY match the case of num < num
                 MatchFound := 0;
              end if;
            end;
          end if;
          end if;  -- L /= 1
          --
        end loop;   -- end of cycle examinating the Left side of the rule and collecting vars
        --
        -- if matchfound add the ruleresult to the tablesresult
        --  where ruleresult = observation right side + dollar replacements
        --
        if MatchFound > 0 and then
          All_Observations(MatchFound).Rlabels /= null then
          --
          RuleSize :=0;
          if All_Observations(MatchFound).Rlabels.all'Length >0 then
             RuleResult := new String_Table(1..10);
             RuleSize := All_Observations(MatchFound).Rlabels.all'Length;
          end if;
          for J in All_Observations(MatchFound).Rlabels.all'Range loop
           if All_Observations(MatchFound).Rlabels(J) /= null  and then
             All_Observations(MatchFound).Rlabels(J).all'Length >0 then
              if All_Observations(MatchFound).Rlabels(J)(1) /= '$' then
                RuleResult(J) := All_Observations(MatchFound).Rlabels(J);
              elsif  J = All_Observations(MatchFound).Rlabels.all'Last and then
                  All_Observations(MatchFound).Rlabels(J).all'Length > 1 and then
                  All_Observations(MatchFound).Rlabels(J)(1..2) = "$*"   then
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J .. J+DollarVectors(K).all'length-1) := DollarVectors(K).all;
                    RuleSize := RuleSize-1+DollarVectors(K).all'length;
                    exit;
                  end if;
                end loop;
              else
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J) := DollarValues(K);
                    exit;
                  end if;
                end loop;
--                if RuleResult(J) = null or else
--                    RuleResult(J).all'Length =0 then
--                  Runtime_Errors_Count := Runtime_Errors_Count +1;
--                  Runtime_Error_Num := InvalidAbstraction;
--                  Runtime_Error_Val :=0;
--                  raise UML_Error;
--                end if;
              end if;
            end if;
          end loop;
          RulePrev := RuleResult;
          RuleResult := new String_Table'(RuleResult(1..RuleSize));
          Free(RulePrev);
          Prev := TablesResult;
          --   TBD:  avoid duplicates ...
          TablesResult :=
             new String_Tables_Vector'(RuleResult & TablesResult.all);
          if Prev /= Empty_String_Tables_Vector_Ref then
                Free(Prev);
          end if;
        else
          for V in  1.. DOLLARCOUNT loop
            Free(DOLLARVALUES(DOLLARCOUNT));
          end loop;
        end if;
        --
      end loop;  -- for all Observation rules
    end;  
    --
    end loop;   -- for all signals
    --   MISSING:  when no signals, generate abstract labels from object name or discaded event
    --
    return TablesResult;
  end AbstractView;


  ---------------------------------------------------------------------------
  -- gived the currently evolving object returns the abstract labels which do not 
  --  neither from the trigger (accepted or discarded) nor from the signals generated.
  -- E.g.   Action    Src: ->  Src
  -- E.g.   Action    $1: ->  $1
  -- E.g.   Action    *: ->  xx
  ---------------------------------------------------------------------------
  function AbstractEvolvingView (Current_Chart: Natural) return String_Tables_Vector_Ref is
      --
      -- Observation  is record
      --     Kind: AbstractionKind := BothKinds;           -- State/Action
      --     LTerm: String_Ref;                            -- src/$o/*/null
      --     LPartner: String_Ref;                         -- target/$o/*/null
      --     LOp: String_Ref;                              -- event/$o/ obj (InState clause)
      --     LMode: String_Ref;                            --   unused
      --     LArgs: String_Table_Ref := Empty_String_Table_Ref; -- [arg1,arg2]/ (Top.S1.s2)
      --------  additional "and" part    --  ONLY for  State observations
      --     IdsL: String_Table_Ref := Empty_String_Table_Ref;  -- [obj1,var]/$v/literal/
      --     LeftOp: BinOp;                                     --  EQ/NE/LT/GTL/LE/GE
      --     IdsR: String_Table_Ref :=  Empty_String_Table_Ref; -- [obj2,var]/$v/literal
      --     RLabels: String_Table_Ref := Empty_String_Table_Ref; -- [mainlabel,aarg1,aarg2]
      -- ennd record
      --
      TablesResult: String_Tables_Vector_Ref :=  Empty_String_Tables_Vector_Ref;
      Prev: String_Tables_Vector_Ref;
      --
      RulePrev: String_Table_Ref;
      RuleResult: String_Table_Ref;
      DollarNames: String_Table(1..10);
      DollarValues: String_Table(1..10);
      DollarVectors: String_Tables_Vector(1..10);
      RuleSize: Natural;
      DollarCount: Natural;
      II: Natural :=1;
      MatchFound: Natural :=0;
  begin
      --
      for I in All_Observations.all'Range loop
        --
        MatchFound :=I;
        DollarCount :=0;    
        for L in All_Observations(I).Left'Range loop
        --
        if L = 1 then
        --
        if All_Observations(I).Kind /= ActionKind then
          -- Skip this rule
          MatchFound :=0;   -- we give up
          DollarCount :=0;
        end if;
        --
        -- CHECK SOURCE OBJECT
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LTerm /= null and then
           All_Observations(I).Left(L).LTerm(1) /= '$' and then
           All_Observations(I).Left(L).LTerm(1) /= '*' and then
           All_Observations(I).Left(L).LTerm.all /=
                All_Charts(Current_Chart).Name.all then
           MatchFound :=0;  -- we give up
        end if;
        -- SAVE MATCHING OBJECT IN VARS
        if MatchFound >0 and then   -- not already given up
            All_Observations(I).Left(L).LTerm /= null  and then
            All_Observations(I).Left(L).LTerm(1) = '$' then
          DollarCount := DollarCount+1;
          Dollarnames(DollarCount) := All_Observations(I).Left(L).LTerm;
          Dollarvalues(DollarCount) := new String'(All_Charts(Current_Chart).Name.all);
        end if;
        --
       -- CHECK TARGET OBJECT
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LPartner /= null then
           MatchFound :=0;  -- we give up
        end if;
       -- CHECK SIGNAL
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LOp /= null then
           MatchFound :=0;  -- we give up
        end if;
        --
        else  -- L /= 1
        --
        --    CHECK    $i = $j  or   $i /= step
        --
        if MatchFound >0 and then
          All_Observations(I).Left(L).LeftOp /= NOOP then
          declare
            str1, str2: String_Ref;
            N : Natural := 0;
          begin
            if All_Observations(I).Left(L).IdsL(1)(1)='$' then
              for J in 1..DollarCount loop
                 if DollarNames(J).all = All_Observations(I).Left(L).IdsL(1).all then
                    N := J;
                    exit;
                 end if;
              end loop;
              if N = 0 then 
                 Runtime_Errors_Count := Runtime_Errors_Count +1;
                 Runtime_Error_Num := InvalidAbstraction;
                 Runtime_Error_Val :=0;
                 raise UML_Error; 
              end if;
              str1 := DollarValues(N);
            else
              str1 := All_Observations(I).Left(L).IdsL(1);
            end if;
            N :=0;
            if All_Observations(I).Left(L).IdsR(1)(1)='$' then
              for J in 1..DollarCount loop
                 if DollarNames(J).all = All_Observations(I).Left(L).IdsR(1).all then
                    N := J;
                    exit;
                 end if;
              end loop;
              if N = 0 then 
                 Runtime_Errors_Count := Runtime_Errors_Count +1;
                 Runtime_Error_Num := InvalidAbstraction;
                 Runtime_Error_Val :=0;
                 raise UML_Error; 
              end if;
              str2 := DollarValues(N);
            else
              str2 := All_Observations(I).Left(L).IdsR(1);
            end if;
            if All_Observations(I).Left(L).LeftOp = EQ and then
               str1.all /= str2.all then
               MatchFound := 0;
            elsif All_Observations(I).Left(L).LeftOp = NE and then
               str1.all = str2.all then
               MatchFound := 0;
            end if;
          end;
        end if;
        end if;  -- L = 1
        --
        end loop;  -- END CYCLE ON LEFT_SIDE COMPONENTS
        --
        -- if matchfound add the ruleresult to the tablesresult
        --  where ruleresult = observation right side + dollar replacements
        --
        if MatchFound > 0 and then
          All_Observations(MatchFound).Rlabels /= null then
          RuleSize :=0;
          if All_Observations(MatchFound).Rlabels.all'Length >0 then
             RuleResult := new String_Table(1..10);
             RuleSize := All_Observations(MatchFound).Rlabels.all'Length;
          end if;
          for J in All_Observations(MatchFound).Rlabels.all'Range loop
            if All_Observations(MatchFound).Rlabels(J) /= null  and then
                All_Observations(MatchFound).Rlabels(J).all'Length >0 then
              --
              if All_Observations(MatchFound).Rlabels(J)(1) /= '$' then
                 RuleResult(J) := All_Observations(MatchFound).Rlabels(J);
              elsif  J = All_Observations(MatchFound).Rlabels.all'Last and then
                  All_Observations(MatchFound).Rlabels(J).all'Length > 1 and then
                  All_Observations(MatchFound).Rlabels(J)(1..2) = "$*"   then
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J .. J+DollarVectors(K).all'length-1) := DollarVectors(K).all;
                    RuleSize := RuleSize-1+DollarVectors(K).all'length;
                    exit;
                  end if;
                end loop;
              else
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J) := DollarValues(K);
                    exit;
                  end if;
                end loop;
                if RuleResult(J) = null or else
                      RuleResult.all'Length =0 then
                  Runtime_Errors_Count := Runtime_Errors_Count +1;
                  Runtime_Error_Num := InvalidAbstraction;
                  Runtime_Error_Val :=0;
                  raise UML_Error;
                end if;
              end if;
            end if;
          end loop;
          RulePrev := RuleResult;
          RuleResult := new String_Table'(RuleResult(1..RuleSize));
          Free(RulePrev);
          Prev := TablesResult;
          TablesResult :=
             new String_Tables_Vector'(RuleResult & TablesResult.all);
          if Prev /= Empty_String_Tables_Vector_Ref then
            Free(Prev);
          end if;
        else
          for V in  1.. DOLLARCOUNT loop
            Free(DOLLARVALUES(DOLLARCOUNT));
          end loop;
        end if;
        --
      end loop;  -- for all Observation rules
    --
    return TablesResult;
    --
  end AbstractEvolvingView;

  ---------------------------------------------------------------------------
  -- gived the encoding a trigger beingdiscared by the stuttering evolution
  -- returns the table of abstract events corresponding to this ground lostevent
  -- 
  -- LR-template   "ERR.*"   matches lostevent!!! implicitly
  -- LR-template   "ERR.$1   matches lostevent!!! implicitly
  -- LR-template   "SRC:ERR.*"  matches lostevent!!! implicitly
  -- LR-template   "SRC:ERR.$1" matches lostevent!!! implicitly
  -- LR-template   "$0:ERR.$1" matches lostevent!!! implicitly
  -- LR-template   "$0:lostevent"  matches lostevent!!! implicitly
  -- LR-template   "SRC:lostevent"  matches lostevent!!! implicitly
  -- LR-template   lostevent(ev)  matches lostevent(ev,$*) per any args
  --  --
  -- LR-template   "SRC:*.$1"   DOES NOT matches lostevent!!! implicitly
  -- LR-template   "SRC:*.*"    DOES NOT matches lostevent!!! implicitly
  -- LR-template   "SRC:$0.$1"  DOES NOT matches lostevent!!! implicitly
  -- LR-template   "SRC:*"    DOES NOT matches lostevent!!! implicitly
  -- LR-template   "SRC:$1"   DOES NOT matches lostevent!!! implicitly
  -- LR-template   "SRC:"   DOES NOT matches lostevent!!! implicitly
  -- LR-template   "$0:$1.lostevent"   DOES NOT matches lostevent!!! implicitly
  -- LR-template   "*:*.lostevent"   DOES NOT matches lostevent!!! implicitly
  ---------------------------------------------------------------------------
  --        The Signal is: (Lost_Event.Num_Key, ERR.chartindex, event.Num_key);
  ---------------------------------------------------------------------------
  function AbstractLostEventView (TheTrigger: Int64;
                         Current_Chart: Natural)  return String_Tables_Vector_Ref is
    --
    TablesResult: String_Tables_Vector_Ref :=  Empty_String_Tables_Vector_Ref;
    Prev: String_Tables_Vector_Ref;
      --
      The_Signal: Int_Table := Vectors_DB.Retrieve(TheTrigger);
      -- (1=> event, 2 => target, 3 => arg1)
      --
      -- Observation  is record
      --     Kind: AbstractionKind := BothKinds;           -- State/Action
      --     LTerm: String_Ref;                            -- src/$o/*/null
      --     LPartner: String_Ref;                         -- target/$o/*/null
      --     LOp: String_Ref;                              -- event/$o/ obj (InState clause)
      --     LMode: String_Ref;                            --   unused
      --     LArgs: String_Table_Ref := Empty_String_Table_Ref; -- [arg1,arg2]/ (Top.S1.s2)
      --------  additional "and" part    --  ONLY for  State observations
      --     IdsL: String_Table_Ref := Empty_String_Table_Ref;  -- [obj1,var]/$v/literal/
      --     LeftOp: BinOp;                                     --  EQ/NE/LT/GTL/LE/GE
      --     IdsR: String_Table_Ref :=  Empty_String_Table_Ref; -- [obj2,var]/$v/literal
      --     RLabels: String_Table_Ref := Empty_String_Table_Ref; -- [mainlabel,aarg1,aarg2]
      -- ennd record
      --
      RulePrev: String_Table_Ref;
      RuleResult: String_Table_Ref;
      DollarNames: String_Table(1..10);
      DollarValues: String_Table(1..10);
      DollarVectors: String_Tables_Vector(1..10);
      RuleSize: Natural;
      DollarCount: Natural;
      II: Natural :=1;
      MatchFound: Natural :=0;
    begin
      --
      for I in All_Observations.all'Range loop
        --
        MatchFound :=I;
        DollarCount :=0;
        --
        for L in All_Observations(I).Left'Range loop
        --
        if L = 1 then
        --
        if All_Observations(I).Kind /= ActionKind then
          -- Skip this rule
          MatchFound :=0;   -- we give up
          DollarCount :=0;
        end if;
        --
        -- CHECK SOURCE OBJECT
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LTerm /= null and then
           All_Observations(I).Left(L).LTerm(1) /= '$' and then
           All_Observations(I).Left(L).LTerm(1) /= '*' and then
           All_Observations(I).Left(L).LTerm.all /=
                All_Charts(Current_Chart).Name.all then
           MatchFound :=0;  -- we give up
        end if;
        -- SAVE MATCHING OBJECT IN VARS
        if MatchFound >0 and then   -- not already given up
            All_Observations(I).Left(L).LTerm /= null  and then
            All_Observations(I).Left(L).LTerm(1) = '$' then
          DollarCount := DollarCount+1;
          Dollarnames(DollarCount) := All_Observations(I).Left(L).LTerm;
          Dollarvalues(DollarCount) := new String'(All_Charts(Current_Chart).Name.all);
        end if;
        --
        -- CHECK TARGET OBJECT
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LPartner /= null and then
           All_Observations(I).Left(L).LPartner.all /=  "ERR" then
           MatchFound :=0;  -- we give up
        end if;
        if MatchFound >0 and then 
           All_Observations(I).Left(L).LPartner = null and then
           All_Observations(I).Left(L).LOp = null then
           MatchFound :=0;  -- we give up
        end if;
        --
        -- CHECK LOSTEVENT CLAUSE
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LOp /= null and then
           All_Observations(I).Left(L).LOp.all /= "lostevent" then
           MatchFound :=0;  -- we give up
        end if;
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LPartner = null  and then 
           All_Observations(I).Left(L).LOp = null then
           MatchFound :=0;  -- we give up
        end if;
        --
        -- SAVE MATCHING LOSTEVENT IN VARS
        if MatchFound >0 and then   -- not already given up
            All_Observations(I).Left(L).LOp /= null  and then
            All_Observations(I).Left(L).LOp(1) = '$' then
          --    ERR.$1 ->  error($1) 
          DollarCount := DollarCount+1;
          DollarNames(DollarCount) := All_Observations(I).Left(L).LOp;
          Dollarvalues(DollarCount) := new String'("lostevent");
        end if;
        -- CHECK FIRST LOSTEVENT PARAM AS LOST EVENT
        if MatchFound >0 and then   -- not already given up
             All_Observations(I).Left(L).LArgs /= null and then
             All_Observations(I).Left(L).LArgs.all'Length >0  and then
             All_Observations(I).Left(L).LArgs(1) /= null and then
             All_Observations(I).Left(L).LArgs(1).all'Length >0 and then
             All_Observations(I).Left(L).LArgs(1)(1) /= '$' and then
             All_Observations(I).Left(L).LArgs(1)(1) /= '*' and then
               All_Observations(I).Left(L).LArgs(1).all /=
                 All_Events(The_Signal(1)).Name.all then
           MatchFound :=0;  -- we give up
        end if;
        --  SAVE LOST EVENT IN VARS
        if MatchFound >0 and then   -- not already given up
             All_Observations(I).Left(L).LArgs /= null and then
             All_Observations(I).Left(L).LArgs.all'Length >0  and then
             All_Observations(I).Left(L).LArgs(1) /= null and then
             All_Observations(I).Left(L).LArgs(1).all'Length >0 and then
             All_Observations(I).Left(L).LArgs(1)(1) = '$' then
              DollarCount := DollarCount+1;
              DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(1);
              DollarValues(DollarCount) := new String'(All_Events(The_Signal(1)).Name.all);
              -- The following is just a Ack to avoid errors in case of "$*"
              DollarVectors(DollarCount) := 
                 new String_Table'(1 => new String'(All_Events(The_Signal(1)).Name.all));
        end if;
        --
        --
        -- we have a  (event, target,arg1,arg2) signal as discarded trigger
        -- and an "lostevent(event,  ...)" left side of observation
        --
        --
        -- CHECK NOT SURELY FAR TOO MANY TEMPLATE PARAMS
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LArgs /= null and then
           All_Observations(I).Left(L).LArgs.all'Length > (The_Signal'Length-2) +1 then
           --  abstract args count-1 >  ground args count +1
           --    abstract lostevent(foo,a,b)  does not match  lost foo()
           MatchFound :=0;  -- we give up
           --
        --  CHECK JUST ONE MORE TEMPLATE PARAM ONLY IF LAST IS '*'
        elsif  MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LArgs /= null and then
           All_Observations(I).Left(L).LArgs.all'Length-1 = (The_Signal'Length-2)+1 and then
           (All_Observations(I).Left(L).LArgs.all'Length-1 = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length)(1) /= '*' ) and then
           (All_Observations(I).Left(L).LArgs.all'Length-1 = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "$*" ) then
           --  LR  args count <  ground args count and last LR arg /= '*'
           --  LR lostevent(foo,$1)  does not match lost foo()
           --  but LR lostevent(foo,$*)   matches ground lost foo() 
           --  and LR lostevent(foo,a,$*)   matches ground lost foo(a) 
           --  but LR lostevent(foo,*)   matches ground lost foo()  ?? (3.5 compatibitity)
           --  and LR lostevent(foo,a,*)   matches ground lost foo(a) ?? (3.5 compatibility)
           MatchFound :=0;  -- we give up
        end if;
        --
        --  CHECK NOT TOO FEW TERMPLATE PARAMS, UNLESS LAST IS '*' or size=1
        if  MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LArgs /= null  and then
           All_Observations(I).Left(L).LArgs.all'Length-1 < (The_Signal'Length-2) and then
           All_Observations(I).Left(L).LArgs.all'Length-1 > 0 and then
           (All_Observations(I).Left(L).LArgs.all'Length = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length)(1) /= '*' ) and then
           (All_Observations(I).Left(L).LArgs.all'Length = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "$*" ) then
           --
           --  abstract args count <  ground args count and last abstract arg /= '*'
           --  abstract lostevent(foo,a)  does not match  lost foo(a,b)
           --  abstract lostevent(foo)   MATCHES  lost foo(a)
           MatchFound :=0;  -- we give up
        end if;
        -- CHECK ACTUAL MATCHING FOR TEMPLATE PARAMS, SAVING MATCHING VARS
        if MatchFound >0 and then   -- not already given up
          All_Observations(I).Left(L).LArgs /= null  then
          for J in 2 .. All_Observations(I).Left(L).LArgs.all'Last loop
            if All_Observations(I).Left(L).LArgs(J) /= null  and then
               All_Observations(I).Left(L).LArgs(J).all'Length >1 and then
                All_Observations(I).Left(L).LArgs(J)(1..2) = "$*" then
              DollarCount := DollarCount+1;
              DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(J);
              declare
                ST: String_Table(1..Args_Count(The_Signal)-(J-1)+1);
              begin
                for K in 1..ST'Last loop
                  ST(K) := new String'(Arg_Image(The_Signal,(J-1)-1+K,Current_Chart));
                end loop;
                DollarVectors(DollarCount) := new String_Table'(ST);
              end;
              --
            elsif All_Observations(I).Left(L).LArgs(J) /= null  and then
               All_Observations(I).Left(L).LArgs(J).all'Length >0 and then
                All_Observations(I).Left(L).LArgs(J)(1) = '$' then
              DollarCount := DollarCount+1;
              DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(J);
              DollarValues(DollarCount) := 
                 new String'(Arg_Image(The_Signal,J-1,Current_Chart));
              --
            elsif All_Observations(I).Left(L).LArgs(J) /= null  and then
                All_Observations(I).Left(L).LArgs(J)(1) /= '$' and then
                All_Observations(I).Left(L).LArgs(J)(1) /= '*' and then
                Arg_Image(The_Signal,J-1,Current_Chart) /= 
                   Normalized_Literal(All_Observations(I).Left(L).LArgs(J).all) then
              --
              MatchFound :=0;     --  we give up
              exit;
            end if;
          end loop;
        end if;
        --
        else  -- L /= 1
        --
        --    CHECK    $i = $j  or   $i /= step
        --
        if MatchFound >0 and then
          All_Observations(I).Left(L).LeftOp /= NOOP then
          declare
            str1, str2: String_Ref;
            N : Natural := 0;
          begin
            if All_Observations(I).Left(L).IdsL(1)(1)='$' then
              for J in 1..DollarCount loop
                 if DollarNames(J).all = All_Observations(I).Left(L).IdsL(1).all then
                    N := J;
                    exit;
                 end if;
              end loop;
              if N = 0 then 
                 Runtime_Errors_Count := Runtime_Errors_Count +1;
                 Runtime_Error_Num := InvalidAbstraction;
                 Runtime_Error_Val :=0;
                raise UML_Error; 
              end if;
              str1 := DollarValues(N);
            else
              str1 := All_Observations(I).Left(L).IdsL(1);
            end if;
            N :=0;
            if All_Observations(I).Left(L).IdsR(1)(1)='$' then
              for J in 1..DollarCount loop
                 if DollarNames(J).all = All_Observations(I).Left(L).IdsR(1).all then
                    N := J;
                    exit;
                 end if;
              end loop;
              if N = 0 then 
                 Runtime_Errors_Count := Runtime_Errors_Count +1;
                 Runtime_Error_Num := InvalidAbstraction;
                 Runtime_Error_Val :=0;
                raise UML_Error; 
              end if;
              str2 := DollarValues(N);
            else
              str2 := All_Observations(I).Left(L).IdsR(1);
            end if;
            if All_Observations(I).Left(L).LeftOp = EQ and then
               str1.all /= str2.all then
               MatchFound := 0;
            elsif All_Observations(I).Left(L).LeftOp = NE and then
               str1.all = str2.all then
               MatchFound := 0;
            end if;
          end;
        end if;
        end if;  -- L = 1
        --
        end loop;  -- END CYCLE ON LEFT_SIDE COMPONENTS
        --
        -- if matchfound add the ruleresult to the tablesresult
        --  where ruleresult = observation right side + dollar replacements
        --
        if MatchFound > 0 and then
          All_Observations(MatchFound).Rlabels /= null then
          RuleSize :=0;          
          if All_Observations(MatchFound).Rlabels.all'Length >0 then
             RuleResult := new String_Table(1..10);
             RuleSize := All_Observations(MatchFound).Rlabels.all'Length;
          end if;
          for J in All_Observations(MatchFound).Rlabels.all'Range loop
            if All_Observations(MatchFound).Rlabels(J) /= null  and then
                All_Observations(MatchFound).Rlabels(J).all'Length >0 then
              --
              if All_Observations(MatchFound).Rlabels(J)(1) /= '$' then
                 RuleResult(J) := All_Observations(MatchFound).Rlabels(J);
              elsif  J = All_Observations(MatchFound).Rlabels.all'Last and then
                  All_Observations(MatchFound).Rlabels(J).all'Length > 1 and then
                  All_Observations(MatchFound).Rlabels(J)(1..2) = "$*"   then
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J .. J+DollarVectors(K).all'length-1) := DollarVectors(K).all;
                    RuleSize := RuleSize-1+DollarVectors(K).all'length;
                    exit;
                  end if;
                end loop;
              else
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J) := DollarValues(K);
                    exit;
                  end if;
                end loop;
                if RuleResult(J) = null or else
                      RuleResult.all'Length =0 then
                  Runtime_Errors_Count := Runtime_Errors_Count +1;
                  Runtime_Error_Num := InvalidAbstraction;
                  Runtime_Error_Val :=0;
                  raise UML_Error;
                end if;
              end if;
            end if;
          end loop;
          RulePrev := RuleResult;
          RuleResult := new String_Table'(RuleResult(1..RuleSize));          
          Free(RulePrev);
          Prev := TablesResult;
          TablesResult :=
             new String_Tables_Vector'(RuleResult & TablesResult.all);
          if Prev /= Empty_String_Tables_Vector_Ref then
            Free(Prev);
          end if;
        else
          for V in  1.. DOLLARCOUNT loop
            Free(DOLLARVALUES(DOLLARCOUNT));
          end loop;
        end if;
        --
      end loop;  -- for all Observation rules
    --
    return TablesResult;  
    --
  end AbstractLostEventView;



  -- called when an object evolution is the result of triggering event.
  -- generates the abstract lables associated to  accept  Action rules
  --  which have the form:
  --     obj:accept(events, arg1,arg2)   (obj can be omitted)
  --     arg1,arg2 are the user parameters of the trigger,
  --     in case of Operations we should discard the first _caller arg.
  --
  function AbstractTriggerView (TheTrigger: Int64;
                         Current_Chart: Natural)  return String_Tables_Vector_Ref is
      --
      The_Signal: Int_Table := Vectors_DB.Retrieve(TheTrigger);
      -- (1=> event, 2 => target, 3... args)
      -- (1=> event, 2 => target, 3 => _caller, 4.. args)
      --
      -- Observation  is record
      --     Kind: AbstractionKind := BothKinds;           -- State/Action
      --     LTerm: String_Ref;                            -- src/$o/*/null
      --     LPartner: String_Ref;                         -- target/$o/*/null
      --     LOp: String_Ref;                              -- event/$o/ obj (InState clause)
      --     LMode: String_Ref;                            --   unused
      --     LArgs: String_Table_Ref := Empty_String_Table_Ref; -- [arg1,arg2]/ (Top.S1.s2)
      --------  additional "and" part    --  ONLY for  State observations
      --     IdsL: String_Table_Ref := Empty_String_Table_Ref;  -- [obj1,var]/$v/literal/
      --     LeftOp: BinOp;                                     --  EQ/NE/LT/GTL/LE/GE
      --     IdsR: String_Table_Ref :=  Empty_String_Table_Ref; -- [obj2,var]/$v/literal
      --     RLabels: String_Table_Ref := Empty_String_Table_Ref; -- [mainlabel,aarg1,aarg2]
      -- end record
      -- 
      RulePrev: String_Table_Ref;
      RuleResult: String_Table_Ref;
      Prev: String_Tables_Vector_Ref;
      Result: String_Tables_Vector_Ref := new String_Tables_Vector(1..0);
      DollarNames: String_Table(1..10);
      DollarValues: String_Table(1..10);
      DollarVectors: String_Tables_Vector(1..10);
      RuleSize: Natural;
      DollarCount: Natural;
      II: Natural :=1;
      MatchFound: Natural :=0;
    begin
      --
      for I in All_Observations.all'Range loop    -- CYCLE on RULES
        --
        MatchFound :=I;
        DollarCount :=0;
        --
        for L in All_Observations(I).Left'Range loop   -- CYCLE ON LEFT ELEMENTS of RULE
        --
        if L = 1 then
        if All_Observations(I).Kind /= ActionKind then
          -- Skip this rule
          MatchFound :=0;   -- we give up
          DollarCount :=0;
        end if;
        --
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LTerm /= null and then
           All_Observations(I).Left(L).LTerm(1) /= '$' and then
           All_Observations(I).Left(L).LTerm(1) /= '*' and then
           All_Observations(I).Left(L).LTerm.all /=
                All_Charts(Current_Chart).Name.all then
           MatchFound :=0;  -- we give up
        end if;
        if MatchFound >0 and then   -- not already given up
            All_Observations(I).Left(L).LTerm /= null  and then
            All_Observations(I).Left(L).LTerm(1) = '$' then
          DollarCount := DollarCount+1;
          Dollarnames(DollarCount) := All_Observations(I).Left(L).LTerm;
          Dollarvalues(DollarCount) := All_Charts(Current_Chart).Name;
        end if;
        --
        -- no LParter
        --
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LOp /= null and then
           All_Observations(I).Left(L).LOp.all /= "accept" then
           --  accept is the only matching LOp
           MatchFound :=0;  -- we give up
        end if;
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LOp = null then
           --  accept is the only matching LOp
           MatchFound :=0;  -- we give up
        end if;
        if MatchFound >0 and then   -- not already given up
          (All_Observations(I).Left(L).LArgs = null or else
           All_Observations(I).Left(L).LArgs.all'Length =0) then
           --  accept(event, ....)   event is MANDATORY otherwise the rule is ignored
           MatchFound :=0;  -- we give up
        end if;
        --            MATCHING GROUND ACTIONS PARAMETERS
        -- GROUND ACTION  MATCHES                               DOES NOT MATCH
        --        e       accept(e),accept(*),accept(e,*)               accept(e,$v)
        --       e(a)     accept(e),accept(*),accept(e,*),accept(e,a,*) accept(e),accept(e,a,b)
        --
        --  abstract args count                      ground args count
        --  All_Observations(I).Left(L).LArgs.all'Length     (The_Signal'Length-2) (SIGNALS)
        --  All_Observations(I).Left(L).LArgs.all'Length     (The_Signal'Length-3) (OPERATIONS)
        --                                             Args_Count(The_Signal)
        -- we have a  (event, target,arg1,arg2) signal as trigger
        -- we have a  (event, target,caller,arg1,arg2) signal as trigger
        -- and an "accept(event,  ...)" left side of observation
        -- LArgs.all'Length is >= 1
        --
       -- CHECK FIRST EVENTNAME PARAM 
       if MatchFound >0 and then
          The_Signal'Length = 0 then
           MatchFound :=0;  -- we give up    (trigger = "-")
       end if;
       if MatchFound >0 and then   -- not already given up
             All_Observations(I).Left(L).LArgs(1).all /=
                All_Events(The_Signal(1)).Name.all and then
             All_Observations(I).Left(L).LArgs(1).all /= "*"  and then
             All_Observations(I).Left(L).LArgs(1)(1) /= '$' then
           -- the first parameter must be '*' or '$v' or the eventname
           MatchFound :=0;  -- we give up
        end if;
        --
        -- CHECK NOT SURELY FAR TOO MANY TEMPLATE PARAMS   ( >=2)
        if MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LArgs.all'Length-1 > Args_Count(The_Signal) +1 then
           --  abstract args count-1 >  ground args count +1
           --    abstract accept(foo,a,b)  does not match  ground foo()
           MatchFound :=0;  -- we give up
           --
        --  CHECK JUST ONE MORE TEMPLATE PARAM ONLY IF LAST IS '*'  or "$*"
        elsif  MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LArgs.all'Length-1 = Args_Count(The_Signal)+1 and then
           (All_Observations(I).Left(L).LArgs.all'Length-1 = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length)(1) /= '*' ) and then
           (All_Observations(I).Left(L).LArgs.all'Length-1 = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "$*" ) then
           --
           --  abstract args count <  ground args count and last abstract arg /= '*'
           --    abstract accept(foo)  does not match  ground foo(a)
           --    abstract accept(foo,a)  does not match  ground foo(a,b)
           --  but accept(foo,*)   matches foo()
           --  and accept(foo,a,*)   matches foo(a)
           MatchFound :=0;  -- we give up
        end if;
        --
        --  CHECK NOT TOO FEW TERMPLATE PARAMS, UNLESS LAST IS '*'
        if  MatchFound >0 and then   -- not already given up
           All_Observations(I).Left(L).LArgs /= null  and then
           All_Observations(I).Left(L).LArgs.all'Length-1 > 0 and then
           All_Observations(I).Left(L).LArgs.all'Length-1 < Args_Count(The_Signal) and then
           (All_Observations(I).Left(L).LArgs.all'Length = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length)(1) /= '*' ) and then
           (All_Observations(I).Left(L).LArgs.all'Length = 0 or else
           All_Observations(I).Left(L).LArgs(All_Observations(I).Left(L).LArgs.all'Length).all /= "$*" ) then
           --
           --  abstract args count <  ground args count and last abstract arg /= '*'
           --    LR accept(foo,a)  does not match  ground trigger foo(a,b)
           --    LR accept(foo)  MATCHES  ground trigger foo(a)
           MatchFound :=0;  -- we give up
        end if;
        --
        -- DO NOT OBSERVE RETURN TRIGGERS UNLESS EXPLICITLY SPECIFIED
        if MatchFound >0 and then   -- not already given up
           (All_Observations(I).Left(L).LArgs(1).all = "*"  or else
                All_Observations(I).Left(L).LArgs(1)(1) = '$')  and then
            All_Events(The_Signal(1)).Name.all = "return"  then
            --  return(a,b)   ....  not maching accept(*,..)  or accept($v,..)
           MatchFound :=0;  -- we give up
        end if;
        --
        -- CHECK ACTUAL MATCHING FOR TEMPLATE PARAMS, SAVING MATCHING VARS
        if MatchFound >0 then   -- not already given up
           if All_Observations(I).Left(L).LArgs(1)(1) = '$' then 
              DollarCount := DollarCount+1;
              DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(1);
              DollarValues(DollarCount) := new String'(All_Events(The_Signal(1)).Name.all);
           end if;
        end if;
        -- CHECK ACTUAL MATCHING FOR TEMPLATE PARAMS, SAVING MATCHING VARS
        if MatchFound >0 then   -- not already given up
          for J in 2 .. All_Observations(I).Left(L).LArgs.all'Last loop
            --            
             if All_Observations(I).Left(L).LArgs(J) /= null  and then
               All_Observations(I).Left(L).LArgs(J).all'Length >1 and then
                All_Observations(I).Left(L).LArgs(J)(1..2) = "$*" then
              DollarCount := DollarCount+1;
              DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(J);
              declare
                ST: String_Table(1..Args_Count(The_Signal)-(J-1)+1);
              begin
                for K in 1..ST'Last loop
                  ST(K) := new String'(Arg_Image(The_Signal,(J-1)-1+K,Current_Chart));
                end loop;
                DollarVectors(DollarCount) := new String_Table'(ST);
              end;
              --
            elsif All_Observations(I).Left(L).LArgs(J) /= null  and then
               All_Observations(I).Left(L).LArgs(J).all'Length >0 and then
                All_Observations(I).Left(L).LArgs(J)(1) = '$' then
              DollarCount := DollarCount+1;
              DollarNames(DollarCount) := All_Observations(I).Left(L).LArgs(J);
              DollarValues(DollarCount) := 
                  new String'(Arg_Image(The_Signal,J-1,Current_Chart));
            elsif All_Observations(I).Left(L).LArgs(J) /= null  and then
                All_Observations(I).Left(L).LArgs(J)(1) /= '$' and then
                All_Observations(I).Left(L).LArgs(J)(1) /= '*' and then
                  Arg_Image(The_Signal,J-1,Current_Chart) /= 
                   Normalized_Literal(All_Observations(I).Left(L).LArgs(J).all) then
              --
              MatchFound :=0;     --  we give up
              exit;
            end if;
          end loop;        -- CYCLE ON EVENT ARGS
        end if;
        --
        else  --  not L = 1 
        --
        --    CHECK    $i = $j  or   $i /= step
        --
        if MatchFound >0 and then
          All_Observations(I).Left(L).LeftOp /= NOOP then
          declare
            str1, str2: String_Ref;
            N : Natural := 0;
          begin
            if All_Observations(I).Left(L).IdsL(1)(1)='$' then
              for J in 1..DollarCount loop
                 if DollarNames(J).all = All_Observations(I).Left(L).IdsL(1).all then
                    N := J;
                    exit;
                 end if;
              end loop;
              if N = 0 then 
                 Runtime_Errors_Count := Runtime_Errors_Count +1;
                 Runtime_Error_Num := InvalidAbstraction;
                 Runtime_Error_Val :=0;
                 raise UML_Error; 
              end if;
              str1 := DollarValues(N);
            else
              str1 := All_Observations(I).Left(L).IdsL(1);
            end if;
            N :=0;
            if All_Observations(I).Left(L).IdsR(1)(1)='$' then
              for J in 1..DollarCount loop
                 if DollarNames(J).all = All_Observations(I).Left(L).IdsR(1).all then
                    N := J;
                    exit;
                 end if;
              end loop;
              if N = 0 then 
                 Runtime_Errors_Count := Runtime_Errors_Count +1;
                 Runtime_Error_Num := InvalidAbstraction;
                 Runtime_Error_Val :=0;
                 raise UML_Error; 
               end if;
              str2 := DollarValues(N);
            else
              str2 := All_Observations(I).Left(L).IdsR(1);
            end if;
            if All_Observations(I).Left(L).LeftOp = EQ and then
               str1.all /= str2.all then
               MatchFound := 0;
            elsif All_Observations(I).Left(L).LeftOp = NE and then
               str1.all = str2.all then
               MatchFound := 0;
            end if;
          end;
        end if;
        --
        end if;  -- if L = 
        --
        end loop;     -- CYCLE ON LEFT ELEMENTS OF RULE
        --
        -- if matchfound add the RULEREsult to the tablesresult
        --  where ruleresult = observation right side + dollar replacements
        --
        if MatchFound > 0 and then
          All_Observations(MatchFound).Rlabels /= null then
          if All_Observations(MatchFound).Rlabels.all'Length >0 then
             RuleResult := new String_Table(1..10);
             RuleSize := All_Observations(MatchFound).Rlabels.all'Length;
          end if;
          for J in All_Observations(MatchFound).Rlabels.all'Range loop
           if All_Observations(MatchFound).Rlabels(J) /= null  and then
             All_Observations(MatchFound).Rlabels(J).all'Length >0 then
             --
              if All_Observations(MatchFound).Rlabels(J)(1) /= '$' then
                RuleResult(J) := All_Observations(MatchFound).Rlabels(J);
              elsif  J = All_Observations(MatchFound).Rlabels.all'Last and then
                  All_Observations(MatchFound).Rlabels(J).all'Length > 1 and then
                  All_Observations(MatchFound).Rlabels(J)(1..2) = "$*"   then
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J .. J+DollarVectors(K).all'length-1) := DollarVectors(K).all;
                    RuleSize := RuleSize-1+DollarVectors(K).all'length;
                    exit;
                  end if;
                end loop;
              else
                for K in 1..DollarCount loop
                  if DollarNames(K) /= null and then
                    DollarNames(K).all = All_Observations(MatchFound).Rlabels(J).all then
                    RuleResult(J) := DollarValues(K);
                    exit;
                  end if;
                end loop;
                if RuleResult(J) = null or else
                    RuleResult.all'Length =0 then
                  Runtime_Errors_Count := Runtime_Errors_Count +1;
                  Runtime_Error_Num := InvalidAbstraction;
                  Runtime_Error_Val :=0;
                  raise UML_Error;
                end if;
              end if;
            end if;
          end loop;
          RulePrev := RuleResult;
          RuleResult := new String_Table'(RuleResult(1..RuleSize));
          Free(RulePrev);
          Prev:= Result;
          Result := new String_Tables_Vector'(RuleResult & Result.all);
          Free(Prev);
        end if;
        --
      end loop;  --  CYCLE ON ALL RULES
    --
    return Result;
  end AbstractTriggerView;

  function Apply_Stuttering(
     Active_Object: Positive;    -- the current active object  (self)
     Current_Vars: Int_Table;    -- vars data;
     Current_Queue: Int64_Table; -- queue data
     Current_States: States_Table;  -- states data
     Trigger: Int_Table;            -- trigger data
     Trigger_Pos: Natural           --  it is always > 0
     ) return Object_Runtime_Evolution is
    --
    Current_Chart: Natural := Active_Charts(Active_Object);
    Signals_Count: Natural :=0;
    New_ORC: Object_Runtime_Configuration(Current_States'Length,
                                         Current_Vars'Length);
    New_Queue: Int64;      
    Result: Object_Runtime_Evolution;
  begin
    --
    declare
      All_Signals: Int64_Table(1..1);
      All_Signals_Count: Natural :=0;
      Trimmed_Queue: Int64_Table(1..Current_Queue'Length-1);
      The_Sequence: Transitions_Table(1..0);
    begin
      --  remove the trigger from the queue
      --
      for I in 1..Trigger_Pos-1 loop
        Trimmed_Queue(I) := Current_Queue(I);
      end loop;
      for I in Trigger_Pos+1 .. Current_Queue'Length loop
        Trimmed_Queue(I-1) := Current_Queue(I);
      end loop;
      New_Queue := abs(Signals_DB.NickNum(Trimmed_Queue));
      --
      --  if requested, generate the LOSTEVENT signal
      --
      if ShowStuttering  then
        declare
          The_Signal: Int_Table(1..3);
          --    (Lost_Event.Num_Key, ERR.chartindex, event_index);
        begin
          All_Signals_Count:= 1;
          The_Signal(1) := Lost_Event.Num_Key;  -- (Num_key of Lost_Event)
          The_Signal(2) := 2;  -- (target of Lost_Event = ERR)
          The_Signal(3) := Trigger(1);   -- event really lost
          All_Signals(1) := abs(Vectors_DB.NickNum(The_Signal));
        end;
      end if;
      --
      Result.Evolving_Object := Active_Object;
      Result.Transitions :=  abs(Transitions_DB.NickNum(The_Sequence));
      Result.Trigger := Current_Queue(Trigger_Pos);
      declare
        Tmp1: String_Tables_Vector_Ref;
        Tmp2: String_Tables_Vector_Ref;
        Tmp3: String_Tables_Vector_Ref;
      begin
        Tmp1 := AbstractEvolvingView(Current_Chart);
        Tmp2 := AbstractLostEventView(Result.Trigger,Current_Chart);
        if Tmp1 = null or else Tmp1.all'Length =0 then
           Tmp3 := Tmp2;
           Tmp2 := null;
        else
          Tmp3 := new String_Tables_Vector'(Tmp1.all & Tmp2.all);
        end if;
        Result.AbstractInfo.Labels :=  SortUnique(Tmp3);
        if Tmp1 /= null and then Tmp1.all'Length >0 then
           Free(Tmp1);
        end if;        
        if Tmp2 /= null and then Tmp2.all'Length >0 then
           Free(Tmp2);
        end if;        
      end;
      Result.Signals  := 
            abs(Signals_DB.NickNum(All_Signals(1..All_Signals_Count)));
    end;
    New_ORC.Self := Current_Chart;
    New_ORC.Current_States := Current_States;
    New_ORC.Current_Vars := Current_Vars;
    New_ORC.Current_Queue := New_Queue; 
    Result.Target :=  abs(ORC_DB.NickNum(New_ORC));
    return Result;
  end Apply_Stuttering;

 procedure ReInitialize_Model is
 begin
   null;
 end ReInitialize_Model;


 ------------------------------------------------------------------
  ------------------------------------------------------------------
  ------------------------------------------------------------------


 
 procedure Print_Evolution (This_Evolution: Transitions_Table;
                            Count: Positive) is
   This_Transition: Transition_ref ;
 begin
   Put (Integer'Image(Count) & ')' );
   --
   This_Transition := This_Evolution(1);
   --
   Put ("  (");
   Put (This_Transition.Source(1).FullName.all);
   for J in 2.. This_Transition.Source.all'Length loop
      Put ("," & This_Transition.Source(J).FullName.all);
   end loop;
   Put (")   - (");
   --
   Put (This_Transition.Label.all);
   --
   Put (") ->  (");
   Put (This_Transition.Target(1).FullName.all);
   for J in 2.. This_Transition.Target.all'Length loop
      Put ("," & This_Transition.Target(J).FullName.all);
   end loop;
   Put (") ");
   New_Line;
   --
   --
   for I in 2 .. This_Evolution'Length loop
     This_Transition := This_Evolution(I);
     Put ("    ");
     --
     Put ("  (");
     Put (This_Transition.Source(1).FullName.all);
     for J in 2.. This_Transition.Source.all'Length loop
        Put ("," & This_Transition.Source(J).FullName.all);
     end loop;
     Put (")   - (");
     --
     Put (This_Transition.Label.all);
     --
     Put (") ->  (");
     Put (This_Transition.Target(1).FullName.all);
     for J in 2.. This_Transition.Target.all'Length loop
        Put ("," & This_Transition.Target(J).FullName.all);
     end loop;
     Put (") ");
     New_Line;
   end loop;
   --
 end Print_Evolution;


 --------------------------------------------------------------------
 -- Given a table containing the data corresponding to a sequence
 -- of signals, returns a string corresponding the their image.
 -- Notice that a signal is represented by:
 --  a sequence of integers  e,c,p1..pn   (n > =0)    where
 --    "e" is the event numkey
 --    "c" is the target_chart index
 --    "p1..pn"  are the args values
 --
 -- the returned value is:  "chart.event(p1,..,pn);chart2.event2(p1...pn)"
 --
 --  If "explicit_Tau_Requested", the string "tau" should be returned
 --    in case of empty transitions, instead of "".
 --
 --  If "stuttering requested"  "OUT.lostevent(event)" should be returned
 --   in case of stuttering transitions.
 --
 --  If a variable V is assigned by the transition, and V is "observed",
 --   then "assign(var,val)" should be generated for the operation.
 --
 --  Assign events, if generated, should be always displayed. 
 --  they are implicitly always observed.
 --------------------------------------------------------------------
-- function Signals_Image  (Current_Signals: Int_Table;
--                          Current_Chart: Positive) return String is
--   I: Natural;
--   Result: String_Ref := new String'("");
--   This_Event: Event_Ref;
--   This_Target: Natural;
--   First: Boolean := True;
-- begin
--   if Current_Signals'Length = 0 then
--     if Explicit_Tau_Requested then
--         return "tau";
--     else
--       return "";
--     end if;
 --  else
 --     This_target := Current_Signals(Current_Signals'First+1);
 --  end if;
 ----  --
 --  --  if signals = [OUT.Lost_Event(event)]   
--   --  (if present, Lost_Event is necesarily the unique signal)
--   --
--   if This_Target=1  and then Current_Signals(1)=Lost_Event.Num_Key  then
--     if Lost_Event.Observed = True or not Set_Observations then
--       return "OUT.lostevent(" & All_Events(Current_Signals(3)).Name.all & ")";
--     else
----       if Explicit_Tau_Requested then
--         return "tau";
 ----      else
  --       return "";
  --     end if;
 --    end if;
--   end if;
 --  --
 --  I :=Current_Signals'First;
 --  while I in Current_Signals'Range loop
 --    This_target := Current_Signals(I+1);
--     This_Event := All_Events(Current_Signals(I));
 --    if not Set_Observations  or else
 --        Is_Observable(This_Target, Current_Signals(I)) then
 --      if not First then
 --        Result := Append (Result,";");
 --      end if;
--       First := False;
--       Result :=
--         Append (Result,
--             Signal_Image (
--               Current_Signals (I .. I + 1 + This_Event.Params.all'Length)));
--     end if;
--     I := I + This_Event.Params.all'Length +2;
--   end loop;
----  
----  PERCHE'  AND SET OBSERVATIONS?!?!??!  NON MI RICORDO
----  perche normalemente (in umc, totab senza obs) set observations = false 
---- e infatti si osserva tutto. 
--
----  IN GENERALE, nei charts e nelle evolution maps i TAU non vengono
----  espclicitamente inseriti, invece in totab ci vogliono.
---- da qui la necessita' di introdurre un flag di supporto
----
 --  if Result.all'Length = 0 and Explicit_Tau_Requested then
 --     Result := new String'("tau");
 ----  end if;
   --
 --  declare
 --    Static: String := Result.all;
 --  begin
 --    Free (Result);
 --    return Static;
 --  end;
 --end  Signals_Image;
 
  ----------------------------------------------------------------------
  -- Given a set of fireable transitions
  --   (i.e an element of Get_Fireable_transitions)
  -- return the number of different execution sequences it can originate
  -- Currently all the possible sequential ordering are considered
  -- al leading to different evolutions, however this number could
  -- be reduced if for, example there are no data conflicts and at most
  -- one signal il generated.
  ----------------------------------------------------------------------
  function Count_Combinations (These_Transitions_Nick: Int64)
                   return Natural is
     These_Transitions: Transitions_Table := 
          Transitions_DB.Retrieve(These_Transitions_Nick);
  begin
    if These_Transitions'Length =0 then
      return 0;
    else
      return Factorial(These_Transitions'Length);
    end if;
  end Count_Combinations;


  function Interferes (Active_Object: Positive;
                       T1: Transition_Ref;
                       T2: Transition_Ref) return Boolean is
     n1,n2: Natural;
     Current_Chart: Natural := Active_Charts(Active_Object);
     TheseInterferences: BoolMat_Ref := All_Charts(Current_Chart).ChartInterferences;
  begin
     n1 := T1.Num_Key;
     n2 := T2.Num_Key;
     if TheseInterferences(n1,n2) /= null then
        return TheseInterferences(n1,n2).all;
     end if; 
     -- due transizioni interferiscono se due azioni interferiscono
     -- due azioni interferiscono se:
     --- a)  sono signals/operations dirette al medesimo target
     --  b) sono due write della medesima class  var 
     --  c) una e' una write, e l'altra usa, come valore, parametro o target la stessa var
     --
     TheseInterferences(n1,n2) := new Boolean'(True);
     return True;
  end Interferes;

  ---------------------------------------------------------------------
  -- Data una Tabella di transizioni fireable,
  --  restituisce la lista degli enconding di tutte le possibile sequenzializzazioni
  --   degli elementi dell'insieme.
  --  Esse saranno, al massimo Factorial(Firing_Set'length)
  ---------------------------------------------------------------------
  function  Get_Firing_Sequences (Active_Object: Positive;
                                   Firing_Set: Transitions_Table_Ref)  return Int64_Table is

     Transitions_Count: Natural := Firing_Set'Length;
     Max_Sequences: Natural := Factorial(Transitions_Count);
     All_Encodings: Int64_Table(1..Max_Sequences); 
     Partial_Count:Natural := 1;
     subtype TheseTransitions is Transitions_Table(1..Transitions_Count);
     type VTAble is array (Natural range 1..Max_Sequences) of TheseTransitions;
     All_Sequences: VTAble;
     Base_Count: Natural :=1;
     isnice: Boolean := False;
     Theclass: Natural;
  begin
    --  empty firing-set exists
    if Firing_Set'Length = 0 then
       return All_Encodings(1..0);
    end if;
    --
    TheClass := All_Charts(Active_Charts(Active_Object)).ChartParent;
    isnice:= All_Classes(TheClass).NiceParallelism;
    if Firing_Set'Length =1 or isnice then
      All_Encodings(1) :=  abs(Transitions_DB.NickNum(Firing_Set.all));
      return All_Encodings(1..1);
      --
    end if;
    --
    --  build all the necessary sequences
    --
    All_Sequences(1)(1) := Firing_Set(1);
    Base_Count :=1;
    --
    --  for each element of the given set, incrementally add it to the generated sequences
    --
    for I in  2 .. Firing_Set'Last loop
       --
       Partial_Count := Base_Count;
       --
       -- step 1:  add the transition to the right of all previous base results.
       for J in 1..Partial_Count loop
          All_Sequences(J)(I) := Firing_Set(I);
       end loop;
       --
       -- step 2:
       -- moreover, for each transition Tk previously elaborated,
       --  if Ti and Tk do interfere,
       --   add Ti just before Tk, instead then at the end,
       --    DUPLICATING all the previous base sequences.
       --
       for K in 1..I-1 loop
         if Interferes(Active_Object, Firing_Set(K), Firing_Set(I)) then
             --
             -- add a duplicate of the original base sequences
             All_Sequences(Partial_Count+1.. Partial_Count+Base_Count) :=
                 All_Sequences(1..Base_Count);
             -- insert Ti before Tk in all duplicates,
             for L in Partial_Count+1.. Partial_Count+Base_Count loop
               -- find the position, shift to the right and add the Ti
               for M in 1..I-1 loop
                 null;
                 if All_Sequences(L)(M) = Firing_Set(K) then
                    All_Sequences(L)(M+1..I) := All_Sequences(L)(M..I-1);
                    All_Sequences(L)(M) := Firing_Set(I);
                    exit;
                 end if;
               end loop;
             end loop;
             Partial_Count := Partial_Count+Base_Count;
         end if;
       end loop;  -- for all interfering  elements already considered
       Base_Count := Partial_Count;
       --
    end loop;  -- for all elements of the set to be incrementaly added
    --
    -- build the encodings of the generated sequences
    --
    for I in 1..Partial_Count loop
       All_Encodings(I) :=  abs(Transitions_DB.NickNum(All_Sequences(I)));
    end loop;
    --
    return All_Encodings(1..Partial_Count);
    --
  end Get_Firing_Sequences;

  ------------------------------------------------------------------
  ------------------------------------------------------------------


function IsNested (s1,s2: State) return boolean is
begin
  if s1.FullName.all'Length > s2.FullName.all'Length and then
     s1.FullName.all(1..s2.FullName.all'Length) = s2.FullName.all and then
     s1.FullName.all(s2.FullName.all'Length+1) = '.' then
     return True;
  else
     return False;
  end if;
end IsNested;

function Compare (t1, t2: Transition) return PriorityResult is
   Source1: State renames t1.Source(1).all;
   Source2: State renames t2.Source(1).all;
begin
  -- Current_Trigger = null  means that we are simply charting
  -- all the transitions diagram, without taking in considerations
  --  actual guards, trigger and interactions with the environment.
  --
--  if Current_Trigger = null then
--     return  Uncomparable;
--  end if;
--
  if IsNested (Source1, Source2) then
      return Higher;
  elsif IsNested (Source2, Source1) then
      return Lower;
  end if;
  return Uncomparable;
end Compare;


  ----------------------------------------------------------------------
  -- This Trigger e' il primo  event della queue or null
  -- This Event e' il trigger della transizione che viene analizzata
  ----------------------------------------------------------------------
  function Satisfies_Trigger (This_Trigger: Event_Ref; 
                                This_Event: Event_Ref) return Boolean is
  begin
    if This_Event = This_Trigger then
       return true;
    elsif This_Trigger = null and  This_Event = Null_Event then
       return true;
    elsif This_Trigger /= null and then 
           -- we must compare the names, and we should compare also 
           --  the params list!! becase Event and trigger might be
           --  different Event objects (the trigger a dynamic one)
           This_Event.Name.all = This_Trigger.Name.all and then
           This_Event.params.all'Length = 
                This_Trigger.params.all'Length  then
      return True;
    else 
      return False;
    end if;
  end Satisfies_Trigger;


  ----------------------------------------------------------------------
  ------------------------------------------------------------------------
  function Currently_Active_Substate (This_State: State_Ref;
                         Current_RState: States_Table ) return State_Ref is
   Leaf: State_Ref;
  begin
   Leaf := Current_RState(This_State.FirstRuntimePosition);
   return Leaf.Ancestors(This_State.Depth+1);
  end Currently_Active_Substate; 
  
  ------------------------------------------------------------------------
  --  Given a state, checks whether this state (simple or composite, or
  --  parallel) is active and completed in th current runtime configuration.
  ------------------------------------------------------------------------
  function Is_Completed (The_State: State_Ref;
                        Current_RState: States_table) return Boolean  is
    The_Active_Substate: State_Ref;
  begin
    if The_State.Kind = Simple then
      return True;
    end if;
     -- otherwise,
   
    if The_State.Kind /= Parallel then
      The_Active_Substate := 
           Currently_Active_Substate(The_State,Current_RState);
      if The_Active_Substate.Kind = Simple then
        if Is_Final(The_Active_Substate) then 
          return True;
        else
          return False;
        end if;
      else
        return Is_Completed(The_Active_Substate, Current_RState);
      end if; 
    else
      --  The_State.Kind = Parallel
      for I in The_State.Substates'Range loop
        The_Active_Substate :=  The_State.Substates(I);
          if not Is_Completed(The_State.Substates(I) ,Current_RState) then
            return False;
          end if;
      end loop;
      return True;
    end if;
  end Is_Completed;
  
  ---    FINALS   and SET_FINALS  no more used ????


  ------------------------------------------------------------------------
  --  Given a state, checks whether this state (simple or composite, or
  --  parallel) is active in th current runtime configuration.
  ------------------------------------------------------------------------
  function Is_Currently_Active (The_State: State_Ref;
                                 Current_States: States_table) 
                                  return Boolean is
    Leaf: State_Ref := Current_States(The_State.FirstRuntimePosition);
  begin
    if The_State.Kind = Simple then
      return Leaf = The_State;
    else
      return Leaf.Ancestors(The_State.Depth) = The_State;
    end if;
  end Is_Currently_Active;  
  
  ------------------------------------------------------------------------
  --
  ------------------------------------------------------------------------
--  procedure Free (What: in out Transitions_Tables_Vector) is
--  begin
--    for I in What'Range loop
--       Free(What(I));
--    end loop;
--  end;
  
  ----------------------------------------------------------------------
  -- The given signal, which is a queue item, does not trigger any
  -- transition in the current ORC. We return FALSE if we can 
  -- stutter and remove it from the queue. 
  -- We return TRUE if we cannot remove it from the queue because 
  -- defferred, having in this way to continue to look for other
  -- queue items.
  ----------------------------------------------------------------------
  function Is_Deferred(This_ORC: Object_Runtime_Configuration;
                        This_Signal: Int64) return Boolean is
     The_Signal: Int_Table := Vectors_DB.Retrieve(This_Signal);
     This_Event: Event_Ref := All_Events(The_Signal(1));
     This_State: State_Ref;
  begin
    for I in This_ORC.Current_States'Range loop
      This_State := This_ORC.Current_States(I);
      while This_State /= null loop
        for J in This_State.Deferred.all'Range loop
          if This_State.Deferred(J).num_key = This_Event.num_key then
            return True;
          end if;
        end loop;  -- for all local deferres items
        This_State := This_State.Parent;
      end loop;   -- for all ancestrors
    end loop; -- for all active simple states
    return False;
  end Is_Deferred;
 

  -----------------------------------------------------------------
  --  similar to Apply_Transitions_Sequence ,  only that it is called
  --  when a return signal is received.  The already started
  --   evolution must be resumed.
  --  The resumed transition can be a completion transition or 
  --    a triggered transition
  --  The trigger, in the case of triggered transitions, 
  --    HAS ALREADY BEEN REMOVED FROM THE QUEUE
  -- the return signal is at the top of the queue
  -----------------------------------------------------------------
  function Resume_Transitions_Sequence(
      Active_Object: Positive;   -- the current active object  (self)
      Current_Vars: Int_Table;          -- vars data
      Current_Queue: Int64_Table;                  -- queue data
      Current_States: States_Table;        -- states data
      This_Sequence_Nick: Int64;  -- supension point
      Suspended_Transition_Index: Natural;
      Suspended_Actions: Natural; --nick in Vectors_DB (the suspended actions table)
      Transition_Env: in Natural;   
      TriggerArgs: Natural    -- index in Vectors_DB
             ) return Object_Runtime_Evolution is
    --
    New_Vars: Int_Table := Current_Vars;
    Current_Chart: Natural := Active_Charts(Active_Object);
    Return_Signal: Int_Table := Vectors_DB.Retrieve(Current_Queue(1));
    Return_Vars: Int_Table(1..1) := (1..1 => Return_Signal(3));
    Signals_Count: Natural :=0;
    This_Sequence: Transitions_Table :=
            Transitions_DB.Retrieve(This_Sequence_Nick);
    Trimmed_Size: Natural;
    New_Queue: Int64;
    Exited: State_Ref;
    New_States: States_Table := Current_States;
    --
    Result: Object_Runtime_Evolution;
    New_ORC: Object_Runtime_Configuration(Current_States'Length,
                                         Current_Vars'Length);
    Current_TEnv: Natural :=0;
    RANDOMQUEUE: Boolean := Has_Random_Queues(Current_Chart);
  begin
    --
    --  prepara la rimozione del  return signal 
    -- 
    Trimmed_Size := Current_Queue'Length -1;
    --
    -- in realta' i segnali generati potranno essere di meno,
    --  soprattutto se ci saranno sospensioni su call ..
--    for I in Suspended_Transition_Index .. This_Sequence'Length loop
--      Signals_Count := Signals_Count + This_Sequence(I).Actions.all'Length;
--    end loop;
    --
    declare
      Trimmed_Queue: Int64_Table(1..Trimmed_Size);
--      All_Signals: Int64_Table(1..Signals_Count) := (others => 0);
      All_Signals: Int64_Table(1..1000);
      All_Signals_Count: Natural := 0;
      Further_Suspended_Transition_Index: Natural := Suspended_Transition_Index;
      Further_Suspended_Action_Index: Natural :=0;
      Full_Suspended_Actions_Table: Int_Table := Vectors_DB.REtrieve(Suspended_Actions);
      Further_Suspended_Actions: Natural :=0;
      Further_Suspended_Call : natural :=0;
      Actions_Table: Int_Table(1..1) := (1..1 => Full_Suspended_Actions_Table(1));
      IsExited: Boolean := False;
    begin
      -- continue with the suspended Transition
      -- resume the suspended action
--    declare
--      TEnv: Int_Table := Vectors_DB.Retrieve(Transition_Env);
--    begin
           -- incrementally initializes
           --     New_RVars, All_Signals, All_Signals_Count, Suspended
        Resume_Action(Full_Suspended_Actions_Table, Actions_Table,
             Current_Chart,New_Vars,Current_States, Current_Queue,
             Transition_Env, Return_Vars, All_Signals,All_Signals_Count,
               This_Sequence(Suspended_Transition_Index).Actions(Actions_Table(1)),
             Further_Suspended_Actions, Further_Suspended_Call,
             Current_TEnv, IsExited);
--    end; 
      --
      -- then continue with the other actions of the suspended transition
      --
      if Further_Suspended_Actions =0 then
       Further_Suspended_Transition_Index :=0;
       for J in Actions_Table(1)+1 ..
          This_Sequence(Suspended_Transition_Index).Actions.all'Length loop
         --
--       declare
--         TEnv: Int_Table := Vectors_DB.Retrieve(Current_TEnv);
--         --  after each loop, the size of the vector denoted by Current_Env
--         --  can grow only if the action is TVar declaration
--         --  Moreover only the -non-trigger part of the TEnv can change as
--         --  an effect of the action execution (no assignemtn allowed to eventvars).
--       begin
--         -- incrementally initializes
--         --     New_RVars, All_Signals, All_Signals_Count, Suspended
           Actions_Table(1) := J;
           Apply_Action(Actions_Table,
              Current_Chart,New_Vars,Current_States, Current_Queue,
              Current_TEnv, All_Signals,All_Signals_Count,
              This_Sequence(Suspended_Transition_Index).Actions(J), 
              Further_Suspended_Actions, Further_Suspended_Call,
              Current_TEnv, IsExited);
--       end;
         if Further_Suspended_Actions > 0 then
           Further_Suspended_Transition_Index := Suspended_Transition_Index; 
           exit;
         end if;
       end loop;
     end if;

      -- now hadle the remaining transitions ... (unless already suspended)
      --  The Current_TEnv returned by the previous transitions execution
      --  MUST be TRIMMMED to the size of Trigger Params.
      if Further_Suspended_Actions = 0 then
        for I in Suspended_Transition_Index+1 ..  This_Sequence'Length loop
          --
          Current_TEnv := TriggerArgs;
          --
          for J in This_Sequence(I).Actions.all'Range loop
             --
             -- incrementally initializes
             --     New_RVars, All_Signals, All_Signals_Count, Suspended
             --
             -- recover clean Transition Env  from TriggerArgs (if any)
--           if Current_Trigger = then
--              Actions_Table(1) := J;
--             Apply_Action(Actions_Table,
--                       Current_Chart,New_Vars,Current_States, Current_Queue,
--                       Empty_Int_Table, All_Signals,All_Signals_Count,
--                       This_Sequence(Suspended_Transition_Index).Actions(J),
--                       Further_Suspended_Actions, Further_Suspended_Call,
--                       Current_TEnv);
--           else
--             declare
--               TT: Int_Table := Vectors_DB.Retrieve(Current_Trigger);
--               TEnv: Int_Table(1..TT'Length-2) := TT(3..TT'Length);
--             begin
                 Actions_Table(1) := J;
                 Apply_Action(Actions_Table,
                         Current_Chart,New_Vars,Current_States, Current_Queue, 
                         Current_TEnv, All_Signals,All_Signals_Count,
--                       This_Sequence(Suspended_Transition_Index).Actions(J),   BACONE BACONE
                         This_Sequence(I).Actions(J), 
                         Further_Suspended_Actions, Further_Suspended_Call,
                         Current_TEnv, IsExited);
--             end;
--           end if;
             if Further_Suspended_Actions > 0 then
               exit;
             end if;
          end loop;
         if Further_Suspended_Actions > 0 then
            Further_Suspended_Transition_Index := I;
            exit;
          end if;
        end loop;
      end if;
      -- New_VARS  and ALL_SIGNALS have been updated 
      -- and maybe Further_Suspended_ ...

      --1)  revove the return signal from the queue
      for I in 2 .. Current_Queue'Length loop
        Trimmed_Queue(I-1) := Current_Queue(I);
      end loop;

      -- 3) update the local queue with local signals,
      --
      declare
        New_Queue_Items : Int64_Table(1..Trimmed_Size+All_Signals_Count);
        New_Queue_Size: Natural := Trimmed_Size;
      begin
         New_Queue_Items(1..Trimmed_Size) := Trimmed_Queue(1..Trimmed_Size);
         for I in 1.. All_Signals_Count loop
           declare
             This_Signal: Int_Table := Vectors_DB.Retrieve(All_Signals(I));
           begin
             if This_Signal(2) = Current_Chart and then
                 This_Signal(1) /= Assign_Event.Num_Key then
               --
               -- IN CASE OF RANDOM QUEUE INSERT THE SIGNAL KEEPING THE QUEUE SORTED
               --
               if RANDOMQUEUE then
                  New_Queue_Size := New_Queue_Size +1;
                  New_Queue_Items(1..New_Queue_Size) :=
                     InsertSorted(All_Signals(I), New_Queue_Items(1..New_Queue_Size-1));
               else
                  New_Queue_Size := New_Queue_Size +1;
                  New_Queue_Items(New_Queue_Size) := All_Signals(I);
               end if;
               --  assign signals are  OUT.Assign  events
             end if;
           end;
         end loop;
         New_Queue :=
           abs(Signals_DB.NickNum(New_Queue_Items(1..New_Queue_Size)));
         if New_Queue_Size > Configurations.Kernel.MAXQUEUE then
            MAXQUEUE := New_Queue_Size;
         end if;
      end;

      -- 4) update the currently active states,
      --
      if Further_Suspended_Actions > 0 then
        -- in case of suspension ..
        New_States := Current_States;
      else
        New_States := Current_States; 
        for I in This_Sequence'Range loop
          -- for all sources of this transition, clear the source
          Exited :=
            Currently_Active_Substate (This_Sequence(I).Owner,Current_States);
          for J in Exited.FirstRuntimePosition .. Exited.LastRuntimePosition loop
               New_States(J) := null;
          end loop;                           --  JUST ADDED  11-01-2006
          -- Apply the transition mask
          for K in This_Sequence(I).Mask.all'Range loop
            if This_Sequence(I).Mask(K) /= null then
              New_States(K) :=  This_Sequence(I).Mask(K);
            end if;
          end loop;
        end loop;
      end if;

      -- 5) build the target and cache its nick
      --
      New_ORC.Self := Current_Chart;
      if Further_Suspended_Actions > 0 then
        New_ORC.Suspended_Transition_Index := Further_Suspended_Transition_Index;
        New_ORC.Suspended_Actions := Further_Suspended_Actions;
        New_ORC.TriggerArgs := TriggerArgs; 
        New_ORC.Suspended_Call := Further_Suspended_Call;
        New_ORC.Transition_Env := Current_TEnv;
        New_ORC.Suspended_Sequence := This_Sequence_Nick;
      end if;
      New_ORC.Current_States := New_States;
      New_ORC.Current_Vars := New_Vars;
      New_ORC.Current_Queue := New_Queue;

      -- 6) build and return the ore
      --
      Result.Evolving_Object := Active_Object;
      Result.Transitions :=  abs(Transitions_DB.NickNum(This_Sequence));
      -- Result.Trigger .. already adjusted
      Result.Trigger := Current_Queue(1);
      Result.Signals  :=
          abs(Signals_DB.NickNum(All_Signals(1..All_Signals_Count)));
      -- Result.Transitions .. already adjusted
      Result.Target :=  abs(ORC_DB.NickNum(New_ORC));
      declare
        Tmp1: String_Tables_Vector_Ref;
        Tmp2: String_Tables_Vector_Ref;
        Tmp3: String_Tables_Vector_Ref;
      begin
        Tmp1 := AbstractEvolvingView(Current_Chart);
        Tmp2 := AbstractView(All_Signals(1..All_Signals_Count), Current_Chart);
        if Tmp1 = null or else Tmp1.all'Length =0 then
           Tmp3 := Tmp2;
           Tmp2 := null;
        else
          Tmp3 := new String_Tables_Vector'(Tmp1.all & Tmp2.all);
        end if;
        Result.AbstractInfo.Labels :=  SortUnique(Tmp3);
        if Tmp1 /= null and then Tmp1.all'Length >0 then
           Free(Tmp1);
        end if;
        if Tmp2 /= null and then Tmp2.all'Length >0 then
           Free(Tmp2);
        end if;
      end;
      return Result;
    end; 
  end Resume_Transitions_Sequence; 

  ----------------------------------------------------------------------
  -- First we check if we are in the case of resume triggered by a return.
  -- Then we check if there are completion transitions to be handled
  -- Finally we scan the queue for the first signal triggering some
  --   transition or allowing to stutter.
  -- If nothing is possible, return empty evolutions table.
  ----------------------------------------------------------------------
  function Compute_Object_Evolutions(
      Current_Conf: Int64;
      Active_Object: Natural) return Object_Runtime_Evolutions_Ref is
    --
    This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(Current_Conf); 
    Queue: Int64_Table := Signals_DB.Retrieve(This_ORC.Current_Queue); -- RAISE: Current_Queue=0 ???
    Current_Chart: Natural := Active_Charts(Active_Object);
    RANDOMQUEUE: Boolean := Has_Random_Queues(Current_Chart);
    Evolutions_Count : Natural := 0;
    ORE_Ref: Object_Runtime_Evolutions_Ref := new Object_Runtime_Evolutions(1..0);
    ORE_Prev: Object_Runtime_Evolutions_Ref;
  begin
    --
    --  Handle Resuming after a Call;
    --
    if This_ORC.Suspended_Sequence > 0 then 
      -- we are in the middle of some evolution, waiting for a return signal
      if Queue'Length < 1 then
        -- no continuation possible;  nothing to change
        return ORE_Ref;
      end if;
      declare
        Top: Int_Table := Vectors_DB.Retrieve(Queue(1));
      begin
        if Top(1) not in OpReturn_Event.Num_Key .. OpReturnBoolC_Event.Num_Key then
          -- no continuation possible;  nothing to change
          return ORE_Ref;
        end if;
        -- Otherwise we can now resume the suspended evolution;
        Free(ORE_Ref);
        ORE_Ref := new Object_Runtime_Evolutions(1..1);
        ORE_Ref(1) :=
           Resume_Transitions_Sequence(
                Active_Object,   -- the current active object  (self)
                This_ORC.Current_Vars,          -- vars data
                Queue,                          -- queue data
                This_ORC.Current_States,        -- states data
                This_ORC.Suspended_Sequence,  -- supension point
                This_ORC.Suspended_Transition_Index,
                This_ORC.Suspended_Actions,
                This_ORC.Transition_Env,
                This_Orc.TriggerArgs  -- 0 when completion Transition
               );
        --Q  Adjust ORE_Ref(1).AbstractInfo
      end; 
      return ORE_Ref;
    end if;
    --
    --  Handle Completion Transitions;
    --
    declare
      These_Firing_Sets: Transitions_Tables_Vector :=
          UML_Model.Get_Fireable_Sets(This_ORC,Active_Object, 0);
    begin
      if These_Firing_Sets'Length > 0 then
      --
      -- for each Firabler-Set
      --
      for This_Set in These_Firing_Sets'Range loop
        --
        declare
          All_Sequences : Int64_Table := 
               Get_Firing_Sequences(Active_Object, These_Firing_Sets(This_Set));
          Previous_Size: Natural := ORE_Ref.all'Length;
          ORE: Object_Runtime_Evolutions(1..All_Sequences'Length);
        begin
          --
          -- for each sequence:
          -- 1   save it in the Transitions_DB
          -- 2   build The corresponding  Object_Runtime_Evolution data,
          -- 3   compute the resulting target
          -- 4  build the evolutions table and save it ihn the OCI_DB
          --
          for I in All_Sequences'Range loop
            -- notice that the evolution might be only a fragment of the sequence
            --  ending with a suspention on a call.
            declare
            begin
            ORE(I) := 
              Apply_Transitions_Sequence(
                Active_Object,   -- the current active object  (self)
                This_ORC.Current_Vars,          -- vars data 
                Queue,                          -- queue data  
                This_ORC.Current_States,        -- states data 
                (0,0),                          -- trigger data 
                0,                            -- trigger pos (0 = completion)
               All_Sequences(I)        -- the given transition sequence nick
               );   
              ORE(I).TransitionsSetKey := All_Sequences(I); --These_Firing_Sets(This_Set);
            exception
             when STORAGE_ERROR => raise;
             when others =>
               ORE(I).Trigger := 0; -- no trigger
               ORE(I).TransitionsSetKey :=  All_Sequences(I); --These_Firing_Sets(This_Set);
               ORE(I).Transitions :=  
                            abs(Transitions_DB.NickNum(Transitions_DB.Retrieve(All_Sequences(I))));
               ORE(I).Signals :=   -- nick in signals_db
                  Signals_DB.Nicknum(
                      (1 => Vectors_DB.NickNum((Runtime_Error_Event.Num_Key,
                             ErrChart.ChartParent, Runtime_Error_Num, Runtime_Error_Val))));
               ORE(I).Target := Current_Conf;  -- nick in orc_db
               ORE(I).AbstractInfo.Labels :=
                   new String_Tables_Vector'
                        (1 => new String_Table'
                          (1=> new String'("Runtime_Error"),
                           2 => new String'(Value_Image(Runtime_Error_Num,Undefined)),
                           3 => new String'(Value_Image(Runtime_Error_Val,Undefined))));
               Runtime_Error_Num := UnspecifiedError;
               Runtime_Error_Val := 0; 
            end;
          end loop;
          --
          ORE_Prev := ORE_Ref;
          ORE_Ref := new Object_Runtime_Evolutions'(ORE_Ref.all & ORE);
          Free(ORE_Prev);
          -- 
        end;
      end loop;
      --
      return ORE_Ref;
      -- 
      end if;
    end;
    --
    -- if we have not yet returned then  no completion transitions ... 
    --  see the queue
    -- Handle Triggered_Transitions;  HERE WE CAN SUPPORT RANDOM QUEUES!!
    --
    -------------  RANDOM QUEUES ------------
    --  Q= [ s1,s2,s3]
    -- for each S in  Q'Range
    --    Trigger := Queue(S) 
    --    Firing := Ger_Fireable_Sets(.. Q(S));
    --    if Firing'Length >0 
    --        for This_set in Firing
    --           for all sequences
    --            ORE(I) := Apply_Transitions_Sequnce(..,Q, ... Trigger,S)
    --           end loop;  -- sequences
    --           ORE_Ref := new Object_Runtime_Evolutions'(ORE_Ref.all & ORE);
    --        end loop  -- Firing
    --        return ORE_Ref ( NOT IN CASE OF RANDOM)
    --    else
    --       if IsDeferred(Trigger) then    (( DEFERRED OR RANDOM)
    --            null;  continue with next Signal in queue
    --       else
    --          ORE(1) := Apply_Stuttering(...,Q,...,Trigger,S)
    --          return ORE;
    --    end if;
    ------------------------------------------
    for S in Queue'Range loop
      -- do the same,  but skip deferred objects,
      --  remove the trigger of the transitions or as last
      --  resort, computer a stuttering transition.
      declare
        Trigger: Int_Table := Vectors_DB.Retrieve(Queue(S));
        These_Firing_Sets: Transitions_Tables_Vector :=
           UML_Model.Get_Fireable_Sets(This_ORC,Active_Object,Queue(S));
      begin
        if These_Firing_Sets'Length > 0 then
        --
        -- for each Firabler-Set
        --         
        for This_Set in These_Firing_Sets'Range loop
          --
          declare      
            All_Sequences : Int64_Table := 
                    Get_Firing_Sequences(Active_Object, These_Firing_Sets(This_Set));
            ORE: Object_Runtime_Evolutions(1..All_Sequences'Length);
          begin
            --
            -- now we have in All_Sequences all the fireable seqs of trans.
            -- for each sequence:
            -- 1   save it in the Transitions_DB
            -- 2   build The corresponding  Object_Runtime_Evolution data,
            -- 3   compute the resulting target
            -- 4  build the evolutions table and save it ihn the OCI_DB
            for I in All_Sequences'Range loop
              -- notice that the evolution might be only a fragment of the sequence
              --  ending with a suspention on a call.
              declare
              begin
              ORE(I) :=
                Apply_Transitions_Sequence(
                  Active_Object,   -- the current active object  (self)
                  This_ORC.Current_Vars,          -- vars data
                  Queue,                          -- queue data
                  This_ORC.Current_States,        -- states data
                  Trigger,                        -- trigger data  
                  S,                            -- trigger pos (0 = completion)
                  All_Sequences(I));   -- the given transition sequence
                 ORE(I).TransitionsSetKey := All_Sequences(I); --These_Firing_Sets(This_Set);
               exception
                 when STORAGE_ERROR => raise;
                 when others =>
                   ORE(I).TransitionsSetKey := All_Sequences(I); --These_Firing_Sets(This_Set);
                   ORE(I).Transitions :=
                            abs(Transitions_DB.NickNum(Transitions_DB.Retrieve(All_Sequences(I))));
                   ORE(I).Signals :=   -- nick in signals_db
                      Signals_DB.Nicknum( 
                         (1 => Vectors_DB.NickNum((Runtime_Error_Event.Num_Key,
                                          ErrChart.ChartParent, Runtime_Error_Num, Runtime_Error_Val))));
                   ORE(I).Target := Current_Conf;  -- nick in orc_db
                   ORE(I).AbstractInfo.Labels :=     
                      new String_Tables_Vector'
                        (1 => new String_Table'
                          (1=> new String'("Runtime_Error"),
                           2 => new String'(Value_Image(Runtime_Error_Num,Undefined)),
                           3 => new String'(Value_Image(Runtime_Error_Val,Undefined))));
                 Runtime_Error_Num := UnspecifiedError;
                 Runtime_Error_Val := 0;
               end;
            end loop;
            --
            ORE_Prev := Ore_Ref;
            ORE_Ref := new Object_Runtime_Evolutions'(ORE_Ref.all & ORE);
            Free(ORE_Prev);
            --
          end;
        end loop;
         --
         if not RANDOMQUEUE then
           exit;
         end if;
         --
        else
          --
          -- this signal does not trigger any transition:
          --  if deferred, continue, otherwise stutter
          if Is_Deferred(This_ORC, Queue(S)) then
            null; --  continue with next signal in the queue
          else
            declare
              ORE: Object_Runtime_Evolutions(1..1);
            begin
              declare
              begin
              ORE(1) := Apply_Stuttering(
                Active_Object,   -- the current active object  (self)
                This_ORC.Current_Vars,          -- vars data
                Queue,                          -- queue data
                This_ORC.Current_States,        -- states data
                Trigger,                        -- trigger data
                S                             -- trigger pos (0 = completion)
                );
               --
              ORE_Ref := new Object_Runtime_Evolutions'(ORE_Ref.all & ORE);
              exception
               when STORAGE_ERROR => raise;
               when others =>
                  ORE(1).Evolving_Object :=  Active_Object;  -- index in Active_Object'Range
                  ORE(1).Trigger := -1;       -- nick in signals_db
                  ORE(1).Transitions := -1;   -- nick in transitions_db
                  ORE(1).TransitionsSetKey := 0;
                  ORE(1).Signals :=   -- nick in signals_db
                      Signals_DB.Nicknum(
                         (1 => Vectors_DB.NickNum((Runtime_Error_Event.Num_Key,
                                          ErrChart.ChartParent,Runtime_Error_Num, Runtime_Error_Val))));
                  ORE(1).Target := Current_Conf;  -- nick in orc_db
                  ORE(1).AbstractInfo.Labels :=
                   new String_Tables_Vector'
                        (1 => new String_Table'
                          (1=> new String'("Runtime_Error"),
                           2 => new String'(Value_Image(Runtime_Error_Num,Undefined)),
                           3 => new String'(Value_Image(Runtime_Error_Val,Undefined))));
                  Runtime_Error_Num := UnspecifiedError;
                  Runtime_Error_Val := 0;
                  ORE_Prev := ORE_Ref;
                  ORE_Ref := new Object_Runtime_Evolutions'(ORE_Ref.all & ORE);
                  Free(ORE_Prev);
              end;
            end ;
            --
            if not RANDOMQUEUE then
               exit;
            end if;
            --
          end if;  -- not deferred
        end if;  -- firing_sets =0
      end;
    end loop;   -- S in queue'Range
    --
    -- If still no evolutions, then return the empty list.
    --  (this can be a deadlock/final state in no more signals arrive)
    --
    return ORE_Ref;
  exception
    --  CANNOT BE REMOVED  becuase of return transitions....
    when STORAGE_ERROR => raise;
    when others =>
      declare
         ERR_Ref : Object_Runtime_Evolutions_Ref  := new Object_Runtime_Evolutions(1..1);
      begin
          ERR_Ref(1).Evolving_Object :=  Active_Object;  -- index in Active_Object'Range
          ERR_Ref(1).Trigger := -1;       -- nick in signals_db
          ERR_Ref(1).Transitions := -1;   -- nick in transitions_db
          ERR_Ref(1).Signals :=   -- nick in signals_db
             Signals_DB.Nicknum( 
               (1 => Vectors_DB.NickNum((Runtime_Error_Event.Num_Key,
                                          ErrChart.ChartParent,Runtime_Error_Num, Runtime_Error_Val))));
          ERR_Ref(1).Target := Current_Conf;  -- nick in orc_db
          ERR_Ref(1).AbstractInfo.Labels :=
                   new String_Tables_Vector'
                        (1 => new String_Table'
                          (1=> new String'("Runtime_Error"),
                           2 => new String'(Value_Image(Runtime_Error_Num,Undefined)),
                           3 => new String'(Value_Image(Runtime_Error_Val,Undefined))));
               Runtime_Error_Num := UnspecifiedError;
               Runtime_Error_Val := 0;
          return ERR_Ref;
      end;
    --
  end Compute_Object_Evolutions;

  -- Implement   RESUME_ACTION
  --  distribute return signals  on the top of queue, 
end Uml_Model;



begin
  null;
end Object_Model;

