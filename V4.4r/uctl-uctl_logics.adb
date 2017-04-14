with Ada.Exceptions; 
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with System; use System;
with Dyn_Store_Obj; 
with My_Atomic_Counters; use My_Atomic_Counters;
with Ada.Unchecked_Deallocation;
with Ada.text_IO; use Ada.Text_IO;
with My_Hash;
---
separate(UCTL)
package body UCTL_Logics is
use UCTL_Types;
use UCTL_Utilities;
use Ada.Text_IO;
--use Computations_DB;
use Ada.Exceptions; 

  type BoolRef is access all Boolean;

-------------------------------------------------------------------------
-------------------------------------------------------------------------
package Classic is

  procedure Check_True (Form: UCTL_Types.Formula_Ref;
                  FromState: System_Configuration;
                  Context: Computations_Table := Empty_Computations_Table;
                  Cref: Out Computation_Step;
                  Result: Out Computation_Status;
                  N_Depth: Natural;
                  C_Depth: Natural;
                  Rec_Depth: out Natural);

  function Evolution_Satisfies (
         It: Evolutions_Iterator;
         Action_Formula: Action_Ref) return Boolean;

 function Eval (Formula: UCTL_Types.Formula_Ref;
                 State: System_Configuration) return Computation_Status;
end Classic;
-------------------------------------------------------------------------
-------------------------------------------------------------------------

-------------------------------------------------------------------------
-------------------------------------------------------------------------
package NWO is   -- (TO BE MOVED INTO LOGIS_SPEC.ads)

  function Eval (Formula: UCTL_Types.Formula_Ref;
                 State: System_Configuration) return Computation_Status; 
end NWO;
-------------------------------------------------------------------------
-------------------------------------------------------------------------


-------------------------------------------------------------------------
  function Eval (Formula: UCTL_Types.Formula_Ref;
                 State: System_Configuration) return Computation_Status is
  begin
    if Flags.Cores =0 or else
         (Formula.Kind =Fall and then Formula.Pref.Kind = Wuntil1) then
      return Classic.Eval (Formula, State);
    else
      return NWO.Eval (Formula, State);
    end if;
  end;
-------------------------------------------------------------------------

package body NWO is separate;
package body Classic is separate;

end UCTL_Logics;


