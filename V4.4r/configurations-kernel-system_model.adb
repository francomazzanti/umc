with text_IO; use Text_IO;
--with GNAT.Semaphores; use GNAT.semaphores;
--with SYSTEM; use System;
separate (Configurations.Kernel)
package body System_Model is

--SMSEM: Binary_Semaphore(true, Default_ceiling);
--pragma Volatile(SMSEM);

-------------------------------------------------
--  IMPROVEMENT:
--    SCI_DB viene eliminato!  no more caching of partial evolutions.
--    Partial system_evolutions are COMPUTED on the fly from the Object_Runtime_Evolutions.
--    (Eventually when the memory-saving flag is set)
--   Evolution_Data in this case can be defined as <System_Configuration,Active_Object,Active_Evolution>
--   e da questo vengono calcolate on the fly target, e labels
--
--   On the fly computations of evolutions can be made more effcient separating in the SRC_DB elements
--     the object queues  from the object runtime configurtation (ORC) (vars and active states, and digested queue).  
--   this would allow to not recompute an ORC evolution when one already exists and more events arrive.
--   Also the transformation of the ORC evolution into a system evolution would be more efficient.
--   
-------------------------------------------------
--
--  MEMORY FRIENDLY (no wastes)
--
---------------------  from SPEC -----------------------------------------
--
--  function Get_System_Initial_Configuration return Int64;
--  procedure Set_System_Initial_Configuration;
--
-- -- function Compute_Object_Evolutions
-- --             (Current_Configuration: Int64;   -- System Configuration
-- --              Current_Object: Natural) return Object_Runtime_Evolutions_Ref;

-- function Compute_System_Evolutions
--             (Current_Configuration: Int64 return Object_Runtime_Evolutions_Table_Ref;
--
--  function Compute_Target_Configuration(
--             Current_Configuration: Int64;  -- System Configuration
--             Active_Object: Natural;        -- index in Active_Charts
--             Current_Object_Evolution: Natural) return Int64;  -- System Configuration
--
--  function  Configurations_Count return Int64
--
--  function SRC_Count return Int64;
--  function Get_Object_Conf(NickNum: Int64; Obj:Natural) return Int64;
--  function Exists (NickNum: Int64) return Boolean;
--  function MaxQueueSize (NickNum: Int64) return Natural;
--  procedure Initialize_System_Model;
--------------------------------------------------------------------------
use UML_Types;

--------------------------------------------------------------------------------
--  SRC_DB = [SysConf, SysConf, ...]  Indexed by  Nicknums (:System_Configuration)
--  The Table is hashed, and given a SysConf we can easily get its index (nicknum)
--
--  a SysConf is an Int64_Table which can be of two kinds:
--  + and actual table [objconf,objconf,objconf,...] containing all object configurations
--  + a synbolic table [nicknum, objindex, objconf, objindex,objconf]
--     containing a rerefence to a previous Systconf and a vector of differences from it
--
--  Object Configurations (objconf) are recorded in the ORC_DB structure
-------------------------------------------------------------------------------------
--
-- 
--   Object_Runtime_Evolution    [ Evolving_Object |  (Natural)
--                                         Trigger |  (Nick of SIGNALS_DB)
--                                     Transitions |  (Nick of TRANSITIONS_DB)
--                                          Target |  (Nick od ORC_DB)
--                               TransitionsSetKey |  (Natural)
--                                    AbstractInfo ]
--                                           ||
--                                           ||
--              AbstractData [ Labels: String_Tables_Vector_Ref ]
-- 
-------------------------------------------------------------------------------------

--------------------------------------------------------------------------
-- La SYSTEM RUNTIME CONFIGURATION e' costituita dal vettore dei nick
--  delle runtime configurations degli oggetti attivi che lo costituiscono.
--------------------------------------------------------------------------

  function SRC_Count return Int64 is
  begin
    return SRC_DB.Current_Count;
  end SRC_Count;
--------------------------------------------------------------------------
--  La SYSTEM_CONFIGURATION_INFO e' costituita dalle informazioni via
--  via associate alle SRC (System_Runtime_Configurtation).
--  In particolare, esse sono le system_evolutions della SRC e la sua
--  profondita' dal nodo iniziale.
--------------------------------------------------------------------------

  function  Configurations_Count return Int64 is
  begin
    if Active_Charts.all'Length =1 then
     return ORC_DB.Current_Count;
    else
     return SRC_DB.Current_Count;
    end if;
  end Configurations_Count;

-------------------------------------------------------------------------
--  Chiamata da Initialize_System_Model:
-------------------------------------------------------------------------

  Initial_SRC: Int64 :=0;
  pragma Volatile (Initial_SRC);

  procedure Set_System_Initial_Configuration is
    SRC: Int64_Table(1..Active_Charts.all'Length);
  begin
    for I in SRC'Range loop
      SRC(I) := Object_Model.Get_Initial_Configuration(I);
    end loop;
    if Active_Charts.all'Length = 1 then
      Initial_SRC := SRC(1);  -- same as ORC nicknum
    else
      Initial_SRC:=  abs(SRC_DB.NickNum(SRC));
    end if;
--    Initial_SRC:=  abs(SRC_DB.NickNum(SRC));
  end Set_System_Initial_Configuration;

-------------------------------------------------------------------------
-- La funzione "Get_Initial_Configuration" restituisce la configurazione
-- iniziale risultante dal parsing del file sorgente uml.
-------------------------------------------------------------------------
  function Get_System_Initial_Configuration return Int64 is
  begin
    if Initial_SRC =0 then
       UML_Configurations.Initialize_Configurations;
    end if;
    return  Initial_SRC;
  end Get_System_Initial_Configuration;


   function Get_Object_Priority (Object_Conf: Int64;
                                 ActiveObj: Positive) return Integer is
     TheChart: Positive := Active_Charts(ActiveObj);
   begin 
     if All_Charts(TheChart).ChartVars.all'Length >0 and then
        All_Charts(TheChart).ChartVars(1).Name.all = "Priority" then
       return Object_Model.Get_Var_Value(Object_Conf,1);
     elsif All_Charts(TheChart).ChartVars.all'Length >1 and then
        All_Charts(TheChart).ChartVars(2).Name.all = "Priority" then
       return Object_Model.Get_Var_Value(Object_Conf,2);
     else
       return 0;
     end if;
   end Get_Object_Priority;


  function Optimized_Retrieve(Current_Configuration: Integer) return Int64_Table is
  begin
    if Active_Charts.all'Length =1 then
       return (1 => Int64(Current_Configuration)); -- SRC_NickNum = ORC_Nicknum = SRC(1)
    else
      return SRC_DB.Retrieve(Int64(Current_Configuration));
    end if;
  end Optimized_Retrieve;

  function Optimized_NickNum(TGT: Int64_Table) return Integer is
  begin
    if Active_Charts.all'Length =1 then
       return TGT(1); -- SRC_NickNum = ORC_Nicknum = SRC(1)
    else
      return SRC_DB.NickNum(TGT); 
    end if;
  end Optimized_NickNum;

  ------------------------------------------------------------
  -- se ci sono piu' di 100 active objects:  (BUGGED ROMOSSO!!!!)
  --
  -- Current_Configuration e' il nicknum di una struttura che puo' essere
  -- Sym_SRC:  (-controlkey, index1, diffvalue1, index2, diffvalue2, ...)
  --   oppure
  -- SRC: (realvalue1, realvalue2, realvalue3, ....)
  --
  -------------------------------------------------------------------
  function Compute_Target_Configuration(
       Current_Configuration: Int64;  -- System Configuration
       Active_Object: Natural;        -- index in Active_Charts
       Current_Object_Evolution: Natural) return Int64 is
    --
--    SRC: Int64_Table := SRC_DB.Retrieve(Current_Configuration);
    SRC: Int64_Table := Optimized_Retrieve(Current_Configuration);
    -- QQ1
    OCI: Object_Configuration_Info := OCI_DB.Retrieve(SRC(Active_Object));
    Object_Evolutions: Object_Runtime_Evolutions_Ref := OCI.Object_Evolutions;
    Evolution_Signals: Int64_Table := 
        Object_Model.Get_Signals(Object_Evolutions(Current_Object_Evolution).Signals);
    --
    TGT: Int64_Table(1..Active_Charts.all'Length);
    Moved_Signals: Int64_List_Table(1..Active_Charts.all'Length);
    Return_Signals: Int64_List_Table(1..Active_Charts.all'Length);
    I: Natural := Current_Object_Evolution;
  begin
    --
    for K in Moved_Signals'Range loop
      Moved_Signals(K) := new Int64_Table(1..0);   -- Freed later
      Return_Signals(K) := new Int64_Table(1..0);  -- Freed later
    end loop;  -- K
    --
    for J in Evolution_Signals'Range loop
      declare
        This_Signal: Int_Table := Object_Model.Get_Signal(Evolution_Signals(J));
        Active_Target: Natural;
        Tmp: Int64_Table_Ref;
      begin
        if This_Signal(2) not in All_Charts.all'Range then
           Put_Line (Current_error,
               "Error in sending an event to an inconsistent (null?) object.");
           raise UML_Error;
        end if;
        Active_Target := All_Charts(This_Signal(2)).Chart_Position;
        if Active_Target = Active_Object  then
           null;  -- signal already handled
        elsif Active_Target = 0  then
            null; -- signal external w.r. to the model
        else
           -- the signal must be moved to the target object queue.
          if This_Signal(1) not in
                 OpReturn_Event.Num_Key .. OpReturnBoolC_Event.Num_Key then
            Tmp := Moved_Signals(Active_Target);
            Moved_Signals(Active_Target) :=
             new Int64_Table'(Tmp.all &  (1=>Evolution_Signals(J)));
            Free(Tmp);
          else
            Return_Signals(Active_Target) :=
             new Int64_Table'(1..1=>Evolution_Signals(J));
          end if;
        end if;
      end;
    end loop;   -- J
    --
    --  all signals have been moved to the target
    --
    for L in Active_Charts'Range loop
      if Return_Signals(L).all'length >0 or else
           Moved_Signals(L).all'Length >0 then
        declare
          ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(SRC(L));
          Old_Queue: Int64_Table := Signals_DB.Retrieve(ORC.Current_Queue);
          New_Queue: Int64_Table :=
                Return_Signals(L).all & Old_Queue & Moved_Signals(L).all;
          Z: Integer := New_Queue'Length;
        begin
           if Z > Configurations.Kernel.MAXQUEUE then
              Configurations.Kernel.MAXQUEUE := Z;
           end if;
           ORC.Current_Queue := abs(Signals_DB.NickNum(New_Queue));
           TGT(L) := abs(ORC_DB.NickNum(ORC));
        end;
      else
        -- no change for this component
        TGT(L) := SRC(L);
      end if;
    end loop;   -- L
    --
    -- Should Free remaings Moved_Signals / Return_Signals !!!
    --
    for F in Active_Charts'Range loop
       if Return_Signals(F) /= null then
          Free(Return_Signals(F));
       end if;
       if Moved_Signals(F) /= null then
          Free(Moved_Signals(F));
       end if;
    end loop;
    --
    -- the target of the evolving object is taken from the object evolution
    TGT(Active_Object) := Object_Evolutions(I).Target;
    --
    --  TGT e' stato modificato con le estensioni delle code.
    --
    --  OPTIMIZED AS BELOW    return SRC_DB.NickNum(TGT);
--    return SRC_DB.NickNum(TGT);
    return Optimized_NickNum(TGT);
    --
  end Compute_Target_Configuration;


  ----------------------------------------------------------------------------------
  -- returns a table of object_evolutions which is a empty table is the
  --  object has a too low priority to evolve in the current system status.
  -- If the object priority is OK the table returned is the one saved/retrieved from
  --  the OCI_DB.
  --  NOTICE THAT THE FIRST IT IS CALLED for System Configuration are computed
  --  ALL the evolutions for ALL objects (necessary because of priorites).
  --MAYBE WE SHOULD CALL THIS   COMPUTE SYSTEM EVOLUTIONS
  -- first we compute all the object evolutions, then we record in the iterator only the 
  -- high priority ones
  ----------------------------------------------------------------------------------
--  function Compute_Object_Evolutions 
--             (Current_Configuration: Int64;   -- System Configuration
--              Current_Object: Natural) return Object_Runtime_Evolutions_Ref is  
--    OCI: Object_Configuration_Info;
--    --  OPTIMIZED  SRC: Int64_Table := SRC_DB.Retrieve(Current_Configuration);
--    Sym_SRC: Int64_Table := SRC_DB.Retrieve(Current_Configuration);
--    SRC: Int64_Table := Get_Actual_Conf(Sym_SRC);
--    --
--    Max_Prio: Integer;
--    OP: Integer;
--    Disabled: Boolean := False;  -- mean : this object cannot evolve because of low priority
--  begin
 --   --
--    if Priorities_Enabled and then Priorities_Defined then
--        -- gestione delle priorities ...
--        --
--        --  (a) maxprio= priority of current object
--        --  (b) iterate on objects with priority > thisprio and see if they can evolve
--        --  (c) if at least one of then can evolve disable evolution of curent object
--        --  (d) if no other highpriority object can evolve, compute evolutions of this object
--        -----------------------------------------
--        Max_Prio := Get_Object_Priority(SRC(Current_Object),Current_Object);
--        for I in Active_Charts.all'Range loop
--          OP := Get_Object_Priority(SRC(I),I);
--          if OP > Max_Prio  then
--             if Object_Model.Get_Object_Evolutions(SRC(I),I)'Length >0 then
--               Disabled := True;
--               exit;
--             end if;
--          end if;
--        end loop;
--        -----------------------------------------
--        --  Max_Prio e' la priota' massima trovata pe le evoluzioni di questo stato
--        --  for each Active_Object ,
--        --      if its priority=max_prio  and its_evolutions'length > 0 then
--        --      compute evolutions, compute the corresponding System_Evolution into SCI
--        --      else set its System Evoltiuons'length to 0
--        --      end if;
--        --
--        --   c) viene calcolata la System_Evolution solo per gli oggetti a priorita' massima
--        --      per gli altri non vengono generate system evolutions
--        --   cio' significa spostare i signals generati dalla object evolution nelle code
--        --   delle altre object configurations.
--        -----------------------------------------        
--    end if;
--    if Disabled then
--      return No_Evolutions_Ref;
--    else
--      OCI := OCI_DB.Retrieve(SRC(Current_Object));
--      if OCI.Object_Evolutions = null then
--        OCI.Object_Evolutions :=
--           Object_Model.Get_Object_Evolutions(SRC(Current_Object),Current_Object);
--        OCI_DB.Store(OCI,SRC(Current_Object));
--      end if;
--      return OCI.Object_Evolutions;
--    end if;
--  end Compute_Object_Evolutions;

  function Compute_System_Evolutions
             (Current_Configuration: Int64) return Object_Runtime_Evolutions_Table_Ref is
    System_Evolutions: Object_Runtime_Evolutions_Table_Ref
         := new Object_Runtime_Evolutions_Table(1..Active_Charts.all'Length);
    OCI: Object_Configuration_Info;
    Max_Prio: Integer;
    SRC: Int64_Table := Optimized_Retrieve(Current_Configuration);
--    SRC: Int64_Table := SRC_DB.Retrieve(Current_Configuration);
    Priorities: Int_Table(Active_Charts.all'Range);
  begin

    --
    -- set all Object potential Evolutions
    for I in Active_Charts.all'Range loop
        OCI := OCI_DB.Retrieve(SRC(I));
        if OCI.Object_Evolutions = null then
          OCI.Object_Evolutions :=
             Object_Model.Get_Object_Evolutions(SRC(I),I);
          OCI_DB.Store(OCI,SRC(I));
        end if;
        System_Evolutions(I) := OCI.Object_Evolutions;
    end loop;
    --
    --  compute max priority of actually evolving object
    Max_Prio := Integer'First;
    for I in Active_Charts.all'Range loop
      Priorities(I) := Get_Object_Priority(SRC(I),I);
      if System_Evolutions(I).all'Length >0 and then
         Priorities(I) > Max_Prio then
        Max_Prio := Priorities(I);
      end if;
    end loop;
    --
    -- discard low priorites evolutions
    for I in Active_Charts.all'Range loop
      if System_Evolutions(I).all'Length >0 and then
         Priorities(I) < Max_Prio then
        System_Evolutions(I) := No_Evolutions_Ref;
      end if; 
    end loop;   
    --
    return System_Evolutions; 
  end Compute_System_Evolutions;

         ------------- query functions ---------------
  function Get_Object_Conf(NickNum: Int64; Obj:Natural) return Int64 is
    -- OPTIMIZED Sys: Int64_Table := SRC_DB.Retrieve(Int64(NickNum));
    -- Sym_SRC: Int64_Table := SRC_DB.Retrieve(Int64(NickNum));
--    Sym_SRC: Int64_Table := Optimized_Retrieve(Int64(NickNum));
--    SRC: Int64_Table := Get_Actual_Conf(Sym_SRC);
    SRC: Int64_Table := Optimized_Retrieve(Int64(NickNum));
  begin
    return SRC(Obj);
  end Get_Object_Conf;


  function MaxQueueSize (NickNum: Int64) return Natural is
    -- OPTIMIZED Sys: Int64_Table := SRC_DB.Retrieve(Int64(NickNum));
    -- Sym_SRC: Int64_Table := SRC_DB.Retrieve(Int64(NickNum));
--    Sym_SRC: Int64_Table := Optimized_Retrieve(Int64(NickNum));
--    SRC: Int64_Table := Get_Actual_Conf(Sym_SRC);
    SRC: Int64_Table := Optimized_Retrieve(Int64(NickNum));
    Result:Natural := 0;
    Tmp: Natural;
  begin
   for I in SRC'Range loop
     Tmp := Object_Model.QueueSize(SRC(I)); 
     if Tmp > Result then
         Result := Tmp;
     end if;
   end loop;
   return Result; 
  end MaxQueueSize;

  function Exists (NickNum: Int64) return Boolean
  is begin
    if Active_charts.all'Length =1 then
      return NickNum <= ORC_DB.Current_Count;
    else
    return NickNum <= SRC_DB.Current_Count;
    end if;
  end Exists;
 
---------------  initialization utilities -----------------

 procedure Initialize_System_Model is
 begin
   Object_Model.Initialize_Object_Model;
--   SRC_DB.Initialize_DB;
--   SCI_DB.Initialize_DB;
   Set_System_Initial_Configuration;
 end Initialize_System_Model;
  
  begin
    null;
end System_Model;
