with Ada.Text_IO; use Ada.Text_Io;
separate (Configurations.Kernel)
package body UML_Configurations is
use UML_Types;
use System_Model;
use Object_Model;
use Global_Env;
  
  ------------------ Info Utilities ------------------------------

  function Get_Source_Configuration (It: Evolutions_Iterator)
    return System_Configuration is
  begin
    return System_Configuration(It.Current_Configuration);
  end Get_Source_Configuration;

  --
  --  return the current count of uml configurations in the database
  --
  function  Configurations_Count return Int64 is
  begin
    return System_Model.Configurations_Count;
  end Configurations_Count;

--  function Get_AbstractLabels(It: Evolutions_Iterator) return String_Tables_Vector;

 --
 -- given the current evolution, returns the Nick in Signals_DB
 --
  function Get_Evolution_Signals (It: Evolutions_Iterator) return Int64 is
     This_Evolution: Object_Runtime_Evolution;
  begin
    if It.Objects_Evolutions(it.Active_Object) = null then
       raise UML_Error;
    end if;
    This_Evolution :=
         It.Objects_Evolutions(it.Active_Object)(It.Current_Object_Evolution);
    return  This_Evolution.Signals;
  end Get_Evolution_Signals;

  function Is_Suspended(This_Conf: System_Configuration;
                         Active_Object:Positive) return Boolean is
    Sys: Int64 := Int64(This_Conf);
  begin
     return Is_Suspended(Get_Object_Conf(Sys,Active_Object));
  end Is_Suspended;


  function Get_Sequence (It: Evolutions_Iterator) return Transitions_Table is
     This_Evolution: Object_Runtime_Evolution;
  begin
    if It.Objects_Evolutions(it.Active_Object) = null then
       raise UML_Error;
    end if;
    This_Evolution := It.Objects_Evolutions(it.Active_Object)(It.Current_Object_Evolution);
--    Obj_Evolution_Data := This_Evolution.Object_Evolution;
    return  Get_Transitions_Sequence(This_Evolution.Transitions);
  end Get_Sequence;
  ---------------- Iteration Utilities --------------



  ------------------------------------------------------------
  -- Se viene fornito un Active_Object /= 0  allora l'iteratore
  --  scorre solo le possibili evoluzioni relative all'oggetto indicato.
  -- Appena inizializzato l'iteratore si posiziona sulla prima
  --  evoluzione possibile (se c'e');
  ------------------------------------------------------------
  
  -------------------    query   functions --------------------

  function Eval_Vector (Ids: String_Table) return Int_Table is
    --
    The_Vect: Int_Table(1..Ids'Length-1);
  begin
    for I in The_Vect'Range loop
      The_Vect(I) := Eval_Literal(Ids(I+1).all);
    end loop;
    return The_Vect;
  end Eval_Vector;

  -----------------------------------------------------------------
  --  Eval_Attribute viene chiamata quando deve essere valutata una
  --   asserzione del tipo   ASSERT ( LEFTEXPR OP  RIGHTEXPR )
  --    (Cioe' chiamata da "Configuration_Satisfies")
  --  data una sequenza di token che identificano un attributo o
  --   un literal, o un nome speciale, ne restituisce il valore
  --
  --  E.g. Ids= true    (false)
  --  E.g. Ids= null
  --  E.g. Ids= 123
  --  E.g. Ids= maxqueuesize   (the longest object queue)
  --  E.g. Ids= queuedevents   (the total number of queued events)
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
  -----------------------------------------------------------------
  function Eval_Attribute (This_Conf: System_Configuration;
                           Ids: String_Table) return Integer is
    Sys: Int64 := Int64(This_Conf);
    Tmp_Var: SystemVar_Ref;
    This_Var: Natural :=0;
    This_Chart: Natural := 0;
    Default_Chart: Natural := 0;
    This_Pos: Natural;
    Result: Integer :=0;
    Index: Integer;
  begin 
    --  global assertion
    --  this is just a literal, or maxqueuesize or IsActive
    --
    --   True, False,  Null, maxqueuesuze,  123, -123
    --   maybe we should add  "empty" for "[]" 
    --
    if ids'Length =0 then
         return UML_Types.IntEmptyStruct;
       --
    elsif ids(1).all="false" or
          ids(1).all="FALSE" or
            ids(1).all="False" then
        return IntFalse;
      --
      elsif ids(1).all="true" or
          ids(1).all="TRUE" or
            ids(1).all="True" then
        return IntTrue;
      --
      elsif ids(1).all="null" or
          ids(1).all="NULL" or
            ids(1).all="Null" then
        return  UML_Types.IntNullObject;
      --
      elsif ids(1)(1) in '0'..'9' then
         return Integer'Value(ids(1).all);
      --
      elsif ids(1).all'length >1 and then
           ids(1)(1) = '-' and then
            ids(1)(2) in '0'..'9' then
           return Integer'Value(ids(1).all);
      --
      elsif ids(1).all = "maxqueuesize" then
           return  MaxQueueSize(Sys); 
      --
      elsif ids(1).all = "confid" then
           return Integer(Sys);
      --
      elsif ids(1).all = "Is_Active" then
        for I in Active_Charts.all'Range loop
          if All_Charts(Active_Charts(I)).Name.all = Ids(2).all then
            This_Chart:= Active_Charts(I);
            This_Pos := 3;
            exit;
          end if;
        end loop;
        if This_Chart =0 then
          -- if only 1 active chart allow the omission of the chart name prefix
          if Active_Charts.all'Length=1 then
            This_Chart := Active_Charts(1);
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
        declare
          ThisName: String_Ref;
          Prev: String_Ref;
          TheChart: Chart renames All_Charts(This_Chart);
          TheObjectConf: Int64:= Get_Object_Conf(Sys,TheChart.Chart_Position);
          Indent: Integer :=0;
          Active_States: States_Table := Get_Active_States(TheObjectConf);
        begin
          ThisName := new String'(Ids(This_Pos).all);
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
                return IntTrue;
              end if;
            end loop;
         end loop; 
          -- MEMORY
          if  ThisName /= Ids(This_Pos) then
            Free(ThisName);
          end if;
          return IntFalse;
        end;
      end if;
      --
    -- OBJ  must be an active chart 
    --  (i.e. one DEFINED by the model and containing at least a var or state)
    --
    -- This is the classical  obj.attribute[selector]?[selector2]   case
    --
    This_Pos := 1;   ---  the position of the identifier being evaluated
    --
    -- This_Chart has a default value (the only active chart, when unique)
    --  as soon as an object attribute is evaluated (or an object name is found) 
    --  This_Chart is updated.
    --
    This_Chart := Active_Charts(1);   -- just for implicit unique active object
    --
    --  START THE LOOP OVER ALL THE STRINGS OF THE ATTRIBUTE
    --
    while This_Pos <= Ids'Length loop
      --
      -- if Ids(This_Pos) is an attribute of This_Chart, get its value.
      --
      This_Var := 0;
      if Ids(This_Pos).all/= "queuesize" and then 
          (This_Pos > 1 or else Active_Charts.all'Length=1) then
        --  
        --  The OBJ prefix is IMPLICIT (and unuique)
        --
        --   ... ATTR ....    
        --   ATTR ...
        if Ids(This_Pos)(1) in '0'..'9' then  
           -- this is an index selection
           Index := Integer'Value(ids(This_Pos).all);
           declare
              The_vect: int_table := Vectors_DB.retrieve(Result);
           begin
              if Index+1 not in The_vect'Range then
                 Put_Line (Current_Error,
                     "Error in ASSERT expression: invalid index"); 
              end if;
              Result := The_vect(Index+1);
              This_Chart := Result;
           end;
        elsif Ids(This_Pos).all="Length" or else Ids(This_Pos).all="length"  then
           declare
              The_vect: int_table := Vectors_DB.retrieve(Result);
           begin
              Result := The_vect'Length;
              This_Chart := Result;
              exit;
           end;
        else
          for I in All_Charts(This_Chart).ChartVars.all'Range loop
            Tmp_Var := All_Charts(This_Chart).ChartVars(I);
            if Tmp_Var.Name.all = Ids(This_Pos).all then
               This_Var := I;
               Result := Get_Var_Value(
                           Get_Object_Conf(
                              Sys, All_Charts(This_Chart).Chart_Position),I);
--               This_Chart := Result;
               exit;
            end if;
          end loop;
        end if;
        --
      elsif Ids(This_Pos).all= "queuesize" and then
          (This_Pos > 1 or else Active_Charts.all'Length=1) then
        --   ...  queuesize 
        --   queuesize
        return QueueSize(Get_Object_Conf(
                               Sys,All_Charts(This_Chart).Chart_Position));
      end if;
      --
      if This_Pos=1 and then 
           This_Var/=0  and then 
            Active_Charts.all'Length >1 then
          -- ATTR ..  invalid 
          Put_Line (Current_Error,
                     "Error in ASSERT expression: missing object prefix"); 
      end if;
      --
      -- if Ids(This_Pos)  is an object name, not an attribute ...
      --   (and This_Pos = 1) get its value.
      --
      if This_Var=0  and This_Pos=1  then 
        --
        --   OBJ ... ... 
        --
        This_Chart := 0;
        for I in All_Charts.all'Range loop
          if All_Charts(I).Name.all = Ids(1).all then
            This_Chart:= I;
            Result := I;
            exit;
          end if;
        end loop;
        if This_Chart=0 then
          Put_Line (Current_Error,
                     "Error in ASSERT expression: undefined object " &
                     "(or attribute) " & Ids(1).all );
           raise UCTL_Error;
        end if;
        --
      elsif (not (Ids(This_Pos)(1) in '0'..'9'))  and This_Var=0  then   -- This_Pos > 1
         --   ...   ID  ...
         Put_Line (Current_Error,
                   "Error in ASSERT expression: undefined attribute " &
                   Ids(This_Pos).all );
         raise UCTL_Error;
      end if;
      --
      This_Pos := This_Pos +1;
    end loop;
    --
    return Result;
  end Eval_Attribute;


  --------------  Ititialization Utilities --------------------


  procedure Initialize_Configurations is
  begin
    Initialize_System_Model;
  end Initialize_Configurations;

  function Initial_Configuration return System_Configuration is
    N: System_Configuration;
  begin
    N := System_Configuration(System_Model.Get_System_Initial_Configuration);
    return N;
  end Initial_Configuration;



  ----------------  Display Utilitites --------------------------
  
  -----------------------------------------------------------------
  --  Displays the sequence of transitions of the current 
  --   system evolution
  -----------------------------------------------------------------
  function Display_Sequence (It: Evolutions_Iterator) return String is
      This_Evolution: Object_Runtime_Evolution;
  begin
    if It.Objects_Evolutions(it.Active_Object) = null then
       raise UML_Error;
    end if;
    This_Evolution := 
        It.Objects_Evolutions(it.Active_Object)(It.Current_Object_Evolution);
    declare
       These_Transitions: Transitions_Table
          := Get_Transitions_Sequence(This_Evolution.Transitions);
    begin
       if These_Transitions'Length >0 then
         return Transitions_Image(These_Transitions);
       else
         return "-";
       end if;
    end;
  end Display_Sequence;
  
  function Get_Chart_Name(It: Evolutions_Iterator) return String_Ref is
  begin
    if It.Active_Object  /= 0 then
      return All_Charts(Active_Charts(It.Active_Object)).name;
    else
       return null;
    end if;
  end Get_Chart_Name;
 
  function Display_Chart_Name(It: Evolutions_Iterator) return String is
  begin
    if It.Active_Object  /= 0 then
      return All_Charts(Active_Charts(It.Active_Object)).name.all;
    else
       return "";
    end if;
  end Display_Chart_Name; 

  -----------------------------------------------------------------------
  -- displays all the sets of fireable sequences of transitions  of the 
  --  given object (i.e. all the lists of transitions sequences)
  -----------------------------------------------------------------------
  function Display_Fireable_Sequences (This_Conf: System_Configuration;
                               Active_Object: Positive) return String is
    Tmp: String_Ref := new String'("{ ");
    My_Iter: Evolutions_Iterator;
  begin
    Iterator_Initialize (My_Iter,This_Conf,Active_Object);
    if Has_System_Transition(My_Iter) then
      Tmp := Append (Tmp, "(" & Display_Sequence(My_Iter) & ")");
      Iterator_Advance (My_Iter);
    end if;
    while Has_System_Transition(My_Iter) loop
      Tmp := Append (Tmp, ", (" & Display_Sequence(My_Iter) & ")");
      Iterator_Advance (My_Iter);
    end loop;
    Tmp := Append (Tmp, " }");
    Iterator_Finalize (My_Iter);
    declare
      Static: String := Tmp.all;
    begin
      Free (Tmp);
      return Static;
    end;
  end Display_Fireable_Sequences;
  

  function Has_Fireable_Transitions(This_Conf: System_Configuration;
                         Active_Object:Positive) return Boolean is
    My_Iter: Evolutions_Iterator;
  begin
    Iterator_Initialize(My_Iter,This_Conf,Active_Object);
    if  not Has_System_Transition(My_Iter) then
       Iterator_Finalize(My_Iter);
       return False;
    end if;
    declare
        These_Transitions: Transitions_Table := Get_Sequence(My_Iter);
    begin
      if These_Transitions'Length > 0 then
         Iterator_Finalize(My_Iter);
         return True;
      else
         Iterator_Finalize(My_Iter);
         return False;
      end if;
    end;
  end Has_Fireable_Transitions;


  -----------------------------------------------------------------------
  -- prints on std-output the sets of fireable transitions of the current
  --  chart (tags all transitions of all transition sequences
  --  and prints the complete transitions, one for each line)
  -----------------------------------------------------------------------
  procedure Print_Fireables (The_File: File_Type;
                             This_Conf: System_Configuration;
                             Active_Object: Natural) is
    Current_Chart: Natural := Active_Charts(Active_Object);
    Chart_Transitions: Transitions_Table  :=
          All_Charts(Current_Chart).ChartTransitions.all;
    Which_Transitions: 
      array (Natural range 1..Chart_Transitions'Length) of Boolean :=
        (others => False);
    My_Iter: Evolutions_Iterator;
  begin
    Iterator_Initialize(My_Iter,This_Conf,Active_Object);
    while Has_System_Transition(My_Iter) loop
      declare
        These_Transitions: Transitions_Table := Get_Sequence(My_Iter);
      begin
        for I in These_Transitions'Range loop
          Which_Transitions(These_Transitions(I).Num_Key) := True;
        end loop; 
      end;
      Iterator_Advance (My_Iter);
    end loop;
    Iterator_Finalize(My_Iter);
    -- 
    for I in Which_Transitions'Range loop
      if Which_Transitions(I) then
        Put_Line (The_File, " " & Transition_Image(Chart_Transitions(I),False));
      end if;
    end loop;
  end Print_Fireables;
 

  function HTML_Transition_Format (Source: String) return String is
   Result:String(1..10000);
   OUTC: Natural := 1;
   htmlspace: String := "&nbsp;";
   indent: natural :=0;
   insidebody: Boolean := False;
 begin
   -- s1 -> s2 { - / allElements[2].setMode(Straight); trains[1].start(allElements[6])} 
   -- s1 -> s2  
   --   {  - / 
   --     allElements[2].setMode(Straight); 
   --     trains[1].start(allElements[6]);
   --   } 
   --       
   -- copy SRC into result replacing "<" ">" and adding "<br>
   for I in Source'Range loop
     if Source(I)='<' then
       Result(OUTC..OUTC+3) := "&lt;";
       OUTC := OUTC+4;
       --
     elsif Source(I)='>' then
       Result(OUTC..OUTC+3) := "&gt;";
       OUTC := OUTC+4;
       --
     elsif Source(I)='{' then
       indent := indent+2;
       Result(OUTC..OUTC+3) := "<br>";
       OUTC := OUTC+4;
       for K in 1..indent loop
         Result(OUTC..OUTC+4) := "&nbsp";
         OUTC := OUTC+5;
       end loop;
       Result(OUTC) := '{';
       OUTC := OUTC+1;
       indent := indent+2;
       --
     elsif Source(I)='}' then
       indent := indent-2;
       Result(OUTC..OUTC+3) := "<br>";
       OUTC := OUTC+4;
       for K in 1..indent loop
         Result(OUTC..OUTC+4) := "&nbsp";
         OUTC := OUTC+5;
       end loop;
       Result(OUTC) := '}';
       OUTC := OUTC+1;
     elsif Source(I)=';' and then Source(I+1)/='}'then
       Result(OUTC) := ';';
       OUTC := OUTC+1;
       Result(OUTC..OUTC+3) := "<br>";
       OUTC := OUTC+4;
       for K in 1..indent loop
         Result(OUTC..OUTC+4) := "&nbsp";
         OUTC := OUTC+5;
       end loop;
       --
     elsif Source(I) = '/' and then not insidebody and then Source(I) /= '=' then
       Result(OUTC) := '/';
       OUTC := OUTC+1;
       insidebody := True;
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
   return Result (1..OUTC-1);
  end HTML_Transition_Format;

  -----------------------------------------------------------------------
  -- prints on std-output the sets of fireable transitions of the current
  --  chart (tags all transitions of all transition sequences
  --  and prints the complete transitions, one for each line)
  -----------------------------------------------------------------------
  procedure HTML_Print_Fireables (The_File: File_Type;
                             This_Conf: System_Configuration;
                             Active_Object: Natural) is
    Current_Chart: Natural := Active_Charts(Active_Object);
    Chart_Transitions: Transitions_Table  :=
          All_Charts(Current_Chart).ChartTransitions.all;
    Which_Transitions:
      array (Natural range 1..Chart_Transitions'Length) of Boolean :=
        (others => False);
--    Tmp: String_Ref := new String'("");
    My_Iter: Evolutions_Iterator;
  begin
    Iterator_Initialize(My_Iter,This_Conf,Active_Object);
    while Has_System_Transition(My_Iter) loop
      declare
        These_Transitions: Transitions_Table := Get_Sequence(My_Iter);
      begin
        for I in These_Transitions'Range loop
          Which_Transitions(These_Transitions(I).Num_Key) := True;
        end loop;
      end;
      Iterator_Advance (My_Iter);
    end loop;
    Iterator_Finalize(My_Iter);
    -- QQQ
    for I in Which_Transitions'Range loop
      if Which_Transitions(I) then
        Put_Line (The_File, "<br> " & 
            HTML_Transition_Format(Transition_Image(Chart_Transitions(I),False)));
      end if;
    end loop;
  end HTML_Print_Fireables;


  -----------------------------------------------------------------------
  -- displays the current sequence of actions being execution by 
  -- the current evolution of the current chart
  -----------------------------------------------------------------------
  function Display_Actions (It: Evolutions_Iterator) return String is
    Tmp: String_Ref; 
    This_Evolution: Object_Runtime_Evolution;
  begin
    if It.Objects_Evolutions(it.Active_Object) /= null then
      This_Evolution := 
          It.Objects_Evolutions(it.Active_Object)(It.Current_Object_Evolution);
      declare
       These_Transitions: Transitions_Table
          := Get_Transitions_Sequence(This_Evolution.Transitions);
      begin
        Tmp := new String'("");
        for I in These_Transitions'Range loop
          if These_Transitions(I).Actions /= null then
            Tmp := Append(Tmp,Actions_Image(These_Transitions(I).Actions.all));
          else
            Tmp := Append (Tmp, "-");
          end if;
          if I /= These_Transitions'Last then
            Tmp := Append (Tmp, ", ");
          end if;
        end loop;
      end;
      --
      declare
       Static: String := Tmp.all;
      begin
        Free (Tmp);
        return Static;
      end;
    else
      return "-";
    end if;
  end Display_Actions;
  
  -----------------------------------------------------------------------
  -- displays the current set of active simpel states of the given Chart
  -----------------------------------------------------------------------
  function Display_States (This_Conf: System_Configuration;
                         Active_Object:Positive) return String is
    Sys: Int64 := Int64(This_Conf);
    Active_States: States_Table := 
      Get_Active_States(Get_Object_Conf(Sys,Active_Object));
    Result: String_Ref := new String'("");
    Current_Chart: Natural := Active_Charts(Active_Object);
  begin
    Result := Append (Result,Active_States(1).FullName.all);
    for I in 2.. Active_States'Length loop
       Result := Append (Result, ", " & Active_States(I).FullName.all);
    end loop;   
   declare
     Static: String := Result.all;
   begin
     Free (Result);
     return Static;
   end;
  end Display_States;
  
  
  -----------------------------------------------------------------------
  -- displays the current set of variables=values of the given Chart
  -----------------------------------------------------------------------
  function Display_Vars(This_Conf: System_Configuration;
                         Active_Object:Positive) return String is
    Sys: Int64 := Int64(This_Conf);
    Var_Values: Int_Table := 
       Get_Var_Values(Get_Object_Conf(Sys,Active_Object)); 
    Current_Chart: Natural := Active_Charts(Active_Object);
    This_Var: SystemVar_Ref;
    This_Kind: Value_Kind;
    Result: String_Ref := new String'("");
  begin
    for I in All_Charts(Current_Chart).ChartVars.all'Range loop
       This_Var := All_Charts(Current_Chart).ChartVars(I);
       This_Kind := This_Var.Kind;
       if I > 1 then
          Result := Append (Result, "; ");
       end if;
       Result := Append (Result, This_Var.Name.all & "=" &
         Value_Image(Var_Values(I), This_Kind));
    end loop;
    --
    declare
      Static: String := Result.all;
    begin
      Free (Result);
      if Static'Length > 1 and then
        Static(Static'Length) = ' ' then
        return Static (1.. Static'Length-2);   --  ???????
      else
        return Static;
      end if;
    end;
  end Display_Vars;
  
  -----------------------------------------------------------------------
  -- displays the current trigger of the given chart
  --  BUGGED!!  When we have RANDOMQUEUE we can have several different triggers
  -----------------------------------------------------------------------
--  function Display_Trigger (This_Conf: System_Configuration;
--                               Active_Object: Positive :=1) return String is
--    --
--    Sys: Int64 :=  Int64(This_Conf);
--    Trigger_Signal: Int64 :=
--       Get_Trigger_Signal(
--           Get_Object_Conf(Sys,Active_Object), Active_Object);
--  begin
--    if Trigger_Signal = 0 then
--      --  completion event
--      return "-";
--    elsif Trigger_Signal > 0 then
--      -- stuttering or transiton activation
--      return Signal_Image(Vectors_DB.Retrieve(Trigger_Signal));
--    else
--      return "";
--    end if;
--  end Display_Trigger;

  -----------------------------------------------------------------------
  -- displays the current trigger of the given chart
  --  BUGGED!!  When we have RANDOMQUEUE we can have several different triggers
  -----------------------------------------------------------------------
  function Display_Trigger(It: Evolutions_Iterator) return String is
    --
    Sys: Int64 := It.Current_Configuration;
    Trigger_Signal: Int64 := 
       It.objects_evolutions(it.active_object)(it.current_object_evolution).trigger;
--       Get_Trigger_Signal(
--           Get_Object_Conf(Sys,It.Active_Object), It.Active_Object);
  begin
    if Trigger_Signal = 0 then
      --  completion event
      return "-";
    elsif Trigger_Signal > 0 then  
      -- stuttering or transiton activation
      return Trigger_Image(Vectors_DB.Retrieve(Trigger_Signal));
    else
      return "";
    end if;
  end Display_Trigger;
  
  -----------------------------------------------------------------------
  -- displays the current queue of the given chart
  -----------------------------------------------------------------------
  function Display_Queue (This_Conf: System_Configuration;
                         Active_Object:Positive) return String is
    Sys: Int64 := Int64(This_Conf);
    ORCNick: Int64  := Get_Object_Conf(Sys,Active_Object);
    This_ORC: Object_Runtime_Configuration := ORC_DB.Retrieve(ORCNick);
    QI : Int64_Table :=  Get_Queue_Items(This_ORC.Current_Queue);
    Result: String_Ref := new String'("[");
  begin
    for I in QI'Range loop
       if I > 1 then
          Result := Append (Result, ", ");
       end if;
       Result := 
         Append (Result, Signal_Image(Get_Signal(QI(I)),False));
    end loop;
    declare
      Static: String := Result.all & "]" ;
    begin
      Free (Result);
      return Static;
    end;
  end Display_Queue;

  -----------------------------------------------------------------------
  -----------------------------------------------------------------------
  function Get_Active_States(This_Conf:System_Configuration;
                             Active_Object:Positive) return States_Table is
    Sys: Int64 := Int64(This_Conf);
    Result: States_Table := Get_Active_States(Get_Object_Conf(Sys,Active_Object));
  begin
     return Result;
  end Get_Active_States;

  -----------------------------------------------------------------------
  -- given the GLOBAL position key of the variable return the string
  --    "varname=varvalue"  or "chart.varname=varvalue"
  -----------------------------------------------------------------------
--  function Display_Variable (This_Conf: System_Configuration;
--      The_Variable: Positive) return String is
--    --
--    Sys: Int64 := Int64(This_Conf);
--    This_Var: SystemVar_Ref := All_Vars(The_Variable);
--    Obj: Int64 := Get_Object_Conf(Sys,
--                                   All_Charts(This_Var.Chart).Chart_Position);
--    This_Value: Integer := Get_Var_Value(Obj,This_Var.Local_Num_Key);
--    Varname: String := This_Var.Name.all;
--    Varvalue: String := Value_Image(This_Var, This_Value);
--    --
--  begin
--    if Active_Charts.all'Length = 1 then
--      return Varname & "=" & Varvalue;
--    else 
--      return 
--        All_Charts(This_Var.Chart).Name.all & "." &
--           Varname & "=" & Varvalue;
--    end if;
--  end Display_Variable;

  function Display_Variable (This_Conf: System_Configuration;
                             TheObj: String; 
                             TheVar: String)  return String is
    Sys: Int64 := Int64(This_Conf);
    This_Var: SystemVar_Ref;
    This_Chart: Natural;
--    Result: String_Ref := new String'("");
    TheValue: Integer;
  begin
    This_Chart := 0;
    for I in All_Charts.all'Range loop
      if All_Charts(I).Name.all = TheObj then
         This_Chart:= I;
         for J in All_Charts(I).ChartVars.all'Range loop
            This_Var := All_Charts(I).ChartVars(J);
            if This_Var.name.all = TheVar then
              TheValue := 
                Get_Var_Value(
                    Get_Object_Conf(
                       Integer(This_Conf), All_Charts(I).Chart_Position),J);
              return Value_Image(TheValue, This_Var.Kind);
            end if;
         end loop;
      end if;
    end loop;
    Put_Line (Current_Error,
             "Error in Display_Varioable undefined object (or attribute) ");
    raise UCTL_Error;
  end Display_Variable;


--  function Get_AbstractLabels(It: Evolutions_Iterator) return String_Tables_Vector  is
--  begin
--    return It.Object_Evolutions(
--               It.Current_Object_Evolution).Object_Evolution.AbstractInfo.Labels.all;
 -- end Get_AbstractLabels;

--  function Display_AbstractLabels(It: Evolutions_Iterator) return String is
--  begin
--    return Display_AbstractLabels(Get_AbstractLabels(It));
--  end Display_AbstractLabels;


--  function Get_AbstractStateLabels(This_Conf: System_Configuration) return String_Tables_Vector is
--   LeftValue: Integer;
--   LV,RV: Integer;
--   Ids: String_Table_Ref;
--   Thelabels: String_Table_Ref := new String_Table(1..0);
--   Result: String_Tables_Vector_Ref := new String_Tables_Vector(1..0);
--   -- Observation rule =  (Kind => StateKind, LTerm=null, LOp=obj, LArgs=activestate)
--   EV: Boolean;
--   Tmp: Boolean;
--   DollarNames: array(1..10) of String_Ref;
--   DollarValues: array(1..10) of String_Ref;
--   DollarCount: Natural :=0;
-- begin
--    if All_Observations.all'Length=0 then return Empty_String_Tables; end if;
--    for I in All_Observations.all'Range loop
--      --
--      Tmp := True;
--      DOLLARCOUNT := 0;
--      --
--      -- Tmp is set to False as soon as a rule is seen as not matching
--      --
--      if All_Observations(I).Kind = StateKind then
--        --
--        for L in All_Observations(I).Left.all'Range loop
--        --
--        if All_Observations(I).Left(L).LOp /= null then
--          --
--          --  inState(obj.top.S1.c1)   -- NO VARS IN THESE RULE ELEMENTS 
--          --
--          Ids := new String_Table'( new String'("Is_Active") &
--                                    All_Observations(I).Left(L).LOp &
--                                    All_Observations(I).Left(L).LArgs.all);
--          LeftValue := Eval_Attribute(This_Conf, Ids.all);
--          if  LeftValue /= IntTrue then
--            Tmp := False;
--            exit;
--          end if;
--          --
--        else
--        -- eval the relation
--        EV := True;
--        if Tmp and then All_Observations(I).Left(L).LeftOp /= NOOP and then
--            All_Observations(I).Left(L).IdsR(1).all(1) /= '$' then
--          --
--          --      NO VARS IN THESE LEFTRULE ELEMENTS
--          --   and  obj.attr1 = obj.attr2 ,  obj.attr = null,  obj.attr > 1
--          --
--          LV := Eval_Attribute(This_Conf, All_Observations(I).Left(L).IdsL.all);
 --         RV := Eval_Attribute(This_Conf, All_Observations(I).Left(L).IdsR.all);
--          --
--          case All_Observations(I).Left(L).LeftOp is
--            when GT => EV :=   LV > RV;
--            when GE => EV :=  LV >= RV;
--            when LT => EV :=  LV < RV;
--            when LE => EV :=  LV <= RV;
--            when EQ =>  EV :=  LV = RV;
--            when NE =>  EV :=  LV /= RV;
--            when NOOP => raise UML_Error;
--          end case;
--          --
--          if not EV then
--             Tmp := False;
--             exit;
--          end if;
--          --
--        elsif Tmp and then All_Observations(I).Left(L).LeftOp = EQ and then
 --           All_Observations(I).Left(L).IdsR(1).all(1) = '$' then
--          -- 
--          --   RULE ALWAYS MATCH,  VAR DEFINED
--          --  obj.attr = $i
--          --
--          -- get the value of  IdsL(1).Ids(2)
--          --
--          DOLLARCOUNT := DOLLARCOUNT+1;
--          DOLLARNAMES(DOLLARCOUNT) := All_Observations(I).Left(L).IdsR(1);
--          declare
--            thedynvalue: String_Ref;  -- DOLLARVALUE
 --         begin
--            --
--            if All_Observations(I).Left(L).IdsL.all'Length > 1 and then 
--               All_Observations(I).Left(L).IdsL(2).all /= "queuesize" then
--              -- obj.localvar
--              TheDynValue := 
--                 new String'(
--                    Display_Variable(
--                       This_Conf,
--                       All_Observations(I).Left(L).IdsL(1).all,
--                       All_Observations(I).Left(L).IdsL(2).all));
--            else
--               -- maxqueuesize,  obj.queuesize
--               LV := Eval_Attribute(This_Conf, All_Observations(I).Left(L).IdsL.all);
--               declare
--                 STR: String := Integer'Image(LV);
--               begin
--                if STR(1) = ' ' then
--                  TheDynValue := new String(1..STR'Length-1);
--                  TheDynValue.all := STR(STR'First+1..STR'Last);
--                else
--                  TheDynValue := new String'(STR);
--                end if;
--               end;
--            end if;
--            --
--            DOLLARVALUES(DOLLARCOUNT) := thedynvalue;
--          end;
--          --
--        end if;
--        end if;
--        end loop;  -- CYCLE OBSERVING LEFT_ELEMENTRS AND COLLEECTING VARS
--        --
--        if Tmp then   -- This is MATCHING RULE
--          Thelabels := new String_Table'(All_Observations(I).Rlabels.all);
--          --
--          for V in 1.. DOLLARCOUNT loop   -- for each var defined
--            for K in Thelabels'Range loop
--              if Thelabels(K).all = DOLLARNAMES(V).all then  -- do the substitution
--                Thelabels(K) := DOLLARVALUES(V);
--              end if;
--            end loop;
 --         end loop;  -- for each var
--          Result := new String_Tables_Vector'(Result.all & Thelabels);
--        end if;   -- This is MATCHING RULE
--        --
 --     end if;  -- ruleKind=action
--     end loop;   -- CYCLE FOR ALL RULES
--     return  Result.all;
--  end Get_AbstractStateLabels;


-- function Display_AbstractStateLabels(This_Conf: System_Configuration) return String is
--   Thelabels: String_Tables_Vector := Get_Abstract_State_Labels(This_Conf);
-- begin
--     return Display_AbstractStateLabels(Thelabels);
-- end Display_AbstractStateLabels;

--  function NickName (State: System_Configuration;  Prefix: String := "#") return String is
--     Static: String := Integer'Image(abs(Integer(State)));
--  begin
--     return Prefix & Static(2..Static'Length);
--  end NickName;

-- begin
--   Initialize_Configurations;   -- MOVED INTO LOAD_MODEL
end UML_Configurations;
