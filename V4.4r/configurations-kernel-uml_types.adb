with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
separate (Configurations.Kernel)
package body UML_Types is
--
--  MEMORY FRIENDLY
--

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
 
  --
  --
  --
  function Args_Image (These_Params: EventVars_Table;
                         With_Types: Boolean := True) return String is
  begin
    if These_Params'Length = 1 then
       return These_Params(These_Params'First).Name.all;
    else
       return These_Params(These_Params'First).Name.all & 
          "," & Args_Image(These_Params(These_Params'First+1 ..
                                   These_Params'Last));    
    end if;
  end Args_Image;


 

  function IntBoolExpr_Image (This_IntBoolExpr: IntBoolExpr) return String is
  begin
    case This_IntBoolExpr.Op is
      when LT =>
         return (IntExpr_Image(This_IntBoolExpr.Left.all) & 
                "<" & IntExpr_Image(This_IntBoolExpr.Right.all));
      when GT =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) & 
                ">" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when EQ =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) & 
                "=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when NE =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) & 
                "/=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when LE =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) & 
                "<=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when GE =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) & 
                ">=" & IntExpr_Image(This_IntBoolExpr.Right.all);
    end case;
  end IntBoolExpr_Image;


  function BoolBoolExpr_Image (This_BoolBoolExpr: BoolBoolExpr) return String is
  begin
     case This_BoolBoolExpr.Kind is
       when NoOp =>
         return IntBoolExpr_Image(This_BoolBoolExpr.Compare);
       when NotOp =>
         return "( ! "& BoolBoolExpr_Image(This_BoolBoolExpr.Left.all) & ")" ;
       when AndOp =>
         return "(" & BoolBoolExpr_Image(This_BoolBoolExpr.Left.all) &
             ") & (" & BoolBoolExpr_Image(This_BoolBoolExpr.Right.all) & ")" ;
       when OrOp =>
         return "(" & BoolBoolExpr_Image(This_BoolBoolExpr.Left.all) &
             ") | (" & BoolBoolExpr_Image(This_BoolBoolExpr.Right.all) & ")" ;
     end case;
  end BoolBoolExpr_Image;


  function SimpleIntExpr_Image (This_Simple: SimpleIntExpr) return String is
  begin
    if This_Simple.Image /= null then
      return This_Simple.Image.all; 
    end if;
    --  IS THE FOLLOWIMG EVER NEEDED ? BOH 
    if This_Simple.Local_Variable /= 0 then
        return This_Simple.Image.all;
    elsif This_Simple.Event_Variable /= null then 
        return This_Simple.Image.all;
    elsif This_Simple.Special_Token /= null then 
        return This_Simple.Special_Token.all;
    else 
      declare
        Tmp: String := Integer'Image(This_Simple.Literal_Value);
      begin
        if Tmp(1) =' ' then
          return Tmp (2..Tmp'Length);
        else 
          return Tmp;
        end if;
      end;
    end if;
  end SimpleIntExpr_Image;

  function IntExpr_Image (This_IntExpr: IntExpr) return String is
  begin
    case This_IntExpr.Op is
      when No_Op => 
        if This_IntExpr.left = null then 
          return SimpleIntExpr_Image(This_IntExpr.Simple.all);
        else
          if This_IntExpr.Left.Op = No_Op then
            return (IntExpr_Image(This_IntExpr.Left.all));
          else
            return ("(" & IntExpr_Image(This_IntExpr.Left.all) & ")" );
          end if;
        end if;
      when Plus | Join  =>
        return IntExpr_Image(This_IntExpr.Left.all) &
               "+" & IntExpr_Image(This_IntExpr.Right.all);
      when Times =>
        return IntExpr_Image(This_IntExpr.Left.all) & 
              "*" & IntExpr_Image(This_IntExpr.Right.all);
      when Minus =>
        return IntExpr_Image(This_IntExpr.Left.all) &
            "-" & IntExpr_Image(This_IntExpr.Right.all);
      when Div =>
        return IntExpr_Image(This_IntExpr.Left.all) &
            "/" & IntExpr_Image(This_IntExpr.Right.all);
      when Modulus =>
         return IntExpr_Image(This_IntExpr.Left.all) &
             " mod " & IntExpr_Image(This_IntExpr.Right.all);
      when Enclosed =>   --  NEVER NEEDED !!!!
         return  "(" & IntExpr_Image(This_IntExpr.Left.all) & ")" ;
    end case;
  end IntExpr_Image ;

  function IntExpr_Kind (This_IntExpr: IntExpr) return Value_Kind is
    Result: Value_Kind;
  begin
    if This_IntExpr.Left = null then
       Result := This_IntExpr.Simple.Kind;
    else
       return (IntExpr_Kind(This_IntExpr.Left.all));
    end if;
    return Result;
  end IntExpr_Kind ;

  function umlExpr_Kind (This_umlExpr: umlExpr) return Value_Kind is
  begin
    if This_umlExpr.umlInt /= null then 
          return IntExpr_Kind(This_umlExpr.umlInt.all);
    else
       return Bool;
    end if;
  end umlExpr_Kind ;

  function umlExpr_Image (This_umlExpr: umlExpr) return String is
  begin
    if This_umlExpr.umlInt /= null then
        return IntExpr_Image(This_umlExpr.umlInt.all);
    else
        return BoolBoolExpr_Image(This_umlExpr.umlBool.all);
    end if;
  end umlExpr_Image;
  -------------------------
  -- called by Image of Action:
  --  in the case of calls, the table is the slice starting at the
  --   second position
  -------------------------
  function Exprs_Image (These_IntExprs: IntExpr_Table) return String is
  begin
     if These_IntExprs'Length = 0  then
        return "";
     elsif These_IntExprs'Length = 1 then
        return IntExpr_Image(These_IntExprs(These_IntExprs'First).all);
     else
        return IntExpr_Image(These_IntExprs(These_IntExprs'First).all) & "," &
               Exprs_Image(These_IntExprs(These_IntExprs'First+1 ..
                                    These_IntExprs'Last));

     end if;
  end Exprs_Image;

  -------------------------
  -- called by Image of Action:
  --  in the case of calls, the table is the slice starting at the
  --   second position
  -------------------------
  function Exprs_Image (These_IntExprs: umlExpr_Table) return String is
  begin
     if These_IntExprs'Length = 0  then
        return "";
     elsif These_IntExprs'Length = 1 then
        return umlExpr_Image(These_IntExprs(These_IntExprs'First).all);
     else
        return umlExpr_Image(These_IntExprs(These_IntExprs'First).all) & "," &
               Exprs_Image(These_IntExprs(These_IntExprs'First+1 ..
                                    These_IntExprs'Last));

     end if;
  end Exprs_Image;

  function Target_Image (This_Instance: Event_Instance) return String is
  begin
    if This_Instance.The_Target.Literal_Value /= -1 then
      return All_Charts(This_Instance.The_Target.Literal_Value).Name.all;
    end if;
    if This_Instance.The_Target.Local_Variable /= 0 then
      return This_Instance.The_Target.Image.all;
    else 
      return This_Instance.The_Target.Image.all;
    end if;
  end Target_Image;

  --  
  -- called by  Image (of Action)  called by Image  (of Transition)
  --
  function SignalAct_Image (This_Instance: Event_Instance) return String is
    Target: String :=  Target_Image(This_Instance);
  begin
    if Active_Charts.all'Length >1 or else 
      This_Instance.The_Target.Literal_Value >1 then
      if This_Instance.The_Args.all'Length = 0 then
        return Target & "." & This_Instance.The_Event.Name.all;
      else
         return Target & "." & This_Instance.The_Event.Name.all & 
                "(" & Exprs_Image(This_Instance.The_Args.all) & ")";
      end if;
    else
      if This_Instance.The_Args.all'Length = 0 then
        return This_Instance.The_Event.Name.all;
      else
         return This_Instance.The_Event.Name.all & 
                "(" & Exprs_Image(This_Instance.The_Args.all) & ")";
      end if;
    end if;
  end SignalAct_Image;

  --
  -- Notice that Call actions have an additional first parameter 
  -- (the caller) which should not be displayed
  -- 
  function Action_Image (This_Action: Action) return String is
     This_Event: Event_Instance;
     This_Target: SimpleIntExpr_Ref;
     Prefix: String_Ref;
     Prev: String_Ref;
  begin
     if This_Action.Kind = Exitloop then
       return "exit"; 
       --
     elsif This_Action.Kind = Conditional then
      --
      --   if  <cond>  { list } else {list}
      --
      if This_Action.ElseBody.all'length > 0 then
        Prefix := 
          new String'("if " & BoolBoolExpr_Image(This_Action.IfCond.all)  &
            " {" &
             Actions_Image(This_Action.ThenBody.all(1..This_Action.ThenBody.all'length)) &
            "} else {" &
             Actions_Image(This_Action.ElseBody.all(1..This_Action.ElseBody.all'length)) &
            "}" );
       else
         Prefix :=
          new String'("if " & BoolBoolExpr_Image(This_Action.IfCond.all)  &
            " {" &
             Actions_Image(This_Action.ThenBody.all(1..This_Action.ThenBody.all'length)) &
            "}" );
       end if;
       declare
         Result: String := Prefix.all;
       begin
          Free(Prefix);
          return Result;
       end;
       --
     elsif This_Action.Kind = WhileLoop then
       --
       --   while cond {  }
       --
       Prefix :=
          new String'("while " &
             BoolBoolExpr_Image(This_Action.LoopCond.all)  &
            " {" &
             Actions_Image(This_Action.LoopBody.all(1..This_Action.LoopBody.all'length)) &
            "}" );    --  NOMORE  skip the last Endloop intyroduced by the parser
       --
       declare
         Result: String := Prefix.all;
       begin
          Free(Prefix);
          return Result;
       end;
       --

     elsif This_Action.Kind = ForLoop then
       --
       --   for v in min .. max {  }
       --
       Prefix := 
          new String'("for " & This_Action.For_Var.Name.all  &  " in " &
            IntExpr_Image(This_Action.For_Min.all) &
            " .. " &
            IntExpr_Image(This_Action.For_Max.all) &
            " { " & 
             Actions_Image(This_Action.LoopBody.all(1..This_Action.LoopBody.all'length)) &
            "}" );    --  NOMORE  skip the last Endloop intyroduced by the parser
       --
       declare
         Result: String := Prefix.all;
       begin
          Free(Prefix);
          return Result;
       end;
       --
     elsif This_Action.Kind = VarDecl then
       --
       --  tvar: tttype;   var: ttype[];
       --
       Prefix := new String'(This_Action.TVar.Name.all);
       if  This_Action.TVar.Kind  = Number then
         Prev := Prefix;
         Prefix := new String'(Prefix.all & ":int");
         Free (Prev);
       elsif  This_Action.TVar.Kind   = Numvector then
         Prev := Prefix;
         Prefix := new String'(Prefix.all & ":int[]");
         Free (Prev);
       elsif  This_Action.TVar.Kind   = Bool then
         Prev := Prefix;
         Prefix := new String'(Prefix.all & ":bool");
         Free (Prev);
       elsif  This_Action.TVar.Kind   = Boolvector then
         Prev := Prefix;
         Prefix := new String'(Prefix.all & ":bool[]");
         Free (Prev);
       elsif  This_Action.TVar.Kind   = Object then
        Prev := Prefix;
        if This_Action.TVar.TypeInfo   = 0 then
          Prefix := new String'(Prefix.all & ":obj");
         else
           Prefix := new String'(Prefix.all & 
                      ":" & All_Classes(This_Action.TVar.TypeInfo).name.all);
         end if;
         Free (Prev);
       elsif  This_Action.TVar.Kind= objvector then
         Prev := Prefix;
         if This_Action.TVar.TypeInfo = 0 then
           Prefix := new String'(Prefix.all & ":obj[]");
         else
           Prefix := new String'(Prefix.all & 
                 ":" & All_Classes(This_Action.TVar.TypeInfo).name.all &"[]");
         end if;
         Free (Prev);
       end if;  -- Kind = ...
       --
       if  This_Action.TValue /= null then
         Prev := Prefix;
         Prefix := new String'(Prefix.all & 
                   " := " & umlExpr_Image(This_Action.TValue.all));
         Free (Prev);
       end if; 
       return Prefix.all;
       --
     elsif This_Action.Kind = Assignment then
       --
       -- Assignment
       --
       return This_Action.Assignment_Left_Image.all &  
              ":=" & umlExpr_Image(This_Action.Assignment_Right.all);
       --
     end if;
     --
     -- self.event(args)    -- self can be omitted
     -- OUT.event(Args)
     -- CHART.event(args)   -- CHART can be omitted if only active
     -- var.event(args)
     -- eventvar.event(args)
     --
     if  This_Action.Kind = Signal  or else
          This_Action.Kind =  Call  then
       --
       -- self.event(args)    -- self can be omitted
       -- OUT.event(Args)
       -- CHART.event(args)   -- CHART can be omitted if only active
       -- var.event(args)
       -- eventvar.event(args)
       --
       This_Event := This_Action.Signalled_Event;
       This_Target := This_Event.The_Target;
       if This_Target.Event_Variable /= null then
         Prefix := new String'(This_Target.Event_Variable.Name.all);
       elsif  This_Target.Local_Variable /= 0 then
         Prefix := new String'(This_Target.Image.all);
       elsif This_Target.Special_Token = null and then
            This_Target.Kind = Object and then
            Active_Charts.all'Length > 1 then
         Prefix := new String'(This_Target.Image.all); 
       elsif This_Target.Literal_Value in All_Charts.all'Range  then
         Prefix := new String'(All_Charts(This_Target.Literal_Value).Name.all);
       end if;
       -- 
       if This_Action.Kind = Signal and then 
              Prefix /= null and then This_Event.The_Args.all'Length = 0 then 
         Prev := Prefix;
         Prefix := new String'(Prefix.all & "." & This_Event.The_Event.Name.all);
         Free(Prev);
         declare
           result: String := Prefix.all;
         begin
           Free(Prefix);
           return Result;
         end;
         --
       elsif This_Action.Kind = Call and then 
              Prefix /= null and then This_Event.The_Args.all'Length = 1 then
         Prev := Prefix;
         Prefix := new String'(Prefix.all & "." & This_Event.The_Event.Name.all);
         Free(Prev);
         declare
           result: String := Prefix.all;
         begin
           Free(Prefix);
           return Result;
         end;
         --
       elsif This_Action.Kind = Signal and then 
           Prefix = null and then This_Event.The_Args.all'Length = 0 then
         return This_Event.The_Event.Name.all;
         --
       elsif This_Action.Kind = Call and then 
           Prefix = null and then This_Event.The_Args.all'Length = 1 then
         return This_Event.The_Event.Name.all;
         --
       elsif This_Action.Kind = Signal and then
          Prefix /= null and then This_Event.The_Args.all'Length > 0 then
         Prev := Prefix;
         Prefix := 
           new String'(Prefix.all & "." & This_Event.The_Event.Name.all &
                      "(" & Exprs_Image(This_Event.The_Args.all) & ")");
         Free(Prev);
         declare
           result: String := Prefix.all;
         begin
           Free(Prefix);
           return Result;
         end;
         --
       elsif This_Action.Kind = Call and then
          Prefix /= null and then This_Event.The_Args.all'Length > 1 then
         Prev := Prefix;
         Prefix := 
           new String'(Prefix.all & "." & This_Event.The_Event.Name.all &
             "(" &
              Exprs_Image(
                This_Event.The_Args.all(2..This_Event.The_Args.all'Length)) &
              ")");
         Free(Prev);
         declare
           result: String := Prefix.all;
         begin
           Free(Prefix);
           return Result;
         end;
         --
       else 
         return  This_Event.The_Event.Name.all &
             "(" &
              Exprs_Image(This_Event.The_Args.all) &
              ")";
       end if; 
     end if;
     --
     if This_Action.Kind = Function_Call then
       --
       --  Function_Call     "  var :=   target.event (args)
       --
       Prefix := new String'(This_Action.Assignment_Left_Image.all & 
              ":=" & This_Action.Signalled_Event.The_Target.Image.all &
              "." & This_Action.Signalled_Event.The_Event.Name.all);
       if This_Action.Signalled_Event.The_Args.all'Length >0 then
         Prev := Prefix;
         Prefix := new String'(Prefix.all &
           "(" & Exprs_Image( This_Action.Signalled_Event.The_Args.all) & ")");
         Free(Prev);
       end if;
       declare
         result: String := Prefix.all;
       begin
         Free(Prefix);
         return Result;
       end;
     end if;
    -- 
    if This_Action.Kind = OpReturn then
      if This_Action.Signalled_Event.The_Args(1) = null then
        return "return";
      else  
        return "return(" & 
              umlExpr_Image(This_Action.Signalled_Event.The_Args(1).all) &
               ")" ;
      end if;
    end if;
    --
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Undefined kind of action in Action_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Undefined kind of action in Action_Image");
    return "Runtime_Error";
  end Action_Image;

  function Actions_Image (These_Actions: Actions_Table) return String is
    --
  begin
     if These_Actions'Length = 0  then
        return "-";
     elsif These_Actions'Length = 1 then
       return Action_Image(These_Actions(These_Actions'First).all);
     else
       return Action_Image(These_Actions(These_Actions'First).all) & "; " &
        Actions_Image(These_Actions(These_Actions'First+1..These_Actions'Last));
     end if;
  end Actions_Image;

procedure Print_Transition (This: Transition_Ref) is
begin
  Put (Transition_Image(This));
end;


 function Transition_Label (This: Transition_Ref) return String is
 begin
   if This.Label= null or else This.Label.all'Length=0 then
    declare
      Tmp: String := Integer'Image(This.Num_Key);
    begin
      Tmp(1) := '#';
      return Tmp;
    end;
  else
    return This.Label.all;
  end if;
 end Transition_Label;


 function Dot_Format(Source:String) return String is
   Tmp: String(1..10000);
   Ind: Natural :=0;
 begin
   for I in Source'range loop
    Ind := Ind+1;
    Tmp(Ind) := Source(I);
    if Source(I) = '{' or else 
       (Source(I) = '}' and then I < Source'Last and then Source(I+1) /= ';') 
       or else Source(I) = ';' then
      Tmp(Ind+1..Ind+2) := "\n";   
      Ind := Ind+2;
    end if;
   end loop;
   return Tmp(1..Ind);
 end Dot_Format;

 ---------------------------------------------------------------
 -- called while drawing arcs inside dot diagrams
 ---------------------------------------------------------------
 function Transition_DotImage (This: Transition_Ref;
                                Label_Included: Boolean := True) return String is
   --
   Result: String_Ref := new String'("");
   Tmp: Transition_Ref := This;
 begin
   --
--   if Label_Included then
--     Result := new String'(Transition_Label(This) & ": ");
--   end if; 
  --
  -- display the trigger/guard/actions
  --
  Result := Append (Result,  "");
  if Tmp.Trigger.Params.all'Length = 0  or else
      (Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length=1) then
    Result := Append (Result,Tmp.Trigger.Name.all);
  elsif Tmp.Trigger.Kind=Signal then
    Result := Append (Result,Tmp.Trigger.Name.all & "(" &
                Args_Image(Tmp.Trigger.Params.all) & ")" );
  elsif Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length>1 then
    Result := Append (Result,Tmp.Trigger.Name.all & "(" &
       Args_Image(Tmp.Trigger.Params(2..Tmp.Trigger.Params.all'Length)) & ")" );
  end if;
  --
  if Tmp.Guard /= null then
    Result := Append (Result, "\n  [" & BoolBoolExpr_Image(Tmp.Guard.all) & "]\n");
  end if;
  if Tmp.Actions.all'length >0 then
    Result := Append (Result, "/\n");
    Result := Append (Result, Dot_Format(Actions_Image(Tmp.Actions.all)));  
  end if;
  --
  declare
    Static: String := Result.all;
  begin
     Free (Result);
     return Static;
  end;  
 end Transition_DotImage;

function Transition_Image (This: Transition_Ref;
                              Label_Included: Boolean := True) return String is
  Result: String_Ref := new String'("");
  Owner_Name: String := This.Owner.FullName.all;
  From: Positive := Owner_Name'Length + 2; -- if we want to skip the full name
  To: Positive;
  Tmp: Transition_Ref := This;
begin
  --  but we want the full name except the first "Top" (or other defined toplike)
  for K in Owner_Name'Range loop
    if Owner_Name(K)= '.' then
      From := K+1;
      exit;
    end if;
  end loop;
  -- display the label
  --
  if Label_Included then
    Result := Append (Result, Transition_Label(This));
    Result := Append (Result,  ": ");
  end if;
  --
  -- display the sources
  --
  if Tmp.Source.all'Length > 1 then
      Result := Append (Result,  " (" );
  end if;
  To := Tmp.Source(1).FullName.all'Length;
  Result := Append (Result, Tmp.Source(1).FullName(From..To));
  for L in 2..Tmp.Source.all'Length loop
      To := Tmp.Source(L).FullName.all'Length;
      Result := Append (Result, "," & Tmp.Source(L).FullName(From..To));
  end loop;
  if Tmp.Source.all'Length > 1 then
    Result := Append (Result,  ")" );
  end if;
  --
  Result := Append (Result,  " -> ");
  --
  -- Display the targets
  --
  if Tmp.Target.all'Length > 1 then
      Result := Append (Result,  "(" );
  end if;
  To := Tmp.Target(1).FullName.all'Length;
  Result := Append (Result, Tmp.Target(1).FullName(From..To));
     for L in 2..Tmp.Target.all'Length loop
        To := Tmp.Target(L).FullName.all'Length;
        Result := Append (Result, "," & Tmp.Target(L).FullName(From..To));
     end loop;
  if Tmp.Target.all'Length > 1 then
    Result := Append (Result,  ")" );
  end if;
  --
  --
  -- display the trigger/guard/actions
  --
--  Result := Append (Result,  " -( ");
  Result := Append (Result,  " { ");
  if Tmp.Trigger.Params.all'Length = 0  or else
      (Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length=1) then
    Result := Append (Result,Tmp.Trigger.Name.all);
  elsif Tmp.Trigger.Kind=Signal then
    Result := Append (Result,Tmp.Trigger.Name.all & "(" &
                Args_Image(Tmp.Trigger.Params.all) & ")" );
  elsif Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length>1 then
    Result := Append (Result,Tmp.Trigger.Name.all & "(" &
       Args_Image(Tmp.Trigger.Params(2..Tmp.Trigger.Params.all'Length)) & ")" );
  end if;
--  Result := Append (Result, Event_Image(Tmp.Trigger.all));
  if Tmp.Guard /= null then
    Result := Append (Result, " [" & BoolBoolExpr_Image(Tmp.Guard.all) & "]");
  end if;
  Result := Append (Result, " / ");
  Result := Append (Result, Actions_Image(Tmp.Actions.all));
--  Result := Append (Result,  " )-> ");
  Result := Append (Result,  "} ");
  --
  declare
    Static: String := Result.all;
  begin
     Free (Result);
     return Static;
  end;
 end Transition_Image;

function Transition_ShortImage (This: Transition_Ref;
                              Label_Included: Boolean := True) return String is
  Result: String_Ref := new String'("");
  Tmp: Transition_Ref := This;
begin
  -- display the label
  --
  if Label_Included then
    Result := Append (Result, Transition_Label(This));
    Result := Append (Result,  ": ");
  end if;
  --
  --
  -- display the trigger/guard
  --
  Result := Append (Result,Tmp.Trigger.Name.all & " / ");
  --
--  if Tmp.Trigger.Params.all'Length = 0  or else
--      (Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length=1) then
----    Result := Append (Result,Tmp.Trigger.Name.all);
--  elsif Tmp.Trigger.Kind=Signal then
--    Result := Append (Result,Tmp.Trigger.Name.all & "(" &
--                Args_Image(Tmp.Trigger.Params.all) & ")" );
--  elsif Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length>1 then
--    Result := Append (Result,Tmp.Trigger.Name.all & "(" &
--       Args_Image(Tmp.Trigger.Params(2..Tmp.Trigger.Params.all'Length)) & ")" );
--  end if;
--  if Tmp.Guard /= null then
--    Result := Append (Result, "[" & BoolBoolExpr_Image(Tmp.Guard.all) & "]");
--  end if;
--  --
--  -- display the first action, if present, and if SIMPLE
--  if Tmp.Actions.all'length >0  and then
--      (Tmp.Actions(1).Kind = Signal or else
--       Tmp.Actions(1).Kind = Call or else
--       Tmp.Actions(1).Kind =  Function_Call or else
--       Tmp.Actions(1).Kind =  Assignment or else
--       Tmp.Actions(1).Kind =  OpReturn )  then
--     Result := Append (Result, "/");
--     Result := Append (Result, Actions_Image(Tmp.Actions.all(1..1)));
--     if Tmp.Actions.all'length >1 then
--          Result := Append (Result, ";...");
--     end if;
--  elsif Tmp.Actions.all'length >0 then
--     Result := Append (Result, "/...");
--  end if;
  --
  declare
    Static: String := Result.all;
  begin
     Free (Result);
     return Static;
  end;
 end Transition_ShortImage;


function Is_Active_Chart(The_Chart: Natural) return Boolean is
begin
  if The_Chart not in All_Charts.all'Range  then 
     return False;
  else
    return All_Charts(The_Chart).Top_State_Num /= 0;
  end if;
end Is_Active_Chart; 


 function Value_Image (Code: Integer; Kind: Value_kind) return String is
    NewCode: Integer;
    NewKind: Value_Kind;
 begin
   if Code = IntTrue then 
      return "true";
   elsif Code = IntFalse then
      return "false";
   end if;
   if Code <= ObjectBase then
     NewCode := ObjectBase - Code;
     NewKind := Object;
   elsif Code <=  StructBase then
     NewCode := StructBase - Code;
     NewKind := Composite;
   elsif Kind = Bool then
     NewCode := Code;
     NewKind := Bool;
   else
     NewCode := Code;
     NewKind := Number;
   end if;
   --
   if NewKind = Object and NewCode in All_Charts.all'Range  then
        return All_Charts(NewCode).name.all;
   elsif NewKind = Object and NewCode = 0 then
        return "null";
   elsif NewKind = Object then
       return "undefined";
   end if;
   if NewKind = Bool and NewCode = IntFalse then 
        return "False";
   elsif NewKind = Bool then
      return "True";
  end if ;
  --
  if NewKind = Composite or
       NewKind = Objvector or
         NewKind =Boolvector or
           NewKind =Numvector then
    declare
       thisdata: Int_Table := Vectors_DB.Retrieve(NewCode);
       result: String_Ref := new String'("[");
       prev: String_Ref;
    begin
       if NewKind = Objvector then
         for I in thisdata'Range loop
           if I > thisdata'First then
             prev := result;
             result := new String'(result.all & ",");
             Free(prev);
           end if;
           prev := result;
           result :=
              new String'(result.all & Value_Image(thisdata(I),Object));
           Free(prev);
         end loop;
       elsif NewKind = Composite then
         for I in thisdata'Range loop
           if I > thisdata'First then
             prev := result;
             result := new String'(result.all & ",");
             Free(prev);
           end if;
           prev := result;
           result :=
              new String'(result.all & Value_Image(thisdata(I), Undefined));
           Free(prev);
         end loop;
       elsif NewKind = Numvector then
         for I in thisdata'Range loop
           if I > thisdata'First then
             prev := result;
             result := new String'(result.all & ",");
             Free(prev);
           end if;
           prev := result;
           result :=
              new String'(result.all & Value_Image(thisdata(I), Number));
           Free(prev);
         end loop;
       elsif NewKind = Boolvector then
         for I in thisdata'Range loop
           if I > thisdata'First then
             prev := result;
             result := new String'(result.all & ",");
             Free(Prev);
           end if;
           prev := result;
           result :=
              new String'(result.all & Value_Image(thisdata(I), Bool));
           Free(prev);
         end loop;
       end if;
       prev := result;
       result := new String'(result.all & "]");
       Free(prev);
       declare
         Str: String := result.all;
       begin
          Free(result);
          return Str;
       end;
    end;
  else
    declare
      Num : String := Integer'Image(NewCode);
    begin
      if num(1) = ' ' then
        declare
          res: String (1..Num'Length-1) := Num(2..Num'Last);
        begin
          return  res;
        end;
      else
        return Num;
    end if;
    end; 
   end if;
end Value_Image;

 --------------------------------------------------------------
  -- like Signal_Image  but without showing the target
  --  do not cosider  assign and lostevent case
 --------------------------------------------------------------
 function Trigger_Image (The_Signal: Int_Table) return String is
   Result: String_Ref;
   This_Event: Event_Ref;
   I: Natural := The_Signal'First;
   This_Target: Integer;
   This_Kind: Value_Kind := Undefined;
 begin
   if The_Signal'Length = 0 then
      return "-";
   end if;
   if The_Signal(I) not in
      All_Events.all'Range then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Undefined event in Trigger_Image");
    end if;
    Runtime_Error_Msg := new String'("Undefined event in Trigger_Image");
    Runtime_Errors_Count := Runtime_Errors_Count +1;
     return "Runtime_Error";
     --raise UML_Error;
   end if;
   This_Event := All_Events(The_Signal(I));
   -- At this point (when displaying a trigger) the event params 
   --  DOES ALWAYS ALREADY INCLUDE the implicit caller arg.
   if This_Event.Kind = Signal then
     if The_Signal'Length < 2 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect param profile in Trigger_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Incorrect param profile in Trigger_Image");
     return "Runtime_Error";
--     raise UML_Error;
     end if;
   else  --  Kind = Operation  (consider also the implicit caller param)
     if The_Signal'Length < 3 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect param profile in Trigger_Image");
    end if;
    Runtime_Error_Msg := new String'("Incorrect param profile in Trigger_Image");
    Runtime_Errors_Count := Runtime_Errors_Count +1;
     return "Runtime_Error";
--     raise UML_Error;
     end if;
   end if;
   --
   This_Target := The_Signal(I+1);
   if This_Target not in All_Charts.all'Range then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Erroneous target object in Trigger_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Erroneous target object in Trigger_Image");
     return "Runtime_Error";
--   raise UML_Error;
   end if;
   --
     -- not the special LOSTEVENT or ASSIGN cases (hence true SIGNAL OR CALL)
     --
     -- only if the user specification contains exctly one chart and the
     --  event is not relative to some non-active (e.g. "Out") chart  then
     --  avoid to expand the signal with the explicit chart name
     --
     --
     Result := Append (Result,This_Event.Name.all);
     --
     if This_Event.Kind = Signal and then
           This_Event.Params.all'Length > 0 then
       Result := Append (Result,"(");
       for J in This_Event.Params.all'Range loop
         if This_Event.Params /= null and then
           This_Event.Params(J) /= null then
           This_Kind := This_Event.Params(J).Kind;
         else
           This_Kind := Undefined;
         end if;
         if This_Event.Kind = Signal  then
           Result := Append( Result, Value_Image(The_Signal(I+1+J),This_Kind));
         else
           Result := Append( Result, Value_Image(The_Signal(I+2+J),This_Kind));
         end if;
         if J < This_Event.Params.all'Length then
           Result := Append (Result,",");
         else
           Result := Append (Result,")");
         end if;
       end loop;
     end if;
   --
   declare
     Static: String := Result.all;
   begin
     Free (Result);
     return Static;
   end;
 end Trigger_Image;


 function Event_Image (The_Signal: Int_Table) return String is
   This_Event: Event_Ref;
   I: Natural := The_Signal'First;
 begin
   if The_Signal'Length = 0 then
      return "-";
   end if;
   if The_Signal(I) not in
      All_Events.all'Range then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Undefined event in Event_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Undefined event in Event_Image");
     return "Runtime_Error";
--   raise UML_Error;
   end if;
   This_Event := All_Events(The_Signal(I));    -- The_Signal(1)
   if This_Event.Kind = Signal or This_Event.Kind = Undefined then
     if The_Signal'Length < 2 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect parameters in Event_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Incorrect parameters in Event_Image");
     return "Runtime_Error";
--     raise UML_Error;
     end if;
   else  --  Kind = Operation  (consider also the implicit caller param)
     if The_Signal'Length < 3 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect parameters in Event_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Incorrect parameters in Event_Image");
     return "Runtime_Error";
 --    raise UML_Error;
     end if;
   end if;
   return This_Event.Name.all;
 end Event_Image;


 function Target_Image (The_Signal: Int_Table) return String is
   --
   I: Natural := The_Signal'First;
 begin
   if The_Signal'Length = 0 then
      return "-";
   end if;
   if The_Signal(I+1) not in
      All_Charts.all'Range then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Invalid target in Target_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Invalid target in Target_Image");
     return "Runtime_Error";
--   raise UML_Error;
   end if;
   return All_Charts(The_Signal(I+1)).Name.all;
 end Target_Image;


 function Args_Count (The_Signal: Int_Table) return Natural is
   This_Event: Event_Ref;
   I: Natural := The_Signal'First;
 begin
   if The_Signal'Length = 0 then
      return 0;
   end if;
   if The_Signal(I) not in
      All_Events.all'Range then
     return 0;
--     raise UML_Error;
   end if;
   This_Event := All_Events(The_Signal(I));
   if This_Event.Kind = Signal or This_Event.Kind = Undefined then
      if This_Event.Params /= null then 
        return This_Event.Params.all'Length;
      else 
       return 0;
      end if;
   else
      return This_Event.Params.all'Length-1;
   end if;
 end Args_Count;

 function Arg_Image (The_Signal: Int_Table; 
             ArgNum: Positive; Chart:Natural) return String is
   This_Event: Event_Ref;
   I: Natural := The_Signal'First;
   This_Kind: Value_Kind := Undefined;
 begin
   if The_Signal'Length = 0 then
      return "-";
   end if;
   if The_Signal(I) not in
      All_Events.all'Range then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Undefined event in Arg_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Undefined event in Arg_Image");
     return "Runtime_Error";
--   raise UML_Error;
   end if;
   This_Event := All_Events(The_Signal(I));
   --
   if This_Event.Kind = Signal then
     if The_Signal'Length < 2 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect args in Arg_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Incorrect args in Arg_Image");
     return "Runtime_Error";
--     raise UML_Error;
     end if;
   else  --  Kind = Operation  (consider also the implicit caller param)
     if The_Signal'Length < 3 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect args in Arg_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Incorrect args in Arg_Image");
     return "Runtime_Error";
--     raise UML_Error;
     end if;
   end if;
   --
   if  This_Event.Num_Key=Assign_Event.Num_Key then    --   "assign(x, v)"
     if ArgNum=1 then 
         --return All_Vars(The_Signal(I+2)).Name.all; 
         return All_Charts(Chart).ChartVars(The_Signal(I+2)).Name.all; 
     elsif ArgNum=2 then
--        return Trim(Integer'Image(The_Signal(I+3)-1));
       -- QQQ
        return Value_Image(The_Signal(I+3),numvector);
     elsif ArgNum=3 then
        return Value_Image(The_Signal(I+4), 
            All_Charts(Chart).ChartVars(The_Signal(I+2)).Kind);
     else
     if Runtime_Errors_Enabled then
        Put_Line(Current_Error, 
           "UMC Runtime_Error!  args in Arg_Image");
     end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("args in Arg_Image");
       return "Runtime_Error";
--     raise UML_Error;
     end if;
     --
   elsif  This_Event.Num_Key=2 then    -- ERR.lostevent(e) 
       if ArgNum=1 then
          return All_Events(The_Signal(I+2)).Name.all;
       else
         if Runtime_Errors_Enabled then
          Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect args in Arg_Image");
         end if;
        Runtime_Errors_Count := Runtime_Errors_Count +1;
        Runtime_Error_Msg := new String'("Incorrect args in Arg_Image");
         return "Runtime_Error";
--        raise UML_Error;
       end if;
     --
   end if;
     --  no  ASSIGN nor  LOSTEVENT,  just plain signal or operation-call or return
     --     WHAT IF  "Show_Params == FALSE??"
     if This_Event.Kind = Signal or This_Event.Kind = Undefined then
       if ArgNum not in This_Event.Params.all'Range then
         if Runtime_Errors_Enabled then
           Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect args in Arg_Image");
         end if;
         Runtime_Errors_Count := Runtime_Errors_Count +1;
         Runtime_Error_Msg := new String'("Incorrect args in Arg_Image");
         return "Runtime_Error";
--        raise UML_Error;
       end if;
        if This_Event.Params /= null and then
           This_Event.Params(ArgNum) /= null then
           This_Kind := This_Event.Params(ArgNum).Kind;
         else
           This_Kind := Undefined;
         end if;
        return Value_Image(The_Signal(I+1+ArgNum),This_Kind);
     elsif This_Event.Kind = Operation then
         if This_Event.Params = null  or else
            This_Event.Params.all'Length <= 1 or else
            ArgNum+1 not in This_Event.Params.all'Range then
            if Runtime_Errors_Enabled then
              Put_Line(Current_Error, 
               "UMC Runtime_Error! Incorrect args in Arg_Image");
             end if;
             Runtime_Errors_Count := Runtime_Errors_Count +1;
             Runtime_Error_Msg := new String'("Incorrect args in Arg_Image");
           return "Runtime_Error";
--         raise UML_Error;
         end if;
         if This_Event.Params /= null and then
           This_Event.Params(ArgNum+1) /= null then
           This_Kind := This_Event.Params(ArgNum+1).Kind;
         else
           This_Kind := Undefined;
         end if;
         return  Value_Image(The_Signal(I+2+ArgNum),This_Kind);
     else  
       if Runtime_Errors_Enabled then
           Put_Line(Current_Error, 
           "UMC Runtime_Error! Undefined event kind in Arg_Image");
       end if;
       Runtime_Errors_Count := Runtime_Errors_Count +1;
       Runtime_Error_Msg := new String'("Undefined event kind in Arg_Image");
        return "Runtime_Error";
--      raise UML_error;
     end if;
 end Arg_Image; 

 --------------------------------------------------------------
 --  A signal is a tuple:
 --  <event_key, target, arg1, arg2, ....>
 -- we should change integer values to ids when they denote objects or events
 --  or vars
 --  E.g.  lostevent(n), assign(var,val)
 --  
 -- In case of call operations, the tuple contains an additional
 --  implicit value: the caller. We should not consider it.
 -- At runtime  All_Events(I) is a complete signal definition
 -- which contains the implicit caller arg.
 --------------------------------------------------------------
 function Signal_Image (The_Signal: Int_Table; 
                        Target_Included: Boolean := True) return String is
   Result: String_Ref;
   Prev: String_Ref;
   This_Event: Event_Ref;
   I: Natural := The_Signal'First;
   This_Target: Integer;
   This_Kind: Value_Kind := Undefined;
 begin
   if The_Signal'Length = 0 then
      return "-";
   end if;
   --  Signal= (Event_id, Chart_id, arg1(var_id), arg2(indexes),arg3(value))
   if The_Signal(I) not in
      All_Events.all'Range then
      if Runtime_Errors_Enabled then
        Put_Line(Current_Error, 
           "UMC Runtime_Error! Undefined event in Signal_Image");
      end if;
      Runtime_Errors_Count := Runtime_Errors_Count +1;
      Runtime_Error_Msg := new String'("Undefined event in Signal_Image");
     return "Runtime_Error";
--   raise UML_Error;
   end if;
   --
   This_Target := The_Signal(I+1);
   if This_Target not in All_Charts.all'Range then
    if Runtime_Errors_Enabled then
      Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect target in Signal_Image");
    end if;
    Runtime_Errors_Count := Runtime_Errors_Count +1;
    Runtime_Error_Msg := new String'("Incorrect target in Signal_Image");
     return "Runtime_Error";
--   raise UML_Error;
   end if;
   --
   This_Event := All_Events(The_Signal(I));
   --
   if This_Event.Kind = Signal then
     if The_Signal'Length < 2 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
        if Runtime_Errors_Enabled then
          Put_Line(Current_Error, 
           "UMC Runtime_Error! Incorrect args in Signal_Image");
        end if; 
        Runtime_Errors_Count := Runtime_Errors_Count +1;
        Runtime_Error_Msg := new String'("Incorrect args in Signal_Image");
        return "Runtime_Error";
--     raise UML_Error;
     end if;
   elsif  This_Event.Kind = Operation then  -- (consider also the implicit caller param)
     if The_Signal'Length < 3 or else
        The_Signal'Length /= This_Event.Params.all'Length+2 then
        if Runtime_Errors_Enabled then
          Put_Line(Current_Error,
           "UMC Runtime_Error! Incorrect args in Signal_Image");
        end if;
        Runtime_Errors_Count := Runtime_Errors_Count +1;
        Runtime_Error_Msg := new String'("Incorrect args in Signal_Image");
        return "Runtime_Error";
--     raise UML_Error;
     end if;
   elsif This_Target>2   --- Kind = Undefined but not ERR or OUT Classes
     then
        if Runtime_Errors_Enabled then
          Put_Line(Current_Error,
           "UMC Runtime_Error! Found use of undeclared event " & This_Event.Name.all);
        end if;
        Runtime_Errors_Count := Runtime_Errors_Count +1;
        Runtime_Error_Msg := 
              new String'("Found use of undeclared event " & This_Event.Name.all);
        return "Runtime_Error";
---     return This_Event.Name.all ;
--   raise UML_Error;
   end if;
   --
   --
   if  This_Event.Num_Key=Assign_Event.Num_Key then
     --
     --  this is an "assign(var,[index],value) signal
     --    show it as  "var[index]:=value"  (or as  "var:=value")
     --    show it as  "tmp_j[index]:=value"  (or as  "tmp_j:=value")
     --
     -- NOTICE: if The_Signal(3) < 0 the variable is a transition variable!!
     -- (wemust not serach its name in the class but call it tmp#n
     --
     --   MAYBE  CONSIDER  ALSO THE CASES OF X++   and X--  ?
     --
     Result := new String'("");
     if The_Signal(I+3) =IntEmptyStruct then
       if The_Signal(I+2) > 0 then
         Result := 
           Append(Result, 
            All_Charts(This_Target).ChartVars(The_Signal(I+2)).Name.all & ":=");
       else
         Result := 
           Append(Result,"tmp#" &  Trim(Integer'Image(-The_Signal(I+2)),Left) & ":=");
       end if;
     else
       declare
         Indexes: Int_Table := Vectors_DB.Retrieve(StructBase-The_Signal(I+3));
       begin
       if The_Signal(I+2) > 0 then
         Result := 
          Append(Result, All_Charts(This_Target).ChartVars(The_Signal(I+2)).Name.all & "[");
       else
         Result := 
           Append(Result,"tmp#" &  Trim(Integer'Image(-The_Signal(I+2)),Left) & "[");
       end if;
       for K in Indexes'Range loop
          Result := Append(Result, Value_Image(Indexes(K),number));
          if K < Indexes'Last then
             Result := Append(Result, "][");
          end if;
        end loop;
        Result := Append (Result, "]:=");
        --  " Value_Image(The_Signal(I+3),Numvector) & ":=");
       end;
     end if;
     if The_Signal(I+2) > 0 then
       This_Kind := All_Charts(This_Target).ChartVars(The_Signal(I+2)).Kind;
       if The_Signal(I+3) >0 then 
         if This_Kind = Numvector then This_Kind := Number; end if; 
         if This_Kind = Boolvector then This_Kind := Bool; end if; 
         if This_Kind = Objvector then This_Kind := Object; end if; 
         if This_Kind = Composite then This_Kind := Number; end if; 
       end if;
     end if;
     Result := Append (Result, Value_Image(The_Signal(I+4),This_Kind) );
     --
   elsif This_Event.Num_Key=2 then
    --
    -- this is an ERR.lostevent(e) signal
    --   display e as event-id
    --
      Result := new String'("lostevent");
      if Show_Target then
        Prev := Result;
        Result := new String'("ERR." & Result.all);
        Free(Prev);
      end if;
      if Show_Params then
        Prev := Result;
        Result := new String'(Result.all & "(" &
               All_Events(The_Signal(I+2)).Name.all & ")" );
        Free(Prev);
      end if;
     --
   else
     -- not the special LOSTEVENT or ASSIGN cases (hence true SIGNAL OR CALL)
     --
     -- only if the user specification contains exctly one chart and the
     --  event is not relative to some non-active (e.g. "Out") chart  then
     --  avoid to expand the signal with the explicit chart name
     --
     if Show_Target and then Target_Included then 
--     if Show_Target and then Target_Included and then 
--         (Active_Charts.all'Length > 1 or else
--            not Is_Active_Chart(This_Target)) then
        --
        -- prefix the signal with the corrent target
        --
        if This_Target in All_Charts.all'Range then
          Result := Append (Result, All_Charts(This_Target).Name.all & ".");
        else
          Result := new String'("Undefined.");
        end if;
      end if;
     -- 
     Result := Append (Result,This_Event.Name.all);
     --
     if This_Event.Kind /= Operation   -- (i.e. Signal or Undefined)
         and then  
           Show_Params and then
           This_Event.Params.all'Length > 0 then
       Result := Append (Result,"(");
       for J in This_Event.Params.all'Range loop
         if This_Event.Params /= null and then
           This_Event.Params(J) /= null then
           This_Kind := This_Event.Params(J).Kind;
         else
           This_Kind := Undefined;
         end if;
         Result := Append( Result, Value_Image(The_Signal(I+1+J),This_Kind));
         if J < This_Event.Params.all'Length then
           Result := Append (Result,",");
         else
           Result := Append (Result,")");
         end if;
       end loop;
     elsif This_Event.Kind = Operation and then
           Show_Params and then
           This_Event.Params.all'Length > 1 then
       Result := Append (Result,"(");
       for J in 2.. This_Event.Params.all'Length loop
         if This_Event.Params /= null and then
           This_Event.Params(J) /= null then
           This_Kind := This_Event.Params(J).Kind;
         else
           This_Kind := Undefined;
         end if;
         Result := Append( Result, Value_Image(The_Signal(I+1+J),This_Kind));
         if J < This_Event.Params.all'Length then
           Result := Append (Result,",");
         else
           Result := Append (Result,")");
         end if;
       end loop;
     end if;
   end if;
   --
   declare
     Static: String := Result.all;
   begin
     Free (Result);
     return Static;
   end;
 end Signal_Image;

 function Transitions_Image (This_Sequence:Transitions_Table)
        return String is
   Result: String_Ref := new String'("");
 begin
   for I in This_Sequence'Range loop
     if This_Sequence(I).Label = null or else
         This_Sequence(I).Label.all'Length =0 then
       declare
         str: String := Integer'Image(This_Sequence(I).Num_Key);
       begin
         str(1) := '#';
         Result := Append (Result,str);
       end;
     else
       Result := Append (Result,This_Sequence(I).Label.all);
     end if;
     if I < This_Sequence'Length then
       Result := Append (Result,"+");
     end if;
   end loop;
   --
   declare
     Static: String := Result.all;
   begin
     Free (Result);
     return Static;
   end;
 end Transitions_Image;

 function Value_Image(This_Var:SystemVar_Ref; 
                      This_Value:Integer) return String is
   --
--   This_Image: String := Integer'Image(This_Value);
 begin 
   return Value_Image(This_Value,Object);
---   if (This_Var.Kind = Object or else 
--        (This_Var.Initial /= null and then This_Var.Initial.Kind = Object))  then
--      return Value_Image(This_Value,Object);
--     --
--   elsif (This_Var.Kind = Bool or else 
--           (This_Var.Initial /= null and then 
--           This_Var.Initial.Kind = Bool)) and then
--             This_Value=0 then
 --    return "false";
--    --
--  elsif (This_Var.Kind = Bool or else
--         (This_Var.Initial /= null and then
--            This_Var.Initial.Kind = Bool)) and then
--              This_Value/=0 then
--     return "true";
--  end if;
 -- --
--  if This_Var.Kind = Objvector or 
--        This_Var.Kind =Composite or
--        This_Var.Kind =Boolvector or
--        This_Var.Kind =Numvector then
--    return Value_Image(This_Value, Composite);
--
----    declare
----       thisdata: Int_Table := Vectors_DB.Retrieve(This_Value);
----       result: String_Ref := new String'("[");
----       prev: String_Ref;
----    begin
----       if This_Var.Kind = Objvector then
----         for I in thisdata'Range loop
----           if I > thisdata'First then
----             prev := result;
----             result := new String'(result.all & ",");
----             Free(prev);
----           end if;
----           prev := result;
----           result := 
----              new String'(result.all & Value_Image(thisdata(I),Object));
----           Free(prev);
----         end loop;
----       elsif This_Var.Kind = Numvector then
----         for I in thisdata'Range loop
----           if I > thisdata'First then
----             prev := result;
----             result := new String'(result.all & ",");
----             Free(prev);
----           end if;
----           result :=
----              new String'(result.all & Value_Image(thisdata(I), Number));
----         end loop;
----       elsif This_Var.Kind = Boolvector then
----         for I in thisdata'Range loop
----           if I > thisdata'First then
----             prev := result;
----             result := new String'(result.all & ",");
----             Free(prev);
----           end if;
----           result :=
----              new String'(result.all & Value_Image(thisdata(I), Bool));
----         end loop;
----       end if;
----       prev := result;
----       result := new String'(result.all & "]");
----       Free(prev);
----       declare
----         Str: String := result.all;
----       begin
----         Free(result);
----         return Str;
-- --      end;
----    end;
----     -- 
----  elsif This_Image(1) /= ' ' then   --  This_Var.Kind = Number
----      return This_Image;
----      --
----  else  -- This_Image(1) = ' '      --  This_Var.Kind = Number
----      return This_Image(2..This_Image'Length);
--   end if;
 end Value_Image;

  function Is_Default_Initial (This: State_Ref) return Boolean is
  begin
   if This.FullName.all'Length > 8 and then
     This.FullName.all(This.FullName.all'Length-7 ..
                       This.FullName.all'Length ) = ".initial" then
     return True;
    else
      return False;
    end if;
  end Is_Default_Initial;

  function Is_Final (This: State_Ref) return Boolean is
  begin
   if This.FullName.all'Length > 6 and then
     This.FullName.all(This.FullName.all'Length-5 ..
                       This.FullName.all'Length ) = ".final" then
     return True;
    else
      return False;
    end if;
  end Is_Final;


 function  Normalized_Literal(Source: String) return String is
 begin
    if Source="false" or
       Source="FALSE" or
       Source="False" then
      return "False" ;
    elsif Source="true" or
       Source="TRUE" or
       Source="True" then
       return "True";
    elsif Source="null" or
       Source="NULL" or
       Source="Null" then
       return "null";   
    else
      return Source;
    end if;
 end Normalized_Literal;


  function Eval_Literal (Id: String) return Integer is
  begin
    if id="false" or
       id="FALSE" or
       id="False" then
      return IntFalse;
    elsif id="true" or
       id="TRUE" or
       id="True" then
       return IntTrue;
    elsif id="null" or
       id="NULL" or
       id="Null" then
       return 0;
    elsif id(Id'First) in '0'..'9' then
       return Integer'Value(id);
    elsif id'Length >1 and then
       id(Id'First)= '-' and then
       id(Id'First+1) in '0'..'9' then
       return Integer'Value(id);
    else
      -- must be an object name, (object attribues NOT allowed so far)
      for J in All_Charts.all'Range loop
        if All_Charts(J).Name.all=id then
          return J;
        end if;
      end loop;
      if Runtime_Errors_Enabled then
         Put_Line (Current_Error,
           "Error in ASSERT expression: unexpected object name");
      end if;
      raise UCTL_Error;
    end if;
  end Eval_Literal;
--  ----------------------------------------------------------------------
--  --   Observations = e.g.   "x,y,obj2.sig1,obj2.collop2,obj3.var_x,"
--  --   WE DO NOT RESET OBSERVATIONS!!!
--  --   RESETTING MUST BE DONE EXPLICITLY
--  --   MAYBE WE SHOULD SAVE THIS INFO IN THE MODEL PREFERENCES ?!?!?
--  ----------------------------------------------------------------------
--  procedure Set_Observations (Mode: Observation_Kind; Args: String) is
--    First: Natural;
--    Last: Natural;
--  begin
--    --
--    if Mode = CUSTOM then
--      --
--      --    Last is the last good char
--      --    First is the first good char
--      First := 1;
--      Last := First;
--      if Args'Length /= 0 then
--        --
--        -- scan the Observations string for the requests.
--        --
--        while Last <= Args'Length loop
--          --
--          --  look for ',' or end of line  (Last is the last good char)
--          --
--          while Last < Args'Length and then 
--               ( Args(Last+1) /= '+' and 
--                 Args(Last+1) /= ',' and 
--                 Args(Last+1) /= '&' ) loop
--            Last := Last+1;
--          end loop;
--          --
--          declare
--            This_Obs: String(1..Last-First+1) := Args(First..Last);
--            Pivot_Index: Natural := 0;
--            Chart_Index: Natural := 0;
--            Var_Index: Natural :=0;
--          begin
--            --
--            -- adjust the cursors for the next cycle
--            --
--            First := Last+2;
--            Last := First;
--            --
--            -- check the presence of a Chart prefix.
--            --
--            for K in This_Obs'Range loop
--              if This_Obs(K)='.' then
--                 Pivot_Index := K;
--                 for J in All_Charts.all'Range loop
--                   if All_Charts(J).Name.all=This_Obs(1..K-1) then
--                      Chart_Index := J;
--                   end if;
--                 end loop;
--              end if;
--            end loop;
--            if Chart_Index = 0 and then Active_Charts.all'Length =1 then
--              Chart_Index := Active_Charts(1);
--            end if;
--            --
--            -- check if the observed entity is an abject attribute
--            --   (which by default are all NOT observable)
--            --  (we can have more variables with the same name 
--            --    in more charts)
--            --
--            for K in All_Vars.all'Range loop
--              if (Chart_Index = 0 or 
--                   Chart_Index = All_Vars(K).Chart) and then
--                All_Vars(K).Name.all =
--                   This_Obs(Pivot_Index+1 ..This_Obs'Length) then
--                Observed_Vars := 
--                     new Num_Table'(Observed_Vars.all & (1=> K));
--                All_Vars(K).Observed := True;
--                Var_Index := K;
 --             end if;
--            end loop;
--            --
--            -- if it is an object event (which by default 
--            --    are all not observable)  mark it as observable
--            --
--            if Var_Index = 0 then
--              --  mark this event as observable
--              for EE in All_Events.all'Range loop
--                if All_Events(EE).Name.all = This_Obs(Pivot_Index+1 ..This_Obs'Length) then
--                   All_Events(EE).Observed := True;
--                end if;
--              end loop;
 --               for C in All_Charts.all'Range loop
--                  if Chart_Index = 0 or C = Chart_Index then
--                    for K in All_Charts(C).ChartEvents.all'Range loop
--                      if All_Charts(C).ChartEvents(K).Name.all =
--                           This_Obs(Pivot_Index+1 ..This_Obs'Length) then
--                          All_Charts(C).ObservedEvents(K) := True;
----                        exit;
--                      end if;
--                    end loop;
--                  end if;
--                end loop;
--            end if;
--          end;
--        end loop;
--      end if;
--      --
--    elsif  Mode = INTERACTIONS or Mode = SELECTIVE then 
--      -- observe OUTGOING and INCOMING signals of obj
--      Observed_Objects:= new Num_table(1..0);  -- indexes in All_Charts
--      --
--      --  Args=  "obj"    obj2,obj2
--      First := Args'First;
--      while First <= Args'Last loop
--        --
--        -- skip initial garbage, if any
--        while First <= Args'Last and then
--            ( Args(First) not in 'a'..'z' and then
 --             Args(First) not in 'A'..'Z'  and then
--              Args(First) not in '0'..'9'  and then
--              Args(First) /= '_' ) loop
--           First := First+1;
--        end loop; 
--        if First <= Args'Last then
--          Last := First;
--          -- go the end of the identifier
--          while Last < Args'Last and then
--                ( Args(Last+1) in 'a'..'z' or else
--                Args(Last+1) in 'A'..'Z'  or else
--                Args(Last+1) in '0'..'9'  or else
--                Args(Last+1) = '_' ) loop
--             Last := Last+1;
--          end loop;
--          declare
--            Obj: String(1..Last-First+1) := Args(First..Last);
--            Selective_Choice: Natural;
--          begin
--            for I in Active_Charts.all'Range loop
--              if All_Charts(Active_Charts(I)).Name.all = Obj then
--                 Selective_Choice := Active_Charts(I);
--                 Observed_Objects := new Num_table'(Observed_Objects.all & Selective_Choice);
--                 All_Charts(Selective_Choice).ObservedEvents.all := (others => True);
 --               end if;
 ----           end loop;
 --         end;
 --         First := Last+2;
  --      end if;
 --     end loop;
 --     --
 --   end if;
 --   end Set_Observations;
--
-- procedure Set_Parameter(Input_Line: String) is
 --    Arg: Natural;
--     Clean_Line: String(1..Input_Line'Length) := Input_Line;
 --    Clean_Length: Natural :=0;
--  begin
--   -- 
--   -- remove  end-of-line and other control characters
--   --
--   for I in Input_Line'Range loop
--     if Input_Line(I) in 'a'..'z' or else
--         Input_Line(I) in 'A'..'Z' or else
--         Input_Line(I) in '0'..'9' or else
--         Input_Line(I) = ' ' or else
--         Input_Line(I) = '=' or else
--         Input_Line(I) = ',' or else
--         Input_Line(I) = '_' or else
--         Input_Line(I) = '.' or else
--         Input_Line(I) = ':' or else
--         Input_Line(I) = '#' or else
--         Input_Line(I) = '?' or else
--         Input_Line(I) = '!' then
--       Clean_Length := Clean_Length+1;
--       Clean_Line(Clean_Length) := Input_Line(I); 
--     end if;
--   end loop;
--   --
--   -- default_lts_depth
--   --
--   if Clean_Length > 22 and then
--      Clean_Line(1..22)="set initial_lts_depth=" then
--     begin
--       Arg := Integer'Value(Clean_Line(23..Clean_Length));
--       Default_LTS_Depth := Arg;
--       return;
--     exception
--       when others =>
--         if Runtime_Errors_Enabled then
--           Put_Line (Current_Error,
--          "Error: incorrect arg of ""initial_lts_depth"" ");
--         end if;
--         return;
--     end;
--   end if;
--   --
--   -- max_explanation_depth
--   --
--   if Clean_Length > 26 and then
--      Clean_Line(1..26)="set max_explanation_depth=" then
--     begin
--       Arg := Integer'Value(Clean_Line(27..Clean_Length));
--       Max_Explanation_Depth := Arg;
--       Put_Line ("max_explanation_depth=" &
--                  Trim(Integer'Image(Max_Explanation_Depth),Left));
--       return;
--     exception
--       when others =>
--         if Runtime_Errors_Enabled then
--           Put_Line (Current_Error,
--            "Error: incorrect arg of ""max_explanation_depth"" ");
--         end if;
--         return;
--     end;
--   end if;
--   --
--   -- max_evolutions_depth
--   --
--   if Clean_Length > 25 and then
--      Clean_Line(1..25)="set max_evolutions_depth=" then
--     begin
--       Arg := Integer'Value(Clean_Line(26..Clean_Length));
--       Max_Evolutions_Depth := Arg;
--       Put_Line ("max_evolutions_depth=" &
 --                 Trim(Integer'Image(Max_Evolutions_Depth),Left));
 --      return;
--     exception
--       when others =>
--         if Runtime_Errors_Enabled then
--           Put_Line (Current_Error,
--            "Error: incorrect arg of ""max_evolutions_depth"" ");
 --        end if;
 --        return;
 --    end;
 --  end if;
 --  --
 --  --
 --  -- showparams=true
 -- if Clean_Length > 19 and then
--      (Clean_Line(1..20)="set showparams=FALSE" or else
--      Clean_Line(1..20)= "set showparams=false") then
--     Show_Params := False;
--     Put_Line ("showparams=" &
--                  Trim(Boolean'Image(Show_Params),Left));
--      return;
 --  end if;
--   if Clean_Length > 18 and then
--      (Clean_Line(1..19)="set showparams=TRUE" or else
--      Clean_Line(1..19)="set showparams=true") then
--     Show_Params := True;
--     Put_Line ("showparams=" &
--                  Trim(Boolean'Image(Show_Params),Left));
--      return;
--   end if;
--   --
 --  -- showtarget=true
--   --
--   if Clean_Length > 19 and then
--      (Clean_Line(1..20)="set showtarget=FALSE" or else
--      Clean_Line(1..20)= "set showtarget=false") then
--     Show_Target := False;
--     Put_Line ("showtarget=" &
--                  Trim(Boolean'Image(Show_Target),Left));
--      return;
--   end if;
--   if Clean_Length > 18 and then
--      (Clean_Line(1..19)="set showtarget=TRUE" or else
 --     Clean_Line(1..19)="set showtarget=true") then
--     Show_Target := True;
--     Put_Line ("showtarget=" &
 --                 Trim(Boolean'Image(Show_Target),Left));
--      return;
 --  end if;
--   --
--   -- showstuttering=true
--   --
 --  if Clean_Length > 23 and then
--      (Clean_Line(1..24)="set showstuttering=FALSE" or else
 --     Clean_Line(1..24)="set showstuttering=false") then
 --    ShowStuttering := False;
 --    Put_Line ("showstuttering=" &
 --                 Trim(Boolean'Image(ShowStuttering),Left));
 --     return;
 --  end if;
 --  if Clean_Length > 22 and then
--      (Clean_Line(1..23)="set showstuttering=TRUE" or else
--      Clean_Line(1..23)="set showstuttering=true") then
--     ShowStuttering := True;
 --    Put_Line ("showstuttering=" &
 --                 Trim(Boolean'Image(ShowStuttering),Left));
 --     return;
--   end if;
 --  --
--   -- observereturns=false
--   --
 --  if Clean_Length > 23 and then
 --     (Clean_Line(1..24)="set observereturns=FALSE" or else
 --     Clean_Line(1..24)="set observereturns=false") then
--     ObserveReturns := False;
 --    Put_Line ("observereturns=" &
--                  Trim(Boolean'Image(ObserveReturns),Left));
 --     return;
 --  end if;
--   if Clean_Length > 22 and then
 --     (Clean_Line(1..23)="set observereturns=TRUE" or else
--      Clean_Line(1..23)="set observereturns=true") then
 --    ObserveReturns := True;
 --    Put_Line ("observereturns=" &
 --                 Trim(Boolean'Image(ObserveReturns),Left));
 --     return;
 --  end if;
 --  --
--   --
 --  -- observationmode=blackbox   (
--   -- observationmode=whitebox
 --  -- observationmode=custom     (blackbox+hiding+observations)
 --  --
 --if Clean_Length > 28 and then
--      (Clean_Line(1..29)="set observationmode=black_box" or else
--       Clean_Line(1..29)="set observationmode=Black_box" or else
 --      Clean_Line(1..29)="set observationmode=Black_Box" or else
 --      Clean_Line(1..29)="set observationmode=BLACK_BOX") then
 --     Reset_Observations; 
--      Observation_Mode := BLACK_BOX;
--      Explicit_Tau_Requested := True;
--      ShowEvolvingObject := False;
--      Observation_Args := Null_String;
--      Put_Line ("observationmode=BLACK_BOX");
--      Put_Line ("explicit_tau_requested=TRUE");
--      Put_Line ("showevolvingobject=FALSE");
 --     return;
--   end if;
 --  if Clean_Length > 28 and then
--      (Clean_Line(1..29)="set observationmode=white_box" or else
--       Clean_Line(1..29)="set observationmode=White_box" or else
--       Clean_Line(1..29)="set observationmode=White_Box" or else
 --      Clean_Line(1..29)="set observationmode=WHITE_BOX") then
 --     Reset_Observations;
--      Observation_Mode := WHITE_BOX;
----      Explicit_Tau_Requested := False;
--      ShowEvolvingObject := True;
 --     Observation_Args := Null_String;
----      Put_Line ("observationmode=WHITE_BOX");
--      Put_Line ("explicit_tau_requested=FALSE");
--      Put_Line ("showevolvingobject=TRUE");
--      return;
--   end if;
--   if Clean_Length > 27 and then
--      (Clean_Line(1..28)="set observationmode=gray_box" or else
--       Clean_Line(1..28)="set observationmode=Gray_box" or else
--       Clean_Line(1..28)="set observationmode=Gray_Box" or else
--       Clean_Line(1..28)="set observationmode=GRAY_BOX") then
--      Reset_Observations;
--      Observation_Mode := GRAY_BOX;
--      Explicit_Tau_Requested := False;
--      ShowEvolvingObject := True;
--      Observation_Args := Null_String;
--      Put_Line ("observationmode=GRAY_BOX");
--      Put_Line ("explicit_tau_requested=FALSE");
--      Put_Line ("showevolvingobject=TRUE");
--      return;
--   end if;
--   if Clean_Length > 25 and then
--      (Clean_Line(1..26)="set observationmode=custom" or else
--       Clean_Line(1..26)="set observationmode=Custom" or else
--       Clean_Line(1..26)="set observationmode=CUSTOM") then
--      Reset_Observations;
--      Show_Assignments := True;    -- 15/07/2005  ?
--      Observation_Mode := CUSTOM;
--      Explicit_Tau_Requested := True;
--      ShowEvolvingObject:= True;     -- when non tau signal present
--      if Clean_Length > 27 and then Clean_Line(27)=':' then
--           Observation_Args := new String(1..Clean_Length-27);
--           Observation_Args.all := Clean_Line(28..Clean_Length);
--        Put_Line ("observationmode=CUSTOM:" & Observation_Args.all);
--      else
--        Put_Line ("observationmode=CUSTOM");
--      end if;
--      Put_Line ("explicit_tau_requested=TRUE");
--      Put_Line ("showevolvingobject=TRUE");
--      Set_Observations(CUSTOM,Observation_Args.all);
--      return;
--   end if;
-- if Clean_Length > 28 and then
--      (Clean_Line(1..29)="set observationmode=selective" or else
--       Clean_Line(1..29)="set observationmode=Selective" or else
--       Clean_Line(1..29)="set observationmode=SELECTIVE") then
--      Reset_Observations;
--      Observation_mode := SELECTIVE;
--      Explicit_Tau_Requested := True;
--      ShowEvolvingObject:= True;     -- when non tau signal present
--      if Clean_Length > 30 and then Clean_Line(30)=':' then
--         Observation_Args := new String(1..Clean_Length-30);
--         Observation_Args.all := Clean_Line(31..Clean_Length);
--        Put_Line ("observationmode=SELECTIVE:" & Observation_Args.all);
 --     else
--        Put_Line ("observationmode=SELECTIVE");
--      end if;
--      Put_Line ("explicit_tau_requested=TRUE");
--      Put_Line ("showevolvingobject=FALSE");
--      Set_Observations(SELECTIVE,Observation_Args.all);
--      return;
--   end if;
 --  --
--   if Clean_Length > 31 and then
--      (Clean_Line(1..32)="set observationmode=interactions" or else
--       Clean_Line(1..32)="set observationmode=Interactions" or else
--       Clean_Line(1..32)="set observationmode=INTERACTIONS") then
--      Reset_Observations;
--      Observation_mode := INTERACTIONS;
--      Explicit_Tau_Requested := True;
--      ShowEvolvingObject:= True;     -- when non tau signal present
--      if Clean_Length > 32 and then Clean_Line(32)=':' then
--         Observation_Args := new String(1..Clean_Length-32);
--         Observation_Args.all := Clean_Line(33..Clean_Length);
--        Put_Line ("observationmode=INTERACTIONS:" & Observation_Args.all);
--      else 
--        Put_Line ("observationmode=INTERACTIONS");
 --     end if;
--      Put_Line ("explicit_tau_requested=TRUE");
--      Put_Line ("showevolvingobject=TRUE");
--      Set_Observations(INTERACTIONS,Observation_Args.all);
--      return;
--   end if;
--   --
--   -- observationargs= ....
--   --
--   if Clean_Length > 19 and then
--      Clean_Line(1..20)="set observationargs=" then
--     Observation_Args := new String(1..Clean_Length-20);
--     Observation_Args.all := Clean_Line(21..Clean_Length);
--     return;
--   end if;
--   --
--   -- showevolvingobject=true
--   --
--   if Clean_Length > 27 and then
 --     (Clean_Line(1..28)="set showevolvingobject=FALSE" or else
--       Clean_Line(1..28)="set showevolvingobject=False" or else
--       Clean_Line(1..28)="set showevolvingobject=false") then
--      ShowEvolvingObject := False;
--      Put_Line ("showevolvingobject=FALSE");
 --     return;
--   end if;
 --  if Clean_Length > 26 and then
--      (Clean_Line(1..27)="set showevolvingobject=TRUE" or else
--       Clean_Line(1..27)="set showevolvingobject=True" or else
--      Clean_Line(1..27)="set showevolvingobject=true") then
--      ShowEvolvingObject := True;
--      Put_Line ("showevolvingobject=TRUE");
--      return;
--   end if;
--   --
--   --
--   -- explicit_tau_requested=true
--   --
--   if Clean_Length > 31 and then
--      (Clean_Line(1..32)="set explicit_tau_requested=FALSE" or else
--       Clean_Line(1..32)="set explicit_tau_requested=False" or else
 --      Clean_Line(1..32)="set explicit_tau_requested=false") then
--      Explicit_Tau_Requested := False;
--      Put_Line ("explicit_tau_requested=FALSE");
--      return;
--   end if;
--   if Clean_Length > 30 and then
 --     (Clean_Line(1..31)="set explicit_tau_requested=TRUE" or else
 --      Clean_Line(1..31)="set explicit_tau_requested=True" or else
 --     Clean_Line(1..31)="set explicit_tau_requested=true") then
 --     Explicit_Tau_Requested := True;
 --     Put_Line ("explicit_tau_requested=TRUE");
 --     return;
 --  end if;
 --  --
 --  if Runtime_Errors_Enabled then
 --     Put_Line (Current_Error, "Error: incorrect parameter");
 --  end if;
 -- end Set_Parameter;
--
--
--  procedure TXT_Settings is
--  begin
--     --
--     -- print current values
--     --
--     Put_Line ("initial_lts_depth=" &
--                  Trim(Integer'Image(Default_LTS_Depth),Left));
--     Put_Line ("max_explanation_depth=" &
--                  Trim(Integer'Image(Max_Explanation_Depth),Left));
--     Put_Line ("max_evolutions_depth=" &
--                  Trim(Integer'Image(Max_Evolutions_Depth),Left));
--     Put_Line ("showparams=" &
--                  Trim(Boolean'Image(Show_Params),Left));
--     Put_Line ("showtarget=" &
--                  Trim(Boolean'Image(Show_Target),Left));
--     Put_Line ("showstuttering=" &
--                  Trim(Boolean'Image(ShowStuttering),Left));
--     Put_Line ("observereturns=" &
--                  Trim(Boolean'Image(ObserveReturns),Left));
--     Put_Line ("observationargs=" &  Observation_Args.all) ;
--     if  Observation_Mode = BLACK_BOX or
--         Observation_Mode = WHITE_BOX or
--         Observation_Mode = GRAY_BOX then
--        Put_Line ("observationmode=" &
--                   Observation_Kind'Image(Observation_Mode));
--     --
--     elsif Observation_Mode=CUSTOM or else
--            Observation_Mode=SELECTIVE or else
--            Observation_Mode=INTERACTIONS  then
--        Put_Line ("observationmode=" &
--           Observation_Kind'Image(Observation_Mode) &
--             ":" & Observation_Args.all);
--     end if;
--     Put_Line ("showevolvingobject=" &
--                  Trim(Boolean'Image(ShowEvolvingObject),Left));
--     Put_Line ("explicit_tau_requested=" &
--                  Trim(Boolean'Image(Explicit_Tau_Requested),Left));
--  end TXT_Settings;

  function Active_Object_Names return String is
    Str: String_Ref := new String'("");
  begin
    for I in Active_Charts.all'range loop 
      if I = 1 then 
        Str := new String'(All_Charts(Active_Charts(I)).Name.all);
      else
        Str := new String'(Str.all & "," & All_Charts(Active_Charts(I)).Name.all);
      end if;
    end loop;
    declare
      Result: String := Str.all;
    begin
      Free(Str);
      return Result;
    end;
  end Active_Object_Names;

  function Entity_Names return String is
    Str: String_Ref := new String'("");
    Prev: String_Ref;
    First:Boolean := True;
  begin
    for I in All_Classes.all'Range loop
      for J in All_Classes(I).ChartVars.all'Range loop
        if First then 
          Free(Str);
          Str := new String'(All_Classes(I).ChartVars(J).Name.all);
          First := False;
        else
          Prev := Str;
          Str := new String'(Str.all & "," & All_Classes(I).ChartVars(J).Name.all);
          Free(Prev);
        end if;
      end loop;
    end loop;
    for I in OpReturnBoolC_Event.Num_key+1  .. All_Events.all'Length loop
       if First then
          Free(Str);
          Str := new String'(All_Events(I).Name.all);
          First := False;
        else
          Prev := Str;
          Str := new String'(Str.all & "," & All_Events(I).Name.all);
          Free(Str);
        end if;
    end loop;
    declare
      Result: String := Str.all;
    begin
      Free(Str);
      return Result;
    end;
  end Entity_Names;

--  procedure HTML_Settings is
--    HTML_File : File_Type;
--    Unique: String := Integer'Image(Evaluations_Count);
--  begin
--    -------------------------  create  dosettingscallback ------------------------
--    Create (HTML_File, Out_File, "settingscallback");
--       Put_Line(HTML_File,"parent.settings_callback(");
--       Put_Line(HTML_File,"parent." &  Observation_Kind'Image(Observation_Mode) & ",");
--       Put_Line(HTML_File,"""" & Observation_Args.all & """,");
--       if Show_Params then
--          Put_Line(HTML_File, "parent.showparams,");
--       else
--          Put_Line(HTML_File, "parent.hideparams,");
--       end if;
--       if Show_Target then
--          Put_Line(HTML_File, "parent.showtarget,");
--       else
--          Put_Line(HTML_File, "parent.hidetarget,");
--       end if;
--       if ShowEvolvingObject then
--          Put_Line(HTML_File, "parent.showsource,");
--       else
--          Put_Line(HTML_File, "parent.hidesource,");
--       end if;
--       if ObserveReturns then
--          Put_Line(HTML_File, "parent.showreturns,");
--       else
--          Put_Line(HTML_File, "parent.hidereturns);");
--       end if;
--    Close (HTML_File);
--    -------------------------  create  settings.html --------------------------
--    Unique(1) := '&';
--    --
--    Create (HTML_File, Out_File, "settings.html");
--    Set_Output (HTML_File);
--    --
--    Put_Line ("<html><head><title>" & "UMC Settings" & "</title></head>");
--    Put_Line("<body bgcolor=""white"">");
--    Put_line (" <CENTER>");
--    Put_Line("<H2>Preferences</H2>");
--    --
--    Put_Line("<SCRIPT LANGUAGE=""JavaScript""><!-- ");
--    Put_Line("function setmode(mode) {");
--    Put_Line("    if (mode== 0)  // WHITE_BOX");
--    Put_Line("        { document.myForm.explicit_tau_requested.selectedIndex=0;");
--    Put_Line("          document.myForm.observationargs.value=""""; ");
--    Put_Line("          document.myForm.showevolvingobject.selectedIndex=1; };");
--    Put_Line("    if (mode== 2)  // BLACK_BOX");
--    Put_Line("        { document.myForm.explicit_tau_requested.selectedIndex=1;");
 --   Put_Line("          document.myForm.observationargs.value=""""; ");
--    Put_Line("          document.myForm.showevolvingobject.selectedIndex=0; };");
--    Put_Line("    if (mode== 1)  // GRAY_BOX");
--    Put_Line("        { document.myForm.explicit_tau_requested.selectedIndex=1;");
--    Put_Line("          document.myForm.observationargs.value=""""; ");
--    Put_Line("          document.myForm.showevolvingobject.selectedIndex=1; };");
--    Put_Line("    if (mode== 3)  // CUSTOM");
--    Put_Line("        { document.myForm.explicit_tau_requested.selectedIndex=1;");
--    Put_Line("          document.myForm.observationargs.value="""& Entity_Names &"""; ");
--    Put_Line("          document.myForm.showevolvingobject.selectedIndex=0; };");
--    Put_Line("     if (mode== 4)  // SELECTIVE");
--    Put_Line("        { document.myForm.explicit_tau_requested.selectedIndex=1;");
--    Put_Line("          document.myForm.observationargs.value="""& Active_Object_Names &"""; ");
--    Put_Line("          document.myForm.showevolvingobject.selectedIndex=0; };");
--    Put_Line("   if (mode== 5)  // INTERACTIONS");
--    Put_Line("       { document.myForm.explicit_tau_requested.selectedIndex=1;");
--    Put_Line("          document.myForm.observationargs.value="""& Active_Object_Names &"""; ");
--    Put_Line("         document.myForm.showevolvingobject.selectedIndex=1; };");
--    Put_Line("}");
--    Put_Line("--></SCRIPT>");
--    --
----    Put_line (" <form NAME=""myForm"" ACTION=""" & UML_DoSettings& """ >");
--    Put_line (" <form NAME=""myForm"" ACTION="""" >");
--    Put_Line ("<table bgcolor=""white"">");
--
--    Put_Line ("<tr> <td>");
--    --
--    --   observationargs
--   Put_Line ("<textarea NAME=""observationargs"" value=""" &
--                  """ cols=50 rows=3></textarea></td>");
--    --
--    -- observationmode
--    Put_Line ("<td> Observation Mode <br><br>");
--
--    Put_Line ("<select name='observationmode' onChange='setmode(selectedIndex);'>");
--    for M in WHITE_BOX..INTERACTIONS loop
--      if Observation_Mode=M then
--        Put("<option selected value='" & Observation_Kind'Image(M) &"'>");
--        Put_Line(Observation_Kind'Image(M) & "</option>");
--      else
--        Put("<option value='" & Observation_Kind'Image(M) & "'>");
--        Put_Line(Observation_Kind'Image(M) & "</option>");
--      end if;
--    end loop;
 --   Put_Line("</select>");
-- 
--    --
--    -- showstuttering
--    Put_Line ("<tr> <td>");
--    Put_Line ("Show Loss of Events as Error Signals" &
--          "<td><select name='showstuttering'>");
--    for M in Boolean'First..Boolean'Last loop
--      if ShowStuttering=M then
--        Put("<option selected value='" & Boolean'Image(M) &"'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      else
--        Put("<option value='" & Boolean'Image(M) & "'>");
 --       Put_Line(Boolean'Image(M) & "</option>");
--      end if;
--    end loop;
--    Put_Line("</select>");
--
--    --
--    -- observereturns
--    Put_Line ("<tr> <td>");
--    Put_Line ("Observe return events of observed calls" &
--          "<td><select name='observereturns'>");
--    for M in Boolean'First..Boolean'Last loop
 --     if ObserveReturns=M then
--        Put("<option selected value='" & Boolean'Image(M) &"'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      else
--        Put("<option value='" & Boolean'Image(M) & "'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      end if;
--    end loop;
--    Put_Line("</select>");
--
--    --
--    Put_Line("<tr> <td colspan=2> <hr></td></tr>");
--  
--    --
--    --  max_explanation_depth
--    Put_Line ("<tr> <td>");
--    Put_Line ("Max Explanations depth in a single page" &
--     "<td><INPUT TYPE=""text"" NAME=""max_explanation_depth"" SIZE=4 VALUE=""" &
--             Trim(Integer'Image(Max_Explanation_Depth),Left) & """;> ");
--
--    --
--    --  max_evolutions_depth
--    Put_Line ("<tr> <td>");
--    Put_Line ("Max Evolutions depth in a single picture" &
--     "<td><INPUT TYPE=""text"" NAME=""max_evolutions_depth"" SIZE=4 VALUE=""" &
--             Trim(Integer'Image(Max_Evolutions_Depth),Left) & """;> ");
--
--    --
 --   --   showparams
--    Put_Line ("<tr> <td>");
--    Put_Line ("Show Parameters of Events" &
--          "<td><select name='showparams'>");
--    for M in Boolean'First..Boolean'Last loop
--      if Show_Params=M then
--        Put("<option selected value='" & Boolean'Image(M) &"'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      else
----        Put("<option value='" & Boolean'Image(M) & "'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      end if;
--    end loop;
--    Put_Line("</select>");
--
--    --
--    -- showtarget
--    Put_Line ("<tr> <td>");
--    Put_Line ("Show Target of Events" &
--          "<td><select name='showtarget'>");
--    for M in Boolean'First..Boolean'Last loop
--      if Show_Target=M then
--        Put("<option selected value='" & Boolean'Image(M) &"'>");
--        Put_Line(Boolean'Image(M) & "</option>");
 --     else
--        Put("<option value='" & Boolean'Image(M) & "'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      end if;
--    end loop;
--    Put_Line("</select>");
--
--    --
--    -- explicit_tau_requested
--    Put_Line ("<tr> <td>");
--    Put_Line ("Mark labels of evolutions without actions with ""tau""" &
--          "<td><select name='explicit_tau_requested'>");
--    for M in Boolean'First..Boolean'Last loop
--      if Explicit_Tau_Requested=M then
 --       Put("<option selected value='" & Boolean'Image(M) &"'>");
--        Put_Line(Boolean'Image(M) & "</option>");
--      else
--        Put("<option value='" & Boolean'Image(M) & "'>");
--        Put_Line(Boolean'Image(M) & "</option>");
 --     end if;
 --   end loop;
 --   Put_Line("</select>");
--
--    --
--    -- showevolvingobject
--    Put_Line ("<tr> <td>");
--    Put_Line ("Show evolving object in evolution label" &
--          "<td><select name='showevolvingobject'>");
--    for M in Boolean'First..Boolean'Last loop
--      if ShowEvolvingObject=M then
--        Put("<option selected value='" & Boolean'Image(M) &"'>");
 --       Put_Line(Boolean'Image(M) & "</option>");
--      else
 --       Put("<option value='" & Boolean'Image(M) & "'>");
----        Put_Line(Boolean'Image(M) & "</option>");
 --     end if;
--    end loop;
--    Put_Line("</select>");
--
--    --
--    --   initial_lts_depth
--    Put_Line("<tr> <td colspan=2> <hr></td></tr>");
--    Put_Line ("<tr> <td>");
--    Put_Line ("Initial Max Evaluation Depth" &
--     "<td><INPUT TYPE=""text"" NAME=""initial_lts_depth"" SIZE=4 VALUE=""" &
--             Trim(Integer'Image(Default_LTS_Depth),Left) & """;> ");
--    --
--    Put_Line("</table>");
--    Put_Line("<P><INPUT TYPE=""submit"" VALUE=""Change"">");
--    Put_Line("<INPUT TYPE=""button"" VALUE=""Cancel"" " &
--               "ONCLICK=""parent.returnfromprefs();"">" );
--    Put_Line("</form><p>");
--    Put_Line("<script>");
--    Put_Line("document.forms[0].action= parent.cgi_dir + '/dosettings.cgi?'");
--    Put_Line("</script>");
--    --
--    if Observation_Mode=CUSTOM  then
--      Put_Line ("<script> document.myForm.observationargs.value=""" & 
--                  Entity_Names & """</script>");
--    end if;
--    Put_Line("</CENTER>");
--    Put_Line("</BODY> </HTML>");
--    Set_Output(Standard_Output);
--    Close(HTML_File);
--  exception
 --   when others =>
--      Set_Output(Standard_Output);
--      Close(HTML_File);
--  end HTML_Settings;
--
  function Has_Random_Queues(Index:Natural) return Boolean is
  begin
     if All_Charts(Index).ChartVars /= null  and then
         All_Charts(Index).ChartVars.all'Length >0 and then
         All_Charts(Index).ChartVars(1).Name.all = "RANDOMQUEUE" then
        return True;
     elsif All_Charts(Index).ChartVars /= null  and then
         All_Charts(Index).ChartVars.all'Length >1 and then
         All_Charts(Index).ChartVars(2).Name.all = "RANDOMQUEUE" then
        return True;
     else
       return False;
     end if;
  end Has_Random_Queues;

begin
--    Vectors_DB.Initialize_DB;
    NullStruct.Simple.Literal_Value := StructBase - Vectors_DB.NickNum(Empty_Int_Table);
    --
--    Default_Rules(1) :=  
--      (ActionKind,
--       new Rule_Left'( 1 =>
--          (null,null,new String'("$1"),null,
--           new String_Table'(1..1 => new String'("$*")),
--           null, noop,null)),
--      new String_Table'(1 => new String'("$1"),
--                        2 => new String'("$*")));
    --
end UML_Types;
