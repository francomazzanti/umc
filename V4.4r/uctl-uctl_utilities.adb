-- with UCTL_Types; use UCTL_Types;
-- with Ada.Text_IO; use Ada.Text_IO;
separate(UCTL)
package body UCTL_utilities is
use Ada.Text_IO;
use UCTL_Types;
--use Basic_Properties;
--
--  function Is_Monotone (Form: Formula_Ref;
--                       Id: String_Ref  := null;
--                       Direction: Boolean := True) return Boolean;
--
--   procedure Prepare_Formula (Form: Formula_Ref);
--
--   procedure Set_Free_Vars (Form: Formula_Ref);
--
--   procedure Check_Actions (Form: Formula_Ref);
--
--   procedure Print (Formula: Formula_Ref; More_Lines: Boolean := True);
--
--   procedure Print_Formula (Margin: String;
--                            Form: Formula_Ref;
--                            More_Lines: Boolean := True);
--
--   procedure Print_Action (Action: Action_Ref);
--
-- procedure  Check_Params (Form: Formula_Ref); 
-------------------------------------------------------------------
  subtype Action is UCTL_Types.Action;

  procedure Print_Path (Margin: String;
                        Path: Path_Ref;
                        More_Lines: Boolean := True);

  procedure Print_REnclosed (Action: Action_Ref);
  procedure Print_REnclosed (Margin: String;
                             Form: Formula_Ref;
                             More_Lines: Boolean := True);

  procedure Print_LEnclosed (Action: Action_Ref);
  procedure Print_LEnclosed (Margin: String;
                             Form: Formula_Ref;
                             More_Lines: Boolean := True);

  function LFimage (Form: Formula_Ref) return String;
  function RFimage (Form: Formula_Ref) return String;
  function Fimage(Form:Formula_Ref) return String;
  function Pimage (Path: Path_Ref) return String;
--  function Aimage (Action: Action_Ref) return String;

  procedure Insert_Full_Def (Form: Formula_Ref;
                             Defs: Form_Table);

--2  function Has_Free_Vars (Form: Formula_Ref;
--2                             Defs: Form_Table) return Boolean;
                             
  function Is_Ambiguous_Suffix (Form: Formula_Ref) return Boolean;
  function Is_Ambiguous_Prefix (Form: Formula_Ref) return Boolean;
  function Is_Ambiguous_Prefix (Action: Action_Ref) return Boolean;
  function Is_Ambiguous_Suffix (Action: Action_Ref) return Boolean;
  
  procedure Check_Action (Action: Action_Ref);

  function Warning_Needed (Action: String) return Boolean;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function Check_Ident(Params: String_Table; Ident: String_Ref) return String_Table is
  begin
     if Ident /= null and then
        Ident.all'Length >0 and then
        Ident(1) = '$' then
        for i in Params'Range loop
          if Params(I).all = Ident.all then
            Put_Line (Current_Error,
                 "Error: Free variable  """& Ident.all & """ already defined.");
            raise Parsing_Error;
          end if;
        end loop;
        return Params& Ident; 
     elsif Ident /= null and then
        Ident.all'Length >0 and then
        Ident(1) = '%' then
        for I in Params'Range loop
          if Params(I)(2..Params(I).all'Last) = Ident(2..Ident.all'Last) then
              return Params;
          end if;
        end loop;
        Put_Line (Current_Error,
           "Error: Undefined bound variable  """& Ident.all & """ ");
        raise Parsing_Error;
     else
        return Params;
     end if;
  end Check_Ident;

  function Check_Ident_Table (Params: String_Table; Ident_Table: String_Table) return String_Table is
  begin
     if Ident_Table'Length = 0  then return Params; end if;
     declare
       STT1: String_Table := Check_Ident(Params, Ident_Table(Ident_Table'First));
     begin
       declare
         -- just check internal bound vars dont make use of internal free vars
         STT2: String_Table :=  
             Check_Ident_Table (Params, Ident_Table(Ident_Table'First+1..Ident_Table'Last));
       begin
         -- return previous free vars enriched with current ones
         return Check_Ident_Table (STT1, Ident_Table(Ident_Table'First+1..Ident_Table'Last));
       end;
     end;
  end Check_Ident_Table;

  --
  -- Params is a table of $var names already declared inside the action axpression
  -- return the Params list extended with the new names found in this Action
  function Check_Action (Params: String_Table; Act: Action_Ref) return String_Table is
  begin
    if Act = null then return Params; end if;
    case Act.Kind is
      when Atrue | Afalse   =>  null;
      when Anot  =>
         declare
           STT1: String_Table := Check_Action(Params,Act.Anot);
         begin
            return Params;
         end;
      when Aand | Aor   =>
         declare
           STT1: String_Table := Check_Action(Params,Act.Aref1);
           STT2: String_Table := Check_Action(Params & STT1,Act.Aref2);
         begin
--            return Params;
           return STT2;
         end;
      when Aid =>
         declare
           STT1: String_Table := Check_Ident_Table(Params,Act.AidPred.Labels.all);
         begin
            return STT1;
         end;
       when Aas =>
         return Params;
    end case;
    return Params;
  end Check_Action;

  function Check_Params (Form: Formula_Ref; Params: String_Table := Empty_String_Table) return String_Table  is
  begin 
    case Form.Kind is
      when Ftrue | Ffalse  => return Params;
      when  Assertion   =>
         --  (%1 = %2),  (pred(%2))
         declare
           STT1: String_Table := Check_Ident(Params,Form.Pred.Left_Index);
           STT2: String_Table := Check_Ident(Params,Form.Pred.Right_Index);
         begin
           null;
         end; 
         if  Form.Pred.Right_Ids /= null then
           declare
             STT3: String_Table := Check_Ident_Table(Params,Form.Pred.Right_Ids.all);
           begin
             null;
           end;
         end if;
         return Params;
      when Fnot  =>
        return Check_Params(Form.NotRef,Params);
      when Fand | Foor | Fimply  =>
         declare
           STT1: String_Table := Check_Params(Form.LeftRef,Params);
           STT2: String_Table := Check_Params(Form.RightRef,Params);
         begin
            return Params;
         end;
      when Fapply =>
          --  FINAL, COUNT, PRINT(%2,%2), Xfixvar
         if Form.Optargs /= null then
           declare
             STT1: String_Table := Check_Ident_Table(Params,Form.Optargs.all);
           begin
              return Params;
           end;
         else
           return Params;
         end if;
      when Fmax  | Fmin =>
         declare
           STT1: String_Table := Check_Params(Form.FDef,Params);
         begin
            return Params;
         end;          
      when Fangle | FWangle | Fsquare | FWsquare  =>
         declare
           STT1: String_Table := Check_Action(Params,Form.ARef);
           STT2: String_Table := Check_Params(Form.FormRef,STT1);
         begin
            return Params;
         end;
      when Fexist | Fall  =>
        case Form.Pref.Kind is
        when Eventually | Always =>
            declare
              STT2: String_Table := Check_Params(Form.Pref.TForm,Params);
            begin
               return Params;
            end;
        when Act_Next =>
            declare
              STT1: String_Table := Check_Action(Params,Form.Pref.AARef);
              STT2: String_Table := Check_Params(Form.Pref.AFormRef,STT1);
            begin
               return Params;
            end;
        when Until1 | Wuntil1 =>
            declare
              STT1: String_Table := Check_Params(Form.Pref.U1FormRef1,Params);
              STT2: String_Table := Check_Action(Params,Form.Pref.U1Aref);
              STT3: String_Table := Check_Params(Form.Pref.U1FormRef2,Params);
            begin
               return Params;
            end;
        when Until2 | Wuntil2  =>
            declare
              STT1: String_Table := Check_Params(Form.Pref.U2FormRef1,Params);
              STT2: String_Table := Check_Action(Params,Form.Pref.U2Aref1);
              STT3: String_Table := Check_Action(Params,Form.Pref.U2Aref2);
              STT4: String_Table := Check_Params(Form.Pref.U2FormRef2,STT3);
            begin
               return Params;
            end;
        end case;
    end case;
  end Check_Params;

 procedure Check_Params (Form: Formula_Ref) is
    STT: String_table :=  Check_Params (Form);
 begin
    null;
 end;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print (Formula: Formula_Ref; More_Lines: Boolean := True) is
  begin
    if Formula = null then
       Put_Line("No Formula present?!");
       raise Parsing_Error;
    else
      Print_Formula("", Formula, More_Lines);
--      New_Line;
    end if;
    --
  exception
    when Parsing_Error => raise;
    when others =>
         Put_Line("Unrecoverable error in uctl_utilities.Print");
         raise UCTL_Error;
  end Print;

  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function Is_Ambiguous_Suffix (Action: Action_Ref) return Boolean is
  begin
    if Action = null then return false; end if;
    case Action.Kind is
      when Anot  =>
         if Action.Kind = Aand or
            Action.Kind = Aor then
           return True;
         else 
           return False;
         end if;
      when Aand | Aor  =>
          return true;
      when others =>
          return false;
        end case;
  end Is_Ambiguous_Suffix;

  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function Is_Ambiguous_Prefix (Action: Action_Ref) return Boolean is
  begin
    case Action.Kind is
      when Anot | Aand | Aor  =>
          return true;
      when others =>
          return false;
        end case;
  end Is_Ambiguous_Prefix;


  -----------------------------------------------------------------
  procedure Print_REnclosed (Margin: String;
                             Form: Formula_Ref;
                             More_Lines: Boolean := True) is
  begin
    if Is_Ambiguous_Suffix(Form) then
      Put(" (");
      Print_Formula (Margin & "  ", Form, More_Lines);
      Put(" )");
    else
      Print_Formula (Margin & "  ", Form, More_Lines);
    end if;
  end Print_REnclosed;

  function RFimage (Form: Formula_Ref) return String is
  begin
    if Is_Ambiguous_Suffix(Form) then
      return "(" & Fimage(Form) & ")";
    else
       return Fimage(Form);
    end if;
  end RFimage;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print_LEnclosed (Margin: String;
                             Form: Formula_Ref;
                             More_Lines: Boolean := True) is
  begin
    if Is_Ambiguous_Prefix(Form) then
      Put(" (");
      Print_Formula (Margin & "  ", Form, More_Lines);
      Put(" )");
    else
      Print_Formula (Margin & "  ", Form, More_Lines);
    end if;
  end Print_LEnclosed;

  function LFimage (Form: Formula_Ref) return String is
  begin
    if Is_Ambiguous_Prefix(Form) then
      return "(" & Fimage(Form) & ")";
    else
       return Fimage(Form); 
    end if;
  end LFimage;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print_REnclosed (Action: Action_Ref) is
  begin
    if Is_Ambiguous_Suffix(Action) then
      Put(" (");
      Print_Action (Action);
      Put(" )");
    else
      Print_Action (Action);
    end if;
  end Print_REnclosed;

  function RAimage (Action: Action_Ref) return String  is
  begin
    if Is_Ambiguous_Suffix(Action) then
      return "(" & Aimage(Action) & ")";
    else
       return  Aimage(Action);
    end if;
  end RAimage;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print_LEnclosed (Action: Action_Ref) is
  begin
    if Is_Ambiguous_Prefix(Action) then
      Put(" (");
      Print_Action (Action);
      Put(" )");
    else
      Print_Action (Action);
    end if;
  end Print_LEnclosed;

  function LAimage (Action: Action_Ref) return String  is
  begin
    if Is_Ambiguous_Prefix(Action) then
      return "(" & Aimage(Action) & ")";
    else
       return  Aimage(Action);
    end if;
  end LAimage;

  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print_Action (Action: Action_Ref) is
    vin: Boolean := false; -- inside vector literal
    noc: Boolean := True;  -- no colon
  begin
    if Action= null then
       return;
--       Put_Line("Empty pointer ?!");
--       raise Parsing_Error;
    end if;

    case Action.Kind is
      when Atrue   =>
         Put(" true");
      when Afalse   =>
         Put(" false");
      when Anot  =>
         Put(" not");
         Print_REnclosed (Action.ANot);
      when Aand  =>
         Print_LEnclosed (Action.ARef1);
         Put(" and");
         Print_REnclosed (Action.ARef2);
      when Aor   =>
         Print_LEnclosed (Action.ARef1);
         Put(" or");
         Print_REnclosed (Action.ARef2);
      when Aid =>
         Put (" "); 
--         if Action.AidPred.Source /= null then
--            Put (Action.AidPred.Source.all & ":");
--         end if;
--         if Action.AidPred.Target /= null then
--            Put (Action.AidPred.Target.all & ".");
--         end if;
--         if Action.AidPred.Event /= null then
--           Put (Action.AidPred.Event.all);
--         end if;
--         if Action.AidPred.Modality /= null then
--           Put (Action.AidPred.Modality.all);
--         end if;
--         if Action.AidPred.Params /= null and then
--             Action.AidPred.Params.all'Length /= 0 then
--           Put ("<");
--           vin := false; -- inside vector literal
--           noc := True;  -- no colon
--           for I in 1.. Action.AidPred.Params.all'length loop
--             if  Action.AidPred.Params(I).all /= "#" then
--               if not noc then
--                  Put(",");
--               end if;
--               Put (Action.AidPred.Params(I).all);
--               noc := False;
--             elsif vin then 
--               Put("]");
--               noc := False;
--             else 
--               if not noc then
--                  Put(",");
--               end if;
--               Put("[");
--               vin:= True;
--               noc := True;
--             end if; 
--           end loop;
--           Put (">");
--         end if;
         if Action.AidPred.Labels /= null  and then
              Action.AidPred.Labels.all'Length >0 then
          Put (Action.AidPred.Labels(1).all);
          if Action.AidPred.Labels.all'Length >1 then
            Put("(");
            for I in 2.. Action.AidPred.Labels.all'Length loop
             Put (Action.AidPred.Labels(I).all);
             if I < Action.AidPred.Labels.all'Length then
              Put(",");
             end if;
            end loop;
            Put(")");
          end if;
         end if;
      when Aas =>
--        Put ("(" & Action.AasPred.Left_Ids(1).all);
--        for I in 2..Action.AasPred.Left_Ids.all'Length loop
--          Put ("." & Action.AasPred.Left_Ids(I).all);
--        end loop;
--        Put(''');
--        --
--        case Action.AasPred.Op is
--           when GT => Put (" > ");
--           when GE => Put (" >= ");
--           when LT => Put (" < ");
--           when LE => Put (" <= ");
--           when EQ => Put (" = ");
--           when NE => Put (" /= ");
--           when NOOP => null;
--        end case;
--        --
--        Put (Action.AasPred.Right_Ids(1).all);
--        for I in 2..Action.AasPred.Right_Ids.all'Length loop
--          Put ("." & Action.AasPred.Right_Ids(I).all);
--        end loop;
--        if Action.AasPred.RightOp = PLUS then
--          Put ( " + ");
--          Put (Action.AasPred.More_Right_Ids(1).all);
--          for I in 2..Action.AasPred.More_Right_Ids.all'Length loop
--            Put ("." & Action.AasPred.More_Right_Ids(I).all);
--          end loop;
--        end if;
--        Put (")");
        null;
    end case;
  end Print_Action;


  function Ground_UML_Action_Image (Action: Action_Ref) return String is
    Tmp: String_Ref := new String'("");
  begin
    if Action.AidPred.Source /= null then
      Tmp := new String'(Action.AidPred.Source.all & ":");
    end if;
    if Action.AidPred.Target /= null then
      Tmp := new String'(Tmp.all & Action.AidPred.Target.all & ".");
    end if;
    if Action.AidPred.Event /= null then
      Tmp := new String'(Tmp.all & Action.AidPred.Event.all);
    else
      Tmp := new String'(Tmp.all & "*");
    end if;
    if Action.AidPred.Params /= null and then
       Action.AidPred.Params.all'Length /= 0 then
      Tmp := new String'(Tmp.all & "(" & Action.AidPred.Params(1).all);
      for I in 2.. Action.AidPred.Params.all'length loop
        Tmp := new String'(Tmp.all & "," & Action.AidPred.Params(I).all);
      end loop;
      Tmp := new String'(Tmp.all & ")");
    end if;
    return Tmp.all;
  end Ground_UML_Action_Image;


  function Abstract_Action_Image (Action: Action_Ref) return String is
    Tmp: String_Ref := new String'("");
  begin
    if Action.AidPred.Source /= null then
      Tmp := new String'(Action.AidPred.Source.all & ":");
    end if; 
    if Action.AidPred.Labels = null or else Action.AidPred.Labels.all'Length =0 then
      return Tmp.all;
    end if;
    Tmp := new String'(Tmp.all & Action.AidPred.Labels(1).all);  -- mail label
    if Action.AidPred.Labels.all'Length =1 and then Action.AidPred.Params /= null then
      Tmp := new String'(Tmp.all & "()" );
    end if;
    if Action.AidPred.Labels.all'Length =2 then
       Tmp := new String'(Tmp.all & "(" & Action.AidPred.Labels(2).all & ")" );
    elsif Action.AidPred.Labels.all'Length > 2 then
      Tmp := new String'(Tmp.all & "(" & Action.AidPred.Labels(2).all);
      for I in 3..Action.AidPred.Labels.all'Length loop
        Tmp := new String'(Tmp.all & "," & Action.AidPred.Labels(I).all);
      end loop;
      Tmp := new String'(Tmp.all & ")" );
    end if;
    return Tmp.all;
  end Abstract_Action_Image;
  

  function Ground_COWS_Action_Image (Action: Action_Ref) return String is
     Tmp: String_Ref := new String'("");
  begin
    -- GROUND COWS ACTION
    --  source
    if Action.AidPred.Source /= null then
      Tmp := new String'(Action.AidPred.Source.all & ":");
    end if;
    -- communivations
    if Action.AidPred.Target /= null and then
         Action.AidPred.Event /= null  and then
         Action.AidPred.Event.all /= "kill" and then
         Action.AidPred.Modality /= null then
      Tmp := new String'(Tmp.all & Action.AidPred.Target.all & ".");
      Tmp := new String'(Tmp.all & Action.AidPred.Event.all);
      Tmp := new String'(Tmp.all & Action.AidPred.Modality.all);
      if Action.AidPred.Params /= null and then
         Action.AidPred.Params.all'Length /= 0 then
        Tmp := new String'(Tmp.all & "<" & Action.AidPred.Params(1).all);
        for I in 2.. Action.AidPred.Params.all'length loop
          Tmp := new String'(Tmp.all & "," & Action.AidPred.Params(I).all);
        end loop;
        Tmp := new String'(Tmp.all & ">");
      elsif Action.AidPred.Params = null or else
          Action.AidPred.Params.all'Length = 0 then
          Tmp := new String'(Tmp.all & "<>");
      end if;
      return Tmp.all;
    end if;
    -- kill
    if Action.AidPred.Event /= null  and then
       Action.AidPred.Event.all = "kill" then
      Tmp := new String'("kill");
      if Action.AidPred.Params /= null and then
         Action.AidPred.Params.all'Length=1 then
         Tmp := new String'(Tmp.all & "(" & Action.AidPred.Params(1).all & ")");
      end if;
      return Tmp.all;
    end if;
    return "";
  end Ground_COWS_Action_Image;


  function Aimage (Action: Action_Ref) return String is
    Tmp: String_Ref := new String'("");
  begin
    if Action=null then
        return "tau";
    end if;
    case Action.Kind is
      when Atrue   =>
         return "true";
      --
      when Afalse   =>
         return "false";
      --
      when Anot  =>
         return "not " & Aimage(Action.ANot);
      --
      when Aand  =>
         return LAimage(Action.ARef1) & " and " & RAimage(Action.ARef2);
      --
      when Aor   =>
         return LAimage(Action.ARef1) & " or " & RAimage(Action.ARef2);

      when Aid =>
        -- GROUND COWS ACTION
          return Abstract_Action_Image(Action);
        -- 
      when Aas =>
        --
       return "";
    end case;
  end Aimage;

  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function AssertionImage( Form: Formula_Ref) return String is
    Result: String_Ref  := new String(1..0);
  begin
      --       
      --  %id = id
      --
     if Form.Pred.Op = EQ  and then
        Form.Pred.Left_Ids =null and then
        Form.Pred.Right_Ids = null and then
        Form.Pred.Left_Index /= null and then
        Form.Pred.Right_Index /= null then
        return ("(" & Form.Pred.Left_Index.all & "=" & Form.Pred.Right_Index.all & ")");
      end if;
      --
      --  %id /= id
      --
     if Form.Pred.Op = NE  and then
        Form.Pred.Left_Ids =null and then
        Form.Pred.Right_Ids = null and then
        Form.Pred.Left_Index /= null and then
        Form.Pred.Right_Index /= null then
        return ("(" & Form.Pred.Left_Index.all & "/=" & Form.Pred.Right_Index.all & ")");
      end if;
      --
      --  %id < id
      --
     if Form.Pred.Op = LT  and then
        Form.Pred.Left_Ids =null and then
        Form.Pred.Right_Ids = null and then
        Form.Pred.Left_Index /= null and then
        Form.Pred.Right_Index /= null then
        return ("(" & Form.Pred.Left_Index.all & "<" & Form.Pred.Right_Index.all & ")");
      end if;
      --
      --  %id > id
      --
     if Form.Pred.Op = GT  and then
        Form.Pred.Left_Ids =null and then
        Form.Pred.Right_Ids = null and then
        Form.Pred.Left_Index /= null and then
        Form.Pred.Right_Index /= null then
        return ("(" & Form.Pred.Left_Index.all & ">" & Form.Pred.Right_Index.all & ")");
      end if;
      --
      --  %id >= id
      --
     if Form.Pred.Op = GE  and then
        Form.Pred.Left_Ids =null and then
        Form.Pred.Right_Ids = null and then
        Form.Pred.Left_Index /= null and then
        Form.Pred.Right_Index /= null then
        return ("(" & Form.Pred.Left_Index.all & ">=" & Form.Pred.Right_Index.all & ")");
      end if;
      --
      --  %id <= id
      --
     if Form.Pred.Op = LE  and then
        Form.Pred.Left_Ids =null and then
        Form.Pred.Right_Ids = null and then
        Form.Pred.Left_Index /= null and then
        Form.Pred.Right_Index /= null then
        return ("(" & Form.Pred.Left_Index.all & "<=" & Form.Pred.Right_Index.all & ")");
      end if;
      --
      if Form.Pred.Left_Ids /= null and then
        Form.Pred.Left_Ids.all'Length = 1 then
        if Form.Pred.Left_Ids(1) /= null then
          Result := new String'(Form.Pred.Left_Ids(1).all & ":");
        end if;
      end if;
      Result := new String'(Result.all & Form.Pred.Right_Ids(1).all);
      if Form.Pred.Right_Ids.all'Length > 1 then
        Result := new String'(Result.all & "(");
        for J in 2..Form.Pred.Right_Ids.all'Length loop
          Result := new String'(Result.all & Form.Pred.Right_Ids(J).all);
          if J < Form.Pred.Right_Ids.all'Length then
             Result := new String'(Result.all & ",");
          end if;
        end loop;
        Result := new String'(Result.all & ")");
      end if;
    return Result.all;
  end AssertionImage;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print_Formula (Margin: String;
                           Form: Formula_Ref;
                           More_Lines: Boolean := True) is
  begin
    if Form = null then
       Put_Line("No Formula present?!");
       raise Parsing_Error;
    end if;

    case Form.Kind is
      when Ftrue  =>
         Put(" true");
      when  Assertion   =>
         Put(" " & AssertionImage(Form)) ;
      when  Ffalse   =>
         Put(" false");
      when Fnot  =>
         Put(" not" );
         if Form.NotRef.Kind = Fand  or
            Form.NotRef.Kind = Foor or
            Form.NotRef.Kind = Fimply then
           Put (" ( " );
           Print_Formula (Margin & "  ", Form.NotRef, More_Lines);
           Put (" ) " );
         else
           Print_Formula (Margin & "  ", Form.NotRef, More_Lines);
         end if;
      when Fand   =>
         Print_LEnclosed (Margin, Form.LeftRef, More_Lines);
        if More_Lines then
         New_Line; Put (Margin);
        end if;
         Put (" and" );
         Print_REnclosed (Margin & "  ", Form.RightRef, More_Lines);
      when Foor   =>
         Print_LEnclosed (Margin, Form.LeftRef, More_Lines);
        if More_Lines then
         New_Line; Put (Margin);
        end if;
         Put (" or" );
         Print_REnclosed (Margin & "  ", Form.RightRef, More_Lines);
      when  Fimply   =>
         Print_LEnclosed (Margin, Form.LeftRef, More_Lines);
        if More_Lines then
         New_Line; Put (Margin);
        end if;
         Put (" implies" );
         Print_REnclosed (Margin & "  ", Form.RightRef, More_Lines);
      when Fapply =>
         Put(" " & Form.IDen.all);
      when Fmax =>
         Put(" max " & Form.IDef.all & " :");
        if More_Lines then
         New_LIne; Put (Margin & "  ");
        end if;
         Print_REnclosed (Margin & "  ", Form.FDef, More_Lines);
      when Fmin =>
         Put(" min " & Form.IDef.all & " :");
        if More_Lines then
         New_LIne; Put (Margin & "  ");
        end if;
         Print_REnclosed (Margin & "  ", Form.FDef, More_Lines);
      when Fangle   =>
         Put(" <");
         Print_Action (Form.ARef);
         Put(" >");
         Print_REnclosed (Margin , Form.FormRef, More_Lines);
      when FWangle   =>
         Put(" <<");
         Print_Action (Form.ARef);
         Put(" >>");
         Print_REnclosed (Margin , Form.FormRef, More_Lines);
      when  FWsquare  =>
         Put(" [[");
         Print_Action (Form.ARef);
         Put (" ]]");
         Print_REnclosed (Margin , Form.FormRef, More_Lines);
      when  Fsquare  =>
         Put(" [");
         Print_Action (Form.ARef);
         Put (" ]");
         Print_REnclosed (Margin , Form.FormRef, More_Lines);
      when Fexist   =>
         Put(" E");
         Print_Path (Margin & "  ", Form.PRef, More_Lines);
      when Fall  =>
         Put(" A");
         Print_Path (Margin & "  ", Form.PRef, More_Lines);
    end case;
    --
  end Print_Formula;

  procedure SetFimage(Form:Formula_Ref);
 
  function Fimage(Form:Formula_Ref) return String is
  begin
    if Form = null then
        return "";
    else 
      if Form.Fimage = null then
        SetFimage(Form);
      end if;
      return Form.Fimage.all;
    end if;
  end Fimage;   

  procedure SetFimage(Form:Formula_Ref) is
  begin
    if Form = null then return; end if;
    if Form.Fimage /= null then return; end if;
    --
    case Form.Kind is
      when Ftrue  =>
         Form.Fimage  := new String'("true");
      when  Assertion   =>
         Form.Fimage  := new String'(AssertionImage(Form));
      when  Ffalse   =>
         Form.Fimage  := new String'("false");
         --
      when Fnot  =>
        SetFimage(Form.NotRef);
         if Form.NotRef.Kind = Fand  or
            Form.NotRef.Kind = Foor or
            Form.NotRef.Kind = Fimply then
           Form.Fimage  := new String'("not (" & Form.NotRef.Fimage.all & ")");
         else
           Form.Fimage  := new String'("not " & Form.NotRef.Fimage.all);
         end if;
         --
      when Fand   =>
         SetFimage(Form.LeftRef);
         SetFimage(Form.RightRef);
         Form.Fimage  := new String'( LFimage(Form.LeftRef) &
                                       " and " & RFimage(Form.RightRef));                  
         --
      when Foor   =>
         SetFimage(Form.LeftRef);
         SetFimage(Form.RightRef);
         Form.Fimage  := new String'( LFimage(Form.LeftRef) &
                                       " or " & RFimage(Form.RightRef));
         --
      when  Fimply   =>
         SetFimage(Form.LeftRef);
         SetFimage(Form.RightRef);
         Form.Fimage  := new String'( LFimage(Form.LeftRef) &
                                       " implies " & RFimage(Form.RightRef));
         --
      when Fapply =>
         if Form.Optargs = null then
           Form.Fimage  := new String'(Form.IDen.all);
         else
           Form.Fimage :=  new String'(Form.IDen.all & "(");
           for I in Form.Optargs.all'Range loop
             Form.Fimage := new String'(Form.Fimage.all & Form.Optargs(I).all );
             if I /= Form.Optargs.all'Last then
                 Form.Fimage := new String'(Form.Fimage.all & ",");
             end if;
           end loop;
           Form.Fimage := new String'(Form.Fimage.all & ")" );
         end if;
         --
      when Fmax =>
         SetFimage(Form.FDef);
         Form.Fimage  := 
           new String'("max " & Form.IDef.all & ": (" & Form.FDef.Fimage.all & ")");
         --
      when Fmin =>
         SetFimage(Form.FDef);
         Form.Fimage  := 
           new String'("min " & Form.IDef.all & ": (" & Form.FDef.Fimage.all & ")");
         --
      when Fangle   =>
         SetFimage(Form.FormRef);
         Form.Fimage  := 
           new String'("<" & Aimage(Form.ARef) & "> " & RFimage(Form.FormRef));
         --
      when FWangle   =>
         SetFimage(Form.FormRef);
         Form.Fimage  := 
           new String'("<<" & Aimage(Form.ARef) & ">> " & RFimage(Form.FormRef));
         --
      when  FWsquare  =>
         SetFimage(Form.FormRef);
         Form.Fimage  :=
           new String'("[[" & Aimage(Form.ARef) & "]] " & RFimage(Form.FormRef));
         --
      when  Fsquare  =>
         SetFimage(Form.FormRef);
         Form.Fimage  := 
           new String'("[" & Aimage(Form.ARef) & "] " & RFimage(Form.FormRef));
         --
      when Fexist   =>
         Form.Fimage := new String'("E" & Pimage(Form.PRef));
         --
      when Fall  =>
         Form.Fimage := new String'("A" & Pimage(Form.PRef));
         --
    end case;
    --
  end SetFimage;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  procedure Print_Path (Margin: String;
                        Path: Path_Ref;
                        More_Lines: Boolean := True) is
  begin
    if Path = null then
       Put_Line("Empty pointer ?!");
       raise Parsing_Error;
    end if;

    case Path.Kind is
      when  Eventually  =>
         Put("F ");
         Print_REnclosed (Margin & "  ", Path.TForm, More_Lines);
      when  Always =>
         Put("G ");
         Print_REnclosed (Margin & "  ", Path.TForm, More_Lines);
      when Act_Next =>
         if Path.AARef = null then
           Put("T ");
         else
           Put("X {");
           Print_Action (Path.AARef);
           Put("} ");
         end if;
         Print_REnclosed (Margin & "  ", Path.AFormRef, More_Lines);
      when Until1 =>
         Put("[");
         Print_Formula (Margin & "  ", Path.U1FormRef1, More_Lines);
         Put(" {");
         Print_Action (Path.U1ARef);
         Put("}");
         Put(" U ");
        if More_Lines then
         New_Line; Put (Margin );
        end if;
         Print_Formula (Margin & "  " , Path.U1FormRef2, More_Lines);
         Put("]");
      when Wuntil1 =>
         Put("[");
         Print_Formula (Margin & "  ", Path.U1FormRef1, More_Lines);
         Put(" {");
         Print_Action (Path.U1ARef);
         Put("}");
         Put(" W ");
        if More_Lines then
         New_Line; Put (Margin );
        end if;
         Print_Formula (Margin & "  " , Path.U1FormRef2, More_Lines);
         Put("]");
      when Until2 =>
         if Path.U2FormRef1.Kind /= Ftrue or else
              Path.U2ARef1.Kind /= Atrue then
           Put("[");
           Print_Formula (Margin & "  ", Path.U2FormRef1, More_Lines);
           Put(" {");
           Print_Action (Path.U2ARef1);
           Put("}");
           Put(" U");
           if More_Lines then
             New_Line; Put (Margin );
           end if;
           Put(" {");
           Print_Action (Path.U2ARef2);
           Put("}");
           Print_Formula (Margin & "  " , Path.U2FormRef2, More_Lines);
           Put("]");
        else
         Put("F {");
           Print_Action (Path.U2ARef2);
           Put("} ");
           Print_Formula (Margin & "  " , Path.U2FormRef2, More_Lines);
        end if;
      when Wuntil2 =>
         Put("[");
         Print_Formula (Margin & "  ", Path.U2FormRef1, More_Lines);
         Put(" {");
         Print_Action (Path.U2ARef1);
         Put("}");
         Put(" W");
        if More_Lines then
         New_Line; Put (Margin );
        end if;
         Put(" {");
         Print_Action (Path.U2ARef2);
         Put("}");
         Print_Formula (Margin & "  " , Path.U2FormRef2, More_Lines);
         Put("]");
    end case;
  end Print_Path;

  function Pimage (Path: Path_Ref) return String is
  begin
    case Path.Kind is
      when  Eventually  =>
         return "F " & RFimage(Path.TForm);
      --
      when  Always =>
         return "G "  & RFimage(Path.TForm);
      --
      when Act_Next =>
         if Path.AARef = null then
           return "T "  & Fimage(Path.AFormRef);
         else
           return "X {" & Aimage(Path.AARef) & "} " & RFimage(Path.AFormRef); 
         end if;
      --
      when Until1 =>
         return "[" & Fimage(Path.U1FormRef1) & " {" & Aimage(Path.U1ARef) & "} U " & 
                   Fimage(Path.U1FormRef2) & "]" ;
      --
      when Wuntil1 =>
         return "[" & Fimage(Path.U1FormRef1) & " {" & Aimage(Path.U1ARef) & "} W " &
                   Fimage(Path.U1FormRef2) & "]" ;
      --
      when Until2 =>
        if Path.U2FormRef1.Kind /= Ftrue or else
            Path.U2ARef1.Kind /= Atrue then
          return "[" & Fimage(Path.U2FormRef1) & " {" & Aimage(Path.U2ARef1) & "} U {" & 
                   Aimage(Path.U2ARef2) & "}" & Fimage(Path.U2FormRef2) & "]" ; 
        else
            return "F {" & Aimage(Path.U2ARef2) & "} " & Fimage(Path.U2FormRef2);
        end if;
      --
      when Wuntil2 =>
         return "[" & Fimage(Path.U2FormRef1) & " {" & Aimage(Path.U2ARef1) & "} W {" &
                  Aimage(Path.U2ARef2) & "} " & Fimage(Path.U2FormRef2) & "]" ;

    end case;
  end Pimage;
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function Is_Ambiguous_Suffix (Form: Formula_Ref) return Boolean is
  begin
    case Form.Kind is
      when Fand | Foor | Fimply =>
          return true;
      when others =>
          return false;
    end case;
  end Is_Ambiguous_Suffix;


  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function Is_Ambiguous_Prefix (Form: Formula_Ref) return Boolean is
  begin
    case Form.Kind is
      when Fnot | Fand | Foor | Fimply |
           Fmax | Fmin | Fangle | Fsquare | FWangle | FWsquare =>
          return true;
      when Fexist | Fall =>
        case Form.Pref.Kind is
           when Wuntil1 | Wuntil2 | Until1  => return false;
           when Until2  => 
              if Form.Pref.U2FormRef1 = True_Formula and then
                 Form.Pref.U2ARef1 = True_Action then
                 --  becomes  AF {act} true
                return True;
              else
                return False;
              end if;
           when others => return True;
        end case;
      when others =>
          return false;
    end case;
  end Is_Ambiguous_Prefix;

  function Is_Monotone (Form: Formula_Ref;
                        Id: String_Ref  := null;
                        Direction: Boolean := True) return Boolean is
  begin
    case Form.Kind is
      when Ftrue | Ffalse | Assertion  =>
         return True;
      when Fnot  =>
         return Is_Monotone (Form.NotRef, Id, not Direction);
      when Fand | Foor | Fimply   =>
         return (Is_Monotone (Form.LeftRef,Id, Direction) and then
                   Is_Monotone (Form.RightRef,Id, Direction) );
      when Fapply =>
         if Id = null or else Form.IDen.all /= Id.all then
           return True;
         else
           return Direction;
         end if;
      when Fmax | Fmin =>
         return (Is_Monotone (Form.FDef, Form.Idef)
                  and then
                 Is_Monotone (Form.FDef, Id, Direction) );
      when Fangle | Fsquare | FWangle | FWsquare  =>
         return Is_Monotone (Form.FormRef, Id, Direction);
      when Fexist | Fall  =>
         case Form.PRef.Kind is
           when Eventually | Always =>
           return Is_Monotone (Form.PRef.Tform,Id, Direction);
         when Act_Next =>
           return Is_Monotone (Form.PRef.AFormRef,Id, Direction);
         when Until1 | Wuntil1 =>
           return (Is_Monotone (Form.PRef.U1FormRef1,Id, Direction)
                 and then
               Is_Monotone (Form.PRef.U1FormRef2,Id, Direction) );
         when Until2  | Wuntil2 =>
           return (Is_Monotone (Form.PRef.U2FormRef1,Id, Direction)
                 and then
               Is_Monotone (Form.PRef.U2FormRef2,Id, Direction) );
       end case;
    end case;
  end Is_Monotone;


  -------------------------------------------------------------------------
  -- procedure ricorsiva che si richiama sulle sottoformule, partendo da una lista
  -- vuota di defintizioni attive, che vengono utilizzare per completare le definizioni
  -- di Apply formule con le definizioni delle full definitions dei punti fissi a cui si
  -- riferiscono. 
  -- Chiamata initialmente da Prepare_Formula dopo aver fatto il parsing di una nuova formula,
  -- viene chiamata anche quando viene creata una nuova istanza di formula parametrica.
  -- in questo secondo caso possono essere incontrate variabili "libere", la cui full_def e'
  -- gia stata definita precedente, e quindi da non aggiornare.
  -------------------------------------------------------------------------
  procedure Insert_Full_Def (Form: Formula_Ref;
                             Defs: Form_Table) is
  begin
    if Form= null then return; end if;
    case Form.Kind is
      when  Ftrue | Ffalse | Assertion
           => null;
      when Fnot =>
         Insert_Full_Def(Form.NotRef, Defs);
      when Fand | Foor | Fimply =>
         Insert_Full_Def(Form.LeftRef,Defs);
         Insert_Full_Def(Form.RightRef,Defs);
      when Fapply =>
         for I in  reverse Defs'range loop
            if Defs(I).Idef.all = Form.IDen.all then
               Form.FullDef := Defs(I);
               exit;
            end if;
            -- if no definition is present it is an error only if full_def is empty.
            -- This may occur only if a parsed formula has free variables.
         end loop;
      when Fmax | Fmin  =>
        Insert_Full_Def(Form.Fdef,Defs & (1 => Form));
      when Fangle | Fsquare | FWangle | FWsquare =>
        Insert_Full_Def(Form.Formref,Defs);
      when Fexist | Fall =>
        case Form.Pref.Kind is
          when Eventually | Always =>
             Insert_Full_Def(Form.PRef.TForm,Defs);
          when Act_Next =>
              Insert_Full_Def(Form.PRef.AFormRef,Defs);
          when Until1 | Wuntil1 =>
              Insert_Full_Def(Form.PRef.U1FormRef1,Defs);
              Insert_Full_Def(Form.PRef.U1FormRef2,Defs);
          when Until2 | Wuntil2 =>
              Insert_Full_Def(Form.PRef.U2FormRef1,Defs);
              Insert_Full_Def(Form.PRef.U2FormRef2,Defs);
          end case;
        end case;
  end Insert_Full_Def;

  function Free_Vars_Table (Form: Formula_Ref; Env: Form_Table)
           return Form_Table_Ref;

  procedure Set_Env_Selector (Form: Formula_Ref; Env: Form_Table);

  procedure Set_Closed_Context (Form: Formula_Ref);


  procedure Prepare_Formula (Form: Formula_Ref) is
    --
    -- Insert full definitions in Apply nodes ..
    -- Evaluates DEPTH_GT actions
    -- Checks existence of used action tokens
    --
    -- Insert Formula image and subimages in all nodes
    --
    Supposed_Empty: Form_Table_Ref;
  begin
     -- Insert_Full_Def has the effect of  initializing the 
     -- Form.FullDef  fied of the FApply subformulas of formula Form
     -- the full_def of existing free_vars is not modified, nor an error is given
     --  if free vars are found
     Insert_Full_Def (Form, (1..0 => null));

     -- Free_Vars_Table has the side-effect of adjusting the
     --  Free_Vars field of Form (and its subforms)
     --
     -- se FF = " (AG ( Y | [true] X) & Z " 
     --    FF.Free_Vars = [X.Full_def, Y.FullDef, Z.FullDef] 
     Supposed_Empty := Free_Vars_Table (Form, (1..0 => null));

     -- se FF1 =  (AG Y)
     --  e FF2 =  max Y:  X & FF1
     --    FF1.Free_Vars=[Y.Fulldef],  FF1.Closed_Context=[Y.Fulldef,X.FullDef]
     --   (i.e. per poter valutare Y devo sapere anche cosa e' X]
     Set_Closed_Context(Form);
     --
     Set_Env_Selector(Form, (1..0 => null));

     if Supposed_Empty.all'Length /= 0 then
        raise Parsing_Error;                    -- max Z: <receive(*,$a) > < %a > Z
     end if;
     --    
     --  decorate the formula with its string images
     --
     SetFimage(Form); -- .Fimage := new String'(Fimage(Form));
     --
  end Prepare_Formula;

  --  given [3,1,2] and [1,3,4]   returns [3,1,2,4]
  function Join_Form_Tables (First: Form_Table_Ref; 
                             Second: Form_Table_Ref) 
                                       return Form_Table_Ref is
   Index: Natural := First.all'Length;
   Result: Form_Table(1..First.all'Length + Second.all'Length);
   Found:Boolean;
  begin
    Result (1..First.all'Length) := First.all;
    for I in Second.all'Range loop
      Found := False;
      for J in First.all'Range loop
        if First(J) = Second(I) then
           Found := True;
        end if;
      end loop;
      if not Found then
        Index := Index+1;
        Result(Index) := Second(I);
      end if;
    end loop;
    return new Form_Table'(Result(1..Index));
  end Join_Form_Tables;

  -- called when Form.kind = max Z: ..  or min Z: ..
  -- if This (current Form position in Env (Env'Length+1) is in 
  -- Table, return Table - this, otherwise return table
  --  
  function Reduce (This: Formula_Ref; Table: Form_Table_Ref) 
            return Form_Table_Ref is
    Reduced: Form_Table(1..Table.all'Length -1); 
    Index: natural :=0;
  begin
    for I in Table.all'Range loop
      if Table(I) /= This then
         Index := Index+1;
         Reduced(Index) := Table(I);
      end if;  
    end loop;
    if Index = Table.all'Length then
      return Table;
    else
      return new Form_Table'(Reduced(1..Index));
    end if;
  end Reduce;


  ----------------------------------------------------------------------
  --  One parsed a formula, we must "prepare" it for the on-the-fly evalution.
  --  This "preparation" includes the generation of the formula's "Closed_Context"
  --  which requires the knowledge of all the free variables appearing inside the
  --  formula (and all its subformules).
  -- For this reason we have the field "Free_Vars" associated to any formula,
  --  and this function recersively initializes that field.
  --   
  -- e.g given  FF = " (AG ( Y | [true] X) & Z "
  --   FF.Free_Vars = [X.Full_def, Y.FullDef, Z.FullDef]
  -- 
  --  Free_Vars_Table works bottom-up computing the frevars inside subformulas,
  --  merging them and storing them in the free_vars field of the current formula,
  --  and returning the saved result.  (which is the case of anewly parsed formula will
  --  be an empty table).
  ----------------------------------------------------------------------
  ----------------------------------------------------------------------
  -- THE Free_Vars Table is used STATICALLY by the function Set_Closed_Context,
  --  (which is in turn used statiucally by Set_Closed_Context)
  -- THERE IS NO NEED TO UPDATE THE INFORMATION ABOUT THE FREE_VARS and CLOSED_CONTEXT
  -- (in case of new formula instance) once the intial ENV_SELECTOR HAS BEEN COMPUTED.
  ----------------------------------------------------------------------
  ----------------------------------------------------------------------
  function Free_Vars_Table (Form: Formula_Ref; Env: Form_Table) 
           return Form_Table_Ref is
   Is_Undefined:Boolean := True;
  begin
    case Form.Kind is
    when  Ftrue | Ffalse | Assertion =>
         Form.Free_Vars := new Form_Table'(Empty_Form_Table);
    when Fnot =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.NotRef, Env);
    when Fand | Foor | Fimply =>
        Form.Free_Vars := 
           Join_Form_Tables (
              Free_Vars_Table(Form.LeftRef,Env),
              Free_Vars_Table(Form.RightRef,Env));
    when Fapply =>
         for I in  reverse Env'range loop
            if Env(I).Idef.all = Form.IDen.all then
               Form.Free_Vars := new Form_Table'(1..1 => Env(I));
               exit;
            end if;
         end loop;
         if Form.Free_Vars = null then
           -- check Form.IDEn.all = Predefined!
           Is_Undefined := True;
           for I in Predefined_Formulas'Range loop
             if Predefined_Formulas(I).all'Length <= 
                   Form.IDen.all'Length and then
              Predefined_Formulas(I).all(
                   1..Predefined_Formulas(I).all'Length) 
                 = 
              Form.IDen.all(1..Predefined_Formulas(I).all'Length)  then
              Is_Undefined := False;
             end if;
           end loop;
           if Is_Undefined then
              Put_Line (Current_Error, 
                "Error: Undefined identifier """& Form.IDen.all & """ ");
              raise Parsing_Error;
           end if;
           Form.Free_Vars := new Form_Table'(Empty_Form_Table);
         end if;
    when Fmax | Fmin  =>
        Form.Free_Vars := 
           Reduce(Form, Free_Vars_Table(Form.Fdef, Env & (1 => Form)));
    when Fangle  =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.Formref,Env);
    when  FWangle  =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.Formref,Env);
         if Form.Free_Vars.all'Length > 0 then
           Put_Line (Current_Error," Fixpoint variables not allowed inside <<..>> operator.");
           raise Parsing_Error;
         end if;
    when Fsquare  =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.Formref,Env);
    when  FWsquare =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.Formref,Env);
         if Form.Free_Vars.all'Length > 0 then
           Put_Line (Current_Error," Fixpoint variables not allowed inside [[..]] operator.");
           raise Parsing_Error;
         end if;
    when Fexist | Fall =>
      case Form.Pref.Kind is
      when Eventually | Always =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.PRef.TForm,Env);
         if Form.Free_Vars.all'Length > 0 then
           Put_Line (Current_Error," Fixpoint variables not allowed inside AG AF EG EF operators.");
           raise Parsing_Error;
         end if;
      when Act_Next =>
         Form.Free_Vars :=
           Free_Vars_Table(Form.PRef.AFormRef,Env);
      when Until1 | Wuntil1 =>
         Form.Free_Vars :=
           Join_Form_Tables (
              Free_Vars_Table(Form.PRef.U1FormRef1,Env),
              Free_Vars_Table(Form.PRef.U1FormRef2,Env));
         if Form.Free_Vars.all'Length > 0 then
           Put_Line (Current_Error, " Fixpoint variables not allowed inside Until operators.");
           raise Parsing_Error;
         end if;
      when Until2 | Wuntil2  =>
        Form.Free_Vars :=
         Join_Form_Tables (
              Free_Vars_Table(Form.PRef.U2FormRef1,Env),
              Free_Vars_Table(Form.PRef.U2FormRef2,Env));
         if Form.Free_Vars.all'Length > 0 then
           Put_Line (Current_Error, " Fixpoint variables not allowed inside Until operators.");
           raise Parsing_Error;
         end if;
      end case;
    end case;
    return Form.Free_Vars;
  end Free_Vars_Table;

  ----------------------------------------------------------------------
  --  One parsed a formula, we must "prepare" it for the on-the-fly evalution.
  --  This "preparation" includes the generation of the formula's "Closed_Context"
  --  which requires the knowledge of all the free variables appearing inside the
  --  formula (and all its subformules).
  --  All the subformulas of the parsed formula have the "Closed_Context" field. 
  --  The "Closed_Context" of a subformula denotes the list of fixed-point definitions
  --  which needed for the evalution of the subformula.
  --  If the subformula does not contain Free_Variables, its closed context is empty.
  --  If the subformula contains a free_var Y, then the closed contex of the subformula
  --  contains the definition of Y and the definition of the free_variables which 
  -- appear inside Y, and so on recursively.
  -- 
  --  E.g.  given  FF =  max Y:  X & (AG Y)
  --  Y.Fulldef, is a Free_Var of "AG Y"
  --  X.Fulldef,, Y.Fulldef are Frre_Vars of "X & (AG Y)"
  --  X.Fulldef, is a Free_Var of FF
  --
  --  The Closed_Context of FF is [X.FullDef, ..]  -- X.FullDef.Closed_Context
  --  The Closed_Context if "X & (AG Y)" is [X.FullDef,Y.Fulldef]
  --  The Closed_Context of "AG Y" is [X.FullDef,Y.Fulldef]
  --    FF1.Free_Vars=[Y.Fulldef],  FF1.Closed_Context=[X.FullDef,Y.Fulldef]
  --   (i.e. per poter valutare Y devo sapere anche cosa e' X]
  ----------------------------------------------------------------------
  -- THE CLOSED CONTEXT is used STATICALLY by the function Set_Env_Selector,
  -- which computes an equivalent structure of the closed context, where full_defs
  -- are replaced by the index inside the dynamic evaluation ENV
  --  (stack of active fixpoint defs).
  -- THERE IS NO NEED TO UPDATE THE INFORMATION ABOUT THE FREE_VARS and CLOSED_CONTEXT
  -- (in case of new formula instance) once the intial ENV_SELECTOR HAS BEEN COMPUTED.
  ----------------------------------------------------------------------
  procedure Set_Closed_Context (Form: Formula_Ref) is
    These_Free_Vars: Form_Table_Ref := Form.Free_Vars;
    This_Closed: Form_Table_Ref := new Form_Table'(These_Free_Vars.all); 
  begin
    --
    for I in These_Free_Vars.all'Range loop
     This_Closed :=
       Join_Form_Tables(This_Closed, These_Free_Vars(I).Closed_Context);
    end loop; 
    Form.Closed_Context := This_Closed;
    --
  case Form.Kind is
    when  Ftrue | Ffalse | Assertion =>  null;
    when Fnot => Set_Closed_Context(Form.NotRef);
    when Fand | Foor | Fimply =>
        Set_Closed_Context(Form.LeftRef);
        Set_Closed_Context(Form.RightRef);
    When Fapply =>  null;
    when Fmax | Fmin  =>
        Set_Closed_Context(Form.Fdef);
    when Fangle | FWangle  =>
        Set_Closed_Context(Form.Formref);
    when Fsquare | FWsquare =>
        Set_Closed_Context(Form.Formref);
    when Fexist | Fall =>
      case Form.Pref.Kind is
      when Eventually | Always =>
         Set_Closed_Context(Form.PRef.TForm);
      when Act_Next =>
         Set_Closed_Context(Form.PRef.AFormRef);
      when Until1 | Wuntil1 =>
         Set_Closed_Context(Form.PRef.U1FormRef1);
         Set_Closed_Context(Form.PRef.U1FormRef2);
      when Until2 | Wuntil2 =>
         Set_Closed_Context(Form.PRef.U2FormRef1);
         Set_Closed_Context(Form.PRef.U2FormRef2);
      end case;
    end case;
  end Set_Closed_Context;


  ---------------------------------------------------------------------------
  --   Env e' lo stack delle formule max e min in cui Form e' inclusa.
  --
  --   Context e' il Closed_Context dellea formula, cioe' la lista
  --     delle forumule ricorsive (max e min) di cui la sua valutazione
  --      fa uso (diretto o indiretto)
  -- 
  --  In generale Env'Length => Context'Length
  --  la tabella ritornata e' costituita dagli INDICI, relativi ad ENV
  --  degli elementi costituenti il CONTEXT
  -- 
  --  In questo modo, non e' necessario analizzare tutto l'env per 
  --  individuare la parte che serve per valutare la formula, inoltre
  --  l' ENv_Selector permette di selezionare solo la sottoparte
  --   dell'Env da salvare assieme alla formula (Trimmed_Context)
  --  
  ---------------------------------------------------------------------------
  function Mk_Env_Selector (Context: Form_Table_Ref;
                            Env: Form_Table) return Num_Table_Ref is
    Result: Num_Table (1..Context.all'Length);
  begin
    for J in  Context.all'Range loop
      for I in  reverse Env'range loop
        if Env(I) = Context(J) then
           Result(J)  := I;
           exit;
        end if;
      end loop;
    end loop;
    return new Num_Table'(Result);
  end Mk_Env_Selector;


  ---------------------------------------------------------------------------
  --   Env e' lo stack delle formule max e min in cui Form e' inclusa.
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
  --
  ---------------------------------------------------------------------------
  procedure Set_Env_Selector (Form: Formula_Ref; Env: Form_Table) is
  begin
    Form.Env_Selector := Mk_Env_Selector(Form.Closed_Context,Env);
    --
    case Form.Kind is
    when  Ftrue | Ffalse | Assertion =>  null;
    when Fnot => Set_Env_Selector(Form.NotRef, Env);
    when Fand | Foor | Fimply =>
          Set_Env_Selector(Form.LeftRef,Env);
          Set_Env_Selector(Form.RightRef,Env);
    when Fapply =>  null;
    when Fmax | Fmin  =>
          Set_Env_Selector(Form.Fdef, Env & (1 => Form));
    when Fangle | FWangle  =>
          Set_Env_Selector(Form.Formref,Env);
    when Fsquare | FWsquare =>
          Set_Env_Selector(Form.Formref,Env);
--          Set_Env_Selector(Form.NotFormref,Env);
    when Fexist | Fall =>
      case Form.Pref.Kind is
      when Eventually | Always =>
          Set_Env_Selector(Form.PRef.TForm,Env);
      when Act_Next =>
          Set_Env_Selector(Form.PRef.AFormRef,Env);
      when Until1 | Wuntil1  =>
          Set_Env_Selector(Form.PRef.U1FormRef1,Env);
          Set_Env_Selector(Form.PRef.U1FormRef2,Env);
      when Until2 | Wuntil2 =>
          Set_Env_Selector(Form.PRef.U2FormRef1,Env);
          Set_Env_Selector(Form.PRef.U2FormRef2,Env);
      end case;
    end case;
  end Set_Env_Selector;

  
  procedure Check_Actions (Form: Formula_Ref) is
  begin
    case Form.Kind is
    when  Ftrue | Ffalse | Assertion =>
         null;
    when Fnot =>
         Check_Actions(Form.NotRef);
    when Fand | Foor | Fimply =>
         Check_Actions(Form.LeftRef);
         Check_Actions(Form.RightRef);
    when Fapply =>
         null;
    when Fmax | Fmin  =>
         Check_Actions(Form.Fdef);
    when Fangle | Fsquare | FWangle | FWsquare =>
         Check_Actions(Form.Formref);
         Check_Action(Form.ARef);
    when Fexist | Fall =>
      case Form.Pref.Kind is
      when Eventually | Always =>
         Check_Actions(Form.PRef.TForm);
      when Act_Next =>
         Check_Actions(Form.PRef.AFormRef);
         if Form.PRef.AARef /= null then
           Check_Action(Form.PRef.AARef);
         end if;
      when Until1  | Wuntil1 =>
         Check_Actions(Form.PRef.U1FormRef1);
         Check_Action(Form.PRef.U1ARef);
         Check_Actions(Form.PRef.U1FormRef2);
      when Until2  | Wuntil2 =>
         Check_Actions(Form.PRef.U2FormRef1);
         Check_Action(Form.PRef.U2ARef1);
         Check_Actions(Form.PRef.U2FormRef2);
         Check_Action(Form.PRef.U2ARef2);
      end case;
    end case;
  end Check_Actions;
  
  procedure Check_Action (Action: Action_Ref) is
  begin
    case Action.Kind is 
       when Atrue   =>  
           null;
       when  Afalse   =>  
           null;
       when Anot  =>
           Check_Action (Action.Anot);
       when Aand | Aor =>
           Check_Action (Action.Aref1);
           Check_Action (Action.Aref2);
       when Aid =>  null;   ---  TO BE CHECKED
        when Aas => null;   --- TOBE CHECKED!!!!!!!!!
     end case;
  end Check_Action;
  
  function Warning_Needed (Action: String) return Boolean is
  begin
    return False;
  end Warning_Needed;
  
   function NewString (Str: String_Ref; TheBindings: VarBindings) return String_Ref;

   function NewStringTable (StrT: String_Table_Ref; 
          TheBindings: VarBindings) return String_Table_Ref;   

   function NewPath (PRef: Path_Ref; TheBindings: VarBindings) return Path_Ref;
   
  function NewAction(ARef: Action_Ref; TheBindings: VarBindings) return Action_Ref is
    Result: Action_Ref := ARef;
    Tmp1, Tmp2: Action_Ref;
    TmpSource:String_Ref;
    TmpTarget: String_Ref;
    TmpEvent: String_Ref;
    TmpParams: String_Table_Ref;
    TmpLabels: String_Table_Ref;
  begin
    case ARef.Kind is
      when Atrue | Afalse   =>  null;
      when Anot  =>
         Tmp1 := NewAction(ARef.Anot, TheBindings);
         if Tmp1 /= ARef.Anot then
            Result := new Action'(ARef.all);
            Result.Anot := Tmp1;
         end if;
      when Aand | Aor   =>
         Tmp1 := NewAction(ARef.Aref1, TheBindings);
         Tmp2 := NewAction(ARef.Aref2, TheBindings);
         if Tmp1 /= ARef.Aref1 or else
            Tmp2 /= Aref.Aref2 then
            Result := new Action'(ARef.all);
            Result.Aref1 := Tmp1;
            Result.Aref2 := Tmp2;
         end if;
      when Aid =>
        TmpSource:= NewString(ARef.AidPred.Source, TheBindings);
        TmpTarget:= NewString(ARef.AidPred.Target, TheBindings);
        TmpEvent:= NewString(ARef.AidPred.Event, TheBindings);
        TmpParams:= NewStringTable(Aref.AidPred.Params, TheBindings);
        TmpLabels:= NewStringTable(Aref.AidPred.Labels,TheBindings);
        if TmpSource /= ARef.AidPred.Source or else
           TmpTarget /= ARef.AidPred.Target or else
           TmpEvent /= ARef.AidPred.Event or else
           TmpParams /= Aref.AidPred.Params or else
           TmpLabels /= Aref.AidPred.Labels then
          Result := new Action'(ARef.all);
          Result.AidPred := new Basic_Action(Aid);
          Result.AidPred.Source := TmpSource;
          Result.AidPred.Target := TmpTarget;
          Result.AidPred.Event := TmpEvent;
          Result.AidPred.Params := TmpParams;
          Result.AidPred.Labels := TmpLabels;
        end if;
     when Aas =>                   ---   (obj'.x >= obj.x+1)  (x' > x)
        null ;   -- not SUPPORTED IN COWS
    end case;
    return Result;
  end NewAction;


  function RecNewInstance(Form: Formula_Ref; TheBindings: VarBindings) return Formula_Ref is
    Result: Formula_Ref := Form;
    Tmp1, Tmp2: Formula_Ref;
    TmpAref: Action_Ref;
    TmpPath: Path_Ref;
    --
    TmpLeft_Ids: String_Table_Ref;
    TmpMore_Left_Ids : String_Table_Ref;
    TmpRight_Ids: String_Table_Ref;
    TmpMore_Right_Ids : String_Table_Ref;
    TmpLeft_Index: String_Ref;
    TmpMore_Left_Index : String_Ref;
    TmpRight_Index: String_Ref;
    TmpMore_Right_Index : String_Ref;
    TmpOptargs: String_Table_Ref;
  begin
    if TheBindings.Size  = 0 then
        return Form;
    end if;
    case Form.Kind is
      when Ftrue | Ffalse   =>
         null;
      when Fnot  =>
         Tmp1 := RecNewInstance(Form.NotRef, TheBindings);
         if Tmp1 /= Form.NotRef then
            Result := new Formula'(Form.all);
            Result.NotRef:= Tmp1;
            Result.Fimage := null;
         end if;
      when Fand | Foor | Fimply   =>
         Tmp1 := RecNewInstance(Form.LeftRef, TheBindings);
         Tmp2 := RecNewInstance(Form.RightRef, TheBindings);
         if Tmp1 /= Form.LeftRef or else
            Tmp2 /= Form.RightRef then
           Result := new Formula'(Form.all);
           Result.LeftRef := Tmp1;
           Result.RightRef := Tmp2;
           Result.Fimage := null;
         end if;
      when Fapply =>
         if Form.IDen.all="PRINT" and then
             Form.Optargs/= null then
           TmpOptargs := NewStringTable(Form.Optargs,TheBindings);
           if Form.Optargs /= TmpOptargs then
             Result := new Formula'(Form.all);
             Result.Optargs := TmpOptargs;
             Result.Fimage := null;
           end if;
         end if;
      when Fmax | Fmin =>
         Tmp1 := RecNewInstance(Form.FDef,TheBindings); 
         if Tmp1 /= Form.FDef then
              Result := new Formula'(Form.all); 
              Result.FDef := Tmp1;
              Result.Fimage := null;
         end if;
      when Fangle | Fsquare | FWangle | FWsquare =>
         TmpAref := NewAction(Form.ARef, TheBindings);
         Tmp1 := RecNewInstance(Form.FormRef, TheBindings);
         if TmpARef /= Form.ARef or else
            Tmp1 /= Form.FormRef then
            Result := new Formula'(Form.all);
            Result.ARef := TmpARef;
            Result.FormRef := Tmp1;
            Result.Fimage := null;
         end if;
      when Fexist | Fall  =>
         TmpPath := NewPath(Form.PRef, TheBindings);
         if TmpPath /= Form.PRef then
           Result := new Formula'(Form.all);
           Result.PRef := TmpPath;
           Result.Fimage := null;
         end if;
      when Assertion =>
        -- var EQ Value  / value eq value  / var eq var
        --   
        TmpLeft_Ids := NewStringTable(Form.Pred.Left_Ids, TheBindings);
        TmpMore_Left_Ids := NewStringTable(Form.Pred.More_Left_Ids, TheBindings);
        TmpRight_Ids := NewStringTable(Form.Pred.Right_Ids, TheBindings);
        TmpMore_Right_Ids := NewStringTable(Form.Pred.More_Right_Ids, TheBindings);
        TmpLeft_Index := NewString(Form.Pred.Left_Index, TheBindings);
        TmpMore_Left_Index := NewString(Form.Pred.More_Left_Index, TheBindings);
        TmpRight_Index := NewString(Form.Pred.Right_Index, TheBindings);
        TmpMore_Right_Index := NewString(Form.Pred.More_Right_Index, TheBindings);
        if TmpLeft_Ids /= Form.Pred.Left_Ids or else
           TmpMore_Left_Ids /= Form.Pred.More_Left_Ids or else
           TmpRight_Ids /= Form.Pred.Right_Ids or else
           TmpMore_Right_Ids /= Form.Pred.More_Right_Ids or else
           TmpLeft_Index /= Form.Pred.Left_Index or else
           TmpMore_Left_Index /= Form.Pred.More_Left_Index or else
           TmpRight_Index /= Form.Pred.Right_Index or else
           TmpMore_Right_Index /= Form.Pred.More_Right_Index then
          --
          Result := new Formula'(Form.all);
          Result.Pred := new Basic_Predicate'(Form.Pred.all);
          Result.Pred.Left_Ids := TmpLeft_Ids;
          Result.Pred.More_Left_Ids := TmpMore_Left_Ids;
          Result.Pred.Right_Ids := TmpRight_Ids;
          Result.Pred.More_Right_Ids := TmpMore_Right_Ids;
          Result.Pred.Left_Index := TmpLeft_Index;
          Result.Pred.More_Left_Index := TmpMore_Left_Index;
          Result.Pred.Right_Index := TmpRight_Index;
          Result.Pred.More_Right_Index := TmpMore_Right_Index;
          Result.Fimage := null;
        end if;
    end case;
      -- if Result /= Form then
      --     Result.Fimage := new String'(Fimage(Result));
      --     Result.Closed_Context  UNCHANGED
      --     Result.Free_Vars   UNCHANGED
      --    VARIABLE INSTANCES  CANNOT  APPEAR INSIDE FIXED POINT DEFINITIONS
      -- end if;
    return Result;
  end RecNewInstance;

  ---------------------------------------------------------------------------------------
  -- when a parametric formula is evalutated, i.e. some variable binding have been 
  -- computed and a new subformula istance is generated, we need to update also 
  -- all the Fimages  of the formula and its subformulas,  as we need to updated all the
  -- ful_defs of the apply subformulas which noew refer to a difierent fixpoint full definition.
  -- This operation may render the Free_Vars and Closed_Context field of the formula obsolete,
  -- but this is not a problem since these fields are used only statically to generate the
  -- the formula Env_Selector (the only field later used during the evasluation).
  ---------------------------------------------------------------------------------------
  function NewInstance(Form: Formula_Ref; TheBindings: VarBindings) return Formula_Ref is
     Result: Formula_Ref;
  begin
     Result := RecNewInstance(Form,TheBindings);
     Insert_Full_Def (Form, (1..0 => null));
     SetFimage(Result);
     return Result;
  end NewInstance;

   function NewPath (PRef: Path_Ref; TheBindings: VarBindings) return Path_Ref is
     TmpF1, TmpF2 : Formula_Ref;
     TmpA1, TmpA2 : Action_Ref;
     Result: Path_Ref := PRef;
   begin
    case PRef.Kind is
      when  Eventually | Always =>
        TmpF1 := NewInstance(Pref.TForm, TheBindings);
        if TmpF1 /= PRef.TForm then
          Result := new Path'(PRef.all);
          Result.TForm := TmpF1;
        end if;
      when Act_Next =>
        TmpA1 := NewAction(Pref.AAref, TheBindings);
        TmpF1 := NewInstance(Pref.AFormRef, TheBindings);
        if TmpA1 /= Pref.AAref or else
          TmpF1 /= Pref.AFormRef then
          Result := new Path'(PRef.all);
          Result.AAref := TmpA1;
          Result.AFormRef := TmpF1;
        end if;
      when Until1 | Wuntil1 =>
        TmpF1 := NewInstance(Pref.U1FormRef1, TheBindings);
        TmpA1 := NewAction(Pref.U1ARef, TheBindings);
        TmpF2 := NewInstance(Pref.U1FormRef2, TheBindings);
        if TmpA1 /= Pref.U1ARef or else
          TmpF1 /= Pref.U1FormRef1 or else
          TmpF2 /= PRef.U1FormRef2 then
          Result := new Path'(PRef.all);
          Result.U1ARef := TmpA1;
          Result.U1FormRef1 := TmpF1;
          Result.U1FormRef2 := TmpF2;
        end if;
      when Until2 | Wuntil2  =>
        TmpF1 := NewInstance(Pref.U2FormRef1, TheBindings);
        TmpA1 := NewAction(Pref.U2ARef1, TheBindings);
        TmpA2 := NewAction(Pref.U2ARef2, TheBindings);
        TmpF2 := NewInstance(Pref.U2FormRef2, TheBindings);
        if TmpA1 /= Pref.U2ARef1 or else
          TmpF1 /= Pref.U2FormRef1 or else
          TmpA2 /= Pref.U2ARef2 or else
          TmpF2 /= PRef.U2FormRef2 then
          Result := new Path'(PRef.all);
          Result.U2FormRef1 := TmpF1;
          Result.U2ARef1 := TmpA1;
          Result.U2ARef2 := TmpA2;
          Result.U2FormRef2 := TmpF2;
        end if;
    end case;
    return Result;
   end NewPath;

   function NewString (Str: String_Ref; TheBindings: VarBindings) return String_Ref is
      Result: String_Ref := Str;
   begin
     if Str /= null and then Str(1) = '%' then
         for I in TheBindings.VarNames'Range loop
           if TheBindings.VarNames(I)(2..TheBindings.VarNames(I).all'Length) = 
                Str(2..Str'Length) then
             Result := TheBindings.VarValues(I);
           end if;
         end loop; 
     end if;
     return Result;
   end NewString;

   function NewStringTable (StrT: String_Table_Ref; 
           TheBindings: VarBindings) return String_Table_Ref is
      Changed: Boolean := False;
      Result: String_Table_Ref := StrT;
   begin
      if StrT = null then return StrT; end if;
      declare
         Tmp : String_Table := StrT.all;
      begin
      for I in Tmp'Range loop
        Tmp(I) := NewString(StrT(I),TheBindings);
        if Tmp(I) /= StrT(I) then
          changed := True;
        end if;
      end loop;
      if Changed then 
        Result := new String_Table'(Tmp);
      end if;
      end;
      return Result;
   end NewStringTable;

begin
 null;
end UCTL_utilities;
