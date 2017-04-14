with Ada.Directories; use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_Io;
separate (Configurations.Kernel)
package body UML_Explore is
use System_Model;
use Global_Env;
use UML_Types;
use UML_Configurations;

procedure SPAWN_DOT_COMMAND(DotFile: String; ResFile: String) is
  UMCfullpath: String := Normalize_Pathname(Command_Name);
  BinDir: String := Normalize_Pathname(Containing_Directory(UMCfullpath));
  dotcommand: String := BinDir & Dir_Separator & "dot";
  Res: Boolean;
  SubArgs: Argument_List := 
     ( 1 => new String'("-Tjpg"), 
       2 => new String'("-o"),
       3 => new String'(ResFile),
       4 => new String'(DotFile));
begin
  -- command is:
  -- dot -Tjpg  -o file.jpg  file.dot
  -- 
  Spawn(dotcommand,SubArgs,Res);
exception
  when others => null;
end;

--
-----------------------------------------------------------------------
--
--  HISTORY TRACKING
--
-----------------------------------------------------------------------
--
    
-----------------------------------------------------------------------
--
--     DISPLAY  UTILITIES
--
-----------------------------------------------------------------------
--
  procedure Print_Source_Configuration (This_Conf: System_Configuration) is
  begin
    Put_line ("Current Configuration:  " & 
        NickName(This_Conf,"C") );
  end;
 
  procedure Print_Chart_Info (Current_Conf: System_Configuration;
                               Active_Object: Positive :=1) is
    --
  begin
    Put_Line ("------------- CURRENT CONFIGURATION ----------------");
    Put_Line (" OBJECT NAME          = " &  
       All_Charts(Active_Charts(Active_Object)).Name.all );
    Put_Line (" OBJECT QUEUE         = " &  
       Display_Queue(Current_Conf,Active_Object) );
--    Put_Line (" CURRENT TRIGGER      = " & 
--       Display_Trigger(Current_Conf,Active_Object) );
    Put_Line (" CURRENT VARIABLES    = " &  
       Display_Vars(Current_Conf,Active_Object)  );
    Put_line (" ACTIVE STATES       = " &  
       Display_States(Current_Conf,Active_Object) );
    if Is_Suspended(Current_Conf,Active_Object) then
      Put_line (" SUSPENDED IN CALL OPERATION");
    else
      if Has_Fireable_Transitions(Current_Conf,Active_Object) then
      Put_line (" FIREABLE TRANSITIONS = ");
        Print_Fireables(Current_Output, Current_Conf, Active_Object);
      else
        Put_line (" NO FIREABLE TRANSITIONS" );
      end if;
    end if;
    Put_Line ("----------------------------------------------------");
  end Print_Chart_Info;
  

--  function HTML_Signals_Format (Source:String) return String is
--   Result:String(1..1000);
--   OUTC: Natural := 1;
--   htmlspace: String := "&nbsp;";
--   indent: natural :=5;
--  begin
--   for I in Source'Range loop
--     if Source(I)='<' then
--       Result(OUTC..OUTC+3) := "&lt;";
--       OUTC := OUTC+4;
--       --
--     elsif Source(I)='>' then
--       Result(OUTC..OUTC+3) := "&gt;";
--       OUTC := OUTC+4;
--       --
--     elsif Source(I)=';' and then I /= Source'Last then
--       Result(OUTC) := ';';
--       OUTC := OUTC+1;
--       Result(OUTC..OUTC+3) := "<br>";
--       OUTC := OUTC+4;
--       for K in 1..indent loop
--         Result(OUTC..OUTC+4) := "&nbsp";
--         OUTC := OUTC+5;
--       end loop;
--     else
--       Result(OUTC) := Source(I);
--       OUTC := OUTC+1;
--     end if;
--    end loop;
--    return Result(1..OUTC-1);
--  end HTML_Signals_Format;


  --------------------------------------------------------------------------------
  --   called by the "html" command
  --   or activated when clicking over a node of the SVG evolution/explanation map.
  --   (in the www interface activated by the display_structure.cgi driver)
   -----------------------------------------------------------------------------------------
   -- HTML_File is an already existing and opened file, created by HTML_Configuration_Info,
   --  Adds to this file an html description of the current configuration.
  -------------------------------------------------------------------------------- 
  procedure Save_As_HTML (HTML_File: File_Type; Current_Conf: System_Configuration) is
    This_Nick: String := NickName(Current_Conf,"C");
  begin
    --
--    Put_line (HTML_File,"<CENTER>");
--    Put_line (HTML_File,"<h2> Configuration <font color=""red""> " & This_Nick & 
--                "</font></h2>" );
--    Put_Line (HTML_File," </CENTER>");
--    Put_line (HTML_File,"<P>");
--    Put_line (HTML_File,"<P>");
    Put_Line (HTML_File,"The internal Structure of Configuration " & This_Nick & 
              " is the following:<br>");
    Put_Line (HTML_File,"<TABLE cellspacing=10 cellpadding=5  bgcolor=""white"">");
--    Put_line (HTML_File,"<TD bgcolor=""lightyellow"">&nbsp;");
--    Put_line (HTML_File,"<font size=+1>Tokens <br>&nbsp;&nbsp; and<br> Interfaces </font>");
--    Put (HTML_File,"<TD bgcolor=""lightgrey""> " & "<font size=+1> &nbsp;");
--    for I in Predefined_Charts_Count+1 .. All_Charts.all'Length loop
--      --print list of non-active objects
--      if not Is_Active_Chart(I) then 
--        Put(HTML_File, All_Charts(I).Name.all & "&nbsp;&nbsp; ");
--      end if;
--    end loop;
--    Put_line (HTML_File,"</font> &nbsp;</TD></TR>");
    --
    for K in Active_Charts.all'Range loop
      Put_line (HTML_File,"<TR>");
      Put_line (HTML_File,"<TD bgcolor=""yellow""> ");
      Put_line (HTML_File,"<font color=""red"" size=+2>" &
          All_Charts(Active_Charts(K)).Name.all &
          "<br></font>(" & 
          All_Classes(All_Charts(Active_Charts(K)).ChartParent).Name.all &
          ")"); 
      Put_line (HTML_File,"<a href='javascript:top.ViewStatechart(""" &
          All_Classes(All_Charts(Active_Charts(K)).ChartParent).Name.all &
          """);'>View Class Statechart</a>");
      --
      Put_line (HTML_File,"<TD bgcolor=""lightgreen""> ");
      Put (HTML_File,"<b>Vars: &nbsp;</b>");
      Put_Line (HTML_File, Display_Vars(Current_Conf,K) );
      --
      Put_line (HTML_File,"<br> ");
      Put (HTML_File,"<b>Active States: </b>");
      declare
        These: States_Table := Get_Active_States(Current_Conf,K);
      begin
        for I in These'Range loop
          Put (HTML_File,These(I).FullName.all);
          if I < These'Length then 
             Put(HTML_File,", ");
          end if;
        end loop;
        New_line(HTML_File);
      end;
      --
      Put_line (HTML_File,"<br> ");
      Put (HTML_File,"<b>Queue: </b> ");
      Put_Line (HTML_File,Display_Queue(Current_Conf,K) );
      --
      Put_line (HTML_File,"<br> ");
      if Is_Suspended(Current_Conf,K) then
        Put_line (HTML_File,"<br> ");
        Put_line (HTML_File,"<b>Suspended in Call Operation</b><br>");
      else
        if Has_Fireable_Transitions(Current_Conf,K) then 
          Put_Line (HTML_File,"<b>Fireable Transitions:</b> ");
          HTML_Print_Fireables(HTML_File,Current_Conf,K);
        else
          Put_Line (HTML_File,"<b>No Fireable Transitions</b> ");
        end if;
      end if;
      Put_line (HTML_File,"</TR>" );
    end loop;    -- for K in Active_Charts
    Put_line (HTML_File,"</TABLE>" );
    New_line(HTML_File);
    --
  exception
    when others => 
      raise;
  end Save_As_HTML;

-----------------------------------------------------------------------
--
--   EXPLORATION UTILITIES
--
-----------------------------------------------------------------------
--

--  <td 
--    onmouseover= "Tip('<b>EventsQueue:</b><br> [...]<br><br><b>Local Vars:...<br>')" 
--    onmouseout="UnTip()" >
--  
-- function HTML_Object_Data(This_Conf: System_Configuration;
--                           Active_Object: Positive) return String is
--    QQQ
-- end HTML_Object_Data;

  procedure Print_Source_Info (This_Conf: System_Configuration;
                                This_Iter: Evolutions_Iterator;
                               Active_Object: Positive :=1) is
    --
  begin
    Put_Line (" OBJECT NAME          = " &
       All_Charts(Active_Charts(Active_Object)).Name.all );
    Put_Line (" OBJECT QUEUE         = " &
       Display_Queue(This_Conf,Active_Object) );
    Put_Line (" CURRENT VARIABLES    = " &
       Display_Vars(This_Conf,Active_Object)  );
    Put_line (" ACTIVE STATES       = " &
       Display_States(This_Conf,Active_Object) );
    if Is_Suspended(This_Conf,Active_Object) then
      Put_line (" SUSPENDED IN CALL OPERATION");
    else
      if Has_Fireable_Transitions(This_Conf,Active_Object) then
         Put_line (" FIREABLE TRANSITIONS = " );
        Print_Fireables(Current_Output, This_Conf,Active_Object);
      else
        Put_line (" NO FIREABLE TRANSITIONS" );
      end if;
    end if;
    Put_Line ("----------------------------------------------------");
  end Print_Source_Info;

  procedure Print_Info (Current_Conf: System_Configuration) is
     This_Iter: Evolutions_Iterator; 
  begin
    Put_Line ("----------- CURRENT CONFIGURATION : " &
              NickName(Current_Conf,"C") & " ------------");
    Put_Line ("----------------------------------------------------");
    for I in Active_Charts.all'Range loop
      Iterator_Initialize (This_Iter, Current_Conf,I);
      Print_Source_Info (Current_Conf,This_Iter,I);
      Iterator_Finalize(This_Iter);
    end loop;
    Put_Line ("----------------------------------------------------");
    Put_Line ( NickName(Current_Conf,"C") & " Abstract State Labels: ");
    Put_Line( Display_AbstractStateLabels(Get_Abstract_State_Labels(Current_Conf)));
    Put_Line ("----------------------------------------------------");
  end;


   
-----------------------------------------------------------------------
--
--  GRAPHICS
--
-----------------------------------------------------------------------
--
procedure Dump_Local_Transitions (This_State: State_Ref);
procedure Dump_Seq_Composite (This_State: State_Ref; IsRegion: Boolean := False);
function Is_Cycling (This_Transition: Transition_Ref) return Boolean;

procedure Dump_Simple (This_State: State_Ref) is
    StateId: String := Integer'Image(This_State.Num_Key);
begin
    StateId(1) := 'A';
    --
    if This_State.FullName.all'Length > 6 and then
         This_State.FullName.all (This_State.FullName.all'Length-5 ..
                               This_State.FullName.all'Length) = ".final" then
      Put ("   " & StateId &
           "[label="""",shape=doublecircle,width=0.15");
      Put_line ("];" );
      --
    elsif This_State.FullName.all'Length > 8 and then
         This_State.FullName.all (This_State.FullName.all'Length-7 ..
                               This_State.FullName.all'Length) = ".initial" then
      Put_Line ("   " & StateId &
           "[label="""",shape=circle,style=filled,fillcolor=black,width=0.2];" );
      --
    else
      --
      if This_State.Deferred= null or else This_State.Deferred.all'Length=0 then
        Put ( StateId & "[label=""" & Tail(This_State.FullName.all) & """" );
        Put_line ("];" );
        --  for each local transition (OF THIS STATE OR ANY PARENT):
        --    if the transition has a single source, add the node for the label, and
        --    if the transition has more than one target, add a fork pseudo-node.
        declare
           Cur_State: State_Ref := This_State;
           Done: Boolean := False;
        begin
        while Cur_State /= null  loop
          for I in Cur_State.LocalTransitions.all'Range loop
            if Cur_State.LocalTransitions(I).Source.all'Length = 1 and then
               Cur_State.LocalTransitions(I).Source(1)= This_State then
              declare
                Transition_Id: String :=
                Integer'Image(Cur_State.LocalTransitions(I).Num_Key);
                StateId: String := Integer'Image(This_State.Num_Key);
              begin
                Transition_Id(1) := 'x';
                StateId(1) := 'A';
                Put_line ( StateId & Transition_Id &
                   "[shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow];");
             
                if Cur_State.LocalTransitions(I).Target'Length > 1 then
                  Transition_Id(1) := 't';
                  Put_line ("pseudo_" & Transition_Id &
                    "[shape=box,label="""",width=0.5,height=0.05,style=filled,color=black];");
                end if;
                ---Done := True;
                --exit;
              end;
            end if;
          end loop;
          Cur_State := Cur_State.Parent;
        end loop;
        end;
      else
        declare
          Deferredlist: String_Ref;
        begin
          Deferredlist := This_State.Deferred(1).Name;
          for D in 2 .. This_State.Deferred.all'Length loop
           Deferredlist := 
              new String'(Deferredlist.all & "," &This_State.Deferred(D).Name.all);
          end loop;
          Put ( StateId & "[label=""" & Tail(This_State.FullName.all) & 
              "\n(Defers " & Deferredlist.all & ")" &  """" );
          Put_line ("];" );
        end;
      end if;
      --
    end if;
end Dump_Simple;


procedure Dump_Parallel (This_State: State_Ref) is
    StateId: String := Integer'Image(This_State.Num_Key);
begin
    StateId(1) := 'A';
    Put_Line ("  subgraph cluster" & StateId & " {  // PARALLEL");
    --  WARNING:  if parent is parallel this is parallel region and should be gray
--    if This_State.Parent /= null and then This_State.parent.kind= Parallel then
--      Put_Line("color=gray;");  --  TEST QQ
--    else
--      Put_Line("color=black;");
--    end if;
    if This_State.Kind=Parallel then
       Put_Line("style=""filled""; fillcolor=lightgray;"); 
     else
       Put_Line("style=""filled""; fillcolor=white;"); 
    end if;
    --
    Put_line (" { rank = source; " & StateId & "name " &
     "[label="""& Tail(This_State.FullName.all) &""",shape=plaintext, height=0.2]; }" );
    --
    for I in This_State.Substates.all'Range loop
      if This_State.Substates(I).Kind = Parallel then
         Dump_Parallel(This_State.Substates(I));
      elsif This_State.Substates(I).Kind = Composite then
         Dump_Seq_Composite (This_State.Substates(I),This_State.Kind=Parallel);
      end if;
    end loop;
    --
    Dump_Local_Transitions(This_State);
    Put_Line (" };" );
end Dump_Parallel;

  procedure Dump_Seq_Composite ( This_State: State_Ref; IsRegion: Boolean := False) is
    StateId: String := Integer'Image(This_State.Num_Key);
    Initial: Positive;
    Final_Present: Boolean := False;
  begin
    if This_State.SubStates.all'Length = 0 then
       Dump_Simple(This_State);
       return;
    end if;
    
    Initial := This_State.SubStates(1).Num_Key;
    StateId(1) := 'A';
    --
    Put_Line ("  subgraph cluster" & StateId & " {");
    if This_State.Kind=Parallel then
       Put_Line("style=""filled""; fillcolor=lightgray;"); 
     else
       Put_Line("style=""filled""; fillcolor=white;"); 
    end if;
    
--  if IsRegion then
--    Put_Line("color=gray;");
--  else
--    Put_Line("color=black;");
--  end if;
    --
    --  create the implicit initial pseudo-states
    --
    Put_Line ("   {  rank = source;");
    if This_State.Deferred= null or else This_State.Deferred.all'Length=0 then
       Put_line ("   " & StateId & "name " &
         "[label=""" & Tail(This_State.FullName.all) &""",shape=plaintext];" );
    else
      declare
        Deferredlist: String_Ref;
      begin
        Deferredlist := This_State.Deferred(1).Name;
        for D in 2 .. This_State.Deferred.all'Length loop
         Deferredlist := new String'(Deferredlist.all & "," &This_State.Deferred(D).Name.all); 
        end loop;
        Put_line ("   " & StateId & "name " &
          "[label=""" & Tail(This_State.FullName.all) &
            "\n(Defers " & Deferredlist.all & ")" & """,shape=plaintext];" );
      end;
    end if;
    --  create default initial substate (in no explicit one is present)
    if This_State.SubStates(1).FullName.all'Length <= 8 or else
        This_State.SubStates(1).FullName.all 
           (This_State.SubStates(1).FullName.all'Length-7 ..
             This_State.SubStates(1).FullName.all'Length) /= ".initial" then
      Put_line ("   " & StateId & "initial " &
         "[label="""",shape=circle,style=filled,fillcolor=black,width=0.2];" );
    end if;
    Put_Line ("  };");

    --  for each outgoing transition:
    --    if the transition has a single source, add the node for the label, and
    --    if the transition has more than one target, add a fork pseudo-node.
    --
    for I in This_State.OutgoingTransitions.all'Range loop
      for J in This_State.OutgoingTransitions.all(I).These_Transitions.all'Range loop
        if This_State.OutgoingTransitions(I).These_Transitions(J).Source.all'Length = 1 then
          declare
            Transition_Id: String :=
                Integer'Image(This_State.OutgoingTransitions(I).These_Transitions(J).Num_Key);
--                StateId: String := Integer'Image(This_State.Num_Key);
          begin
--            Transition_Id(1) := 'x';
--            Put_line ( StateId & Transition_Id &
--                "[shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow];");
            --
            if This_State.OutgoingTransitions(I).These_Transitions(J).Target'Length > 1 then
              Transition_Id(1) := 't';
              Put_line ("pseudo_" & Transition_Id &
                "[shape=box,label="""",width=0.5,height=0.05,style=filled,color=black];");
            end if;
          end;
        end if;
      end loop;
    end loop;
    -- 
    -- if the composite state owns join transitions  for
    --   transition ti  then create node  pseudo_ti to be used for drawing
    --   the transition.
    --
    -- if the composite state is SOURCE of FORK then create
    --   node  pseudo_ti to be used for drawing the transition.
    --
    for I in This_State.LocalTransitions.all'Range loop
          if This_State.LocalTransitions(I).Source.all'Length > 1 then
           declare
             Transition_Id: String :=
                Integer'Image(This_State.LocalTransitions(I).Num_Key);
           begin
             Transition_Id(1) := 't';
             Put_line ("pseudo_" & Transition_Id &
              "[shape=box,label="""",width=0.5,height=0.05,style=filled,color=black];");
           end;
      elsif Is_Cycling (This_State.LocalTransitions(I)) then
         declare
           Transition_Id: String :=
              Integer'Image(This_State.LocalTransitions(I).Num_Key);
         begin
           Transition_Id(1) := 't';
           Put_line ("pseudo_" & Transition_Id &
              "[shape=point,label="""",width=0.01,height=0.01];");
         end;
      end if;
    end loop;
    --
    --  recursively create the nodes for the substates
    --
    for I in This_State.Substates.all'Range loop
      if This_State.Substates(I).Kind = Parallel then
         Dump_Parallel(This_State.Substates(I));
      end if;
      if This_State.Substates(I).Kind = Composite then
         Dump_Seq_Composite (This_State.Substates(I));
      end if;
      if This_State.Substates(I).Kind = Simple then
         Dump_Simple(This_State.Substates(I));
      end if;
    end loop;
    --
    --  create the implicit final pseudo-states
    --
    Dump_Local_Transitions(This_State);
    Put_Line (" }; ") ;
    --
end Dump_Seq_Composite;

procedure Dump_Fork (This_Transition: Transition_Ref) is
    --
    TransId : String := Transition_label(This_Transition);
    TransDI : String := Transition_DotImage(This_Transition);
    Source_Key: Positive := This_Transition.Source(1).Num_Key;
    Source_Id: String := Integer'Image(Source_Key);
    TID: String := Integer'Image(This_Transition.Num_Key);
    Pseudo_Id: String := "pseudo_t" & TransId(2..TransId'Length);
begin
    --
    -- dump incoming fragment
    --
      Source_Id(1) := 'A';
      TID(1) := 'x';
      if This_Transition.Source(1).Kind = Simple then
         Put_Line (Source_Id & TID &
           " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
           " label = """ & TransDI & """ ];");
           
        Put_Line (Source_Id & " -> " & Source_Id & TID &
                   " [arrowhead=none];");
                   
         Put_Line (Source_Id & TID & " -> " & Pseudo_Id & ";"); 
--        Put ( Source_Id & " -> " & Pseudo_Id & " [" );
--        Put_line ("fontsize=12];"); -- , tailport=n," &
--                    --  "headlabel=""" & TransID & """];");
        
      else
        Put ( Source_Id & "name" & " -> " & Pseudo_Id &  " [" );
        Put_line ("fontsize=12," & -- tailport=n," &
             "headlabel=""" & TransId & """,ltail=cluster" & Source_Id & "];");
      end if;
    --
    --
    -- dump the outgoing fragment
    --
    for I in This_Transition.Target.all'Range loop
      declare
        Target_Key: Positive := This_Transition.Target(I).Num_Key;
        Target_Id: String := Integer'Image(Target_Key);
      begin
        Target_Id(1) := 'A';
        if This_Transition.Target(I).Kind = Simple then
          Put ( Pseudo_Id & " -> " & Target_Id & " [" );
          Put_line ( "fontsize=12];"); -- & ",headport=s];" );
        else
          Put ( Pseudo_Id & " -> " & Target_Id & "name [" );
          Put_line ( "fontsize=12," & -- headport=s," &
                      """,lhead=cluster"  & Target_Id & "];" );
        end if;
      end;
    end loop;
end Dump_Fork;


  procedure Dump_JoinedFork ( This_Transition: Transition_Ref) is
    --
    TransId : String := Transition_label(This_Transition);
    Pseudo_Id: String := "pseudo_t" & TransID(2..TransID'Length);
  begin
    --
    -- dump incoming fragments
    --
    for I in This_Transition.Source.all'Range loop
      declare
        Source_Key: Positive := This_Transition.Source(I).Num_Key;
        Source_Id: String := Integer'Image(Source_Key);
      begin
        Source_Id(1) := 'A';
        if This_Transition.Source(I).Kind = Simple then
          Put ( Source_Id & " -> " & Pseudo_Id & " [" );
          Put_line ("fontsize=12];");  --,tailport=n];");
        else
          Put ( Source_Id & "name" & " -> " & Pseudo_Id &  " [" );
          Put_line ("fontsize=12]," &  --,tailport=n," &
                    "ltail=cluster" & Source_Id & "];" );
        end if;
      end;
    end loop;
    --
    --
    -- dump the outgoing fragment
    --
    for I in This_Transition.Target.all'Range loop
      declare
        Target_Key: Positive := This_Transition.Target(I).Num_Key;
        Target_Id: String := Integer'Image(Target_Key);
      begin
        Target_Id(1) := 'A';
        if This_Transition.Target(I).Kind = Simple then
          Put ( Pseudo_Id & " -> " & Target_Id & " [" );
          Put_line ( "fontsize=12];" ); -- ," & "headport=s];" );
        else
          Put ( Pseudo_Id & " -> " & Target_Id & "name [" );
          Put_line ( "fontsize=12," &  -- headport=s," &
                      """,lhead=cluster"  & Target_Id & "];" );
        end if;
      end;
    end loop;
end Dump_JoinedFork;

procedure Dump_Join (This_Transition: Transition_Ref) is
    -- 
    TransId : String := Transition_label(This_Transition);
    Pseudo_Id: String := "pseudo_t" & TransID(2..TransID'Length);
  begin
    --
    -- dump incoming fragments
    --
    for I in This_Transition.Source.all'Range loop
      declare
        Source_Key: Positive := This_Transition.Source(I).Num_Key;
        Source_Id: String := Integer'Image(Source_Key);
      begin
        Source_Id(1) := 'A';
        if This_Transition.Source(I).Kind = Simple then
          Put ( Source_Id & " -> " & Pseudo_Id & " [" );
          Put_line ("fontsize=12];");  --,tailport=n];");
        else
          Put ( Source_Id & "name" & " -> " & Pseudo_Id &  " [" );
          Put_line ("fontsize=12," & --tailport=n," &
                    "ltail=cluster" & Source_Id & "];" );
        end if;
      end;
    end loop;
    --
    -- dump the outgoing fragment
    --
    declare
      Target_Key: Positive := This_Transition.Target(1).Num_Key;
      Target_Id: String := Integer'Image(Target_Key);
    begin
      Target_Id(1) := 'A';
      if This_Transition.Target(1).Kind = Simple then
        Put ( Pseudo_Id & " -> " & Target_Id & " [" );
        Put_line ( "fontsize=12," & -- "headport=s,
                   "label=""" & TransID & """];" );
      else
        Put ( Pseudo_Id & " -> " & Target_Id & "name [" );
        Put_line ( "fontsize=12," & --"headport=s,
                    "label=""" & TransID &
                    """,lhead=cluster"  & Target_Id & "];" );
      end if;
    end;
  end Dump_Join;

  function Is_Cycling (This_Transition: Transition_Ref) return Boolean is
    SourceLength: Positive;
    TargetLength: Positive;
    MinLength: Positive;
  begin
    if This_Transition.Source.all'Length >1 or else
       This_Transition.Target.all'Length >1 then
     return False;
    end if;
    --
    if This_Transition.Source(1).Kind = Simple and then
       This_Transition.Target(1).Kind = Simple then
      return False;
    end if;
    --
    SourceLength := This_Transition.Source(1).FullName.all'Length;
    TargetLength := This_Transition.Target(1).FullName.all'Length;
    MinLength := SourceLength;
    if TargetLength < SourceLength then
         MinLength  :=  TargetLength;
    end if;
    return This_Transition.Source(1).FullName(1..MinLength) =
            This_Transition.Target(1).FullName(1..MinLength);
  end Is_Cycling;

  procedure Dump_Cycle ( This_Transition: Transition_Ref) is
    --
    TransID : String := Transition_label(This_Transition);
    Pseudo_Id: String := "pseudo_t" & TransID(2..TransID'Length);
  begin
    -- dump outer textnode
    --
    Put_Line (Pseudo_Id &
           " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
           " label = """ & TransID & """ ];");
    --
    -- dump incoming fragments
    --
      declare
        Source_Key: Positive := This_Transition.Source(1).Num_Key;
        Source_Id: String := Integer'Image(Source_Key);
      begin
        Source_Id(1) := 'A';
        if This_Transition.Source(1).Kind = Simple then
          Put ( Source_Id & " -> " & Pseudo_Id & " [" );
          Put_line ("fontsize=12,headport=n,arrowhead=none];");
        else
          Put ( Source_Id & "name" & " -> " & Pseudo_Id &  " [" );
          Put_line ("fontsize=12,headport=n,arrowhead=none," &
                    "ltail=cluster" & Source_Id & "];" );
        end if;
      end;
    --
    -- dump the outgoing fragment
    --
    declare
      Target_Key: Positive := This_Transition.Target(1).Num_Key;
      Target_Id: String := Integer'Image(Target_Key);
    begin
      Target_Id(1) := 'A';
      if This_Transition.Target(1).Kind = Simple then
        Put ( Pseudo_Id & " -> " & Target_Id & " [" );
        Put_line ( "fontsize=12," & -- tailport=s," &
                   "label=""" & TransID & """];" );
      else
        Put ( Pseudo_Id & " -> " & Target_Id & "name [" );
        Put_line ( "fontsize=12," & -- tailport=s," &
                    "label=""" & TransID &
                    """,lhead=cluster"  & Target_Id & "];" );
      end if;
    end;
  end Dump_Cycle;

  procedure Dump_Transition ( This_Transition: Transition_Ref) is
    --
    TransID : String := Transition_DotImage(This_Transition);
    ---
    Source_Key: Positive := This_Transition.Source(1).Num_Key;
    Target_Key: Positive := This_Transition.Target(1).Num_Key;
    TID: String := Integer'Image(This_Transition.Num_Key);
  begin
    TID(1) := 'x';
    --
    if This_Transition.Source.all'Length >1 and then
         This_Transition.Target.all'Length =1 then
      Dump_Join (This_Transition);
      return;
    elsif This_Transition.Target.all'Length >1 and then
      This_Transition.Source.all'Length > 1 then
      Dump_JoinedFork (This_Transition);
      return;
    elsif This_Transition.Target.all'Length >1 and then
      This_Transition.Source.all'Length =1  then
      Dump_Fork (This_Transition);
      return;
    end if;
    --
    --
    if Is_Cycling(This_Transition) then
     --
      --  IF SOURCE IN NESTED IN TARGET , OR TAGET NESTED IN SOURCE,
      --   OR TARGET = SOURCE = Composite , WE MUST MAKE USE OF THE IMPLICIT
      --    PSEUDO NODE  (OwnerId_TransitionId)
      Dump_Cycle (This_Transition);
      return;
    end if;
    --
    declare
      Source_Id: String := Integer'Image(Source_Key);
      Target_Id: String := Integer'Image(Target_Key);
    begin
    Source_Id(1) := 'A';
    Target_Id(1) := 'A';
    --
    --  IF SOURCE AND TARGET ARE NOT NESTED
    --
    if This_Transition.Source(1).Kind = Simple and then
       This_Transition.Target(1).Kind = Simple then
         Put_Line (Source_Id & TID &
           " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
           " label = """ & TransID & """ ];");
         Put_Line (Source_Id & " -> " & Source_Id & TID &
                   " [arrowhead=none];");
         Put_Line (Source_Id & TID & " -> " & Target_Id & ";");   
--       Put ( Source_Id & " -> " & Target_Id & " [" );
--       Put_line ( "fontsize=12,label=""" & TransID & """];" );
    --
    elsif  This_Transition.Source(1).Kind = Simple and then
       This_Transition.Target(1).Kind /= Simple then
         Put_Line (Source_Id & TID &
           " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
           " label = """ & TransID & """ ];");
         Put_Line (Source_Id & " -> " & Source_Id & TID & " [arrowhead=none];");
         Put_Line (Source_Id & TID & " -> " & 
                      Target_Id & "name [lhead=cluster" & Target_Id & "];");
--       Put ( Source_Id & " -> " & Target_Id & "name [" );
--       Put_line ("fontsize=12,label=""" &
--               TransID &""",lhead=cluster" & Target_Id & "];" );
    --
    elsif This_Transition.Source(1).Kind /= Simple and then
       This_Transition.Target(1).Kind = Simple then
         Put_Line (Source_Id & TID &
           " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
           " label = """ & TransID & """ ];");
         Put_Line (Source_Id & "name -> " & 
                     Source_Id & TID & " [arrowhead=none,ltail=cluster" & Source_Id & "];");
         Put_Line (Source_Id & TID & " -> " & Target_Id & ";");
--       Put ( Source_Id & "name -> " & Target_Id &  " [" );
--       Put_line ("fontsize=12,label=""" &
--                  TransID &""",ltail=cluster" & Source_Id &  "];" );
    --
    else
         Put_Line (Source_Id & TID &
           " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
           " label = """ & TransID & """ ];");
         Put_Line (Source_Id & "name -> " & 
                     Source_Id & TID & " [arrowhead=none,ltail=cluster" & Source_Id & "];");
         Put_Line (Source_Id & TID & " -> " & 
                      Target_Id & "name [lhead=cluster" & Target_Id & "];");
--       Put ( Source_Id & "name" & " -> " & Target_Id & "name " & " [" );
--       Put_Line ("fontsize=12,label=""" & TransID & """" &
--       ",lhead=cluster" & Target_Id & ",ltail=cluster" & Source_Id & "];" );
    --
    end if;
    --
    end;
  end Dump_Transition;

  procedure Dump_Explicit_Transitions(The_Class:Natural) is 
    --
    These_Transitions: Transitions_Table_Ref;
  begin
    These_Transitions := All_Classes(The_Class).ChartTransitions;
    for I in These_Transitions.all'Range loop
       Dump_Transition (These_Transitions(I));
    end loop;
  end Dump_Explicit_Transitions;

  procedure Dump_Local_Transitions (This_State: State_Ref) is
    StateId: String := Integer'Image(This_State.Num_Key);
    These_Transitions: Transitions_Table_Ref;
  begin
    StateId(1) := 'A';
    --
    --- default initial transition
    if This_State.Kind /= Parallel then
      declare
        Initial_Id: String := Integer'Image(This_State.Substates(1).Num_Key);
        firstsubname: String := This_State.Substates(1).FullName.all;
      begin
        if firstsubname /= "initial"  and then
            (firstsubname'length < 8 or else
             firstsubname(firstsubname'Last-7..firstsubname'Last) /= ".initial") then
          Initial_Id(1) := 'A';
          if This_State.Substates(1).Kind = Simple then
            Put_line ( StateId & "initial -> " & Initial_Id & " [weight=3];" );
          else
            Put_Line ( StateId & "initial  -> " & Initial_Id & "name " &
                           " [lhead=cluster" & Initial_Id & "];" );
          end if;
        end if;
      end;
    end if;
    --
    These_Transitions := This_State.LocalTransitions;
    if These_Transitions /= null then
      for I in These_Transitions.all'Range loop
         Dump_Transition (These_Transitions(I));
      end loop;
    end if;
  end Dump_Local_Transitions;


  procedure Dump_Implicit_Transitions (This_State: State_Ref) is
    StateId: String := Integer'Image(This_State.Num_Key);
  begin
    StateId(1) := 'A';
    --
    -- if state is sequential-composite dump implicit initial
    --   followed by all local transitions, and repeat for all substates
    -- if state is parellel just repeat for substates
    -- if if state is simple and final dump implicit final
    --   for its parent.
    --
    if This_State.Kind = Composite then
      --
      -- Dump implicit INITIAL transition
      --
         
      declare
        Initial_Id: String := Integer'Image(This_State.Substates(1).Num_Key);
        firstsubname: String := This_State.Substates(1).FullName.all;
      begin
        if firstsubname /= "initial"  and then
            (firstsubname'length < 8 or else
             firstsubname(firstsubname'Last-7..firstsubname'Last) /= ".initial") then
          Initial_Id(1) := 'A';
          if This_State.Substates(1).Kind = Simple then
            Put_line ( StateId & "initial -> " & Initial_Id & " [weight=3];" );
          else
            Put_Line ( StateId & "initial  -> " & Initial_Id & "name " &
                           " [lhead=cluster" & Initial_Id & "];" );
          end if;
        end if;
      end;
      --
      for I in This_State.Substates.all'Range loop
         Dump_Implicit_Transitions(This_State.Substates(I));
      end loop;
      --
    end if;
    --
    if This_State.Kind = Parallel then
      for I in This_State.Substates.all'Range loop
         Dump_Implicit_Transitions(This_State.Substates(I));
      end loop;
    end if;
  end Dump_Implicit_Transitions;

procedure Dump_Class_Statechart(The_Class: Natural) is
  Dot_File: File_Type;
  Dot_Filename: string := "Class_" & All_Classes(The_Class).Name.all & ".dot";
  JPG_Filename: string := "Class_" & All_Classes(The_Class).Name.all & ".jpg";
begin
  --
  if All_Classes(The_Class).ChartStates.all'Length = 0 then
   return;
  end if;
  --
  Create (Dot_File, Out_File, Dot_Filename);
  Set_Output(Dot_File);
  --
  Put_line ("digraph G {");
  Put_line ("compound=true;");
  Put_Line ("    node [shape=Mrecord]");
  --
  if All_Classes(The_Class).ChartStates(1).Kind = Composite  then
    Dump_Seq_Composite (All_Classes(The_Class).ChartStates(1));
  else
    Dump_Parallel(All_Classes(The_Class).ChartStates(1));
--    Dump_Implicit_Transitions (All_Classes(The_Class).ChartStates(1));
--    Dump_Explicit_Transitions (The_Class);
  end if;
  --
  Put_line ("}");
  Close (Dot_File);
  Set_Output (Standard_Output);
--  SPAWN_DOT_COMMAND(Dot_Filename, JPG_Filename);
  exception
    when others =>
      if Is_Open (Dot_File) then
        Close(Dot_File);
      end if;
      Set_Output (Standard_Output);
      raise;
end Dump_Class_Statechart; 

procedure Dump_Signals(The_Class: Natural) is
  First: Boolean := True;
begin
  Put_Line ("<B>Signals</b>: <br>");
  if All_Classes(The_Class).Top_State_Num /= 0 then
     -- All Class_Event must have a well defined Kind
    for I in All_Classes(The_Class).ChartEvents.all'Range loop
      if All_Classes(The_Class).ChartEvents(I).Kind = Signal then
        Put("&nbsp;&nbsp;&nbsp;" & All_Classes(The_Class).ChartEvents(I).Name.all);
        if All_Classes(The_Class).ChartEvents(I).Params.all'Length /= 0 then
          First := True;
          Put("(");
          for J in All_Classes(The_Class).ChartEvents(I).Params.all'Range loop
            if First then
               First := False;
            else
               Put(",");
            end if;
            Put (All_Classes(The_Class).ChartEvents(I).Params(J).Name.all);
            if  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Number then
               Put (":int");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Numvector then
               Put (":int[]");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Bool then
               Put (":bool");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Boolvector then
               Put (":bool[]");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Object then
               if All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo = 0 then
                 Put (":obj");
               else
                 Put (":" & All_Classes(
                        All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo).name.all);
               end if;
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = objvector then
               if All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo = 0 then
                 Put (":obj[]");
               else
                 Put (":" & 
                  All_Classes(
                   All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo).name.all &"[]");
              end if;
            end if;  -- Kind = ...
          end loop; -- Params.all'Range
          Put(")");
        end if;  -- Params.all'Length /= 0
        Put_Line(";<br>");
      end if;  -- Kind = Signal
    end loop;  -- ChartEvents.all'Range
  else
     -- All Class_Event are implicitly signals  (Signals or Undefined)
    for I in All_Classes(The_Class).ChartEvents.all'Range loop
      if All_Classes(The_Class).ChartEvents(I).Kind = Signal or else
          All_Classes(The_Class).ChartEvents(I).Kind = Undefined then
        Put("   " & All_Classes(The_Class).ChartEvents(I).Name.all);
        if All_Classes(The_Class).ChartEvents(I).Params.all'Length /= 0 then
          First := True;
          Put("(");
          for J in All_Classes(The_Class).ChartEvents(I).Params.all'Range loop
            if First then
               First := False;
            else
               Put(",");
            end if;
            Put (All_Classes(The_Class).ChartEvents(I).Params(J).Name.all);
            if  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Number then
               Put (":int");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Numvector then
               Put (":int[]");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Bool then
               Put (":bool");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Boolvector then
               Put (":bool[]");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Object then
               if All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo = 0 then
                 Put (":obj");
               else
                 Put (":" & All_Classes(
                        All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo).name.all);
               end if;
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = objvector then
               if All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo = 0 then
                 Put (":obj[]");
               else
                 Put (":" &
                  All_Classes(
                   All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo).name.all &"[]");
              end if;
            end if;  -- Kind = ...
          end loop; -- Params.all'Range
          Put(")");
        end if;  -- Params.all'Length /= 0
        Put_Line (";<br>");
      end if;  -- Kind = Signal
    end loop;  -- ChartEvents.all'Range
  end if;  -- Top_State_Num /= 0
end Dump_Signals;

procedure Dump_Operations(The_Class: Natural) is
  First: Boolean;
begin
  if All_Classes(The_Class).Top_State_Num /= 0 then
    Put_Line ("<B>Operations</B>: <br>");
    -- All Class_Event must have a well defined Kind
    for I in All_Classes(The_Class).ChartEvents.all'Range loop
      if All_Classes(The_Class).ChartEvents(I).Kind = Operation then
        Put("&nbsp;&nbsp;&nbsp;" & All_Classes(The_Class).ChartEvents(I).Name.all);
        if All_Classes(The_Class).ChartEvents(I).Params.all'Length > 1 then
          --  Operation calls always have at lest the "_caller" parameter, not to be displayed
          First := True;
          Put("(");
          for J in 2 .. All_Classes(The_Class).ChartEvents(I).Params.all'Length loop
            if First then
               First := False;
            else
               Put(",");
            end if;
            Put (All_Classes(The_Class).ChartEvents(I).Params(J).Name.all);
            if  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Number then
               Put (":int");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Numvector then
               Put (":int[]");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Bool then
               Put (":bool");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Boolvector then
               Put (":bool[]");
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = Object then
               if All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo = 0 then
                 Put (":obj");
               else
                 Put (":" & All_Classes(
                        All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo).name.all);
               end if;
            elsif  All_Classes(The_Class).ChartEvents(I).Params(J).Kind = objvector then
               if All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo = 0 then
                 Put (":obj[]");
               else
                 Put (":" &
                  All_Classes(
                   All_Classes(The_Class).ChartEvents(I).Params(J).TypeInfo).name.all &"[]");
              end if;
            end if;  -- Kind = ...
          end loop; -- Params.all'Range
          Put(")");
        end if;  -- Params.all'Length  > 1
        --
        -- dump return type
        --
        if  All_Classes(The_Class).ChartEvents(I).Return_Type = Number then
           Put (":int");
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Numvector then
           Put (":int[]");
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Bool then
           Put (":bool");
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Boolvector then
           Put (":bool[]");
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Object then
            Put (":obj");
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = objvector then
            Put (":obj[]");
        end if;  -- Kind = ...
        Put_Line(";<br>");
      elsif All_Classes(The_Class).ChartEvents(I).Kind = Undefined then
        Put_Line (Current_Error,
          " Error: undefined event " & All_Classes(The_Class).ChartEvents(I).name.all);
        raise UML_error;
      end if;  -- Kind = Operations
    end loop;  -- ChartEvents.all'Range
    --
  else   --  Top_State_Num /= 0
    for I in All_Classes(The_Class).ChartEvents.all'Range loop
      if All_Classes(The_Class).ChartEvents(I).Kind = Operation then
         Put_Line (Current_Error,
           " Error: undefined event " & All_Classes(The_Class).ChartEvents(I).name.all);
         raise UML_error;
       end if;  -- Kind = Operations
    end loop;
  end if;
end Dump_Operations;

procedure Dump_Vars(The_Class: Natural) is
begin
  if All_Classes(The_Class).Top_State_Num /= 0 then
    Put_Line ("<B>Vars</B>: <br>");
    -- All Class_Event must have a well defined Kind
    for I in All_Classes(The_Class).ChartVars.all'Range loop
      Put ("&nbsp;&nbsp;&nbsp;" & All_Classes(The_Class).ChartVars(I).name.all); 
      if  All_Classes(The_Class).ChartVars(I).Kind  = Number then
        Put (":int");
      elsif  All_Classes(The_Class).ChartVars(I).Kind   = Numvector then
        Put (":int[]");
      elsif  All_Classes(The_Class).ChartVars(I).Kind   = Bool then
        Put (":bool");
      elsif  All_Classes(The_Class).ChartVars(I).Kind   = Boolvector then
        Put (":bool[]");
      elsif  All_Classes(The_Class).ChartVars(I).Kind   = Object then
       if All_Classes(The_Class).ChartVars(I).TypeInfo   = 0 then
         Put (":obj");
        else
          Put (":" & All_Classes(
               All_Classes(The_Class).ChartVars(I).TypeInfo).name.all);
        end if;
      elsif  All_Classes(The_Class).ChartVars(I).Kind= objvector then
        if All_Classes(The_Class).ChartVars(I).TypeInfo = 0 then
          Put (":obj[]");
        else
          Put (":" &
           All_Classes(
           All_Classes(The_Class).ChartVars(I).TypeInfo).name.all &"[]");
        end if;
      end if;  -- Kind = ...
      --
      -- initial value
      --
      if  All_Classes(The_Class).ChartVars(I).Initial /= null then
        Put (":=" & All_Classes(The_Class).ChartVars(I).Initial.Image.all);
      end if; 
      Put_Line(";<br>");
    end loop;   -- ChartVars.all'Range
    -- 
  else  -- Top_State_Num = 0 
    if All_Classes(The_Class).ChartVars.all'Length >0 then
      Put_Line (Current_Error, "Error: Vars not alllowed for non active classes");
      raise UML_error;
    end if;
  end if; -- Top_State_Num /= 0 
end Dump_Vars;

 function HTML_TansitionFormat (Source:String) return String is
   Result:String(1..30000);
   OUTC: Natural := 1;
 begin
   for I in Source'Range loop
     if Source(I)='<' then
        Result(OUTC..OUTC+3) := "&lt;";
        OUTC := OUTC+4;
     elsif Source(I)='>' then
        Result(OUTC..OUTC+3) := "&gt;";
        OUTC := OUTC+4;
     elsif Source(I)='/' and then I+1 < Source'Last and then Source(I+1)='='then
        Result(OUTC..OUTC+53) := " /<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
        OUTC := OUTC+54;
     elsif Source(I)=';' then
        Result(OUTC..OUTC+52) := ";<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
        OUTC := OUTC+53;
     elsif Source(I)='{' then
        Result(OUTC..OUTC+52) := "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{";
        OUTC := OUTC+53;
     else
        Result(OUTC) := Source(I);
        OUTC := OUTC+1;
     end if;
   end loop;
   return Result (1..OUTC-1);
 end HTML_TansitionFormat;

procedure Dump_Transition_Labels(The_Class:Natural) is
begin
  Put_Line ("<b>Transitions:</b>");
  for I in All_Classes(The_Class).ChartTransitions.all'Range loop
    Put_Line ("<br>");
    Put(HTML_TansitionFormat(Transition_Image(All_Classes(The_Class).ChartTransitions(I))));
  end loop;
end Dump_Transition_Labels;

  function HTML_Class_Tooltip (HTML_File:File_Type; The_Class: Positive) return String is
    res: String_Ref; 
    tmp: String_Ref;
    First: Boolean := True;
    items: Natural;
  begin
    res := new String'("<B>Signals</b>: <br>");
    -- All Class_Event must have a well defined Kind
    items := 0;
    for I in All_Classes(The_Class).ChartEvents.all'Range loop
      if All_Classes(The_Class).ChartEvents(I).Kind = Signal then
        items := items+1;
        tmp := res; 
        res := new String'(res.all  & All_Classes(The_Class).ChartEvents(I).Name.all);
        Free(tmp);
        if All_Classes(The_Class).ChartEvents(I).Params.all'Length /= 0 then
          First := True;
          tmp := res;    
          res := new String'(res.all  & "(");
          Free(tmp); 
          for J in All_Classes(The_Class).ChartEvents(I).Params.all'Range loop
            if First then
               First := False;
            else
              tmp := res;       
              res := new String'(res.all  & ",");
              Free(tmp);
            end if;
            tmp := res;       
            res := new String'(res.all  & All_Classes(The_Class).ChartEvents(I).Params(J).Name.all);
            Free(tmp);
          end loop; -- Params.all'Range
          tmp := res;
          res := new String'(res.all & ")");
          Free(tmp);
        end if;  -- Params.all'Length /= 0
        tmp := res;
        res := new String'(res.all & ";<br>");
        Free(tmp);
      end if;  -- Kind = Signal
    end loop;  -- ChartEvents.all'Range
    if items = 0 then
      tmp := res;
      res := new String'(res.all & "&nbsp;&nbsp;<i>none</i><br>");
      Free(tmp);
    end if;
    --
    tmp := res;
    res := new String'(res.all  & "<br><B>Operations</B>: <br>");
    Free(tmp);
     --  All Class_Event must have a well defined Kind
    items := 0;
    for I in All_Classes(The_Class).ChartEvents.all'Range loop
      if All_Classes(The_Class).ChartEvents(I).Kind = Operation then
        items := items+1;
        tmp := res;
        res := new String'(res.all  & All_Classes(The_Class).ChartEvents(I).Name.all);
        Free(tmp);
        if All_Classes(The_Class).ChartEvents(I).Params.all'Length > 1 then
          First := True;
          tmp := res;
          res := new String'(res.all  & "(");
          Free(tmp);
          for J in 2 .. All_Classes(The_Class).ChartEvents(I).Params.all'Last loop
            if First then
               First := False;
            else
              tmp := res;
              res := new String'(res.all  & ",");
              Free(tmp);
            end if;
            tmp := res;
            res := new String'(res.all  & All_Classes(The_Class).ChartEvents(I).Params(J).Name.all);
            Free(tmp);
          end loop; -- Params.all'Range
          tmp := res;
          res := new String'(res.all  & ")");
          Free(tmp);
        end if;  -- Params.all'Length /= 0
        --
        if  All_Classes(The_Class).ChartEvents(I).Return_Type = Number then
          tmp := res;
          res := new String'(res.all  & ":int");
          Free(tmp);
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Numvector then
          tmp := res;
          res := new String'(res.all  & ":int[]");
          Free(tmp);
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Bool then
          tmp := res;
          res := new String'(res.all  & ":bool");
          Free(tmp);
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Boolvector then
          tmp := res;
          res := new String'(res.all  & ":bool[]");
          Free(tmp);
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = Object then
          tmp := res;
          res := new String'(res.all  & ":obj");
          Free(tmp);
        elsif  All_Classes(The_Class).ChartEvents(I).Return_Type = objvector then
          tmp := res;
          res := new String'(res.all  & ":obj[]");
          Free(tmp);
        end if;  -- Kind = ...
        --
        tmp := res;
        res := new String'(res.all  & ";<br>");
        Free(tmp);
        --
      elsif All_Classes(The_Class).ChartEvents(I).Kind = Undefined then
        Put_Line (Current_Error,
          " Error: undefined event " & All_Classes(The_Class).ChartEvents(I).name.all);
        raise UML_error;
      end if;  -- Kind = Operations
    end loop;  -- ChartEvents.all'Range
    if items = 0 then
      tmp := res;
      res := new String'(res.all & "&nbsp;&nbsp;<i>none</i><br>");
      Free(tmp);
    end if;

    declare
        Result: String := res.all;
    begin
       Free(res);
       return Result;
    end;
  end HTML_Class_Tooltip;

------------------------------------------------------------------------
--  called by the "htmlmodel" umc command, 
--  or when ckicking on the model icon in the webumc interface.
--  Display an HTML description of the model.
------------------------------------------------------------------------
procedure Dump_HTML_Model(HTML_File:File_Type) is
    HTML_CLass: File_Type;
begin
----------------------------
-- The  HTML_File ("Model.html") is actually left empty, since not used by he HTML interface.
-- We generate, instead, a "Class_ClassName.html" file  for each system class.
-- This class DOES not conatain an embedded image of its statechart.
-- It contains instead a button which allows to retrieve it.
----------
  for The_Class in  TokensChart.ChartParent+1 .. All_Classes.all'Length loop
    Create (HTML_Class, Out_File, "Class_" & All_Classes(The_Class).name.all & ".html");
    Set_Output(HTML_Class);
    Put_Line("<html> <head> </head><body>");
    --
    --  We want to print also the details of the inactive interfaces too ..
    --  Hence not just the active classes ...
    New_Line;
    Put_line("<span style=""background-color:orange"">"); 
--    Put_line ("<a href='javascript:top.ViewStatechart(""" &
--          All_Classes(The_Class).Name.all &
--          """);'>View Class Statechart</a>");
    Put_Line("</span><br>");
    Put_Line ("<B>Class</B> " & All_Classes(The_Class).Name.all & " <B>is</B> <br>");
    Dump_Signals(The_Class);
    Dump_Operations(The_Class);
    Dump_Vars(The_Class);
    Dump_Transition_Labels(The_Class);
    Put_Line("<br><B>end</B> " & All_Classes(The_Class).name.all & "<br><br>");
    --
    --
    Put_Line ("</body></html>");
    Set_Output (Standard_Output);
    Close(HTML_Class);
  end loop;
---------
exception
  when others =>
    Set_Output (Standard_Output);
    raise;
    --
    --  DUMP OBJECTS !!!!
    ---
end Dump_HTML_Model;
-----------------------------------------------------------------------
--
--  STATISTICS
--
-----------------------------------------------------------------------
--

  procedure Print_Stats is
  begin
    Put_Line ("System_Configurations: " & Int64'Image(SRC_Count));
  end Print_Stats;

end UML_Explore;
