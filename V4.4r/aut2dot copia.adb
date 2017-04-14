with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line;
with Flags; use Flags;
with DotLib; use DotLib;
procedure Aut2dot is
-- --------------------------------------------------------------------------
--  declared inside DotLib
--  Encode_State_Labels: Boolean := False;     -- option "-l"
--  Beautify: Boolean := False;                 -- option "-b"
-- ---------------------------------------------------------------------------
--   Perche ci vogliono src e tgt state lables quando si eminimizza
-- des (0,6,5)
-- (0,"[{x(0)}]{a}",1)
-- (0,"[{x(0)}]{b}",2)
-- (1,"[{x(1)}]final",4)
-- (2,"tau",3)          -- non sono in grado di ricostruire la state-label dello stato 2!
-- (2,"[{x(2)}]#loop",4)
-- (3,"[{x(2)}]#final",4)
------------------------------------------------
---   struttura  filae model.aut
--  des (initialnode,edgescount,nodescount)      -- follow edgescount lines
--  (srcnode,edgelabel,tgtnode)
--  (0,"[{src(1),src(2)}][{}]{edge(1)}",1)
--  (1,"{edge(2)}",2)         -- src state labels empty, tgt state labels empty or to be defined
--  (2,"[{src(3)}]#final",3)  -- edge labels not omitted if empty 
--  (1,tau,1)                 -- tau propagates src state labels to tgt state labels with empty edge labels
----------------------------------------------

  Model_FileName: access String;
  Argc: Natural;
  SetOutput: Boolean := False;                --  option "-o"
  OutputFileName: access String;


  procedure Read_and_Translate is
    InputFile: File_Type;
    InputLine: String(1..10000);
    InputSize: Natural := 0;
    InputPos: Natural :=0;
    CurrentLine: Natural :=0;
    StateNum: Natural :=0;
    SourceFrom: Natural :=0;
    SourceTo: Natural :=0;
    TargetFrom: Natural :=0;
    TargetTo: Natural :=0;
    TargetLabelsFrom: Natural :=1;
    TargetLabelsTo: Natural :=0;
    SourceLabelsFrom: Natural :=1;
    SourceLabelsTo: Natural :=0;
    ActionLabelsFrom: Natural :=0;
    ActionLabelsTo: Natural :=0;
    PreviousSource: Natural :=0;
    SourceState: Natural :=0;
    TargetState: Natural :=0;
  begin
    Open (InputFile, In_File, Model_FileName.all);
    --  Skip the first line  "des (0,..,..)"
    --
    Get_Line(InputFile,InputLine,InputSize);
    if InputSize <7 or else InputLine(1..7) /= "des (0," then
       Put_Line(Current_Error, "The input file """ & Model_FileName.all & 
          """ does not look like a CADP/Aldebaran "".aut"" file.");
    end if;
    --
    Print_Dot_Header("W0");
    --
    CurrentLine := 1;
    while not End_Of_File(InputFile) loop
      --
      -- parse all the other transition lines   (n,"...",m)
      --  (n,"tau",m)
      --  (n,"{act,act}",m)
      --  (n,"SpecialLabels",m)
      --  (n,"[sourcelabels][targetlabels]{act,act}",m)
      --  (n,"[sourcelabels]SpecialLabels",m)
      --       where SpecialLabels =  #final, #loop, #truncated, #statelabel, 
      --
      Get_Line(InputFile,InputLine,InputSize);
      CurrentLine := CurrentLine+1;
      InputPos := 1;
      --
      -- Skip initial '('
      --
      if InputLine(InputPos) /= '(' then
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      InputPos := 2;
      SourceFrom :=2;
      --
      -- Parse the digits of the state number n
      --
      if InputLine(InputPos) not in '0'..'9' then
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) & 
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      InputPos := InputPos +1;
      while InputLine(InputPos) in '0'..'9' loop
        InputPos := InputPos+1;
      end loop;
      SourceTo := InputPos - 1;
--      PreviousSource := SourceState;
      SourceState:= Integer'Value(InputLine(SourceFrom..SourceTo))+1;
      --
      -- Skip the ',' after the source state number n
      --
      if InputLine(InputPos) /= ',' then
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) & 
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;      
      InputPos := InputPos +1;
      -- 
      -- Skip initial  '"' of transition label
      --
      if InputLine(InputPos) /= '"' then
         Put_Line(Current_Error,
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      SourceLabelsFrom := 1;
      SourceLabelsTo := 0;
      InputPos := InputPos +1;
      if InputLine(InputPos) = '[' then
        --
        -- Parse the source state labels inside the first [..]
        --
        InputPos := InputPos+1;
        SourceLabelsFrom := InputPos;
        while InputLine(InputPos) /= ']' or else 
            (InputLine(InputPos+1) /= '[' and 
             InputLine(InputPos+1) /= '{' and 
             InputLine(InputPos+1) /= 't' and 
             InputLine(InputPos+1) /= '#') loop
          InputPos := InputPos+1;
        end loop;
        SourceLabelsTo := InputPos-1;
        InputPos := InputPos+1;
      end if;
      --
      TargetLabelsFrom := 1;
      TargetLabelsTo :=0;
      if InputLine(InputPos) = '[' then
        --
        -- Parse the target state labels inside the second [..]
        --
        InputPos := InputPos+1;
        TargetLabelsFrom := InputPos;
        while InputLine(InputPos) /= ']' or else             
            (InputLine(InputPos+1) /= '[' and
             InputLine(InputPos+1) /= '{' and
             InputLine(InputPos+1) /= 't' and
             InputLine(InputPos+1) /= '#') loop
          InputPos := InputPos+1;
        end loop;
        TargetLabelsTo := InputPos-1;
        InputPos := InputPos+1;
      end if;
      --
      ActionLabelsFrom := InputPos;
      InputPos := InputPos+1;
      --
      -- Parse the action labels (#special or {} or tau) until the final '"'
      --
      while InputLine(InputPos) /= '"' or 
           InputLine(InputPos-1) = '\' loop
        InputPos := InputPos+1;
      end loop;
      ActionLabelsTo := InputPos-1;
      InputPos := InputPos+1;
      --
      -- Skip the last ','
      --
      if InputLine(InputPos) /= ',' then
         Put_Line(Current_Error,
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      InputPos := InputPos+1;
      --
      -- Parse the digits of the target state number
      --
      TargetFrom := InputPos;
      while InputLine(InputPos) in '0'..'9' loop
        InputPos := InputPos+1;
      end loop;
      TargetTo := InputPos-1;
      TargetState := Integer'Value(InputLine(TargetFrom..TargetTo))+1;
      if InputLine(InputPos) /= ')' then
         Put_Line(Current_Error,
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      --
      --  Start the generation of the dot Edges
      --
      declare
        Source: String := "W" & InputLine(SourceFrom..SourceTo);
        Target: String := "W" & InputLine(TargetFrom..TargetTo);
        sourcelabels: String := InputLine(SourceLabelsFrom..SourceLabelsTo);
        targetlabels: String := InputLine(TargetLabelsFrom..TargetLabelsTo);
        actionlabels: String := InputLine(ActionLabelsFrom..ActionLabelsTo);
      begin
        if PreviousSource /= SourceState and then 
            sourcelabels /= "" and then
            actionlabels /= "#truncated" and then
            actionlabels /= "#final"  then
           -- se ci sono state labels, una sola volta per nodo,  si  genera
           -- la descrizione del nodo (che incapsula le state labels o dentro il nodo
           --    (se -b) o in una transizione #statelabels (se not -t)
           Print_Dot_Node(Source,sourcelabels);
           PreviousSource := SourceState;
        end if;
        --
        if actionlabels = "#truncated" then
          Print_Dot_Truncated_Node(Source, sourcelabels);
        --
        elsif actionlabels = "#final" then
          Print_Dot_Final(Source, sourcelabels);
        --
        elsif actionlabels = "#loop" then
          Print_Dot_Loop(Source,sourcelabels);
        --
        elsif actionlabels /= "#statelabels" then 
          Print_Dot_Edge(Source, sourcelabels, targetlabels, actionlabels, Target);
        end if;
      end;
    end loop;
    --
    Print_Dot_Footer;
    --
    Close (InputFile);
    exception
       when others => 
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) & ": Unexpected error");
  end Read_and_Translate;

  -- Quando chiamo  Aut2Dot  NON AVESSI L'OPZIONE -t non potrei sapere se il grafo avra'
  --  o meno edges TAU.  Cio' significa che non so se dovro generare o meno transizioni
  --  #statelabels.  
begin
  Argc := 0;
  Beautify := True;
  Encode_State_Labels := False;
  Beautify := False;
  -----------------------------------------------------
  --  aut2dot -b   trasform un encoding di L2TS  in un L2TS decodificando la labels
  --  aut2dot -b   trasform un encoding di L2TS  in un L2TS decodificando la labels
  --  aut2dot -l  trasforma un aut in un dot lasciando l'encoding sugli edges cosi' come e'
  --              (ok pr effettuare ulteriori minimizzazioni con ltsconvert)
  --  aut2dot -b -l  trasforma un aut in un dot lasciando l'encoding sugli edges cosi' come e'
  --                ma aggiustando #loop, #truncated, #final
  -----------------------------------------------------
  loop
    Argc := Argc +1;
    if Argc > Ada.Command_Line.Argument_Count  then
       exit;
    end if;
    declare
      Input_Arg : String := Ada.Command_Line.Argument (Argc);
    begin
      if Input_Arg = "-l" then
         Encode_State_Labels := True;
      elsif Input_Arg = "-b" then
         Beautify := True;
      elsif Input_Arg = "-b" then
         Beautify := True;
      elsif Input_Arg = "-l" then
         Encode_State_Labels := True;
      elsif Input_Arg = "-t" then
         Remove_Tau_Transitions := True;
      elsif Input_Arg = "-o" then
         SetOutput := True;
      elsif SetOutput then
         OutputFileName := new String'(Input_Arg);
         SetOutput := False; 
      elsif Input_Arg(1) /='-' then
         Model_FileName := new String'(Input_Arg);
      else
        null; -- ignore meaningless parameters
      end if;
    end;
  end loop;   -- cycle on arguments 

  if Ada.Command_Line.Argument_Count = 0 or else
    Model_FileName = null then
    Put_line(Current_Error, "aut2dot: ERROR incorrect parameters ...");
    Put_line(Current_Error, " usage: aut2dot [options] [inputmodelfile.aut] ");
    Put_line(Current_Error, " options: "); 
    Put_line(Current_Error, "   -o outputfilename ");
    Put_line(Current_Error, "   -l (do not decode state labels inside edge labels");
    Put_line(Current_Error, "   -b (render the graph fit for visualization)");
    Put_line(Current_Error, "   -t (assume the graph does not contain ""tau"" labels)");
    return;
  end if;
    --
    if OutputFileName /= null and then OutputFileName.all'Length >0 then
      declare
        DOTOUT: File_Type;
      begin
        Create(DOTOUT,Out_File,OutputFileName.all);
        Set_Output(DOTOUT);
        Read_and_Translate;
        Set_Output(Standard_Output);
        Close(DOTOUT);
      end;
    else
      Read_and_Translate;
    end if;
    --
  end Aut2dot;
