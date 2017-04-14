with Flags; use Flags;
with Ada.exceptions; use Ada.exceptions;
with Ada.Text_io; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with MC_Server;  use MC_Server;
with System;
procedure UMC is
 pragma PRIORITY (System.Default_priority);
  --
  Input_Line: String(1..500);
  Input_Count: Natural;
--  HTML_Mode : Boolean := False;
  Model_File: access String;
  Formula_File: access String;
  MC_Status: CommandStatus := Done;  --  Done, Fail, Wait
  HTML_FileName: String := "response.html";
  INSIDE_MACBUNDLE: Boolean := False;
  --
  SkipNext : Boolean := False;
  --
  ---------------
  --  Removes then leading and training spaces if any, 
  --   and the leading /trailing quotation marks  if any.
  ---------------
  function MkClean(Source:String) return String is
    start:Natural :=Source'First;
    stop: Natural :=Source'Last;
  begin
    --  remove leading and trailing spaces
    for I in Source'Range loop
      if Source(I) =' '  then
        Start := Start+1;
      else exit;
      end if;
    end loop;
    for I in reverse Source'Range loop
      if Source(I) =' '  then
        Stop := Stop-1;
      else exit;
      end if;
    end loop;
    if Source(Stop) ='"'  then
      Stop := Stop-1;
    end if;
    if Source(Start) ='"'  then
      Start := Start+1;
    end if;
    --
    --  adjust filename in case of MacOS drag&drop
    declare
      result: string (1..Stop-Start+1) := Source(Start..Stop);
      lineout: String := result;
      sizeout: Natural := lineout'Last;
    begin
      if INSIDE_MACBUNDLE then
          sizeout :=0;
          for I in result'Range loop
            if result(I) /= '\' then
              sizeout := sizeout+1;
              lineout(sizeout) := result(I);
            end if;
          end loop;
      end if;
      return lineout(1..sizeout);
    end;
  end MkClean; 
  --
begin
  --  usage:  umc [-w]  [modelfile] [formulafile] [-ltsdepth]
  --    -w =  html_mode,  -16 = Default_LTS_Depth, others skipped
  for I in 1..Ada.Command_Line.Argument_Count loop
    declare
      CommandArgs: String := Ada.Command_Line.Argument(I);
    begin
      if Model_File = null and then 
          CommandArgs'Length >0 and then
          CommandArgs(1) /= '+' and then
          CommandArgs(1) /= '-' then
         Model_File := new String'(CommandArgs);
         --
      elsif Formula_File = null and then
          CommandArgs'Length >0 and then
          CommandArgs(1) /= '+' and then
          CommandArgs(1) /= '-' then
         Formula_File := new String'(CommandArgs);
         NonInteractive := True;
         Ground_Action_Labels_Needed := False;
         --
      elsif CommandArgs = "-v" then
  ------------------------------------------------------------------------
        Put_line ("---- UMC Version 4.3 ----");
        Put_line( "FM&T Laboratory - ISTI-CNR");
        Put_line( "http://fmt.isti.cnr.it/umc");
        Put_Line( "usage: umc [-w]  [modelfile] [formulafile] [-<n>]");
        Put_Line( "-c<n> => parallel mode, use n workers tasks for evaluations");
        Put_Line( "-m<n> => parallel mode, use m workers tasks for model generation");
        Put_Line( "-d => debug mode, print messages about ongoing evaluation");
        Put_Line( "-s => silent, does not print progressing messages during evaluation");
        Put_Line( "-w => html_mode, used when called from the web interface");
        Put_Line( "-<n> => sets Default_LTS_Depth to n, for n=1 => breadth first like evaluation");
        Put_Line( "+<n> => sets Depth_Increment to n");
        Put_Line( "-x => sets Static_Max_Depth");
        Put_Line( "-y => sets NoGroundData");
        Put_Line( "-z => sets NoExplanations");
        Put_Line( "+z => sets AutoExplanations");
  ------------------------------------------------------------------------
        return;
      elsif CommandArgs'Length >2 and then
            CommandArgs(1) = '-'  and then
            CommandArgs(2) = 'm'  and then
            CommandArgs(3) in '1'..'9' then
        Flags.ModelCores := Integer'Value(CommandArgs(3..CommandArgs'Last));
        Flags.ThreadSafe := True;
      elsif CommandArgs'Length >2 and then
            CommandArgs(1) = '-'  and then
            CommandArgs(2) = 'c'  and then
            CommandArgs(3) in '1'..'9' then
        Flags.Cores := Integer'Value(CommandArgs(3..CommandArgs'Last));
        Flags.ThreadSafe := True;
      elsif CommandArgs = "-d" then
         Flags.Debug := True;
      elsif CommandArgs = "-w" then
         HTML_Mode := True;
      elsif CommandArgs = "-s" then
         Verbose_Eval := False;
      elsif CommandArgs = "-lazy" then
         Lazy_Parsing := True;
      elsif CommandArgs = "-almostlazy" then
         Almost_Lazy_Parsing := True;
      elsif CommandArgs = "-noerr" then
         Set_Error(Standard_Output);   
         NoErr := True;
      elsif CommandArgs = "-x" then
         Static_MAX_Depth := True;         
      elsif CommandArgs = "-y" then
         NoGroundData := True;     
      elsif CommandArgs = "-z" then
         NoExplanations := True;
      elsif CommandArgs = "+z" then
         NoExplanations := False;
         AutoExplanations := True;
      elsif CommandArgs(1) = '-'  and then
            CommandArgs(2) in '1'..'9' then
        Default_LTS_Depth := Integer'Value(CommandArgs(2..CommandArgs'Last));
      elsif CommandArgs(1) = '+'  and then
            CommandArgs(2) in '1'..'9' then
        Depth_Increment := Integer'Value(CommandArgs(2..CommandArgs'Last));
      else
         null;   -- e.g.   -cow2XXXXdir
      end if;
    end;
  end loop;
  ------------------------------------------------------------------------
  --
  ------------------------------------------------------------------------
  --  In case of Mac Application bundle adjust the working dir and filename
  --   on Macs, while doing drag&drop of files, spaces in filenames are prefixed by a \
  ------------------------------------------------------------------------
  declare
    CN: String := Command_Name;                  -- /../Appname.app/Contents/Resources/umc
    DIR: String := Containing_Directory(CN);     -- /../Appname.app/Contents/Resources
    SDIR : String := Simple_Name(DIR);           --   Resources
    PARENT: String := Containing_Directory(DIR); -- /../Appname.app/Contents
    SPARENT: String := Simple_Name(PARENT);      -- Contents
  begin
    if (SDIR = "Resources" or else
        SDIR = "MacOS") and then
       SPARENT = "Contents" then
      INSIDE_MACBUNDLE := True;
      declare
        APPDIR: String := Containing_Directory(PARENT);  -- /.../Workdir/Appname.app
        WORKDIR: String := Containing_Directory(APPDIR); --/.../Workdir/
        WORKFILE: File_Type;
      begin
        Set_Directory(WORKDIR);
      end;
      --
      if Model_File /= null then
        declare
         lineout: String := Model_File.all;
         sizeout: Natural :=0;
        begin
          for I in Model_File.all'Range loop
            if Model_File(I) /= '\' then
              sizeout := sizeout+1;
              Lineout(sizeout) := Model_File(I);
            end if;
          end loop;
          if Lineout(sizeout) = ' ' then
            sizeout := sizeout -1;
          end if;
          Model_File := new String'(Lineout(1..sizeout));
        end;
      end if;
    end if;
  end;

  ------------------------------------------------------------------------
  --
  ------------------------------------------------------------------------
  --
  -- if the command line provide the model and formula file names 
  --   just load and eval all offline
  if Formula_File /= null then
     LoadModel_and_Eval(Model_File.all, Formula_File.all, MC_Status);
     return;
  end if;

  ------------------------------------------------------------------------
  --
  ------------------------------------------------------------------------
  --
  -- if provided in the command line as parameter 
  --   load the requested model and starts its exploration;
  --
  Flags.Ground_Action_Labels_Needed := True;   -- for the display of the initial state
  if Model_File /=null and HTML_Mode= False then
     LoadModel (Model_File.all, MC_Status);
  elsif Model_File /=null and HTML_Mode= True then
    HTML_LoadModel(Model_File.all, HTML_FileName,MC_Status);
  end if;
  if MC_Status = Fail then
     return;
  end if;
  --
  --  then start the main command loop cycle
  --
  --
  ------------------------------------------------------------------------
  loop   
  ------------------------------------------------------------------------
    if not HTML_Mode then Put ("mc> "); end if;
    Get_Line (Input_Line, Input_Count);
    declare
      Trimmed: String := Trim(Input_Line(1..Input_Count),Both);
    begin
      Input_Count:= Trimmed'Length;
      Input_Line(1..Input_Count) := Trimmed;
    end;
    --
    if HTML_Mode = False then
      -------------------------------------------------------------------
      --------------------------  TXT MODE ------------------------------
      -------------------------------------------------------------------
      --
      --  LOAD COMMAND
      --
      if Input_Count > 5 and then Input_Line(1..5) = "load "  then
        Model_File:= new String'(MkClean(Input_Line(6..Input_Count)));
        LoadModel (Model_File.all, MC_Status);
        --
      elsif Input_Count > 1 and then Input_Line(1..1) = "/"  then
        Model_File:= new String'(MkClean(Input_Line(1..Input_Count)));
        LoadModel (Model_File.all, MC_Status);
        if MC_Status = Fail then Model_File := null; end if;
        --
      elsif Input_Count > 2 and then Input_Line(1..2) = "l "  then
        Model_File:= new String'(MkClean(Input_Line(3..Input_Count)));
        LoadModel (Model_File.all, MC_Status);
        if MC_Status = Fail then Model_File := null; end if;
        --
      elsif Model_File /= null and then 
           Input_Count =1 and then Input_Line(1..1) = "l" then
        LoadModel (Model_File.all, MC_Status);
        if MC_Status = Fail then Model_File := null; end if;
      elsif Model_File /= null and then 
           Input_Count =1 and then Input_Line(1..4) = "load" then
        LoadModel (Model_File.all, MC_Status);        
        if MC_Status = Fail then Model_File := null; end if;
      --
      --   EMPTY LINE or  HELP 
      --
      elsif Input_Count = 0 or else
          (Input_Count >= 1 and then Input_Line(1..1) = "?")  or else
          (Input_Count >= 4 and then Input_Line(1..4) = "help")  then
        Put_Line("------------------------------------------------");
        Put_Line("Commands:  info (i)                 -- print term ");
        Put_Line("           load [<FILENAME>] (l)    -- load [a new model]");
        Put_Line("           eval <FILENAME>          -- evaluate formula");
        Put_Line("           <FORMULA>                -- evaluate formula");
        Put_Line("           why                      -- explain last evaluation result");
        Put_Line("           X<n>                     -- explain computation X<n>");
        Put_Line("           C<n>                     -- disply info on configuration C<n>");
        Put_Line("           initial                  -- return to initial state");
        Put_Line("           <n>                      -- select evolution");
        Put_Line("           h<n>                     -- select history");
        Put_Line("           ground                   -- check formulas at ground level");
        Put_Line("           abstract                 -- check formulas at abstract level");
        Put_Line("           help (?)                 -- this help ");
        Put_Line("           dotgen                   -- generate file showlts.dot ");
        Put_Line("           dotgenCnnn               -- generate file showlts.dot starting from root node nnn");
        Put_Line("           end  (.)                 -- exit ");
        Put_Line("------------------------------------------------");
        Display_Status(MC_Status);
        Put_Line("--------------------------------------------------");
      --
      --  EXIT
      --
      elsif (Input_Count = 1 and then Input_Line(1) = '.' ) or else
         (Input_Count = 3 and then Input_Line(1..Input_Count) = "end") or else
         (Input_Count = 4 and then Input_Line(1..Input_Count) = "quit") then
        QuitServer(MC_Status);
        Put_Line ("bye bye ...");
        exit;
      --
      --  MODEL NOT LOADED
      -- 
      elsif Model_File = null then
        Put_Line ("** No model loaded yet ** "); 
      --
      --  CHECK EVALUATION PROGRESS  
      --
      elsif Model_File /= null and then
          Input_Count >8 and then Input_Line(1..9) = "evalcheck" then
        EvalCheck(MC_Status);
      --
      --  EXPLORE FROM INITIAL STATE
      --
      elsif Model_File /= null and then
          Input_Count >= 7 and then Input_Line(1..7) = "initial" then
         Start_Exploration(MC_Status);
         if MC_Status = Fail then Model_File := null; end if;
      --
      --  PRINT CURRENT WORKING DIRECTORY
      --
      elsif Input_Count = 3 and then Input_Line(1..3) = "pwd" then
         Put_Line(Current_Directory);
      --
      --   SET TERSE FLAG
      --
      elsif Model_File /= null and then
          Input_Count >= 5 and then Input_Line(1..5) = "terse" then
          Flags.Terse := True;
      --
      --   UNSET TERSE FLAG
      --
      elsif Model_File /= null and then
          Input_Count >= 7 and then Input_Line(1..7) = "verbose" then
          Flags.Terse := False;
      --
      -- GET ADDITIONAL CURRENT STATE INFO
      --
      elsif Model_File /= null and then
          ((Input_Count = 1 and then Input_Line(1) = 'i') or else
           (Input_Count >= 4 and then Input_Line(1..4) = "info")) then
         Configuration_Info(MC_Status);
         if MC_Status = Fail then Model_File := null; end if;
      --
      --  SELECT EVOLUTION
      -- 
      elsif Model_File /= null  and then
           Input_Count >0 and then
             Input_line(1) in '1'..'9' then
        Flags.Ground_Action_Labels_Needed := True;
        Select_Evolution(Input_line(1..Input_Count), MC_Status);
        if MC_Status = Fail then Model_File := null; end if;
      --
      --  SELECT HISTORY
      -- 
      elsif Model_File /= null  and then
           Input_Count >1 and then
            Input_line(1) = 'h' and then
            Input_line(2) in '1'..'9' then
        Flags.Ground_Action_Labels_Needed := True;
        Select_History(Input_line(1..Input_Count), MC_Status);
        if MC_Status = Fail then Model_File := null; end if;
      --
      --  SELECT STATE
      --
      elsif Model_File /= null  and then
           Input_Count >1 and then
            Input_line(1) = 'C' and then
            Input_line(2) in '1'..'9' then
        Flags.Ground_Action_Labels_Needed := True;
        Select_State(Input_line(1..Input_Count), MC_Status);
        if MC_Status = Fail then Model_File := null; end if;
      --
      --  GROUND
      --
      elsif Model_File /= null  and then
            (Input_Count = 6 and then Input_Line(1..Input_Count) = "ground") then
        --  Ground formulas and abstract formulas should be statically different.
        -- otherwise we might need to clear previous computations when the checking mode changes
        Set_Ground(MC_Status);
        Put_Line ("checking will be done at GROUND level.");
      --
      --  ABSTRACT
      --
      elsif Model_File /= null and then
             (Input_Count = 8 and then Input_Line(1..Input_Count) = "abstract") then
        Set_Abstract(MC_Status);
        if MC_Status = Done then
           Put_Line ("checking will be done at ABSTRACT level.");
        else
          Put_Line("to check at abstract level, the model must define the Abstractions!!!");
          Put_Line ("checking will be done at GROUND level.");
        end if;
      --
      --  ABORT EVAL
      --
      elsif Model_File /= null and then Input_Count > 0 and then
         Input_line(1..Input_Count) = "abort" then
         AbortEvaluation(MC_Status);
         Flags.Ground_Action_Labels_Needed := True;
      --
      --  EXPLAIN
      --
      elsif Model_File /= null and then Input_Count > 0 and then
         Input_line(1..Input_Count) = "why" then
         if Flags.NoExplanations then
            Put_line("*** No Explanations can be requestewd when the ""-z"" option is given. ***");
         else
         ExplainEvaluation(MC_Status);
         end if;
      elsif Model_File /= null and then Input_Count > 0 and then Input_line(1)='X' then
         if Flags.NoExplanations then
            Put_line("*** No Explanations can be requestewd when the ""-z"" option is given. ***");
         else
           declare
             NumImg: String(1..Input_Count-1) := Input_line(2..Input_Count);
             Num: Integer := Integer'Value(NumImg);
           begin
             ExplainEvaluation(MC_Status,Num);
           end;
        end if;
     
      --
      --  GENERATION OF SHOWLTS.DOT
      --
      elsif Model_File /= null and then
          Input_Count =6 and then Input_Line(1..6) = "dotgen" then
        GenerateDOTfile(1);
      elsif Model_File /= null and then
            Input_Count >7 and then Input_Line(1..7) = "dotgenC" then
        declare
        begin
          GenerateDOTfile(Integer'Value(Input_Line(8..Input_Count)));
        exception
         when others => Put_line(Standard_Error,
            "Oops .. invalid dotgen command: " & Input_Line(1..Input_Count));
        end;
        -- MC_Status is unrelevant
      --
      --  EVAL FROM FILE
      --
      elsif Model_File /= null and then
          Input_Count >5 and then Input_Line(1..5) = "eval " then
--        Flags.Ground_Action_Labels_Needed := False;
        Eval_From_File(Input_line(6..Input_Count), MC_Status);
        if Flags.AutoExplanations then
            ExplainEvaluation(MC_Status);
        end if;
        Flags.Ground_Action_Labels_Needed := True;
        -- MC_Status is unrelevant
      --
      --  EVAL FROM STRING
      --
      elsif Model_File /= null and then Input_Count > 0 then
--        Flags.Ground_Action_Labels_Needed := False;
        Eval_From_String(Input_line(1..Input_Count), MC_Status);
        if Flags.AutoExplanations then
            ExplainEvaluation(MC_Status); 
        end if;
        Flags.Ground_Action_Labels_Needed := True;
        -- MC_Status is unrelevant
      --
      --
      --
      end if;
      --
    else  -- HTML_Mode= True 
      --
      -------------------------------------------------------------------
      -------------------------- HTML MODE ------------------------------
      -------------------------------------------------------------------
      --
      --  TAKE FROM SAMPLE MODEL   --  used only in CAWS code
      --
      if Input_Count > 10 and then Input_line(1..10) = "getsample " then
         HTML_TakeFromSampleFile(Input_line(11..Input_Count), HTML_Filename);
      --
      --
      --  LOAD COMMANDa           -- used only ion CAWS code
      --
      elsif Input_Count > 5 and then Input_Line(1..5) = "load "  then
         Model_File:= new String'(MkClean(Input_Line(6..Input_Count)));
         LoadModel (Model_File.all, MC_Status);
         if MC_Status = Fail then Model_File := null; end if;
         if Model_File /= null then
           Flags.Ground_Action_Labels_Needed := True;
           HTML_Start_Exploration(HTML_FileName);
         end if; 
      --
      --  EXPLORE FROM INITIAL STATE    (build response.html in current directory)
      --
      elsif Model_File /= null and then
          Input_Count >= 7 and then Input_Line(1..7) = "initial" then
         Flags.Ground_Action_Labels_Needed := True;
         HTML_Start_Exploration(HTML_FileName);
      --
      -- GET ADDITIONAL CURRENT STATE INFO   (build response.html in current directory)
      --
      elsif Model_File /= null and then
          ((Input_Count = 1 and then Input_Line(1) = 'i') or else
           (Input_Count >= 4 and then Input_Line(1..4) = "info")) then
         Flags.Ground_Action_Labels_Needed := True;
         HTML_Configuration_Info(HTML_FileName);
      --
      --  SELECT EVOLUTION  (build response.html in current directory)
      --
      elsif Model_File /= null  and then
           Input_Count >0 and then
            Input_line(1) in '1'..'9' then
        Flags.Ground_Action_Labels_Needed := True;
        HTML_Select_Evolution(HTML_FileName,
                              Input_line(1..Input_Count));
      --
      --  SELECT HISTORY  (build response.html in current directory)
      --
      elsif Model_File /= null  and then
           Input_Count >1 and then
            Input_line(1) = 'h' and then
            Input_line(2) in '1'..'9' then
        Flags.Ground_Action_Labels_Needed := True;
        HTML_Select_History(HTML_FileName,
                            Input_line(1..Input_Count));
      --
      --  SELECT STATE
      --
      elsif Model_File /= null  and then
           Input_Count >1 and then
            Input_line(1) = 'C' and then
            Input_line(2) in '1'..'9' then
        Flags.Ground_Action_Labels_Needed := True;
        HTML_Select_State(HTML_Filename, 
                          Input_line(1..Input_Count));
      --
      --  CHECK EVALUATION PROGRESS  (build response.html in current directory)
      --
      elsif Model_File /= null and then
          Input_Count >8 and then Input_Line(1..9) = "evalcheck" then
        HTML_EvalCheck(HTML_FileName);
      --
      --  ABORT EVAL  (build response.html in current directory)
      --
      elsif Model_File /= null and then Input_Count > 0 and then
         Input_line(1..Input_Count) = "abort" then
         HTML_AbortEvaluation(HTML_FileName);
         Flags.Ground_Action_Labels_Needed := True;
      --
      --  EXPLAIN (build response.html in current directory)
      --
      elsif Model_File /= null and then Input_Count > 0 and then
         Input_line(1..Input_Count) = "why" then
         if NoExplanations then
            Put_line("*** No Explanations can be given when the ""-z"" option is given. ***");
         else
         HTML_ExplainEvaluation(HTML_FileName);
         end if;
      elsif Model_File /= null and then Input_Count > 0 and then Input_line(1)='X' then
         declare
           NumImg: String(1..Input_Count-1) := Input_line(2..Input_Count);
           Num: Integer := Integer'Value(NumImg);
         begin
           HTML_ExplainEvaluation(HTML_FileName,Num);
         end;
      --
      --  GENERATION OF SHOWLTS.DOT
      --
      elsif Model_File /= null and then
          Input_Count =6 and then Input_Line(1..6) = "dotgen" then
        GenerateDOTfile(1);
      elsif Model_File /= null and then
            Input_Count >7 and then Input_Line(1..7) = "dotgenC" then
        declare
        begin
          GenerateDOTfile(Integer'Value(Input_Line(8..Input_Count)));
        exception
         when others => Put_line(Standard_Error,
            "Oops .. invalid dotgen command: " & Input_Line(1..Input_Count));
        end;
        -- MC_Status is unrelevant
      --
      --  EXIT  (build response.html in current directory)
      --
      elsif (Input_Count = 1 and then Input_Line(1) = '.' ) or else
         (Input_Count = 3 and then Input_Line(1..Input_Count) = "end") or else
         (Input_Count = 4 and then Input_Line(1..Input_Count) = "quit") then
        HTML_QuitServer(HTML_FileName);
        exit;
      --
      --  EVAL FROM FILE   Starts a current evaluation and returns immediately
      --          (no more then one evaluation at the same time are allowed)
      --     (build response.html in current directory)
      --
      elsif Model_File /= null and then
          Input_Count >5 and then Input_Line(1..5) = "eval " then
--        Flags.Ground_Action_Labels_Needed := False;
        HTML_Eval_From_File(Input_line(6..Input_Count), HTML_FileName);
      --
      --
      --
      end if;
      --
    end if;
  ------------------------------------------------------------------------
  end loop;
  ------------------------------------------------------------------------
  --
end UMC;
