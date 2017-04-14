with Ada.Text_IO; use Ada.Text_IO;
with Flags; use Flags;
with MC;
with GNAT.IO_Aux;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
-------------------------------------------------------------------------------------
--------------  MC_SERVER.ADS  (Dierectly used by CMC/UMC) --------------------------
-------------------------------------------------------------------------------------
--  Default_LTS_Depth: Natural := 8;
--  NoErr: Boolean := False;     -- if True  Errors go to Current_Output
--  CallBackName: String := "response.html";
--  PayLoadName: String := "data.html";
--  Eval_CallBackName: String := "evalresponse.html";
--  EVal_PayLoadName: String := "evaldata.html";
--  type CommandStatus is (Done, Fail, Wait);
---------------------------------------------------------------------------------------
--  procedure LoadModel_and_Eval(Model_Filename: String;
--                               Formula_FileName: String;
--                                Status: out CommandStatus);
--  procedure LoadModel(Model_Filename: String; Status: out CommandStatus);
--  procedure Start_Exploration (Status: out CommandStatus);
--  procedure Select_History (historychoice: String; Status: out CommandStatus);
--  procedure Select_State (nickname: String; Status: out CommandStatus);
--  procedure Select_Evolution (evolutionchoice: String; Status: out CommandStatus);
--  procedure Eval_From_File(Formula_FileName: String; Status: out CommandStatus);
--  procedure Eval_From_String(FormulaCode: String; Status: out CommandStatus);
--  procedure EvalCheck (Status: out CommandStatus);
--  procedure ExplainEvaluation (Status: out CommandStatus; Comp: Integer :=1);
--  procedure AbortEvaluation (Status: out CommandStatus);
--  procedure QuitServer (Status: out CommandStatus);
--  procedure Configuration_Info(Status: out CommandStatus);
--  procedure Display_Status (Status: out CommandStatus);
--  procedure Display_Settings (Status: out CommandStatus);
--  ----
--  procedure Set_Ground (Status: out CommandStatus);
--  procedure Set_Abstract(Status: out CommandStatus);
---------------------------------------------------------------------------------------
--  procedure HTML_LoadModel(Model_Filename: String;
--                            Response_FileName: String;
--                            Status: out CommandStatus);       --  "response.html"
--  procedure HTML_Start_Exploration (ResponseName: String);
--  procedure HTML_Configuration_Info (ResponseName: String);
--  procedure HTML_Select_History (ResponseName: String; Input_Line: String);
--  procedure HTML_Select_State (ResponseName: String; Input_Line: String);
--  procedure HTML_Select_Evolution (ResponseName: String; Input_Line: String);
--  procedure HTML_Eval_From_File(Formula_FileName: String;
--                                 Response_FileName: String);  -- "response.html"
--  procedure HTML_EvalCheck (Response_FileName: String);
--  procedure HTML_ExplainEvaluation (Response_FileName: String; Comp: Integer :=1);
--  procedure HTML_AbortEvaluation (Response_FileName: String);
--  procedure HTML_QuitServer (Response_FileName: String);
--
--  procedure HTML_TakeFromSampleFile(ModelName: String; ResponseName: String);
-------------------------------------------------------------------------------------
--  procedure GenerateDOTfile(Root:Integer:=1);  
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------


------------------------------------
--   package body MC_Server
--     LoadModel(Status:MC_Status)
--     EvaluateFormula
--     Start_Exploration
--     Get_Info_on_Current_Status
--     Select_Evolution
--     Select_History
--     Explain_Result
--     Abort_Evaluation
--     Quit_Server
--
--        --------------------------
--        task body Session
--           -----------------------
--           package Server is  new MC
--           -----------------------
--        --------------------------
--
--        Current_Session: Session;
--
-----------------------------------


-----------------------------------
--   generic package body MC
--
--      ---------------------------
--      -- shared defs and vars
--      type String_Ref;
--      -- observations and abstractions
--      All_Observations
--      ---------------------------
--
--      ---------------------------
--      -- COW_Configurations - COW_System
--      LoadFromFile
--      Initial_Configurations
--      Exloration routines
--      Display routines
--      ---------------------------
--
--      ---------------------------
--      -- UCTL
--      EvaluateFormula
--      Explain
--             -- UCTL_Types
--             -- UCTL_Parser
--             -- UCTL_Utilities
--             -- UCTL_Logics
--      ---------------------------
--
-----------------------------------

package body MC_Server is

use Ada.Exceptions;      
use Ada.Characters.Handling;
use Ada.Characters.Latin_1;
  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  --
  task type Session_Handler is
    -------------------  HTML MODE ----------------------
    entry S_HTML_LoadModel (Model_FileName:String; 
                            Data_FileName: String; 
                            LoadOK: out Boolean); 
    entry S_HTML_Start_Exploration(Out_FileName: String);
    entry S_HTML_Select_History (Out_FileName: String; Input_Line: String);
    entry S_HTML_Select_State (Out_FileName: String; Input_Line: String);
    entry S_HTML_Select_Evolution (Out_FileName: String; Input_Line: String);
    entry S_HTML_Eval_From_File(Formula_FileName: String;
                               Start_FileName: String;   -- "evalog.html"
                               Out_FileName: String;     -- "evalout.html"
                               Elog_FileName: String;    -- "evalog.html"
                               ParseOK: out Boolean); 
    entry S_HTML_Explain (Out_FileName: String; Comp: Integer :=1);
    entry S_HTML_Configuration_Info (Out_FileName: String);
    -------------------  TXT MODE -----------------------
    entry S_LoadModel (Model_FileName:String; LoadOK: out Boolean);
    entry S_Start_Exploration; 
    entry S_Select_History (Input_Line: String);
    entry S_Select_State (Input_Line: String);
    entry S_Select_Evolution (Input_Line: String);
    entry S_Eval_From_File(Formula_FileName: String); 
    entry S_Load_and_Eval(Model_FileName:String;
                          Formula_FileName: String;
                           OK : out Boolean); 
    entry S_Eval_From_String(Formula_Code: String);
    entry S_ExplainEvaluation(Comp: Integer :=1);
    entry S_Display_Status;
    entry S_Configuration_Info;
    entry S_Set_Ground;
    entry S_Set_Abstract(OK: out Boolean);
    -----------------------------------------------------
    entry S_GenerateDOTfile(Root:Integer:=1);
    -----------------------------------------------------
    -- PRAGMA STORAGE_SIZE( 8192 * 8192);
    PRAGMA STORAGE_SIZE( taskstacksize);
    
  end Session_Handler;
  
  type Session_Handler_Ref is access Session_Handler;
  
  Current_Session: Session_Handler_Ref with atomic;

  EvalRunning: Boolean := False;       -- running=true  & result=done  ->  n.n
  EvalResult: CommandStatus := Wait;   -- running=false & result=done  -> last evaluation successfully completed
                                       -- running=false & result=fail  -> syntax_error in last formula
                                       -- running=true  & result=fail  ->  n.a.
                                       -- running=true  & result=wait  -> last evaluation still in progress
                                       -- running=false & result=wait  -> no evaluation tried yet 
  pragma SHARED(EvalResult);
  pragma SHARED(EvalRunning);

  ModelName_Ref: access String; -- hold Model_Name of last loaded model
  ElogName_Ref: access String;  
  OutName_Ref: access String;  

  FormName_Ref: access String;
  FormCode_Ref: access String;

    ELog_Name: string := "evalog.html";
    Out_Name: string := "evalout.html";

  ---------------------------------------------------------------------
  ---------------------------------------------------------------------
  task body Session_Handler is
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    package Server is new MC;   -- HERE IS ALL THE MC MACHINERY
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    --
    SessionClosing: Boolean := False;
    Eval_File: File_Type;
    Parse_OK: Boolean := False;
    Load_OK: Boolean := True;
  begin
    --
---    Server.Default_LTS_Depth := Default_LTS_Depth;
    --
    --
    ---------------------------------------------------------------------
    -- the model indicated is parsed and its exploration initialted
    -- In case of errors the session is abandoned
    ---------------------------------------------------------------------
    --
    while Load_OK loop
      select  
      ---------------------------------------------------------------------
      -------------------- HTML  MODE  -------------------------------------
      ---------------------------------------------------------------------
        accept S_HTML_LoadModel (Model_FileName:String; 
                                 Data_FileName: String; 
                                 LoadOK: out Boolean) do
         declare
         begin
           Server.HTML_LoadModel(Model_FileName,Data_FileName, LoadOK);
           Load_OK := LoadOK;
         exception
         when Event: others =>
           LoadOK := False;
           Load_OK := False;
         end;
        end S_HTML_LoadModel;
        if not Load_OK then exit; end if;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Start_Exploration(Out_FileName: String) do
          Server.HTML_Start_ModelExploration(Out_FileName);
        end S_HTML_Start_Exploration;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Select_History (Out_FileName: String; Input_Line: String) do
          Server.HTML_Select_History(Out_FileName,Input_Line);
        end S_HTML_Select_History;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Select_State (Out_FileName: String; Input_Line: String) do
          Server.HTML_Select_State(Out_FileName,Input_Line);
        end S_HTML_Select_State;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Select_Evolution (Out_FileName: String; Input_Line: String) do
          Server.HTML_Select_Evolution(Out_FileName,Input_Line);
        end S_HTML_Select_Evolution;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Eval_From_File(Formula_FileName: String;
                               Start_FileName: String;  -- "evalog.html"  UCTL_parsing_errors
                               Out_FileName: String;     -- "evalout.html"
                               Elog_FileName: String;    -- "evalog.html"
                               ParseOK: out Boolean) do
          ElogName_Ref := new String'(Elog_FileName);
          OutName_Ref := new String'(Out_FileName);
          EvalRunning := True;
          EvalResult := Wait;
          declare
             COWOut_File: File_Type;
          begin 
              --
              Parse_OK := True;
              ParseOK := True;
              EvalRunning := True;
              -- Parse the formula, logging erros into Start_FileName
              Create (COWOut_File, Out_File, Start_FileName);
                -- for error logging  possibile UCTL parsing errors
              Put_Line(COWOut_File, "<html><body bgcolor='white'>");
              Put_Line(COWOut_File,
                "<div align=left style=""background-color:white;width:95%;height:100%"">");
              Put_Line(COWOut_File, "<pre>");
              --
              Set_Error(COWOut_File);
              Server.Parse_Formula(Formula_FileName,ParseOK);
              if NoErr then
                Set_Error(Current_Output);
              else
                Set_Error(Standard_Error);
              end if;
              Parse_OK := ParseOK;
              --
            if  Parse_OK then
              Close(COWOut_File);  -- no errors logged
              Create (COWOut_File, Out_File, Start_FileName);
              Put_Line(COWOut_File, "<html><body bgcolor='white'>");
              Put_Line(COWOut_File,
                "<div align=left style=""background-color:white;width:95%;height:100%"">");
              Put_Line(COWOut_File, "<pre>");
              Put_Line(COWOut_File, "  ... Evaluation is in progress ...");
              Put_Line(COWOut_File, "</pre>");
              Put_Line(COWOut_File, "</div></body></html>");
              Close(COWOut_File);
            else
              EvalRunning := False;
              EvalResult := Fail;
              --  close the HTML EMBEDDING of UCTYL parsing errors
              Put_Line(COWOut_File, "</pre>");
              Put_Line(COWOut_File, "</div></body></html>");
              Close(COWOut_File);
            end if;
            --
          exception
          when Event: others =>
              Parse_OK := False;
              ParseOK := False;
              EvalRunning := False;
              EvalResult := Fail;
              if NoErr then
                Set_Error(Current_Output);
              else
                Set_Error(Standard_Error);
              end if;
              Put_Line (Standard_Error,Exception_Information(Event));
              if Is_Open(COWOut_File) then Close(COWOut_File); end if;
          end;  
        end S_HTML_Eval_From_File;
        --
        --  resume suspended client and continue asynchronously
        --
        if Parse_OK then
          declare
            Eval_OK: Boolean;
          begin
            -- the task performs the evaluation saving the execution log
            --  in the Elog file and the evaluation results in OutName file.
            --
            Server.HTML_EvaluateIt (ElogName_Ref.all,   -- "evalog.html"
                                    OutName_Ref.all,    -- "evalout.html"
                                    Eval_OK);
            --
            if Eval_OK then
              EvalResult := Done;
            else
              EvalResult := Fail;
            end if;
            EvalRunning := False;
          exception
          when Event: others =>
              EvalRunning := False;
              EvalResult := Fail;
              if NoErr then
                Set_Error(Current_Output);
              else
                Set_Error(Standard_Error);
              end if;
              Put_Line (Standard_Error,Exception_Information(Event));
          end;
        end if;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Explain (Out_FileName: String; Comp: Integer :=1) do
          Server.HTML_ExplainEvaluation(Out_FileName,Comp);
        end S_HTML_Explain;
      or
        ---------------------------------------------------------------------
        accept S_HTML_Configuration_Info (Out_FileName: String) do
         Server.HTML_Configuration_Info(Out_FileName);
        end S_HTML_Configuration_Info;
      or
      ---------------------------------------------------------------------
      -------------------- TXT  MODE  -------------------------------------
      ---------------------------------------------------------------------
        accept S_LoadModel (Model_FileName: String; 
                        LoadOK: out Boolean) do
           LoadOK := False;
           Load_OK := False;
           Server.LoadModel(Model_FileName, LoadOK);
           Load_OK := LoadOK;
        end S_LoadModel;
        if not Load_OK then exit; end if;
      or   
        ---------------------------------------------------------------------
        accept S_Eval_From_String(Formula_Code: String) do
          EvalRunning := True;
          EvalResult := Wait;
          FormCode_Ref := new String'(Formula_Code);
        end S_Eval_From_String;
        --
        --  resume suspended client and continue asynchronously
        --
        Server.EvalFromString(FormCode_Ref.all, Parse_OK);
        Put ("mc> ");
        if Parse_OK then
           EvalRunning := False;
           EvalResult := Done;
        else
           EvalRunning := False;
           EvalResult := Fail;
        end if;
      or
        ---------------------------------------------------------------------
        accept S_Eval_From_File(Formula_FileName: String)  do
          EvalRunning := True;
          EvalResult := Wait;
          FormName_Ref := new String'(Formula_FileName);
        end S_Eval_From_File;
        --
        --  resume suspended client and continue asynchronously
        --
        Server.EvalFromFile(FormName_Ref.all, Parse_OK);
        Put ("mc> ");
        if Parse_OK then
           EvalRunning := False;
           EvalResult := Done;
        else
           EvalRunning := False;
           EvalResult := Fail;
        end if;
      or 
        ---------------------------------------------------------------------
        accept S_Load_and_Eval(Model_FileName:String;
                          Formula_FileName: String;
                           OK : out Boolean) do
          --
          Server.Load_and_Eval(Model_FileName,Formula_FileName, OK);
           --
        end S_Load_and_Eval;
        exit;
      or
        ----
        ---------------------------------------------------------------------
        accept S_Start_Exploration do
             Server.Start_ModelExploration;
        exception
            when Event: others =>
              SessionClosing := True;   -- exit the session
              Put_line (Current_Error, 
                 "Sorry! An unexpected error occurred during the exploration");
              Put_Line (Current_Error,Exception_Information(Event));
        end S_Start_Exploration;
      or 
        ---------------------------------------------------------------------
        accept S_Configuration_Info do 
          Server.Configuration_Info;
        exception
          when Event: others =>
              SessionClosing := True;   -- exit the session
              Put_line (Current_Error," Sorry! An unexpected error occurred");
              Put_Line (Current_Error,Exception_Information(Event));
        end S_Configuration_Info;
      or 
        ---------------------------------------------------------------------
        accept S_Select_History (Input_Line: String)  do 
          Server.Select_History(Input_Line);
        end S_Select_History;
      or 
        ---------------------------------------------------------------------
        accept S_Select_State (Input_Line: String)  do 
          Server.Select_State(Input_Line);
        end S_Select_State;
      or 
        ---------------------------------------------------------------------
        accept S_Select_Evolution (Input_Line: String) do 
            Server.Select_Evolution(Input_Line);
        exception
        when Event: others =>
              SessionClosing := True;   -- exit the session
              Put_line (Current_Error," Sorry! An unexpected error occurred");
              Put_Line (Current_Error,Exception_Information(Event));
        end S_Select_Evolution;
      or 
        ---------------------------------------------------------------------
        accept S_ExplainEvaluation(Comp: Integer :=1) do
            Server.ExplainEvaluation(Comp);
        exception
        when Event: others =>
              SessionClosing := True;   -- exit the session
              Put_line (Current_Error," Sorry! An unexpected error occurred");
              Put_Line (Current_Error,Exception_Information(Event));
        end S_ExplainEvaluation;
      or  
        ---------------------------------------------------------------------
        accept S_Display_Status do
            Server.Display_Status;
        exception
        when Event: others =>
              SessionClosing := True;   -- exit the session
              Put_line (Current_Error," Sorry! An unexpected error occurred");
              Put_Line (Current_Error,Exception_Information(Event));
        end S_Display_Status;
      or
      ---------------------------------------------------------------------
        accept S_Set_Ground do
           Server.Set_Ground;
        end S_Set_Ground;
      or
      ---------------------------------------------------------------------
        accept S_Set_Abstract(OK: out Boolean) do
           Server.Set_Abstract(OK);
        end S_Set_Abstract;
      ---------------------------------------------------------------------
      or
      ---------------------------------------------------------------------
        accept S_GenerateDOTfile(Root:Integer:=1) do
           Server.GenerateDOTfile(Root);
        end S_GenerateDOTfile;
      ---------------------------------------------------------------------
      ---------------------------------------------------------------------
      end select;
      --
    end loop;
  end Session_Handler;
  
  
  -----------------------------------------------------------------------------------------
  --  Given a filename of a model file, parses and load the corresponding model.
  --  If there are no errors also the model exploration is started and its first 
  --  exploration page is saved into the "Response_FileName".
  --  In presence of errors, Response_FileName will contain the error messages.
  -----------------------------------------------------------------------------------------
  procedure HTML_LoadModel(Model_Filename: String;
                           Response_FileName: String;
                           Status: out CommandStatus ) is
    HTML_Response: File_Type;
    Data_Name: string := PayLoadName;
    LaodOK: Boolean;
  begin
    --  start  a new session, possibly aborting the previous
    if Current_Session = null then
       Current_Session := new Session_Handler;
    else
       abort Current_Session.all;
       Current_Session := new Session_Handler;
    end if;
    --
    Current_Session.S_HTML_LoadModel(Model_Filename, Data_Name, LaodOK);
    --
    --  If Loading failed returns the response with the err_name
    --
    --  Data_Name will be used in a http://get_File.cgi?DataName   request
    --
      -- otherwisereturns the response with the out_name
    if  LaodOK then
      Status := Done;
      --
      --   ACTUALLY THE OLNY RELEVANT PART OF THE RESPONSE FILE
      --     IS THE STATUS DATA "0" or "1"
      --
      -------------------  CREATE THE CALLBACK -----------------------
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response,
            " parent.loadmodel_callback(0,""" & Data_Name & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
      ----------------------------------------------------------------
    else
      Status := Fail;
      ModelName_Ref := new String'(Model_Filename);
      Current_Session := null;
      --
      --
      --   ACTUALLY THE OLNY RELEVANT PART OF THE RESPONSE FILE
      --     IS THE STATUS DATA "0" or "1"
      --
      -------------------  CREATE THE CALLBACK -----------------------
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response,
            " parent.loadmodel_callback(1,""" & Data_Name & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
      ----------------------------------------------------------------
    end if;
    ---
  exception
  when Event: others =>
    if NoErr then   
      Set_Error(Standard_Output);
    else
      Set_Error(Standard_Error);
    end if;
    Status := Fail;
    Put_line (Current_Error,"Error in HTML_LoadModel");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(HTML_Response) then Close(HTML_Response); end if;
    raise;
  end HTML_LoadModel;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_Start_Exploration (ResponseName: String) is
     Response_File: File_Type;
     contentname: String := PayLoadName;
  begin
     ----------- CREATE  PAYLOAD -----------------------------------
     Current_Session.S_HTML_Start_Exploration(contentname);
     ------------------- CREATE  CALLBACK -------------------------
     Create(Response_File, Out_File, responsename);
     Put_Line(Response_File,"<html> <head></head><body BGCOLOR=""gray""><script>");
     Put_Line(Response_File,"top.displaydata(0, """& contentname &""");");
     Put_Line(Response_File,"</script></body></html>");
     Close(Response_File);
     ---------------------------------------------------------------
   exception
   when Event: others =>
     Put_line ("Error in HTML_Start_Exploration");
     Put_Line (Current_Error, Exception_Name(Event));
     Put_Line (Current_Error, Exception_Message(Event));
     if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_Start_Exploration;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_Configuration_Info (ResponseName: String) is
    Response_File: File_Type;
    contentname: String := PayLoadName;
  begin
    ----------   CREATE  PAYLOAD ------------------------------
    Current_Session.S_HTML_Configuration_Info(contentname);
    -----------  CREATE  CALLBACK ------------------------------
    Create(Response_File, Out_File, responsename);
    Put_Line(Response_File,"<html> <head></head><body><script>");
    Put_Line(Response_File,"top.displaydata(0, """& contentname &""");");
    Put_Line(Response_File,"</script></body></html>");
    Close(Response_File);
    -----------------------------------------------------------
  exception
  when Event: others =>
    Put_line ("Error in HTML_Configuration_Info");
    Put_Line (Current_Error, Exception_name(Event));
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_Configuration_Info;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_Select_History (ResponseName: String;
                                 Input_Line: String) is
    Response_File: File_Type;
    contentname: String := PayLoadName;
  begin
    ----------   CREATE  PAYLOAD ------------------------------
    Current_Session.S_HTML_Select_History(contentname ,Input_Line);
    -----------  CREATE  CALLBACK ------------------------------
    Create(Response_File, Out_File, responsename);
    Put_Line(Response_File,"<html> <head></head><body BGCOLOR=""gray""><script>");
    Put_Line(Response_File,"top.displaydata(0, """& contentname &""");");
    Put_Line(Response_File,"</script></body></html>");
    Close(Response_File);
    -----------------------------------------------------------
    exception
  when Event: others =>
     Put_line ("Error in HTML_Select_History");
     Put_Line (Current_Error, Exception_name(Event));
     Put_Line (Current_Error, Exception_Name(Event));
     Put_Line (Current_Error, Exception_Message(Event));
     if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_Select_History;
  
  procedure HTML_Select_State (ResponseName: String;
                                 Input_Line: String) is
    Response_File: File_Type;
    contentname: String := PayLoadName;
  begin
    ----------   CREATE  PAYLOAD ------------------------------
    Current_Session.S_HTML_Select_State(contentname ,Input_Line);
    -----------  CREATE  CALLBACK ------------------------------
    Create(Response_File, Out_File, responsename);
    Put_Line(Response_File,"<html> <head></head><body BGCOLOR=""gray""><script>");
    Put_Line(Response_File,"top.displaydata(0, """& contentname &""");");
    Put_Line(Response_File,"</script></body></html>");
    Close(Response_File);
    -----------------------------------------------------------
    exception
  when Event: others =>
     Put_line ("Error in HTML_Select_State");
     Put_Line (Current_Error, Exception_name(Event));
     Put_Line (Current_Error, Exception_Name(Event));
     Put_Line (Current_Error, Exception_Message(Event));
     if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_Select_State;

  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_Select_Evolution (ResponseName: String;
                            Input_Line: String) is
       Response_File: File_Type;
       contentname: String := PayLoadName;
  begin
    ----------   CREATE  PAYLOAD ------------------------------
    Current_Session.S_HTML_Select_Evolution(contentname ,Input_Line);
    -----------  CREATE  CALLBACK ------------------------------
    Create(Response_File, Out_File, responsename);
    Put_Line(Response_File,"<html> <head></head><body BGCOLOR=""gray""><script>");
    Put_Line(Response_File,"top.displaydata(0, """& contentname &""");");
    Put_Line(Response_File,"</script></body></html>");
    Close(Response_File);
    -----------------------------------------------------------
    --
  exception
  when Event: others =>
    Put_line ("Error in HTML_Select_Evolution");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_Select_Evolution;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_Eval_From_File(Formula_FileName: String;
                            Response_FileName: String) is        -- "response.html"
    HTML_Response: File_Type;
    Start_Data: String := PayLoadName;
    ELog_Name: string := "evalog.html";
    Out_Name: string := "evalout.html";
    ParseOK: Boolean := False;
  begin
    --
    if EvalRunning then
       -- what to do when a second EVAL is issued? Nothing!!
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response, "<!-- Eval Alread Running! -->");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
      return;
    end if;
    --
    --  Start_Data will contain the UCTL_Parser error messages, or 
    --   the notification of the beginning of the evaluation 
    --
    EvalRunning := True;
    EvalResult := Wait;
    Current_Session.S_HTML_Eval_From_File
           (Formula_FileName, ELog_Name, Out_Name, ELog_Name, ParseOK);
    --      ...           , "evalog.html",  "evalout.html", "evalog.html"
    --
    -- THE EVALUATION PROCEEDES CONCURRENTLY
    --
    -- Out_Name and ELog_Name are used offline and concurrently.
    -- The executing task will have side-effects of the variables:
    --        EvalRunning := False;
    --        EvalResult := Fail;
    --
    --
    if ParseOK then
      HTML_EvalCheck (Response_FileName);
      ----  generate call-back and payload according to the running status ----
    else
      ----------------  CREATE THE CALLBACK (in "response.html") ----------------
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response,
          " parent.verifyformula_callback(parent.ko,""" & Start_Data & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
      ------------------------------------------------------
    end if;
    --
  exception
  when Event: others =>
    EvalRunning := False;
    EvalResult := Fail;
    Set_Error(Current_Error);
    if NoErr then
      Set_Error(Current_Output);
    else
      Set_Error(Standard_Error);
    end if;
    Put_line (Current_Error,"Error in HTML_Eval_From_File");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(HTML_Response) then Close(HTML_Response); end if;
    raise;
  end HTML_Eval_From_File;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_EvalCheck (Response_FileName: String) is
    HTML_Response: File_Type;
    COWOut_File: File_Type;
    Out_Name: string := PayLoadName;
  begin
    -- the ervaluation proceeds concurrently ...
    -- the evaluation log is being dumped in the file ElogName_Ref.all,
    -- the final HTML evaluation results will be written into  OutName_Ref.all,
    if not EvalRunning and EvalResult=Wait then 
       -- what to do when EVAL has not been issued? Nothing!!
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
--      Put_Line(HTML_Response, "<!-- Eval NOT Running! -->");
      Put_Line(HTML_Response,
          " parent.verifyformula_callback(parent.ko,""" &
             ElogName_Ref.all & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
      return;
    end if;
    --
    --  If Loading failed returns the response with the err_name
    if not EvalRunning and then EvalResult=Fail then   -- failure
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response,
          " parent.verifyformula_callback(parent.ko,""" &
             ElogName_Ref.all & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response); 
      --
    elsif  not EvalRunning and then EvalResult=Done then   -- success
      -- otherwisereturns the response with the out_name
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response,
           " parent.verifyformula_callback(parent.ok,""" &
            OutName_Ref.all & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
      --
    elsif EvalRunning  then  -- in progress
      -- otherwisereturns the response with the out_name
--      Create (COWOut_File, Out_File, Out_Name);
--      Put_Line(COWOut_File, "<html><body bgcolor='white'>");
--      Put_Line(COWOut_File,
--         "<div align=left style=""background-color:white;width:95%;height:100%"">");
--      Put_Line(COWOut_File, "<pre>");
--      declare
--        LOG_File: File_Type;
--        STR: String(1..300);
--        N:Natural;
--      begin
--      --  We want to include here all the current logging. But that requires a concurrent
--      --  access to the external evalog.html file, and the semantics of that is implementation
--      --  dependent. The dafult GNAT semantics is the raising of USe_Error.
--      -- We can achieve that either from inside a different process, as the evalcheck cgi script.
--      -- OR WE CAN DO THAT USING GNAT DEPENDENT FORM STRIBN  "shared=no" (which adopts the same
--      -- semantics of multple independent C steams linked to the same object).
--        Open(LOG_File, In_File, ELog_Name,"shared=no");
--        while not End_Of_File(LOG_File) loop
--          Get_Line(LOG_File,STR,N);
--          Put_Line(COWOut_File, STR(1..N));
--        end loop;
--        Close(LOG_File);
--      exception
--      when Event: others =>     -- e.g.LOG_File not yet created
--          Put_Line(COWOut_File, " ..... " & Exception_Name(Event)  & " .....");
--      end;
--      Put_Line(COWOut_File, "  ... Evaluation is still in progress ...");
--      Put_Line(COWOut_File, "</pre>");
--      Put_Line(COWOut_File, "</div></body></html>");
--      Close(COWOut_File);
      --
      Create(HTML_Response, Out_File, Response_FileName);
      Put_Line(HTML_Response, "<http><body><script>");
      Put_Line(HTML_Response,
           " parent.verifyformula_callback(parent.wait,""" & Out_Name & """); ");
      Put_Line(HTML_Response, "</script></body></html>");
      Close(HTML_Response);
    end if;
    --
  exception
  when Event: others =>
    if NoErr then
      Set_Error(Current_Output);
    else
      Set_Error(Standard_Error);
    end if;
    Put_line (Current_Error,"Error in HTML_EvalCheck");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(HTML_Response) then Close(HTML_Response); end if;
    if Is_Open(COWOut_File) then Close(COWOut_File); end if;
    raise;
  end;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_ExplainEvaluation (Response_FileName: String; Comp: Integer :=1) is
       contentname: String := PayLoadName;
       Content_File: File_Type;
       Response_File: File_Type;
  begin
      --------------------------------  CREATE  PAYLOAD -----------------------------
      if Current_Session = null then
        Create (Content_File, Out_File, contentname);
        Put_Line(Content_File, "<html><body bgcolor='white'>");
        Put_Line(Content_File,
                 "<div align=left style=""background-color:white;width:95%;height:100%"">");
        Put_Line(Content_File, "<pre>");
        Put_Line(Content_File,"<center>");
        Put_Line(Content_File,
           "<div align=left style=""background-color:white;width:95%;height:100%"">");
        Put_Line(Content_File, "<pre>");
        Put_Line(Content_File, "  ... Session aborted - please relaod the model! ...");
        Put_Line(Content_File, "</pre>");
        Put_Line(Content_File, "</div></center></body></html>");
        Close(Content_File);
      elsif  EvalRunning = True then
        Create (Content_File, Out_File, contentname);
        Put_Line(Content_File, "<html><body bgcolor='white'>");
        Put_Line(Content_File,
                 "<div align=left style=""background-color:white;width:95%;height:100%"">");
        Put_Line(Content_File, "<pre>");
        Put_Line(Content_File,"<center>");
        Put_Line(Content_File, "  ... Evaluation still in progress - please wait! ...");
        Put_Line(Content_File, "</pre>");
        Put_Line(Content_File, "</div></center></body></html>");
        Close(Content_File);
      elsif EvalResult = Fail then
        Create (Content_File, Out_File, contentname);
        Put_Line(Content_File, "<html><body bgcolor='white'>");
        Put_Line(Content_File,
                 "<div align=left style=""background-color:white;width:95%;height:100%"">");
        Put_Line(Content_File, "<pre>");
        Put_Line(Content_File,"<center>");
        Put_Line(Content_File, "  ... No successful Evaluation to explain! ...");
        Put_Line(Content_File, "</pre>");
        Put_Line(Content_File, "</div></center></body></html>");
        Close(Content_File);
      elsif EvalResult = Wait then
        Create (Content_File, Out_File, contentname);
        Put_Line(Content_File, "<html><body bgcolor='white'>");
        Put_Line(Content_File,
                 "<div align=left style=""background-color:white;width:95%;height:100%"">");
        Put_Line(Content_File, "<pre>");
        Put_Line(Content_File,"<center>");
        Put_Line(Content_File, "  No Evaluation to explain! ...");
        Put_Line(Content_File, "</pre>");
        Put_Line(Content_File, "</div></center></body></html>");
        Close(Content_File);
      else 
        Current_Session.S_HTML_Explain(contentname,Comp); 
      end if;
    --
    -----------------------------  CREATE   CALLBACK ----------------------------------
    Create(Response_File, Out_File, Response_FileName);
    Put_Line(Response_File,"<html> <head></head><body BGCOLOR=""gray""><script>");
    Put_Line(Response_File,"top.displaydata(0, """& contentname &""");");
    Put_Line(Response_File,"</script></body></html>");
    Close(Response_File);
    -----------------------------------------------------------------------------------
    --
  exception
  when Event: others =>
    Put_line ("Error in HTML_ExplainEvaluation");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(Response_File) then Close(Response_File); end if;
    if Is_Open(Content_File) then Close(Content_File); end if;
  end HTML_ExplainEvaluation;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_AbortEvaluation (Response_FileName: String) is
    Response_File: File_Type;
    MC_Status: CommandStatus;
  begin
    if Current_Session /= null then
      abort Current_Session.all;
      Current_Session := null;
    else
      EvalRunning := False;  -- busy
      EvalResult := Wait;  -- busy
      if ModelName_Ref /= null then
        HTML_LoadModel(ModelName_Ref.all, Response_FileName,MC_Status);
      else
        -- do nothing - no callback from abort!!!
        Create(Response_File, Out_File, Response_FileName);
        Put_Line(Response_File,"<html> <head></head><body><script>");
        Put_Line(Response_File,"</script></body></html>");
        Close(Response_File);
      end if;
    end if;
  exception
  when Event: others =>
    Put_line ("Error in HTML_AbortEvaluation");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_AbortEvaluation;
  
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_QuitServer (Response_FileName: String) is
    Response_File: File_Type;
  begin
    if Current_Session /= null then
      abort Current_Session.all;
      Current_Session := null;
    end if;
    -- do nothing - no callback from abort!!!
    Create(Response_File, Out_File, Response_FileName);
    Put_Line(Response_File,"<html> <head></head><body><script>");
    Put_Line(Response_File,"</script></body></html>");
    Close(Response_File);
    --
    -- delete tmp files ...
  exception
  when Event: others =>
    Put_line ("Error in HTML_QuitServer");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(Response_File) then Close(Response_File); end if;
  end HTML_QuitServer;
  

  -------------------------------------------------------------------------------------
  ----------------------------  TXT  MODE ---------------------------------------------
  -------------------------------------------------------------------------------------


  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure LoadModel(Model_Filename: String; Status: out CommandStatus) is
    LaodOK: Boolean;
  begin
    Put_Line("--------------------------------------------------");
    Put_Line ("Loading the Model from file : " & Model_Filename);
    Put_Line("--------------------------------------------------");
    --  start  a new session, possibly aborting the previous
    if Current_Session = null then
       Current_Session := new Session_Handler;
    else
       abort Current_Session.all;
       Current_Session := new Session_Handler;
    end if;
    --
    Current_Session.S_LoadModel(Model_Filename, LaodOK);
    --
    if  LaodOK then
      -- save the model filename for possible reloads ...
      -- ModelName_Ref = null means  ** no model sucessfully loaded **
      ModelName_Ref := new String'(Model_Filename);
      Status := Done;
    else
      Current_Session := null;
      Status := Fail;
    end if;
    Put_Line("--------------------------------------------------");
  end LoadModel;
  
  procedure LoadModel_and_Eval(Model_Filename: String; 
                               Formula_FileName: String;
                                Status: out CommandStatus) is
    LaodOK: Boolean;
  begin
    Put_Line("--------------------------------------------------");
    Put_Line ("Loading the Model from file : " & Model_Filename);
    Put_Line ("Loading the Formula from file : " & Formula_FileName);
    Put_Line("--------------------------------------------------");
    --  start  a new session, possibly aborting the previous
    if Current_Session = null then
       Current_Session := new Session_Handler;
    else
       abort Current_Session.all;
       Current_Session := new Session_Handler;
    end if;
    --
    Current_Session.S_Load_and_Eval(Model_Filename, Formula_FileName, LaodOK);
    if LaodOK then
      Status := Done;
    else
      Status := Fail;
    end if;
    --
    Current_Session:=null;
    --  was  QuitServer(Status);
    --
    Put_Line("--------------------------------------------------");
    --
   --
  end LoadModel_and_Eval;
  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure Eval_From_String(FormulaCode: String; Status: out CommandStatus) is
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
    --
    if EvalRunning then
       -- what to do when a second EVAL is issued? Nothing!!
       Put_Line("An evaluation is already in progress!!");
       Status := Done;
       return;
    end if;
    Current_Session.S_Eval_From_String (FormulaCode);
    Status := Done;   -- actually unrelevant
  exception
  when Event: others =>
     Status := Fail;   -- actually unrelevant
  end Eval_From_String;

  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure Eval_From_File(Formula_FileName: String; Status: out CommandStatus) is
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
     --
    if EvalRunning then
       -- what to do when a second EVAL is issued? Nothing!!
       Put_Line("An evaluation is already in progress!!");
       Status := Done;
       return;
    end if;
    Current_Session.S_Eval_From_File (Formula_FileName);
    Status := Done;   -- actually unrelevant
  exception
  when Event: others =>
     Status := Fail;  -- actually unrelevant
  end Eval_From_File;

  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure EvalCheck (Status: out CommandStatus) is
  begin
    if not EvalRunning and EvalResult=Wait then 
       -- what to do when EVAL has not been issued? Nothing!!
      Put_Line(" No evaluation in progress ... ");
      Status := Done;  -- actually unrelevant
      return;
    end if;
    -- 
    --  If Loading failed returns the response with the err_name
    if not EvalRunning and then EvalResult=Fail then   -- failure
      Put_Line(" No evaluation in progress ... ");
      EvalResult := Wait;
      --
    elsif  not EvalRunning and then EvalResult=Done then   -- success
      -- otherwisereturns the response with the out_name
      Put_Line(" Evaluation successfully completeted. ");
      EvalResult := Wait;
      --
    elsif EvalRunning  then  -- in progress
      -- otherwisereturns the response with the out_name
      Put_Line(" Evaluation still in progress ... ");
    end if;
    Status := Done;  -- actually unrelevant
    --
  exception
  when Event: others =>
    Status := Fail;  -- actually unrelevant
    if NoErr then
      Set_Error(Current_Output);
    else
      Set_Error(Standard_Error);
    end if;
    Put_line (Current_Error,"Error in HTML_EvalCheck");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    raise;
  end;

  -------------------------------------------------------------------------------------
  -- command: "info"
  -------------------------------------------------------------------------------------
  procedure Configuration_Info (Status: out CommandStatus) is
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
     --
    Put_Line("--------------------------------------------------");
     Current_Session.S_Configuration_Info;
     Status := Done;
    Put_Line("--------------------------------------------------");
  exception
  when Event: others =>
     Status := Fail;
  end Configuration_Info;

  -------------------------------------------------------------------------------------
  --  command "C<n>"
  -------------------------------------------------------------------------------------
  procedure Select_History (historychoice: String; Status: out CommandStatus) is
    Response_File: File_Type;
    contentname: String := PayLoadName;
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
     --
    if Current_Session = null or else not Current_Session.all'Callable then
      Status := Fail;
      return;
    end if;
    Current_Session.S_Select_History(historychoice);
    Status := Done;
  exception
  when Event: others =>  -- tasking_error is session termninated while suspended in queue
     Status := Fail;
  end Select_History;
  

  procedure Select_State (nickname: String; Status: out CommandStatus) is
    Response_File: File_Type;
    contentname: String := PayLoadName;
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
     --
    if Current_Session = null or else not Current_Session.all'Callable then
      Status := Fail;
      return;
    end if;
    Current_Session.S_Select_State(nickname);
    Status := Done;
  exception
  when Event: others =>  -- tasking_error is session termninated while suspended in queue
     Status := Fail;
  end Select_State;
  -------------------------------------------------------------------------------------
  --  command "<n>"
  -------------------------------------------------------------------------------------
  procedure Select_Evolution (evolutionchoice: String; Status: out CommandStatus) is
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
    --
    Put_Line("--------------------------------------------------");
    Current_Session.S_Select_Evolution(evolutionchoice);
     Status := Done;
    Put_Line("--------------------------------------------------");
  exception
  when Event: others =>
     Status := Fail;
  end Select_Evolution;

   
  -------------------------------------------------------------------------------------
  -- command why
  -------------------------------------------------------------------------------------
  procedure ExplainEvaluation (Status: out CommandStatus; Comp: Integer :=1) is
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
    --
    if Current_Session = null or else not Current_Session.all'Callable then
         Put_Line("  ... Session aborted - please relaod the model! ...");
    elsif  EvalRunning = True then
         Put_Line("  ... Evaluation still in progress - please wait! ...");
    elsif EvalResult = Fail  then
         Put_Line("  No successful Evaluation to explain!");
    elsif EvalResult = Wait  then
         Put_Line("  No Evaluation to explain! ");
    else
      Current_Session.S_ExplainEvaluation(Comp);
    end if;
  exception
  when Event: others =>
     Status := Done;
  end ExplainEvaluation;
  
  -------------------------------------------------------------------------------------
  --  command "quit"
  -------------------------------------------------------------------------------------
  procedure QuitServer (Status: out CommandStatus) is
  begin
    if Current_Session /= null then
      abort Current_Session.all;
      Current_Session := null;
    end if;
    Status := Done;
  exception
  when Event: others =>
     Status := Fail;
  end QuitServer;
  
  -------------------------------------------------------------------------------------
  --  command "abort"
  -------------------------------------------------------------------------------------
  procedure AbortEvaluation  (Status: out CommandStatus) is
  begin
    if Current_Session /= null then
      abort Current_Session.all;
      Current_Session := null;
      EvalRunning := False;
      EvalResult := Fail;
      -- quit session and reload a new one
      if ModelName_Ref /= null then
        LOADModel(ModelName_Ref.all, Status);
      end if;
    end if;
     Status := Done;
  exception
  when Event: others =>
     Status := Fail;
  end AbortEvaluation;
  
  -------------------------------------------------------------------------------------
  -- command initial
  -------------------------------------------------------------------------------------
  procedure Start_Exploration (Status: out CommandStatus) is
  begin
    Put_Line("--------------------------------------------------");
     if Current_Session= null  then
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
       Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
     Current_Session.S_Start_Exploration;
     Status := Done;
    Put_Line("--------------------------------------------------");
  exception
  when Event: others =>
     Status := Fail;
  end Start_Exploration;
   
  
  -------------------------------------------------------------------------------------
  -- help  - emptyline
  -------------------------------------------------------------------------------------
  procedure Display_Status(Status: out CommandStatus) is
  begin
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       Status := Fail;
       return;
     end if;
     --
     Current_Session.S_Display_Status;
     Status := Done;
  exception
  when Event: others =>
     Status := Fail;
  end Display_Status;
  

  procedure Set_Ground (Status: out CommandStatus) is
  begin
      if Current_Session = null then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        Status := Fail;
      else
        Current_Session.S_Set_Ground;
        Status := Done;
      end if;
  end Set_Ground;

  procedure Set_Abstract(Status: out CommandStatus) is
    OK: Boolean;
  begin
    if Current_Session = null then
      Status := Fail;
      return;
    end if;
    Current_Session.S_Set_Abstract(OK);
    if Ok then 
      Status := Done;    
    else 
      Status := Fail;  
    end if;
  end Set_Abstract; 


  -----------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------
  procedure HTML_TakeFromSampleFile(ModelName: String; ResponseName: String) is
    Response_File: File_Type;
    TXT_Model: File_Type;
    PRE: string := "'";
    POST: String := " " & Reverse_Solidus & "n' +";  -- " \n"
  begin
    Create(Response_File, Out_File, ResponseName);
    Put_Line(Response_File,"<http><head></head><body><script>");
    Put_Line(Response_File,"remotemodel=");
    Open(TXT_Model, In_File, ModelName);
    while not End_of_File(TXT_Model)    loop
      begin
        declare
          LINE: String := GNAT.IO_Aux.Get_Line(TXT_Model);
        begin
          if Is_Control(LINE(LINE'Last)) then
            Put_Line(Response_File, PRE & LINE(LINE'First..Line'Last-1)  & POST );
          elsif Is_Control(LINE(LINE'First)) then
            Put_Line(Response_File, PRE & LINE(LINE'First+1..Line'Last)  & POST );
          else
            Put_Line(Response_File, PRE & LINE  & POST );
          end if;
        end;
      exception
      when others => exit;
      end;
    end loop;
    Close(TXT_Model);
    Put_line(Response_File, "'';");
    Put_Line(Response_File," parent.takefromsamplefile_callback(remotemodel);");
    Put_line(Response_File,"</script></body></html>");
    Close(Response_File);
    --
  exception
  when Event: others =>
    Put_line ("Error in TakeFromSampleFile");
    Put_Line (Current_Error, Exception_name(Event));
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(Response_File) then Close(Response_File); end if;
    if Is_Open(TXT_Model) then Close(TXT_Model); end if;
    raise;
  end HTML_TakeFromSampleFile;

  procedure Display_Settings (Status: out CommandStatus) is
  begin
    Status := Done;
  end;

  procedure GenerateDOTfile(Root:Integer:=1) is
  begin  
     if Current_Session= null  then
        Put_Line("--------------------------------------------------");
        Put_Line("     ERROR:  PLEASE LOAD A SYSTEM MODEL ... ");
        Put_Line("--------------------------------------------------");
        return;
     end if;
     if not Current_Session.all'Callable then
        Put_Line("--------------------------------------------------");
        Current_Session := null;
        Put_Line("     ERROR:  NO VALID SYSTEM MODEL FOUND ... ");
        Put_Line("--------------------------------------------------");
       return;
     end if;
     Current_Session.S_GenerateDOTfile(Root);
  exception
  when Event: others =>
     null;
  end GenerateDOTfile;

end MC_Server;

