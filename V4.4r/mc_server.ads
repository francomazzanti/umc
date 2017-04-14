with Ada.Text_IO; use Ada.Text_IO;
package MC_Server is
  --------------------------------------------------------------------------- 
  -- Not all commands are directly passed to the SessionHandler task.
  -- Some commands just create a new SessionHandler
  -- Some commands just kill it.
  -- Some commands just retrieve some static or dynamic pages
  --------------------------------------------------------------------------- 

  NoErr: Boolean := False;     -- if True  Errors go to Current_Output
  CallBackName: String := "response.html";
  PayLoadName: String := "data.html";

  Eval_CallBackName: String := "evalresponse.html";
  EVal_PayLoadName: String := "evaldata.html";
  
  type CommandStatus is (Done, Fail, Wait);

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

  procedure LoadModel_and_Eval(Model_Filename: String; 
                               Formula_FileName: String;
                                Status: out CommandStatus);
  procedure LoadModel(Model_Filename: String; Status: out CommandStatus);
  procedure Start_Exploration (Status: out CommandStatus);
  procedure Select_History (historychoice: String; Status: out CommandStatus);
  procedure Select_State (nickname: String; Status: out CommandStatus);
  procedure Select_Evolution (evolutionchoice: String; Status: out CommandStatus);
  procedure Eval_From_File(Formula_FileName: String; Status: out CommandStatus);
  procedure Eval_From_String(FormulaCode: String; Status: out CommandStatus);
  procedure EvalCheck (Status: out CommandStatus); 
  procedure ExplainEvaluation (Status: out CommandStatus; Comp: Integer :=1);
  procedure AbortEvaluation (Status: out CommandStatus);
  procedure QuitServer (Status: out CommandStatus);
  procedure Configuration_Info(Status: out CommandStatus);
  procedure Display_Status (Status: out CommandStatus);
  procedure Display_Settings (Status: out CommandStatus);
  ----
  procedure Set_Ground (Status: out CommandStatus); 
  procedure Set_Abstract(Status: out CommandStatus);
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

  procedure HTML_LoadModel(Model_Filename: String;
                           Response_FileName: String;
                           Status: out CommandStatus);
  procedure HTML_Start_Exploration (ResponseName: String);
  procedure HTML_Configuration_Info (ResponseName: String);
  procedure HTML_Select_History (ResponseName: String; Input_Line: String);
  procedure HTML_Select_State (ResponseName: String; Input_Line: String);
  procedure HTML_Select_Evolution (ResponseName: String; Input_Line: String);
  procedure HTML_Eval_From_File(Formula_FileName: String; Response_FileName: String);
  procedure HTML_EvalCheck (Response_FileName: String);
  procedure HTML_ExplainEvaluation (Response_FileName: String; Comp: Integer :=1);
  procedure HTML_AbortEvaluation (Response_FileName: String);
  procedure HTML_QuitServer (Response_FileName: String);

  procedure HTML_TakeFromSampleFile(ModelName: String; ResponseName: String); 
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

  procedure GenerateDOTfile(Root:Integer:=1);
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

  ------------------------  BODY ----------------------------
  --  Declares a task type Sesion_Handler.
  --  A Load_Model operation creates a new Session_Handler task to which
  --    are forwarded all the subsequent operations (until QuitServer).
  --  The body of the Session_Handler task instanziates the generic MC package and
  --   uses this instance to execute all the requested operations.
  --  The interface of the generic MC package is independent from the computational model.
  --
  --  While the MC_Server executed some operations asynchronously
  --   (e.g Eval_From_File just launches a new evalution, withoout wating for the result)
  --   the MC package executes all the operations synchronously and sequentially.
end MC_Server;
