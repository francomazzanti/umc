with Flags; use Flags;
with Ada.Unchecked_Deallocation;
with Ada.Text_Io; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Containers; 
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Sets;
with Ada.Unchecked_Deallocation; 
with Ada.Exceptions;
with System;
with Ada.Characters.Latin_1;
with Configurations;
with DotGen;
with DotLib;
with UCTL;
package body MC is

---------------------------------------------------------------------------
----------------- PACKAGE MC  SPECIFICIATION-------------------------------
--  Default_LTS_Depth: Natural := 1;
--  procedure Load_and_Eval (Model_FileName:String; Formula_FileName: String; OK : out Boolean);
--  procedure LoadModel (Model_FileName: String; LoadOK: out Boolean);
--  procedure Start_ModelExploration;
--  procedure Select_History(Input_Line: String);
--  procedure Select_State(Input_Line: String);
--  procedure Select_Evolution(Input_Line: String);
--  procedure EvalFromFile(Form_FileName: String; Eval_OK: out Boolean);
--  procedure EvalFromString(FormCode: String; Eval_OK: out Boolean);
--  procedure ExplainEvaluation(Comp: Integer :=1);
--  procedure Configuration_Info;
--  procedure Display_Status;
--  procedure Set_Ground;
--  procedure Set_Abstract(OK: out Boolean);
--  procedure Parse_Formula (Formula_FileName: String; Parse_OK: out Boolean);
--  ---------------------- HTML MODE -----------------------
--  procedure HTML_LoadModel (Model_FileName: String;
--                            HTML_FileName: String;  LoadOK: out Boolean);
--  procedure HTML_Start_ModelExploration (HTML_FileName: String);
--  procedure HTML_Select_History (HTML_FileName: String; Input_Line: String);
--  procedure HTML_Select_State (HTML_FileName: String; Input_Line: String);
--  procedure HTML_Select_Evolution (HTML_FileName: String; Input_Line: String);
--  procedure HTML_EvaluateIt (Elog_FileName: String;   -- "evalog.html"
--                             Out_FileName: String;    -- "evalout.html"
--                             Eval_OK: out Boolean);
-- procedure HTML_EvalFromFile (HTML_FileName: String; Form_FileName: String; Eval_OK: out Boolean);
--  procedure HTML_ExplainEvaluation (HTML_FileName: String;Comp: Integer :=1);
--  procedure HTML_Configuration_Info (HTML_FileName: String);
------------------------------------------------------------
--  procedure GenerateDOTfile(Root:Integer:=1);
--  ----------------------  DEBUGGING ------------------------
--  procedure Debug(Str:String);
--  procedure Dump_States;
---------------------------------------------------------------------------
---------------------------------------------------------------------------

  package MyConfigurations is new Configurations;
  package Configurations renames MyConfigurations;
  use Configurations;
  use Configurations.Kernel;

  package MyUCTL is new UCTL(Configurations);
  package UCTL renames MyUCTL;
  use UCTL;
---------------------------------------------------------------------------
---------------------------------------------------------------------------
  type Evolutions_Table is array(Positive Range <>) of Configurations.System_Evolution;
  type Evolutions_Table_Ref is access Evolutions_Table;
  procedure Free is new Ada.Unchecked_Deallocation(Evolutions_Table,Evolutions_Table_Ref);
  ---------------------------------------------
  CurrentConf: System_Configuration;
  Next: Evolutions_Table_Ref;
  HistoryKeys: String_Tables_Vector(1..500);
  HistoryStep: String_Table(1..500);
  Hindex: Natural :=0;
  Bilabelled : Boolean := False;
  --
  function Joined (Vector: String_Table) return String is
  begin
     if Vector'Length =1 and then
           Vector(Vector'First) /= null then
         return Vector(Vector'First).all;
     elsif Vector'Length >1 then
        if Vector(Vector'First) /= null then
          return Vector(Vector'First).all & "," & Joined(Vector(Vector'First+1 .. Vector'Last));
        else
          return " ," & Joined(Vector(Vector'First+1 .. Vector'Last));
        end if;
    else
         return "";
    end if;
  end Joined;
  --
  function Abstract_Label_Image
       (This_Evolution: Configurations.System_Evolution) return String is
    TheLabels: String_Tables_Vector := Get_Abstract_Action_Labels(This_Evolution);
  begin
     return Display_AbstractLabels(TheLabels);
  end;
  --
  function Ground_Label_Image
       (This_Evolution: Configurations.System_Evolution) return String is
    ALI: String_Table := Get_Ground_Action_Labels(This_Evolution);
  begin
     return Joined(ALI);
  end Ground_Label_Image;
---------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

 ---------------------------------------------------------
  --  used in:
  -- onmouseover="Tip(" & HTML_Literal_Hack(text) & ")"
  --   text like p.o<a1,a2>   should become:
  --       'p.o&'+'lt;a1,a2&gt;'  
  --   because we should escape the '<' and '>'
   --  and prevent "&lt;" to be interpreted back as '<'
  -- (is this a browser or tooltio lib problem?)
 ---------------------------------------------------------
 function HTML_Literal_Hack (Source:String) return String is
   Result:String(1..10000);
   OUTC: Natural := 1;
 begin
   Result(1..1) := "'";
   OUTC :=2;
   for I in Source'Range loop
     if Source(I)='<' then
        Result(OUTC..OUTC+6) := "&'+'lt;";
        OUTC := OUTC+7;
     elsif Source(I)='>' then
        Result(OUTC..OUTC+3) := "&gt;";
        OUTC := OUTC+4;
     elsif Source(I)='"' then
        Result(OUTC..OUTC+4) := "&#34;";
        OUTC := OUTC+5;
     else
        Result(OUTC) := Source(I);
        OUTC := OUTC+1;
     end if;
   end loop;
   Result(OUTC..OUTC) := "'";
   return Result (1..OUTC);
 end;

  --
 function HTML_Format (Source:String) return String is
   Result:String(1..10000);
   OUTC: Natural := 1;
 begin
   for I in Source'Range loop
     if Source(I)='<' then
        Result(OUTC..OUTC+3) := "&lt;";
        OUTC := OUTC+4;
     elsif Source(I)='>' then
        Result(OUTC..OUTC+3) := "&gt;";
        OUTC := OUTC+4;
     else
        Result(OUTC) := Source(I);
        OUTC := OUTC+1;
     end if;
   end loop;
   return Result (1..OUTC-1);
 end;

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

use Ada.Text_IO;
use Ada.Containers;
use Ada.exceptions;
use Configurations;

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

  function Get_Evolutions(CurrentConf: System_Configuration) return Evolutions_Table_Ref is 
    This_Iter: Evolutions_Iterator;
    Result: Evolutions_Table_Ref := new Evolutions_Table(1..0);
  begin 
    Iterator_Initialize (This_Iter, CurrentConf);
    while Has_System_Transition(This_Iter) loop
      Result :=  new Evolutions_Table'(Result.all & Get_Evolution_Data(This_Iter));
      Iterator_Advance(This_Iter);
    end loop;
    Iterator_Finalize(This_Iter);
    return Result;
  end;

  procedure Print_HTML_History (HTML_File: File_Type) is
    use Ada.Strings.Fixed;
  begin
    if CurrentConf /=1 then
      Put_Line(HTML_File,
               "<b>The Path from The Initial Configuration to Configuration " &
                NickName(CurrentConf,"C") &  " is:</b><br>");
      --
      if (Hindex = 0 or else Index(HistoryStep(1).all,">C1<",1) = 0) then
        Put_Line(HTML_File, 
             "<a href='javascript:top.sendcommand(""C1"")'>C1</a> -> ... ->" &  
             NickName(CurrentConf,"C") & "<br>");
      end if;
      for I in 1..Hindex loop
              Put_Line(HTML_File, HistoryStep(I).all & "<br>");
      end loop;
      Put_Line(HTML_File, "<hr>");
    end if;
    --
  end Print_HTML_History;
  

  procedure Print_State_Properties (This_Conf: System_Configuration) is
    These_Properties: UCTL.State_Properties := 
       UCTL.Get_State_Properties(This_Conf);
  begin
     if not Flags.NoExplanations and then These_Properties'Length >0 then
        Put_line(" Properties:");
        for K in These_Properties'Range loop
           Put_line("""" & These_Properties(K).PImage.all & 
                    """  (" &  Property_Status'Image(These_Properties(K).Status) & ")");
        end loop;
     end if;
  end Print_State_Properties;
  
  procedure HTML_Print_State_Properties (
                   HTML_File: File_Type;
                   This_Conf: System_Configuration) is
    These_Properties: UCTL.State_Properties := 
       UCTL.Get_State_Properties(This_Conf);
    use Ada.Strings;
    use Ada.Strings.Fixed;
  begin
     if not Flags.NoExplanations and then These_Properties'Length >0 and then
        not (These_Properties'Length=1 and These_Properties(1).PImage.all="true")  then
         New_Line(HTML_File);
         Put_line (HTML_File,
             "<P><b>In Configuration " & NickName(CurrentConf,"C") &
             " the following properties hold:</b>");
        Put_line (HTML_File,  "<table>");
        for K in These_Properties'Range loop
          if These_Properties(K).PImage.all/= "true" then
            Put_line(HTML_File,
               "<tr><td>The formula: <i> " & HTML_Format(These_Properties(K).PImage.all) & 
               "</i><td> is <b>" &  Property_Status'Image(These_Properties(K).Status) & "</b> " &
--               "<td onclick='parent.explainthis(" &  
--                Trim(Integer'Image(These_Properties(K).CompRef),Left) &
--               ");' style='cursor:pointer'> expalain");
               "<td>  ( <a href='#' onclick='parent.explainthis(" &  
                Trim(Integer'Image(These_Properties(K).CompRef),Left) &
               ");' style='cursor:pointer'> explain</a> )");
          end if;
        end loop;
        Put_line (HTML_File,  "</table>");
        Put_Line(HTML_File, "<hr>");
     end if;
  end HTML_Print_State_Properties;
  
--  procedure Print_Current_State_Link (HTML_File: File_Type) is 
--  begin
--       Put_line (HTML_File,
--         "<b>The Current Configuration is " &
--          "<span style=""background-color:red"">" &
--          NickName(CurrentConf,"C") & "</span></b>" &
--          "&nbsp;<a href='javascript:top.sendcommand(""i"")'>" &
--          " (More Details ... )" & "</a><p>");
--  end Print_Current_State_Link;


--  --  NO MORE USED:  REPLACED BY VERSIONN IN CONFIGURATIONS.KERNEL
--  procedure  HTML_Print_Possible_Evolutions (
--                   HTML_File: File_Type;
--                   Next: out Evolutions_Table_Ref) is
--    This_Iter: Evolutions_Iterator;
--    Index: Natural;
--  begin
--       Next := Get_Evolutions(CurrentConf);
--       if Next.all'Length > 0 then
--         New_Line(HTML_File);
--         Put_line (HTML_File,
--             "<P><b>The Possible Evolutions From Configuration " &
--             NickName(CurrentConf,"C") &
--             " are:</b>");
--         Index :=0;
--         for I in Next.all'Range loop
--            Index := Index+1;
--            Put (HTML_File, "<br>" & Integer'Image(Index) & ") ");
--            --
--            Put (HTML_File, "<a href='javascript:top.sendcommand(""" &
--                Integer'Image(I)(2..Integer'Image(I)'Length) &
--                """)'>");
--            Put (HTML_File, NickName(CurrentConf,"C") &
--                  " --> "  &
--                 NickName(Get_Target(Next(I)),"C") );
--            Put (HTML_File,"</a>");
--            --
--            declare
--               Abstract_Labels: String_Table :=
--                  Get_Abstract_Action_Label_Images(Next(Index));
--            begin
--              Put (HTML_File, " { ");
--              for I in Abstract_Labels'Range loop
--                Put (HTML_File, HTML_Format(Abstract_Labels(I).all));
--                if I < Abstract_Labels'last then Put(HTML_File, ","); end if;
--              end loop;
--              Put (HTML_File, " } ");
--            end;
--            declare
--             Ground_Labels: String := Ground_Label_Image(Next(Index));
--            begin
--              Put(HTML_File,"<label href=""index.htm"" onmouseover=""Tip(");   -- TOOLTIP PREFIX
--              Put (HTML_File, "'/*&nbsp;'+");
--                 Put (HTML_File, HTML_Literal_Hack(Ground_Labels));
--              Put (HTML_File, "+'&nbsp;*/'");
--              Put(HTML_File,   -- TOOLTIP SUFFIX
--                   ")"" onmouseout=""UnTip()"">" &
--                   "<FONT color=""blue"">&nbsp;/* ... */</font></label>");
--            end;
--            New_Line(HTML_File);
--         end loop;
--       else
--         New_Line(HTML_File);
--         Put_line (HTML_File,"<P>No More Evolutions.");
--       end if;
--       Put_line(HTML_File,"<br>");
--  end HTML_Print_Possible_Evolutions;


  procedure Print_Possible_Evolutions (
                   Next: out Evolutions_Table_Ref) is
    This_Iter: Evolutions_Iterator;
    Index: Natural;
  begin
     Next := Get_Evolutions(CurrentConf);
       if Next.all'Length > 0 then
         Put_line (
             "Possible Evolutions From State " & NickName(CurrentConf,"C"));
         Index :=0;
         for I in Next.all'Range loop
            Index := Index+1;
            Put (Integer'Image(Index) & ") ");
            --
            Put (NickName(CurrentConf,"C") &
                  " --> "  &
                 NickName(Get_Target(Next(I)),"C") );
            --
            declare
              Abstract_Labels: String_Table := 
                Get_Abstract_Action_Label_Images(Next(Index));
            begin
               Put (" { ");
               for I in Abstract_Labels'Range loop
                  Put (Abstract_Labels(I).all);
                  if I < Abstract_Labels'last then Put(","); end if;
               end loop;
               Put (" } ");
            end;
            if not Flags.Terse then
              declare
                Ground_Labels: String_Table := 
                  Get_Ground_Action_Labels(Next(Index));
              begin
                Put (" /* ");
                for I in Ground_Labels'Range loop
                   Put (Ground_Labels(I).all);
                   if I < Ground_Labels'last then Put(","); end if;
                end loop;
                Put (" */");
              end;
            end if;
            New_Line;
         end loop;
       else
         New_Line;
         Put_line ("No More Evolutions.");
       end if;
  end Print_Possible_Evolutions;


--  procedure Print_Current_State is
--  begin
--    Put_line ("Current Configuration is: " & NickName(CurrentConf,"C"));
--  end Print_Current_State;


  procedure HTML_Print_Abstract_State_Label_Images(HTML_File: File_Type;
                                                   This_Conf: System_Configuration) is
    ASL : String := Display_AbstractLabels(Get_Abstract_State_Labels(CurrentConf));
  begin
    if ASL'Length = 0 then
      Put_Line (HTML_File,
         " <B>There are no Abstract State Labels for Configuration " & NickName(CurrentConf,"C")&
         " </b><br>  ");
    else
      Put (HTML_File,
         " <B>The Abstract State Labels of Configuration " & NickName(CurrentConf,"C")&
         " are:</b><br>  ");
      Put(HTML_FIle, HTML_Format(ASL));
      Put_Line(HTML_File, "");
    end if;
  end HTML_Print_Abstract_State_Label_Images;

  procedure Print_Abstract_State_Label_Images(This_Conf: System_Configuration) is
    ASL : String := Display_AbstractLabels(Get_Abstract_State_Labels(CurrentConf));
  begin
    if ASL'Length = 0 then
       Put_Line("        -  ");
    else
       Put_Line(ASL);
  end if;
  end Print_Abstract_State_Label_Images;

  procedure HTML_Display_Status(HTML_FileName: String) is
    HTML_File: File_Type;
  begin
    Create(HTML_File, Out_File, HTML_FileName);
    Put_Line(HTML_File, "<html><body bgcolor='white'>");
    Put_line(HTML_File, "<script type=""text/javascript"" src=""wz_tooltip.js""></script>");
    Put_Line(HTML_File, "<div style=""background-color:lightyellow;width=95%%"">");
    --
    --  Display Link to HTML class descriptions
    --
    HTML_Print_System_Structure(HTML_File,CurrentConf);
    Put_Line(HTML_File, "<hr>");
    --
--    Put_line (HTML_File,
--         "<b>The Current Configuration is " &
--         NickName(CurrentConf,"C") & "</span></b><br>"); 
    --
    --  Display Path to Current State
    Print_HTML_History(HTML_File);
    --
    -- Display Current State Link
    -- Print_Current_State_Link(HTML_File);
    --
    -- Display abstract action potentialities
    -- HTML_Print_Abstract_State_Label_Images(HTML_File,CurrentConf);
    --
    -- Display possible evolutions -----------------------------------------------
    --HTML_Print_Possible_Evolutions(HTML_File, Next);
    Next := Get_Evolutions(CurrentConf);
    HTML_Print_Possible_Evolutions(HTML_File, CurrentConf);
    --
    Put_line (HTML_File,
          "<p><a href='javascript:top.sendcommand(""i"")'>" &
          " (More Details ... )" & "</a><p>");
    --
    New_Line(HTML_File);
    Put_Line(HTML_File, "</div>");
    Put_Line (HTML_File, "</body></html>");
    Close(HTML_File);
  exception
  when Event: others =>
    Put_line (Current_Error,
       "Error in HTML_Display_Status");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    if Is_Open(HTML_File) then Close(HTML_File); end if;
  end HTML_Display_Status;

  ------------------------------------------------------------
  ---  BEWARE:  Based on the GLOBAL CurrentConf variable   ---
  ------------------------------------------------------------
  procedure HTML_Configuration_Info (HTML_FileName: String)  is
    HTML_File: File_Type;
  begin
    Create(HTML_File, Out_File, HTML_FileName);
    Put_Line(HTML_File, "<html><body bgcolor='white'>");
    Put_line(HTML_File, "<script type=""text/javascript"" src=""wz_tooltip.js""></script>");
    -----------
    Put_Line(HTML_File, "<div style=""background-color:lightyellow;width=95%"">");
    --
    --  Display Link to HTML class descriptions
    --
    HTML_Print_System_Structure(HTML_File,CurrentConf);
    Put_Line(HTML_File, "<hr>");
    --
--    Put_line (HTML_File,
--         "<b>The Current Configuration is " & NickName(CurrentConf,"C") & "</b><br>");
    --
    --  Display Path to Current State
    Print_HTML_History(HTML_File); -- 
    --
    -- Display Current State Link
--    Print_Current_State_Link(HTML_File);  --??
     --
     -- Display abstract action potentialities
--   HTML_Print_Abstract_State_Label_Images(HTML_File,CurrentConf); --??
    --
    -- Display possible evolutions
--    Next := Get_Evolutions(CurrentConf);
--    HTML_Print_Possible_Evolutions(HTML_File, Next);
    Next := Get_Evolutions(CurrentConf);
    HTML_Print_Possible_Evolutions(HTML_File, CurrentConf);  --??
    --
    New_Line(HTML_File);
    HTML_Print_State_Properties(HTML_File,CurrentConf); 
--    Put_Line(HTML_File, "</div>");
--    -----------
--    Put_Line(HTML_File, "<div align=left style=""background-color:white;width:100%"">");
    Put_Line (HTML_File, "<br>");
    HTML_Print_Configuration(HTML_File,CurrentConf);
    New_Line(HTML_File);
    Put_Line(HTML_File, "</div>");
    Put_Line (HTML_File, "</body></html>");
    Close(HTML_File);
    -----------
  exception
  when Event: others =>
    Put_line (Current_Error, "Error in Configuration_Info");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end HTML_Configuration_Info;
  
  procedure Configuration_Info is
  begin
    Print_Configuration(CurrentConf);
    New_Line;
  exception
  when Event: others =>
    Put_line (Current_Error, "Error in Configuration_Info");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end Configuration_Info;
  
  procedure HTML_Start_ModelExploration (HTML_FileName: String) is
  begin
    CurrentConf := Initial_Configuration;
    Hindex := 0;
    HTML_Mode := True;
   -- HTML_Display_Status(HTML_FileName);     -- Details hidden
    HTML_Configuration_Info (HTML_FileName);  -- Details expanded
  exception
  when Event: others =>
    Put_line (Current_Error,
       "Error in SessionHandler.HTML_Set_HistoryStatus");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
    raise;
  end HTML_Start_ModelExploration;
  
  
  procedure HTML_Select_History (HTML_FileName: String; Input_Line: String) is
    Index: Natural;
  begin
    Index := Integer'Value(Input_Line(Input_Line'First+1..Input_Line'Last));
    CurrentConf := FindFromKey(HistoryKeys(Index));
    Hindex := Index -1;
    -- HTML_Display_Status(HTML_FileName);    -- Details hidden
    HTML_Configuration_Info (HTML_FileName);  -- Details expanded
  exception
  when Event: others =>
    Put_line (Current_Error,
       "Error in SessionHandler.HTML_Set_HistoryStatus");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end HTML_Select_History;

  procedure HTML_Select_State (HTML_FileName: String; Input_Line: String) is
    Index: Natural;
  begin
    Index := Integer'Value(Input_Line(Input_Line'First+1..Input_Line'Last));
    CurrentConf := GetProgressive(Index);
    Hindex :=0;
    -- HTML_Display_Status(HTML_FileName);    -- Details hidden
    HTML_Configuration_Info (HTML_FileName);  -- Details expanded
  exception
  when Event: others =>
    Put_line (Current_Error,
       "Error in SessionHandler.HTML_Set_HistoryStatus");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end HTML_Select_State;

  procedure Print_History is
  begin
    if Hindex >0 then
      Put_Line("Path From Initial State C1 To State "
                    & NickName(CurrentConf,"C") );
      for I in 1..Hindex loop
        declare
          ss: String := Integer'Image(I);
        begin
          ss(1) := 'h';
          Put_Line(ss & ") " & HistoryStep(I).all );
        end;
      end loop;
      Put_Line("--------------------------------------------------");
    end if;
    --
  end Print_History;
  
  procedure Display_Status is
  begin
    --  Display Path to Current State
    Print_History;
    -- Display Current State Link
--?    Print_Current_State;
    -- Display abstract action potentialities
      Print_Abstract_State_Label_Images(CurrentConf);
    -- Display possible evolutions
    Print_Possible_Evolutions(Next);
  end Display_Status;

  procedure Select_History (Input_Line: String) is 
    Index: Natural;
  begin
    Put_Line("--------------------------------------------------");
    begin
      Index := Integer'Value(Input_Line(Input_Line'First+1..Input_Line'Last));
      CurrentConf := FindFromKey(HistoryKeys(Index));
      Hindex := Index -1;
      Display_Status;
   exception
   when Event: others =>
     Put_line (Current_Error, "Error in Select_History");
     Put_Line (Current_Error, Exception_Name(Event));
     Put_Line (Current_Error, Exception_Message(Event));
   end;
   Put_Line("--------------------------------------------------");
  end Select_History;

  procedure Select_State (Input_Line: String) is
    Index: Natural;
  begin
    Put_Line("--------------------------------------------------");
    begin
      Index := Integer'Value(Input_Line(Input_Line'First+1..Input_Line'Last));
      CurrentConf := GetProgressive(Index);
      Display_Status;
   exception
   when Event: others =>
     Put_line (Current_Error, "Error in Select_State");
     Put_Line (Current_Error, Exception_Name(Event));
     Put_Line (Current_Error, Exception_Message(Event));
   end;
   Put_Line("--------------------------------------------------");
  end Select_State;

  
  procedure HTML_Select_Evolution (HTML_Filename:String; Input_Line: String) is
    Index: Natural;
  begin
      Index := Integer'Value(Input_line);
      if Index in Next.all'Range then
        Hindex :=Hindex+1;
        HistoryKeys(Hindex):= GetUniqueKey(CurrentConf);
        HistoryStep(Hindex):= 
          new String'(
             "<a href='javascript:top.sendcommand(""h" &
             Integer'Image(HIndex)(2..Integer'Image(HIndex)'length) & """)'>" &
             NickName(CurrentConf,"C") & "</a>"  &
             " --> "  & 
             NickName(Get_Target(Next(Index)),"C") &
             "&nbsp;" & HTML_Format(Abstract_Label_Image(Next(Index))) &
             "<label href=""index.htm"" onmouseover=""Tip(" &         -- TOOLTIP PREFIX
             "'/*&nbsp;'+" & HTML_Literal_Hack(Ground_Label_Image(Next(Index))) & 
             "+'&nbsp;*/'"  &
             ")"" onmouseout=""UnTip()"">" &
             "<FONT color=""blue"">&nbsp;/* ... */</font></label>" -- TOOLTIP
            );
        CurrentConf := Get_Target(Next(Index));
      end if;
      -- HTML_Display_Status(HTML_FileName);    -- Details hidden
      HTML_Configuration_Info (HTML_FileName);  -- Details expanded
    exception
    when Event: others =>
      Put_line (Current_Error, "Error in HTML_Select_Evolution");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
  end HTML_Select_Evolution;

  procedure Select_Evolution (Input_Line: String) is
    Index: Natural;
  begin
      Index := Integer'Value(Input_line);
      if Index in Next.all'Range then
          Hindex :=Hindex+1;
        HistoryKeys(Hindex):= GetUniqueKey(CurrentConf);
          if not Flags.Terse then
          HistoryStep(Hindex):=
            new String'(NickName(CurrentConf,"C") & " --> "  &
                        NickName(Get_Target(Next(Index)),"C") &
                        Abstract_Label_Image(Next(Index))  & 
                        " /* " & Ground_Label_Image(Next(Index)) & " */" 
                       );
          else
          HistoryStep(Hindex):=
            new String'(NickName(CurrentConf,"C") & " --> "  &
                        NickName(Get_Target(Next(Index)),"C") &
                        Abstract_Label_Image(Next(Index)) 
                       );
          end if;
        CurrentConf := Get_Target(Next(Index));
      end if;
      Display_Status;
    exception
    when Event: others =>
      Put_line (Current_Error, "Error in Select_Evolution");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
  end Select_Evolution;
        
  procedure HTML_EvaluateIt (Elog_FileName: String;
                             Out_FileName: String;
                             Eval_OK: out Boolean)  
     renames UCTL.HTML_EvaluateIt;

  procedure EvaluateIt (Eval_OK: out Boolean) renames UCTL.EvaluateIt;


  procedure HTML_ExplainEvaluation (HTML_FileName: String;Comp: Integer :=1)  
      renames UCTL.HTML_ExplainEvaluation;


  procedure ExplainEvaluation(Comp: Integer :=1)   
      renames UCTL.ExplainEvaluation;


  procedure Start_ModelExploration is
   begin
    begin
      CurrentConf := Initial_Configuration;
      Hindex := 0;
      HTML_Mode := False;
      Display_Status;
    exception
    when Event: others =>
      Put_line (Current_Error,"Error in Start_ModelExploration");
      Put_Line (Current_Error, Exception_Name(Event));
      Put_Line (Current_Error, Exception_Message(Event));
      raise;
    end;
  end Start_ModelExploration ;


  procedure LoadModel (Model_FileName: String;
                        LoadOK: out Boolean) is
  begin
    --
--    Verbose_Eval := True;
    --
    LoadOK := True;
    Load_Model(Model_FileName);
    Start_ModelExploration;
  exception
  when Parsing_Error =>
    LoadOK := False;
  when Event: others =>
    LoadOK := False;
    Put_line (Current_Error,"Error in LoadModel");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end LoadModel;


 ----------------------- PARALLLEL MODEL GENERATION -------------------

  ALLDONE: Boolean := False with Atomic;

  procedure ModExplore (I: Natural; Max: Natural)  is
    NextItem: Natural;
    This_Iter: Evolutions_Iterator;
  begin
    NextItem := I;
    while not ALLDONE loop
      if Configurations.Kernel.StatesSpace_Size >= NextItem+Max then
        Iterator_Initialize (This_Iter,System_Configuration(NextItem));
        Iterator_Finalize(This_Iter);
        NextItem := NextItem + Max;
      else
        delay 0.00001;
      end if;
    end loop;
  end ModExplore;

    task type ModWorker(I: Natural; Max: Natural) with priority => System.Default_priority-2 is
    end ;
    task body ModWorker is
    begin
      ModEXplore(I, Max);
    exception
    when Event: others =>
       Put_Line (Current_Error, Exception_Name(Event));
       Put_Line (Current_Error, Exception_Message(Event));
       Put_line(Current_Error, "++++++++  ModWorker dying ... ++++++++++++++");
    end ModWorker;

  procedure DFLEXplore is separate;

    task type worker3 with priority => System.Default_priority-1 is
       PRAGMA STORAGE_SIZE( taskstacksize);
    end worker3;
    task body worker3 is
    begin
      DFLEXplore;
    exception
    when Event: others =>
       Put_Line (Current_Error, Exception_Name(Event));
       Put_Line (Current_Error, Exception_Message(Event));
       Put_line(Current_Error, "++++++++  Worker3 dying ... ++++++++++++++");
    end worker3;

  procedure BFEXplore is separate;

    task type worker4 with priority => System.Default_priority-1;
    task body worker4 is
    begin
      BFEXplore;
    exception
    when Event: others =>
       Put_Line (Current_Error, Exception_Name(Event));
       Put_Line (Current_Error, Exception_Message(Event));
       Put_line(Current_Error, "++++++++  Worker4 dying ... ++++++++++++++");
    end worker4;


    type  Wref is access ModWorker;
    type  Wref3 is access Worker3;
    type  Wref4 is access Worker4;
------------------------------------------------------------------------

  procedure Load_and_Eval (Model_FileName:String; Formula_FileName: String; OK : out Boolean) is
    WW: Wref;
    W3: Wref3;
    W4: Wref4;
  begin
--    Verbose_Eval := True;
    OK := True;
    Load_Model(Model_FileName);
    if NonInteractive and 
       (Flags.ModelCores >=1 and Flags.ModelCores <=16) then
      ALLDONE := False;
      for K in 1..Flags.ModelCores loop
        WW := new ModWorker(K,Flags.ModelCores);
      end loop;
      EvalFromFile (Formula_FileName,OK);
      ALLDONE := True;
    elsif NonInteractive and 
       (Flags.ModelCores =20) then
      ALLDONE := False;
      W3 := new Worker3;
      EvalFromFile (Formula_FileName,OK);
      ALLDONE := True;
    elsif NonInteractive and
       (Flags.ModelCores =30) then
      ALLDONE := False;
      W4 := new Worker4;
      EvalFromFile (Formula_FileName,OK);
      ALLDONE := True;
    else
      EvalFromFile (Formula_FileName,OK);
      if Flags.AutoExplanations then
        UCTL.ExplainEvaluation;
      end if;
    end if;
  exception
  when Event: others =>
    OK := False;
    Put_line (Current_Error,"Error in Load_and _Eval");
    Put_Line (Current_Error, Exception_Name(Event));
    Put_Line (Current_Error, Exception_Message(Event));
  end Load_and_Eval;

  procedure HTML_EvalFromFile (HTML_FileName: String; Form_FileName: String; Eval_OK: out Boolean) 
      renames UCTL.HTML_EvalFromFile;

  procedure EvalFromFile (Form_FileName: String; Eval_OK: out Boolean)  renames UCTL.EvalFromFile;

  procedure EvalFromString(FormCode: String; Eval_OK: out Boolean)  renames UCTL.EvalFromString;

  procedure Parse_Formula (Formula_FileName: String; Parse_OK: out Boolean) 
      renames UCTL.Parse_Formula;



  procedure HTML_LoadModel (Model_FileName: String; 
                            HTML_FileName: String;  LoadOK: out Boolean) is
   Data_File: File_Type;
   HTML_File: File_Type;
  begin
    --
    Verbose_Eval := True;
    --
    LoadOK := True;
    --
    Create(Data_File,Out_File,HTML_FileName);
    Put_Line(Data_File, "<html><body bgcolor='white'>");
    Put_line(Data_File, "<script type=""text/javascript"" src=""wz_tooltip.js""></script>");
    Put_Line(Data_File,"<center>");
    Put_Line(Data_File,
              "<div align=left style=""background-color:white;width:95%;height:100%"">");
    Put_Line(Data_File , "<pre>");
    Set_Error(Data_File);
    begin
      Load_Model(Model_FileName);
      Create(HTML_File,OUT_FILE,"model.html");
      HTML_Print_Model(HTML_File);  -- resets Current_output to Standard_Output
      Close(HTML_File);
    exception
    when Parsing_Error =>
      LoadOK := False;
      Put_Line (Data_File, "</pre>");
      Put_Line (Data_File, "</div></center></body></html>");
      Set_Error(Standard_Error);
      Close(Data_File);
      return;
    when Event: others =>
      LoadOK := False;
      Put_line (Data_File,"Error in HTML_LoadModel");
      Put_Line (Data_File, Exception_Name(Event));
      Put_Line (Data_File, Exception_Message(Event));
      Put_Line (Data_File, "</pre>");
      Put_Line (Data_File, "</div></center></body></html>");
      Set_Error(Standard_Error);
      Close (Data_File);
      return;
    end;
    Set_Error(Standard_Error);
    Close (Data_File); 
    --
    --  in case of success calls also HTML_ModelExploration  
    HTML_Start_ModelExploration(HTML_FileName); 
    --
  exception
  when Event: others =>
    LoadOK := False;
  end HTML_LoadModel;

  procedure Set_Ground is
  begin
     null;
  end Set_Ground;

  procedure Set_Abstract(OK: out Boolean) is
  begin
    OK := True;
      -- MAYBE we should clear prevous computations is the mode was different ?
  end Set_Abstract;

  procedure Debug(Str:String) is
  begin 
    null;
  end;

  procedure Dump_States is
  begin
     Configurations.Kernel.Dump_States;
  end Dump_States;

  procedure GenerateDOTfile(Root:Integer:=1) is
-----------------------------------------------------------------------
-- "$BIN_DIR/mc2dot  -s2000 -a -b model.txt -o $WORKPLACE.ltsmodel.dot"
-----------------------------------------------------------------------
   package MCDotGen is new DotGen (Configurations);
--   package MCDotGen renames DotGen;
  begin
    DOTLIB.Beautify := True;                                --  -b
    DOTLIB.Encode_State_Labels := False;                    --  -b
    MCDOTGEN.RootState := Root;                             --  dotgenCrootstate
    MCDOTGEN.Use_Abstract_Labels := True;                   --  -a
    MCDOTGEN.Use_Ground_Labels := False;                    --  -a  (-g)
    MCDOTGEN.States_Limit := 200;                           --  -s200
    MCDOTGEN.SetOutput := True;                             --  -o
    MCDOTGEN.OutputFileName := new String'("ltsmodel.dot"); --  -o ltsmodel.dot
--    FLAGS.Product_Families := False;                        --  umc
--    FLAGS.Expanded_Tau := True;                             --  umc
    CONFIGURATIONS.MemorySaving := False;
    MCDOTGEN.mc2dot;
  end GenerateDOTfile;

begin
  null;
end MC;
