with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Flags; use Flags;
with Ada.Exceptions; use Ada.Exceptions;
with DotLib; use DotLib;
procedure Aut2dot is
-- --------------------------------------------------------------------------
-- Legge le prima di riga con il numero di nodi e edges e inizializza le strutture
-- Fa il parsing del modello .aut
-- Genera in modo breadth first i primi <maxsize> edges 
--  (visualizzanti gli stati finali ed i trocamenti)
-- Le state labels sono estratte dagli edges e rimesse nei nodi.
-- Non esistono azioni "tau" (senza srsLabels/tgtLables).
-- --------------------------------------------------------------------------
  type Edge is record
     actLabels: String_Ref := Null_String;
     tgt: Natural;
  end record;
  type Edges_Table is array (Positive range <> ) of Edge;
  type Edges_Table_Ref is access Edges_Table;
  procedure Free is new Ada.Unchecked_Deallocation(Edges_Table, Edges_Table_Ref);
  
  type Node is record
     statelabels: String_Ref := Null_String;
     edges: Edges_Table_Ref;
     booked: Boolean := False;
  end record;
  type Nodes_Table is array(Positive range <>) of Node;
  type Nodes_Table_Ref is access Nodes_Table;
  
  NodesCount: Positive;
  AllNodes: Nodes_Table_Ref;
  
  procedure Add_Edge(src:Positive; actLabels: String_Ref; tgt: Natural) is
    ThisEdge: Edge := (actLabels,tgt);
  begin
    if AllNodes(src).edges = null then
       AllNodes(src).edges := new Edges_Table'(1 => ThisEdge);
    else
      declare
         OldEdges: Edges_Table_Ref := AllNodes(src).edges;
         NewEdges: Edges_Table_Ref := new Edges_Table'(OldEdges.all & ThisEdge); 
      begin
         AllNodes(src).edges:= NewEdges;
         Free(OldEdges);
      end;
    end if;
  end Add_Edge;
  
----------------------------------------------
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
  SetOutput: Boolean := False;    -- option "-o filename.dot"
  OutputFileName: access String;
  maxsize: Natural := 600;        -- option "-s<nn>" maximum number of edges visualized
  cursize: Natural :=0;
  initnode: Positive :=1;         -- option "-i<nn>" initial node  (autnodenum+1)

  ----------------------------------------------------------------
  -- Reads the file "Model_FileName.all", initializes AllNodes,
  -- and fills in all the data
  ----------------------------------------------------------------
  procedure Parse_Aut_File is
    InputFile: File_Type;
    InputLine: String(1..1000);
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
    Index1,Index2: Natural;
  begin
    Open (InputFile, In_File, Model_FileName.all);
    ---------------------------------------------------------------
    --  Check the first line:  "des (0,edgescount,nodescount)"
    --  and initialises AllNodes
    ---------------------------------------------------------------
    Get_Line(InputFile,InputLine,InputSize);
    if InputSize <7 or else InputLine(1..7) /= "des (0," then
       Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
       Put_Line (Current_Error, "expected ""des (0, ...""");
       Put_Line(Current_Error, "The input file """ & Model_FileName.all & 
          """ does not look like a CADP/Aldebaran "".aut"" file.");
       return;
    end if;
    Index1 :=8; 
    while Index1 <= InputSize and InputLine(Index1) /= ',' loop
      Index1:= Index1+1;
    end loop;
    Index1:= Index1+1;
    -- "des (0,edgescount,nodescount)"
    --                    ^
    Index2:= Index1+1;
    while Index2 <= InputSize and InputLine(Index2) /= ')' loop
      Index2:= Index2+1;
    end loop;
    if Index2 > InputSize then
       Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
       Put_Line (Current_Error, "expected ""des (0,edgescount,nodescount)""");
       Put_Line (Current_Error, "The input file """ & Model_FileName.all &
          """ does not look like a CADP/Aldebaran "".aut"" file.");
       return;
    end if;
    NodesCount := Integer'Value(InputLine(Index1..Index2-1));
    AllNodes := new Nodes_Table(1..NodesCount);
    --
    ---------------------------------------------------------------  
    -- parse all the other transition lines   (n,"...",m)
    --  (n,"tau",m)
    --  (n,"{act,act}",m)
    --  (n,"SpecialLabels",m)
    --  (n,"[sourcelabels][targetlabels]{act,act}",m)
    --  (n,"[sourcelabels]SpecialLabels",m)
    --    where SpecialLabels =  #final, #loop, #truncated, #statelabel, 
    --          sourcelabels/targetlabels = {aa,bb} 
    ---------------------------------------------------------------
    while not End_Of_File(InputFile) loop
      Get_Line(InputFile,InputLine,InputSize);
      CurrentLine := CurrentLine+1;
      InputPos := 1;
      --
      -- Skip initial '('
      --
      --  (n,"[sourcelabels]{act,act}[targetlabels]",m)
      --  ^
      if InputLine(InputPos) /= '(' then
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      InputPos := 2;
      SourceFrom :=2;
      --
      -- PARSE THE STATE NUMBER nn
      --
      --  (nn,"[{sourcepreds}]{act,act}[{targetpreds}]",m)
      --   ^
      if InputLine(InputPos) not in '0'..'9' then
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) & 
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      InputPos := InputPos +1;
      while InputPos <= InputSize and InputLine(InputPos) in '0'..'9' loop
        InputPos := InputPos+1;
      end loop;
      SourceTo := InputPos - 1;
      if SourceTo > InputSize  then
       Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
       Put_Line (Current_Error, "expected: ""(srcnum,label,tgtnum)""");
       Put_Line (Current_Error, "The input file """ & Model_FileName.all &
          """ does not look like a CADP/Aldebaran "".aut"" file.");
         return;
      end if;
      ----------------------------------------------------------------
      SourceState:= Integer'Value(InputLine(SourceFrom..SourceTo))+1;
      ----------------------------------------------------------------
      --
      -- Skip the ',' after the source state number n
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",m)
      --     ^
      if InputLine(InputPos) /= ',' then
         Put_Line(Current_Error, 
            "Line " &  Integer'Image(CurrentLine) & 
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;      
      InputPos := InputPos +1;
      -- 
      -- Skip initial  '"' of transition label
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",m)
      --      ^
      if InputLine(InputPos) /= '"' then
         Put_Line(Current_Error,
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      SourceLabelsFrom := 1;
      SourceLabelsTo := 0;
      InputPos := InputPos +1;
      --
      -- PARSE THE SOURCE STATE LABELS inside the first [..] 
      -- (if present, and not already known)
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",m)
      --       ^
      if InputLine(InputPos) = '[' then
        InputPos := InputPos+1;
        SourceLabelsFrom := InputPos;
        while InputPos <= InputSize and 
           (InputLine(InputPos) /= ']' or else 
            (InputLine(InputPos+1) /= '[' and 
             InputLine(InputPos+1) /= '{' and 
             InputLine(InputPos+1) /= 't' and 
             InputLine(InputPos+1) /= '"' and 
             InputLine(InputPos+1) /= '#')) loop
          InputPos := InputPos+1;
        end loop;
        SourceLabelsTo := InputPos-1;
        InputPos := InputPos+1;
        if InputPos > InputSize  then
         Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
         Put_Line (Current_Error, "expected: ""(srcnum,""[statelabels]..."",tgtnum)""");
         Put_Line (Current_Error, "The input file """ & Model_FileName.all &
            """ does not look like a UMC/Aldebaran "".aut"" file.");
           return;
        end if;
        ----------------------------------------------------------------
        -- NOTICE WE ASSUME THAT ALL NODES HAVE OUTGOING EDGE (e.g. #final)
        -- THE NODE STATELABELS ARE TAKEN FROM THERE!
        if AllNodes(SourceState).statelabels = Null_String then
          AllNodes(SourceState).statelabels := 
             new String'(InputLine(SourceLabelsFrom..SourceLabelsTo));
        end if;
        ----------------------------------------------------------------
      end if;
      --
      -- PARSE THE ACTION LABELS (#special or {} or tau) until the final '"'
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",mm)
      --                     ^
      ActionLabelsFrom := 1;
      ActionLabelsTo := 0;
      if InputLine(InputPos) = '{' then
        ActionLabelsFrom := InputPos;
        InputPos := InputPos+1;     
        while InputPos <= InputSize and InputLine(InputPos) /= '}' loop
          InputPos := InputPos+1;
        end loop;
        ActionLabelsTo := InputPos;
        InputPos := InputPos+1;
        if InputPos > InputSize  then
         Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
         Put_Line (Current_Error, "expected: ""(srcnum,""...{...}..."",tgtnum)""");
         Put_Line (Current_Error, "The input file """ & Model_FileName.all &
            """ does not look like a UMC/Aldebaran "".aut"" file.");
           return;
        end if;
        --
      elsif InputLine(InputPos) = '#' then
        ActionLabelsFrom := InputPos;
        InputPos := InputPos+1;     
        while InputPos <= InputSize and InputLine(InputPos) /= '"' loop
          InputPos := InputPos+1;
        end loop;
        ActionLabelsTo := InputPos-1;
        if InputPos > InputSize  then
         Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
         Put_Line (Current_Error, "expected: ""(srcnum,""...#special"",tgtnum)""");
         Put_Line (Current_Error, "The input file """ & Model_FileName.all &
            """ does not look like a UMC/Aldebaran "".aut"" file.");
           return;
        end if;
      end if;
      
      --
      -- SKIP THE TARGET STATE LABELS inside the second [..]
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",mm)
      --                              ^
      TargetLabelsFrom := 1;
      TargetLabelsTo :=0;
      if InputLine(InputPos) = '[' then
        InputPos := InputPos+1;
        TargetLabelsFrom := InputPos;
        while InputPos <= InputSize and 
          (InputLine(InputPos) /= ']' or else             
            (InputLine(InputPos+1) /= '"' )) loop
          InputPos := InputPos+1;
        end loop;
        TargetLabelsTo := InputPos-1;
        InputPos := InputPos+1;
        if InputPos > InputSize  then
         Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
         Put_Line (Current_Error, "expected: ""(srcnum,""...[...]"",tgtnum)""");
         Put_Line (Current_Error, "The input file """ & Model_FileName.all &
            """ does not look like a UMC/Aldebaran "".aut"" file.");
           return;
        end if;
      end if;
      --
      -- Skip the last '",'
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",mm)
      --                                             ^
      if InputLine(InputPos) /= '"' or  InputLine(InputPos+1) /= ',' then
         Put_Line(Current_Error,
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      InputPos := InputPos+2;
      --
      -- PARSE THE TARGET STATE number
      --
      --  (nn,"[sourcelabels]{act,act}[targetlabels]",mm)
      --                                              ^
      TargetFrom := InputPos;
      while InputPos <= InputSize and InputLine(InputPos) in '0'..'9' loop
        InputPos := InputPos+1;
      end loop;
        if InputPos > InputSize  then
         Put_Line (Current_Error, "found: """ & InputLine(1..InputSize) & "**");
         Put_Line (Current_Error, "expected: ""(srcnum,""..."",tgtnum)""");
         Put_Line (Current_Error, "The input file """ & Model_FileName.all &
            """ does not look like a UMC/Aldebaran "".aut"" file.");
           return;
        end if;
      TargetTo := InputPos-1;
      TargetState := Integer'Value(InputLine(TargetFrom..TargetTo))+1;
      --
      if InputLine(InputPos) /= ')' then
         Put_Line(Current_Error,
            "Line " &  Integer'Image(CurrentLine) &
            ": Unexpected data found at column " & Integer'Image(InputPos) );
      end if;
      ----------------------------------------------------------------
      Add_Edge(SourceState, new String'(InputLine(ActionLabelsFrom..ActionLabelsTo)),TargetState);
      ----------------------------------------------------------------
    end loop;
    --
    Close (InputFile);
    exception
       when others => 
         Put_Line(Current_Error, 
            "aut2dot: Unexpected error at line " &  Integer'Image(CurrentLine));
         raise;
  end Parse_Aut_File;

-- ===========================================================================
-- ===========================================================================
-- ===========================================================================
   DOTOUT: File_Type;
   truncated: Boolean := False;
   SRC_truncated: Boolean := False;
   --
   MaxBreadth: Natural := 1_000_000;  -- size of the two System_Configuration_Tables used)
   CurTable: Natural :=1;
   LastWidth: Natural;
   T1: Num_Table_Ref  := new  Num_Table(1..MaxBreadth);
   I1: Natural :=0;
   --
   T2: Num_Table_Ref:= new Num_Table(1..MaxBreadth);
   I2: Natural :=0;
     
   ---------------------------------------------------------
   -- if Depth mod2=1 adds Conf in T2
   -- else adds Conf in T1
   ---------------------------------------------------------
   procedure Add_New_Configuration (Conf: Natural) is
   begin
      -- if Already_Seen(Progressive(Conf)) then return; end if;
      --
      --  CHECK ALREADY DONE IN LAST_WEAK_EXPLORE
      --if Already_Booked(Progressive(Conf)) then return; end if;
      --MarkBooked(Progressive(Conf));
      --
      if CurTable =1 then
         -- adds Conf in T1
         I1 := I1 +1;
         if I1 not in T1'Range then
           Put_Line(Current_Error, "Ooops: Nodes_Table too small ..." & Integer'Image(I1));
           raise Program_Error;
         end if;
         T1(I1) := Conf;
      else
         -- adds Conf in T2
         I2 := I2 +1;
         if I2 not in T2'Range then
           Put_Line(Current_Error, "Ooops: Nodes_Table too small ..." & Integer'Image(I2));
           raise Program_Error;
         end if;
         T2(I2) := Conf;
      end if;
   end Add_New_Configuration;
   ---------------------------------------------------------
   
    
   ---------------------------------------------------------
   -- 1) AllNodes.Edges can never be NULL
   -- 2) All nodes have an outgoing edge (either #final, #truncated or else)
   ---------------------------------------------------------
   procedure PRINT_INFO(Current: Natural) is
      tgt: Natural;
      isFinal:Boolean := False;
      isTruncated:Boolean := False;
   begin
     if AllNodes(Current).Edges = null then
       -- do nothing for really final nodes (if any) (they should not exixt)
       return;
     end if;
     --
     for I in AllNodes(Current).Edges.all'Range loop
        --
       if AllNodes(Current).Edges(I).actLabels.all = "#final" then
           isFinal := True;
           --
       elsif AllNodes(Current).Edges(I).actLabels.all = "#truncated" then
           isTruncated := True;
           SRC_truncated := True;
           --
       elsif AllNodes(Current).Edges(I).actLabels.all = "#loop" then
           --  adds self loop without label
           Print_Dot_Loop("W" & Trim(Integer'Image(Current),Left),"");
           --
       else
          tgt := AllNodes(Current).Edges(I).tgt;
          -- adds node with action labels and two edges:  src->act, act->tgt
          cursize := cursize+1;
          Print_Dot_Edge ("W" & Trim(Integer'Image(Current),Left),
                          "unrelevant", "unrelevant",
                          AllNodes(Current).Edges(I).actLabels.all,
                          "W" & Trim(Integer'Image(tgt),Left));
          --
          if AllNodes(tgt).booked= False then
             Add_New_Configuration(tgt);
             AllNodes(tgt).booked := True;
          end if;
       end if;
     end loop;
     --
     if isFinal then
       -- adds double periphery
       Print_Dot_Final("W" & Trim(Integer'Image(Current),Left), 
                           AllNodes(Current).statelabels.all);
     elsif isTruncated then
           -- THIS IS A CASE OF SRC_MODEL TRUNCATED !!!!!!
           --  SHOULD BE PRESENTED DIFFERENTLY
           Print_Dot_End("W" & Trim(Integer'Image(Current),Left),
                                    AllNodes(Current).statelabels.all);
     else
       Print_Dot_Node ("W" & Trim(Integer'Image(Current),Left),
                      AllNodes(Current).statelabels.all);
     end if;
   end PRINT_INFO;

  ----------------------------------------------------------------------------
  -- Partendo da AllNodes(initnode), analizza le evoluzioni in modo Breadth First
  -- e genera il grafo ".dot" troncandolo dopo maxsize edges.
  ----------------------------------------------------------------------------
   procedure BreadthFirstExplore  is
   begin
     Beautify := True;
     Encode_State_Labels := False;
     --
     Print_Dot_Header("W" & Trim(Integer'Image(initnode),Left));
     --       
     T1(1) := initnode;
     I1 := 1;
     --
     while I1 /= 0 loop
       LastWidth := I1;
       CurTable := 2;
       I2 := 0;  -- prepare T2 for getting next round of items
       for I in 1..I1 loop
          -- PRINT info for node AllNodes(T1(I))
--         cursize := cursize+1;
         if cursize > maxsize then
           truncated := True;
           -- adds edge to triangle (??)
           Print_Dot_Truncated_Node("W" & Trim(Integer'Image(T1(I)),Left),
                                   AllNodes(T1(I)).statelabels.all);
         else
           PRINT_INFO(T1(I));
         end if;
       end loop;
       --
       if I2 = 0 then exit; end if;
       --
       LastWidth := I2;
       CurTable := 1;
       I1 := 0;  -- prepare T1 for getting next round of items
--       cursize := cursize+1;
       if cursize > maxsize then
         truncated := True;
       end if;
       for I in 1..I2 loop
          -- PRINT info for node AllNodes(T2(I))
         cursize := cursize+1;
         if cursize > maxsize then
           truncated := True;
           -- adds edge to triangle (??)
           Print_Dot_Truncated_Node("W" & Trim(Integer'Image(T2(I)),Left),
                                    AllNodes(T2(I)).statelabels.all);
         else
           PRINT_INFO(T2(I));
         end if;
       end loop;
       --
       if I1 = 0 then exit; end if;
       --
     end loop;
     --
     if SRC_truncated then
       Put_Line ("WARNING" & " [shape=rectangle,color=red,fontcolor=red,peripheries=2," &
                 "label=""" & " Visualization of Graph Truncated after" &
                 Integer'Image(maxsize) &  " nodes \n" &
                 "(you can dowload the full L2TS encoded in .aut format)" & """];");
       Put_Line("WARNING -> W" & Trim(Integer'Image(initnode),Left) & 
                 "[color=white];");
     end if;
     --
     Print_Dot_Footer;
     --
   end BreadthFirstExplore;
  
begin
  Argc := 0;
  -----------------------------------------------------
  --  aut2dot trasforms an L2TS encoding into a ".dot" reprsdntation
  --     adjusting #loop, #truncated, #final special labels
  --  Called on the result of a call tp ltsconvert, after a weak trace minimisation
  -----------------------------------------------------
  loop
    Argc := Argc +1;
    if Argc > Ada.Command_Line.Argument_Count  then
       exit;
    end if;
    declare
      Input_Arg : String := Ada.Command_Line.Argument (Argc);
    begin
      Beautify := True;
      Remove_Tau_Transitions := True;
      if Input_Arg = "-o" then
         SetOutput := True;
      elsif SetOutput then
         OutputFileName := new String'(Input_Arg);
         SetOutput := False; 
      elsif Input_Arg(1) /='-' then
         Model_FileName := new String'(Input_Arg);
      elsif Input_Arg'Length > 2 and then
             Input_Arg(1)='-' and then 
             Input_Arg(2)='s' and then 
             Input_Arg(3) in '0'..'9' then
        maxsize := Integer'Value(Input_Arg(3..Input_Arg'Length));
      elsif Input_Arg'Length > 2 and then
             Input_Arg(1)='-' and then 
             Input_Arg(2)='i' and then 
             Input_Arg(3) in '0'..'9' then
        initnode := Integer'Value(Input_Arg(3..Input_Arg'Length));
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
    Put_line(Current_Error, "   -s<n> limit the graph visualization to <n> nodes");
    Put_line(Current_Error, "   -i<n> root the graph at the  W<n> node");
    return;
  end if;
    --
    if OutputFileName /= null then
       Put_Line("aut2dot: parsing " & Model_FileName.all & " ... ");
    end if;
    Parse_Aut_File;
    if OutputFileName /= null then
      Put_Line("aut2dot: parsing " & Model_FileName.all & " DONE!");
      Put_Line("aut2dot: starting Dot Conversion ... ");
    end if;
    if OutputFileName /= null and then OutputFileName.all'Length >0 then
      declare
        DOTOUT: File_Type;
      begin
        Create(DOTOUT,Out_File,OutputFileName.all);
        Set_Output(DOTOUT);
        BreadthFirstExplore;
        Set_Output(Standard_Output);
        Close(DOTOUT);
        Put_Line("aut2dot: Dot Conversion DONE!");
      end;
    else
      BreadthFirstExplore;
    end if;
    --
  end Aut2dot;
