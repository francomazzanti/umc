with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line;
with Flags; use Flags;
package body DotLib is
--  ----------------------------------------------------------------------------
--  Remove_Tau_Transitions: Boolean := False;   -- option "-t"
--  Encode_State_Labels: Boolean := False;      -- option "-l"  (was Labels_As_Nodes)
--  Beautify: Boolean := True;                  -- option "-b"
--  Product_Families: Boolean := False  -- option -f (display may/must transitions)
--  ----------------------------------------------------------------------------
--   -t   remove_tau_transitions: utilizzato per trace minimizations.
--        Se e' presente l'opzione -l  (utilizzata per minimizzare con ltsmin)
--        al fine di identificare full-traces viene aggiunto #final e #loop.
--        Se e' presente -b le transizioni speciale aggiunge sono mostrate in
--        una forma meno efficente ma piu' gradevole.
--
--   -l   mette le labels degli stati sulle lables degli edges (per usare minimizzatori su lts)
--        aggiunge #final  agli stati finali,  (se non c'e' -r) aggiunge trasforma {} in tau
--        Se NON e' presente -t le state-labels sono messe su transizioni ad hoc #statelabels, 
--        altrimenti esse vengono aggiunte agli edges esistenti.
--        Se e' presente -b le transizioni speciale aggiunge sono mostrate in
--        una forma meno efficente ma piu' gradevole.
--
--   -b   Se non e' presente -l lascia le labels degli stati sugli stati,
--        con -t trasforma #lopp, con -l trasforma #final, con -d trasforma #truncated
--        Se le edge labels codificano state labels le ripulisce.
--        Se e' presente -l le transizioni speciale aggiunge sono mostrate in
--        una forma meno efficente ma piu' gradevole.
--   RIASSUNTO
--  senza opzioni: statelabels messe dentro gli stati, nessuna transizione speciale aggiunta.
--                 edge labels messe come transition labels.
--    -b         : come senza opzioni, ma edge labels messe come stati di tipo text (UMC ground ts)
--    -l         : statelabels codificate negli edges labels come transizioni #statelabels
--    -l  -t     : statelabels codificate dentro edges labels standard.
--    -t         : visualizza il grafo da minimizzare come trace, senza tau e con #loop e #final
--  ----------------------------------------------------------------------------
   --   Nota sulle trilabels:
   --    (1) -a-> (2)
   --    (1) -a-> (3) Dobbiamo distinguarle!!! quindi bisogna codificare gli edges
   --       come  (1) .(-(1)(2)a-> (2)    e (1) -(1)(3)a-> (3)
--  ----------------------------------------------------------------------------
   --   Nota sulle state labels:
   --  Se ci sono state labels e ci sono azioni TAU (non c'e' l'opzione -t)
   --  e' necessario aggiungere ad ogni nodo (che ha transizioni tau) una transizione
   --   esplicita  ((sourcelabels))#statelabels in modo da essere in grado di ricostruire
   --  la struttura corretta (beautified) dopo una eventuale minimizzazione strong.
   --  Per semplicita la transizione #statelabels viene aggiunta atutti i nodi da Print_Dot_Node
--  ----------------------------------------------------------------------------
--  procedure Print_Dot_Header(Initial: String);
--  procedure Print_Dot_Node (nick: String; labels: String);
--  procedure Print_Dot_Edge (sourcenick: String;
--                             sourcelabels: String;
--                             targetlabels: String;   
--                             actionlabels: String;
--                             targetnick: String);
--  procedure Print_Dot_Truncated_Node (nick: String; sourcelabels: String);
--  procedure Print_Dot_End (nick: String; sourcelabels: String);
--  procedure Print_Dot_Loop (nick: String; sourcelabels: String);
--  procedure Print_Dot_Final (nick: String; sourcelabels: String);
--  procedure Print_Dot_Footer;
--  ----------------------------------------------------------------------------

  Transitions_Count: Natural :=0;  -- to bbuild unique node names


  ---------------------------------------------------------
  -- some some unknown reason, if a label like "{a,b,c}" is used
  -- as a node label, it is actually represented in the graphic
  --  as if it were  "a,b,c" (removing the {})
  -- Escaping { resoves the problem.
  ---------------------------------------------------------
  function Dot_Fix( Str: String) return String is
    res: String(1..Str'Length+2);
  begin
    if Str'Length = 0 or else
        Str(Str'First) /= '{' then
      return Str;
    end if;
    res (2..res'length-1) := Str;
    res(1) := '\';
    res(res'length) := res(res'length-1);
    res(res'length-1) := '\';
    return res;
  end Dot_Fix;

  ----------------------------------------------------------
  --  transform i charatteri '"'  e '"'  in "\""  e "\"" in modo che non
  --  interferiscano con i costrutti dot.
  ----------------------------------------------------------
  function Adjust (Str: String) return String is
    Old: String := Str;
    Count: Natural :=0;
  begin
    for I in Str'Range loop
     if Str(I)='"' and then (I = Str'First or else Str(I-1) /= '\') then
       Count := Count+1; 
     end if;
    end loop;
    if Count =0 then return Str; end if;
    declare
      Changed: String(1..Str'Length+Count);
      Index: Natural :=1;
    begin
    for I in Str'Range loop
     if Str(I)='"' and then (I = Str'First or else Str(I-1) /= '\') then
        Changed(Index..Index+1) := "\""";
        Index := Index+2;
     else
         Changed(Index) := Str(I);
         Index := Index+1;
     end if;
    end loop;
    return Changed;
    end;
  end Adjust;

  ----------------------------------------------------------
  -- inserisce "\n"  tra una label e l'altra  e fra
  -- un parametro e l'altro quando la riga e' piu liunga di 25 chars
  ----------------------------------------------------------
  function Splitted (Str: String) return String is
     N:Natural :=0;
     Result: String(1..2*Str'Length);
     LL: Natural :=0;
  begin
    if Str'Length <25 then return Str; end if;
    for I in Str'Range loop
       N:= N+1;
       LL := LL+1;
       Result(N) := Str(I);
       if Str(I) = ' ' then
         --  labels are separated by ", "
         Result(N+1 .. N+2) := "\n";
         N := N+2;
         LL := 0;
       elsif  Str(I) = ',' and then
              Str(I+1) /= ' ' and then
              LL > 25   then
         Result(N+1 .. N+2) := "\n";
         N := N+2;
         LL := 0;
       end if;
    end loop;
    Return Result(1..N);
  end Splitted;

   procedure Print_Dot_Header(Initial: String) is
   begin
      Put_Line (" digraph mc {" );
      Put_line (" margin=0.5;  ");
--      Put_line (" ranksep=0.2;  ");  --  ADDED 20-09-2010  removed 05-02-2017
      Put_line (" node [style=filled,fillcolor=white,width=0.25, height=0.25, label="""" ];");
      Put_Line (" edge [arrowhead=vee]");
      if Initial /= "C1" and Initial /= "W1" and Initial /= "W0" then
      -- C1 generated by dotgen -a, W1 generated by mc2dot -t, and aut2dot
        if Initial(1)='C' then
          Put_Line ("xC1 [width=0.4,height=0.5,peripheries=3,shape=polygon,sides=3,color=red,comment="""",label=""C1"", " &
            "URL=""javascript:top.showlts('C1');""];");
          Put_Line ( "xC1  -> " & Initial &  " [color=red,style=dotted];");
        elsif Initial(1)='W' then
          Put_Line ("xW1 [width=0.4,height=0.5,peripheries=3,shape=polygon,sides=3,color=red,comment="""",label="""", " &
            "URL=""javascript:top.showtraces('W1');""];");
          Put_Line ( "xW1  -> " & Initial &  " [color=red,style=dotted];");
        end if;
      end if;
      Put_Line (Initial & " [style=bold];");

    --
    --
    -- TRAVERSE THE GRAPH
    --
   end Print_Dot_Header;

   function Img(N: Natural) return String is
     SS : String := Integer'Image(N);
   begin
     SS(1) := 'x';
     return SS;
   end;

   ----------------------------------------------------------------------------
   --  CHIAMATA:    da SHOWLTS per visualizzare le evoluzioni        (Ci \n statelabels)
   --  (opzione -t) dopo ltsconvert pere visualizzare minimal traces (statelables) 
   --
   --  Se non ci sono state labels non occorre fare niente
   --  Se ci sono labels e c'e' l'opzione -l ma e c'e' l'opzione -t non occorre fare niente
   --    (tanto le state labels sono codificate negli edges)
   --  Se ci sono labels  e non c'e' l'opzione -l si inseriscono le labels nel nodo
   --  Se ci sono labels e c'e' l'opzione -l ma non c'e' l'opzione -t 
   --     occorre aggiungere una transizione #statelabels  per gestire poi i TAU
   ----------------------------------------------------------------------------
   procedure Print_Dot_Node (nick: String; labels: String ) is
   begin
     if labels = ""  or labels="{}" then
       if not Encode_State_Labels and nick(nick'First)='C' then
           -- quando si genera un dot file via dotgen (showlts) senza statelabels
           --  si mette solo il nome del nodo "Ci" ed il javascript
              Put_Line (nick & " [label=""" & nick & """" &
                ", URL=""javascript:top.sendcommand('" & nick & "');""," &
                " tooltip="""& nick & """];");
--       else
--         Put_Line (nick & " [, URL=""javascript:top.sendcommand('" & nick & "');""," &
--                " tooltip="""& nick & """];");
       elsif not Encode_State_Labels and nick(nick'First)='W' then
           -- quando si genera un dot file via aut2dot (minimiztrarces) senza statelabels
           --  non si mette nulla
            null;
       end if;
       return;
     end if;
     if Encode_State_Labels and Remove_Tau_Transitions then
         --quando si genera un dotfile senza tau i nodi sono cerchi vuoti
         -- (le state labels sono sugli edges)
        return;
     end if; 
     -- CI SONO labels e NON c'e' -l'opzione -t
     --
     if not Encode_State_Labels and nick(nick'First)='C'  then
           -- quando si genera un dot file via dotgen (showlts) con statelabels
           --  si aggiunge al nome del nodo "Ci" le sue lables ed il javascript
       Put_Line (nick & " [shape=rectangle," &
                " URL=""javascript:top.sendcommand('" & nick & "');""," &
                "label=""" & nick & "\n " & Splitted(Dot_Fix(Adjust(labels))) & """];");
--       Put_Line (nick & " [shape=Mrecord,label=""" & Splitted(Dot_Fix(Adjust(labels))) & """];");
--       Put_Line (nick & " [label=""" & Splitted(Dot_Fix(Adjust(labels))) & """];");
       --
      elsif not Encode_State_Labels and nick(nick'First)='W'  then
           -- quando si genera un dot file via aut2dot (minimizetrace) con statelabels
           --  si aggiunge al nome del nodo "Wi" le sue lables
       Put_Line (nick & " [shape=rectangle," &
                "label=""" & Splitted(Dot_Fix(Adjust(labels))) & """];");
--     Put_Line (nick & " [shape=rectangle," &
--              "label=""" & nick & "\n " & Splitted(Dot_Fix(Adjust(labels))) & """];");
--       Put_Line (nick & " [shape=Mrecord,label=""" & Splitted(Dot_Fix(Adjust(labels))) & """];");
--       Put_Line (nick & " [label=""" & Splitted(Dot_Fix(Adjust(labels))) & """];");
       --
     elsif Encode_State_Labels and not Remove_Tau_Transitions then
       -- ????
       -- ???? non clear when called and used ....
       -- ????
       if Beautify then
         Put_Line ("y" & nick & " [shape=point,width=0.05,height=0.05,color=red];"); 
         Put_Line ( nick & " -> y" & nick &  " [ " &
            "URL=""javascript:top.sendcommand('" & nick & "');""," &
            " color=red,fontcolor=red,arrowhead=none, label=""[" & Dot_Fix(labels) & "]" & 
            "#statelabels" & """ ];");
         Put_Line ("{rank=same;" & nick & "; y" & nick & ";};");
       else
         Put_Line (nick & " -> bottom" & nick &
                    " [URL=""javascript:top.sendcommand('" & nick & "');""," &
                    "label=""[" & Dot_Fix(labels) & "]#statelabels" & """ ];");
       end if;
       --
       -- 
     end if;
   end Print_Dot_Node;


   --------------------------------
   --     {may,aa(..)}  ->  {aa(..)}
   --     {aa(..), may,b}  ->  {aa(..),b}
   --------------------------------
   function StrippedMay(Src: String) return String is
      argindex:Natural :=0;
   begin
     if Src'Length >= 5 then
       for I in Src'First+1..Src'Last-3 loop
         if Src(I..I+2) = "may" and then
             (Src(I+3) = '}' or else Src(I+3) = ',') and then
             (Src(I-1) = ' '  or else
               Src(I-1) = '{' or else
                Src(I-1) = ',') then
           argindex :=I;
           if Src(argindex-1) = ' ' and then Src(argindex-2) = ',' then
             -- {a, may, b}  => {a, b}
             return Src(Src'First..argindex-3) & Src(argindex+3..Src'Last);
           elsif Src(argindex-1) = ',' then
             -- {a,may,b}  => {a,b}
             return Src(Src'First..argindex-2) & Src(argindex+3..Src'Last);
           elsif Src(argindex-1) = '{' then
             -- {may}  => {}
             return Src(Src'First..argindex-1) & Src(argindex+3..Src'Last);
           end if;
           return Src;
         end if;
       end loop;
       return Src;
     else
      return Src;
     end if;
   end StrippedMay;


   --------------------------------
   function StrippedMust(Src: String) return String is
   begin
       return Src;
   end StrippedMust;



   --------------------------
   --     {a(may, ...)}
   --     {a(may)}
   --     {may,a}
   --------------------------
   function IsmayEdge(actionlabels: String) return Boolean is
   begin

     if actionlabels'Length >= 5 then
       for I in actionlabels'First+1..actionlabels'Last-3 loop
         if actionlabels(I..I+2) = "may" and then
             (actionlabels(I+3) = '}' or else actionlabels(I+3) = ',') and then 
             (actionlabels(I-1) = ' '  or else 
               actionlabels(I-1) = '{' or else 
                actionlabels(I-1) = ',') then
           return True;
         end if;
       end loop;
       return False;
     else
      return False;
     end if;
   end IsmayEdge;


   function IsmustEdge(actionlabels: String) return Boolean is
     argindex: Natural :=0;
   begin
     for I in actionlabels'Range loop
       if actionlabels(I) = '(' then
         argindex := I;
         exit;
       end if;
     end loop;
     if actionlabels'Length > 8 and then
          actionlabels(actionlabels'First..actionlabels'First+5)="{must(" then
        return True;
     elsif actionlabels'Length > 6 and then
         actionlabels(actionlabels'First..actionlabels'First+4)="must(" then
        return True;
     elsif actionlabels'Length > 7 and then
          actionlabels(actionlabels'First..actionlabels'First+5)="{must," then
        return True;
     end if;
     if argindex >0 and then
         actionlabels'Last >= argindex+5  and then
         (actionlabels(argindex+1..argindex+5) = "must)" or
          actionlabels(argindex+1..argindex+5) = "must," ) then
       return True;
     else
      return False;
     end if;
   end IsmustEdge;
   
   ----------------------------------------------------------------------------
   --   Nota sulle trilabels:
   --    (1) -a-> (2)
   --    (1) -a-> (3) Dobbiamo distinguarle!!! quindi bisogna codificare gli edges
   --       come  (1) .(-(1)(2)a-> (2)    e (1) -(1)(3)a-> (3)
   --   Nota sulle state labels:
   --  Se ci sono state labels e ci sono azioni TAU (non c'e' l'opzione -t)
   --  e' necessario aggiungere ad ogni nodo (che ha transizioni tau) una transizione
   --   esplicita  ((sourcelabels))#statelabels in modo da essere in grado di ricostruire
   --  la struttura corretta (beautified) dopo una eventuale minimizzazione strong.
   --  Per semplicita la transizione #statelabels viene aggiunta atutti i nodi da Print_Dot_Node
   ----------------------------------------------------------------------------
   procedure Print_Dot_Edge (sourcenick: String; 
                             sourcelabels: String;
                             targetlabels: String; 
                             actionlabels: String; 
                             targetnick: String) is
   begin
     --  Beautify:   
     --    se actionlabels sono "{}":     source  --> target
     --    altrimenti:       source  --> /{actionlabels}/ --> target (aggungendo un nuovo nodo)
     --
     if Beautify and not Encode_State_Labels then
       --
       if actionlabels = "{}" or actionlabels = "" or actionlabels="tau" then
         -- draw the tau edge
         -- skip sourceabels and targetlabels which are printed inside the node
         Put_Line (sourcenick & " -> " & targetnick);
         --
       else
         Transitions_Count := Transitions_Count + 1;

         if IsmayEdge(actionlabels) then
           --   s1  ---may(a)---> s2   ===   s1 - -a- -> s2
           Put_Line (sourcenick & Img(Transitions_Count) &
              " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
              " label = """ & StrippedMay(actionlabels) & """ ];");
           Put_Line (sourcenick & " -> " & sourcenick & Img(Transitions_Count) &
                   " [arrowhead=none,style=dotted];");
           Put_Line (sourcenick & Img(Transitions_Count) & " -> " & targetnick & "[style=dotted];");
           --
         else
            --   s1  ---a---> s2  ===   s1 ---a---> s2
            Put_Line (sourcenick & Img(Transitions_Count) &
              " [shape=plaintext,height=0.3,fontsize=12,style=filled,fillcolor=lightyellow," &
              " label = """ & Adjust(actionlabels) & """ ];");
           Put_Line (sourcenick & " -> " & sourcenick & Img(Transitions_Count) &
                   " [arrowhead=none];");
           Put_Line (sourcenick & Img(Transitions_Count) & " -> " & targetnick & ";");
         end if;
       end if;
     --
     -- Encode_State_Labels:  (ignores Butify)
     --   source   -((..)){..}((..))-> target
     --   source   -((..)){..}-> target
     --   source   -{..}((..))-> target
     --   source   -{..}-> target
     --   source   -tau-> target
     --
     elsif Encode_State_Labels then
         if sourcelabels = targetlabels and then actionlabels ="{}" then
           -- draw the TAU edge and the  #statelabels
           Put_Line (sourcenick & " -> " & targetnick &
              " [ label = ""tau"" ];");
           --
         elsif (sourcelabels /= "" and sourcelabels /= "{}") and 
             (targetlabels /= "" and targetlabels /= "{}") then
           -- draw trilabelled transition label
           Put_Line (sourcenick & " -> " & targetnick &
              " [ label = """ &
              "[" & sourcelabels & "]" & 
                Adjust(actionlabels) & 
              "[" & targetlabels & "]" & 
              """ ];");
           --
         elsif (sourcelabels /= "" and sourcelabels /= "{}") and 
             (targetlabels = "" or targetlabels = "{}") then
           -- draw trilabelled transition label
           Put_Line (sourcenick & " -> " & targetnick &
              " [ label = """ &
              "[" & sourcelabels & "]" & 
                Adjust(actionlabels) & 
              """ ];");
           --
         elsif (sourcelabels = "" or sourcelabels = "{}") and 
             (targetlabels /= "" and targetlabels /= "{}") then
           -- draw trilabelled transition label
           Put_Line (sourcenick & " -> " & targetnick &
              " [ label = """ &
                Adjust(actionlabels) & 
              "[" & targetlabels & "]" & 
              """ ];");
           --
         else  -- sourcelabels=""  targetlabels=""  
           --
           Put_Line (sourcenick & " -> " & targetnick &
              " [ label = """ & Adjust(actionlabels) & """ ];");
         end if;
         --
     else
       -- not Encode_State_Labels   and not Beautify
       --   source   -{..}-> target
       Put_Line (sourcenick & " -> " & targetnick &
          " [ label = """ & Adjust(actionlabels) & """ ];");
     end if;
   end Print_Dot_Edge;

   ----------------------------------------------------------------------------
   --  
   ----------------------------------------------------------------------------
   procedure Print_Dot_Truncated_Node (nick: String; sourcelabels: String) is
   begin
     Print_Dot_Node(nick,sourcelabels);
     --
     if Encode_State_Labels and Remove_Tau_Transitions then
       if not Beautify then
         Put_Line (nick & " -> bottom" & nick & 
            " [ label = ""[" & sourcelabels & "]" & "#truncated" & """ ];");
       else
         Put_Line (nick & " -> " & "x" & nick &
            " [ label = ""[" & sourcelabels & "]" & "#truncated" & """ ];");
         Put_Line ("x" & nick & " [shape=doublecircle,color=red,style=bold,label = "" "" ];");
       end if;
     elsif Encode_State_Labels and not Remove_Tau_Transitions then
       if not Beautify then
         Put_Line (nick & " -> bottom" & nick &
            " [ label = """ & "#truncated" & """ ];");
       else
         Put_Line (nick & " -> " & "x" & nick &
            " [ label = """ & "#truncated" & """ ];");
         Put_Line ("x" & nick & " [shape=doublecircle,color=red,style=bold,label = "" "" ];");
       end if;
     elsif nick(nick'First)='C' then  --not Encode_State_Labels  - showlts
       Put_Line ( " " & nick & " -> " &  "x" & nick & ";");
       Put_Line ("x" & nick & 
         " [width=0.4,height=0.5,peripheries=3,shape=polygon,sides=3,color=red,comment="""",label="""", " &
         "URL=""javascript:top.showlts('" & nick  & "');""  " &
         " ];");
     else --not Encode_State_Labels  - mintraces  - nick(1)='W' 
       Put_Line ( " " & nick & " -> " &  "x" & nick & ";");
       Put_Line ("x" & nick & 
         " [width=0.4,height=0.5,peripheries=3,shape=polygon,sides=3,color=red,comment="""",label="""", " &
         "URL=""javascript:top.showtraces('" & nick  & "');""  " &
         " ];");
     end if;
   end Print_Dot_Truncated_Node;

   ----------------------------------------------------------------------------
   --
   ----------------------------------------------------------------------------
   procedure Print_Dot_End (nick: String; sourcelabels: String) is
   begin
     Print_Dot_Node(nick,sourcelabels);
     Put_Line ( " " & nick & " -> " &  "x" & nick & ";");
     Put_Line ("x" & nick &
       " [width=0.4,height=025,style=filled,shape=rectangle,color=red,comment="""",label="""", " &
       " ];");
   end Print_Dot_End;

   ----------------------------------------------------------------------------
   -- called by mc2dot only when Remove_Tau_Transitions=True
   -- called by aut2dot to beautify #loop edges
   ----------------------------------------------------------------------------
   procedure Print_Dot_Loop (nick: String; sourcelabels: String) is
   begin
     if Beautify and not Encode_State_Labels then
       Put_Line (nick & " -> " & nick );
     elsif Beautify and Encode_State_Labels then
       Put_Line (nick & " -> " & nick & 
        " [color=red,fontcolor=red,label = """ & "#loop" & """ ];");
     else
       if Encode_State_Labels then
         if not Beautify then
           Put_Line (nick & " -> " & "x" & nick &
            " [ label = ""[" & sourcelabels & "]" & "#loop" & """ ];");
         else
           Put_Line (nick & " -> bottom" & nick &
            " [ label = ""[" & sourcelabels & "]" & "#loop" & """ ];");
         end if;
       else -- not Encode_State_Labels
         if not Beautify then
           Put_Line (nick & " -> bottom" & nick &
            " [ label = """ & "#loop" & """ ];");
         else
           Put_Line (nick & " -> " & "x" & nick &
            " [ label = """ & "#loop" & """ ];");
         end if;
       end if;
     end if;
   end Print_Dot_Loop;

   ----------------------------------------------------------------------------
   --  se  -l  e   -t   Aggiunge transizione #final
   --  se  -l  e NON -t non occorre aggiungere nulla perche' le state labels
   --    sono messe in specifiche #statelabels transitions
   --  se -t e NON -l  non occorre fare niente (le state labels sono nel nodo) 
   --   (oppure possimo aggiungere anche #final) 
   --  se NO -l e NON -t  non dobbiamo fare niente di particolare
   ----------------------------------------------------------------------------
   procedure Print_Dot_Final (nick: String; sourcelabels: String) is
   begin
     Print_Dot_Node(nick,sourcelabels);
     if Encode_State_Labels and Remove_Tau_Transitions then
        --  case of old dot generation when removing taus before ltsmin minimizations
       if sourcelabels /= "" then
           Put_Line (nick & " -> bottom" & nick &
             " [label=""[" & sourcelabels & "]#final"" ];");
       else
           Put_Line (nick & " -> bottom" & nick & " [label=""#final"" ];");
       end if;
     else
        -- this is the case of showlts command (calling dotgen)
        -- this is the case of mc2aut command after lts minimization
        --
        Put_Line( nick & "[peripheries=2];");
     end if;
   end Print_Dot_Final;

   procedure Print_Dot_Footer is
   begin
     Put_Line ( "}" );
   end Print_Dot_Footer;

end DotLib;
