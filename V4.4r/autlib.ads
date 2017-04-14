package autLib is
  -- UTILIZZATO DA MC2AUT:
  -- dato un modello, genera:
  -- * un automa "tracemodel.aut" in formato .aut, dove eventuali state labels
  --   sono codificate nelle edge labels, senza "tau" transition, ma con "#loop" e #final"
  -- * un automa "flatmodel.aut" in formato .aut, dove eventuali state labels
  --   sono codificate nelle edge labels, dove "[labelss] {} [labels]"  diventa "tau"
  ----------------------------------------------------------------------------
  Remove_Tau_Transitions: Boolean := False;   -- option "-t"
  Encode_State_Labels: Boolean := False;      -- option "-l" 
  Beautify: Boolean := False;                 -- option "-b"
  ----------------------------------------------------------------------------

  procedure Print_Dot_Header(Initial: String);

  procedure Print_Dot_Node (nick: String; labels: String);

  procedure Print_Dot_Edge (sourcenick: String; 
                             sourcelabels: String;
                             targetlabels: String;   -- actually unused
                             actionlabels: String; 
                             targetnick: String);

  procedure Print_Dot_Truncated_Node (nick: String; sourcelabels: String);
  
  procedure Print_Dot_Loop (nick: String; sourcelabels: String);

  procedure Print_Dot_Final (nick: String; sourcelabels: String);

  procedure Print_Dot_Footer;

  function StrippedMust(Src: String) return String; 
  function StrippedMay(Src: String) return String; 
  function IsmayEdge(actionlabels: String) return Boolean;
  function IsmustEdge(actionlabels: String) return Boolean;

end autLib;

------------------------------------------------------------------------------
-- libreria usata in mc2dot ed in aut2dot  (e dot2dot)
--  
--   -g   use le ground labels come edge labels
--        INCOMPARTIBILE CON -a  (o no???)
--  
--   -a   usa le abstract labels come edge labels
--        INCOMPARTIBILE CON -g  (o no???)
--  
--   -l   mette le labels degli stati sulle lables degli edges (per usare minimizzatori su lts)
--        aggiunge #final  agli stati finali,  (se non c'e' -r) aggiunge trasforma {} in tau
--        INCOMPATIBILE CON -b
--  
--   -b   lascia le labels degli stati sugli stati,
--        con -r trasforma #lopp, con -l  trasforma #final, con -d trasforma #truncated
--        se le edge labels codificano state labels le ripulisce.
--        INCOMPATIBILE CON -l
--------------------
--  USI:
--  
--  -d -b -g   --- visualizzazione parziale di grafi ground
--  -d -b -a   --- visualizzazione parziale di grafi abstract
--  
--  -d -r -l -a  -- prepara LTS per minimizzazzione trace
--  -d -l -a  -- prepara LTS per minimizzazzione branching/strong
--  
--  -b generazone da aut a dot
------------------------------------------------------------------------------

