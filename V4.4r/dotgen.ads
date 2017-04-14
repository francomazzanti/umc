
-----------------------------------------------------------------------
-- standard calling from UMC equivalent to:
-----------------------------------------------------------------------
-- "$BIN_DIR/mc2dot  -s500 -a -b model.txt -o $WORKPLACE.ltsmodel.dot"
-----------------------------------------------------------------------
--  package MCDOTGEN is new DOTGEN (Configurations);
--  DOTLIB.Beautify := True;                                --  -b
--  DOTLIB.Encode_State_Labels := False;                    --  -b
--  MCDOTGEN.Use_Abstract_Labels := True;                   --  -a
--  MCDOTGEN.Use_Ground_Labels := False;                    --  -a  (-g)
--  MCDOTGEN.States_Limit := 500;                           --  -s500
--  MCDOTGEN.SetOutput := True;                             --  -o
--  MCDOTGEN.OutputFileName := new String'("ltsmodel.dot"); --  -o ltsmodel.dot
--  FLAGS.Product_Families := False;                        --  umc
--  FLAGS.Expanded_Tau := True;                             --  umc
--  CONFIGURATIONS.MemorySaving := False;
--  MCDOTGEN.mc2dot;
-----------------------------------------------------------------------
with Configurations;
generic
 with package MyConfigurations is new Configurations(<>);
package DOTgen is
--  package MyConfigurations renames Configurations;
-- --------------------------------------------------------------------------
--  USED from DOTLIB
--  DOTLIB.Remove_Tau_Transitions: Boolean := False;   -- option "-t"
--  DOTLIB.Encode_State_Labels: Boolean := False;      -- option "-l"
--  DOTLIB.Beautify: Boolean := False;                 -- option "-b"

-- ---------------------------------------------------------------------------
--  USED from DOTGEN
--  DOTGEN.Just_Count: Boolean :=False;                -- option "-c"
--  DOTGEN.States_Limit: Integer := 2_000_000;         -- option "-s"
--  DOTGEN.Depth_Limit: Integer := 500_000;            -- option "-d"
--  DOTGEN.OutputFileName: access String;              -- option "-o"
--  DOTGEN.Use_Ground_Labels: Boolean := True;         -- option "-g"
--  DOTGEN.Use_Abstract_Labels: Boolean := False;      -- option "-a"
--  DOTGEN.Model_FileName: access String;
-- ---------------------------------------------------------------------------
-- USED from FLAGS
--  FLAGS.Product_Families := False;                   -- option "-f"
--  FLAGS.Expanded_Tau: Boolean := False;              -- option "-l"
--  FLAGS.Debug := False;                              -- option -debug
-- ---------------------------------------------------------------------------
-- USED from CONFIGURATIONS
--  MemorySaving                                       -- "-a" "-g"
-- ---------------------------------------------------------------------------
  RootState: Integer :=1;
  States_Limit: Integer := 2_000_000;         -- option "-s"
  Depth_Limit: Integer := 500_000;            -- option "-d"
  SetOutput: Boolean := False;                -- option "-o"
  OutputFileName: access String;
  Use_Ground_Labels: Boolean := True;         -- option "-g"
  Use_Abstract_Labels: Boolean := False;      -- option "-a"
  Model_FileName: access String;
-- ---------------
  Model_OK : Boolean := False;
  Argc: Natural;
  Just_Count: Boolean :=False;
  Aborted: Boolean := False;
-- ---------------
  procedure MC2dot;
------------------
end DOTgen;
