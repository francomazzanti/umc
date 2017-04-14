--with Text_io; use Text_io;
with Unchecked_Conversion;
with Ada.Containers; use Ada.Containers;
separate(UCTL)
package body Computations_DB is
--use Computation_Sets;
use Ada.Text_io;
use Ada.Containers;

--  type Computation_Element is record           -- the structure of actual database items
--    -- identifying part
--    FormImage: String_Ref;
--    Context_Image: String_Ref;
--    StateImage: String_Ref;
--    --  redundart, stable parts
--    Form: Formula_Ref;
--    Context: Computations_Table_Ref;
--    State: System_Configuration;
--    Progressive: String_Ref;
--    --  modifiable part
--    Owner: System_Configuration;
--    Rec_Cref: Computation_Step;
--    Status: Computation_Status;
--    SubComputations:  Computations_Table_Ref;
--    SubEvolutions:  Evolutions_Table_Ref;
--    Last_Exploration: Natural := 0;
--    Max_LTS_Depth: Natural := 0;
--  end record;


   --
   -- used by Start_Exploration  / Already_Explored  (purpose?)
   --
   Explorations_Count : Natural :=0;

   function Null_Computation return Computation_Element is
      Res: Computation_Element;
   begin
      Res.Form := null;
      return res;
   end Null_Computation;

   function Mk_Comp_Key(This_Elem: Computation_Element; 
                   Cache_Max: Natural := Cache_Module) return Positive;

   function Equivalent_Computations (Left, Right : Computation_Element) return Boolean;

   package Comp_DB is new NickNames 
         (Computation_Element,
          Null_Computation,
          Mk_Comp_Key,
          Equivalent_Computations,
          False);  -- thread safety not required           
   use Comp_DB;


--  type BitVector is array(1..Integer'Size) of Boolean;
--  Pragma Pack(Bitvector);
--  for BitVector'Size use Integer'Size;
--  --
--  type BitVector128 is array(1..128) of Boolean;
--  Pragma Pack(Bitvector128);
--  for BitVector128'Size use 128;  -- String_Ref'Size;
--  function To128bits is new Unchecked_Conversion (String_Ref, BitVector128);
--  function ToInt is new Unchecked_Conversion (BitVector,Integer);

--  function Tobits(S1: String_Ref) return BitVector is
--    bits128: BitVector128;
--    Result: BitVector;
--    work: BitVector;
--  begin
--    bits128 := To128bits(S1);
--    Result (1..32) := BitVector(bits128(1..32));
--    Work(1..32) := BitVector(bits128(33..64));
 --   Work(1..32) := Work(25..32) &  Work(17..24) & Work (9..16) & Work(1..8);
 --   Result (1..32) := Result (1..32) xor  Work;
--    Work(1..32) := BitVector(bits128(65..96));
--    Work(1..32) := Work(17..24) & Work (9..16) & Work(1..8) & Work(25..32); 
--    Result (1..32) := Result (1..32) xor  Work;
--    Work(1..32) := BitVector(bits128(97..128));
--    Work(1..32) := Work (9..16) & Work(1..8) &  Work(25..32) &  Work(17..24);
--    Result (1..32) := Result (1..32) xor  Work;
--    return Result;
--  end;

--  function Hashit (This_Table: String_Table) return Integer is
--    WorkBitsI: BitVector;
--    Tmp: BitVector;
--    WorkBits: BitVector;
--    J: Natural;
--  begin
--   WorkBits := Tobits(null);
--   for I In This_Table'Range loop
--     WorkBitsI := Tobits(This_Table(I));
--     J := ((I-1) *10) mod 32;
--     -- rotate This_table(I) J  bits on the right
--     Tmp(1..J) := WorkBitsI(1..J);
--      WorkBitsI(1..32-J) := WorkBitsI(J+1..32);
--      WorkBitsI(32-J+1..32) := Tmp(1..J);
--     WorkBits := WorkBits xor WorkbitsI;
--   end loop;
--   return ToInt(WorkBits);
--  end Hashit;

--  function Mk_Key(S1,S2,S3: String_Ref;
--       Cache_Max: Natural := Global_Env.Cache_Max) return Positive is
--    Result: Integer := 0 ;
--  begin
--    Result := HashIt((S1,S2,S3));
--    return Natural (Result mod Cache_Max) +1;
--  end Mk_Key;
  ----------------------------------------------------------------------------------
  -- We can avoid to include Element.OwnerImage.all in the hash key.
  -- In this case we have more collisions, but we can dynamically change the owner of
  -- the computation.
  -- Given a Form, a Context, and a State there is at most ONE in progress computation
  -- for each Owner.
  ----------------------------------------------------------------------------------
  function Computation_Hash (Element : Computation_Element) return Hash_Type is
  begin
     return Ada.Strings.Hash(
         Element.FormImage.all  &
         Element.ContextImage.all &
         Element.StateImage.all);
  end;

   function Mk_Comp_Key(This_Elem: Computation_Element; 
                   Cache_Max: Natural := Cache_Module) return Positive is
--      MH: Hash_Type;
   begin
--      MH := Computation_Hash(This_Elem);
--      return Natural (MH mod Hash_Type(Cache_Max)) +1;
--      return Mk_Key(This_Elem.FormImage,This_elem.ContextImage,This_Elem.StateImage);
      return  abs(Progressive(This_Elem.State));
   end Mk_Comp_Key;

  ----------------------------------------------------------------------------
  --  MAYBE - BUT THIS IS TO BE CHECKED - WE TAKE AVOID TO LOOK AT THE OWNER
  ----------------------------------------------------------------------------
  function Equivalent_Computations (Left, Right : Computation_Element)
                 return Boolean is
  begin
    return
       Left.FormImage.all = Right.FormImage.all and 
       Left.ContextImage.all = Right.ContextImage.all  and
       Left.StateImage.all = Right.StateImage.all;
  end Equivalent_Computations;

------------------------------------------------------------------------
 procedure Initialize_DB is
 begin
    Comp_DB.ReInitialize_DB;
    Done_Computations :=0;
    All_Computations_Count :=0;
 end Initialize_DB;
------------------------------------------------------------------------
  

 -----------------------------------------------------------
 -- Context e' lo stack delle computazione ricorsive (max e min) 
 --  attualmente in corso.  Context'Length = Env'Length
 -- Selector e' la tabella che indica quali computazioni sono
 -- realmente utilizzate da questa subformula.
 -- What_Needed ritorna la tabella delle computazioni necessitate.
 -----------------------------------------------------------
 function What_Needed (Context: Computations_Table; Selector: Num_Table_Ref)
     return Computations_Table is
 begin
   if Selector = null then
      return Empty_Computations_Table;
   end if;
   declare
     Result: Computations_Table(1..Selector.all'Length);
   begin
     -- o forse dovrebbe gia' esserlo "selector"?
     for I in Selector.all'Range loop
       Result(I) := Context(Selector(I));
     end loop;
     return Result;
   end;
 end What_Needed;


 ---------------------------------------------------------------- 
 --  Form e' la formula sintattica in via di valutazione
 --  Context e' lo stack delle computazioni ricorsive (max,min) in cui
 --  la valutazione attuale e' annidatata.
 --  State e' lo stato attuale in cui si sta' valutando la formula
 --  Cref (OUT) e' l'identificatore unique per questa computazione
 --  Status (Not_Yet_Started, FoundXX, In_progress) lo stato di
 --  avanzamento di questa computazione
 --  Nel caso in cui la computatione sia gia presente nel DB, ma con uno
 --   stato  ABORTED,NOT_YET_STARTED,TMP_XX, in campo Rec_Depth nel DB viene
 --   aggiornato con quello dato.
 ---------------------------------------------------------------- 
 -- Looks if equivalent computation has already been included in the DB.  
 -- If it is already there, returns its identifying position and true/false status
 -- If it is already there, but was an obsolete Aborted computation
 --  updates it resetting the status to Not_Yet_Started
 -- If it is not already there it adds the item into the DB,
 --    returning its identifying position and Not_Yet_Started status
 ---------------------------------------------------------------- 
 --  OPTIMIZATION (LINEAR COMPLEXITY)
 --   prima di analizzare l'esistenza di una computazione
 --     <form state owner> si guarda se esiste gia' un risultato definitivo
 --  per la computatione <form state state>: se c'e', si restituisce quello.
 -------------------------------------------------------------------------- 
 --  Il parametro  REC_DEPTH restituito e' sempre quello ricevuto come input,
 --  tranne quando il saved status e' ABORTED (era una volta IN_PROGRESS), 
 -- nel qual caso viene sostuito da quello salvato. (che era il N_DEPTH?)????
 ---------------------------------------------------------------------------
 procedure Check_Computation (Form: Formula_Ref; 
                            Context: Computations_Table;
                            Owner: System_Configuration;
                            State: System_Configuration;
                            Cref:  out Computation_Step;
                            Status: out Computation_Status;
                            Rec_Depth: in out Natural) is
   --
   Needed_Context: Computations_Table := What_Needed(Context,Form.Env_Selector);
   --
   This_Comp: Computation_Element;
   Tmp2: Computation_Element;
 begin
   --
   -----------------------------------------------------------------------
   --  prepare the actual data for  the identifying and stable components
   -----------------------------------------------------------------------
   This_Comp.Form := Form;
   This_Comp.Context := new Computations_Table'(Needed_Context);  -- OPTIMIZE WITH EMPTY_CONTEXT!!!
   This_Comp.State := State;
   This_Comp.FormImage :=  Form.Fimage;
   This_Comp.ContextImage:= new String(1..0);
   for I in This_Comp.Context'Range loop
     Tmp2 := Retrieve(Integer(This_Comp.Context(I)));
     This_Comp.ContextImage := 
          new String'(This_Comp.ContextImage.all & "." & Tmp2.Progressive.all);
   end loop;
   This_Comp.StateImage:= new String'(NickName(State,"C"));
   --
   -------------------------------------------------------------------------
   -- Retrieve the other modifiable parts 
   --  or initialize them if the element is a new one
   --------------------------------------------------------------------------
   Cref := Computation_Step(CheckNum(This_Comp));
   --
   if Cref > 0 then
     --
     -- if this computation already exists, 
     --  maybe still in progress, or definitely aborted, or aborted but resumable, or unstable
     --  return its Cref/status, possibly updating the details as needed. 
     --
     Free(This_Comp.Context);
     Free(This_Comp.ContextImage);
     --
     This_Comp := Retrieve(Integer(Cref));  --  retrieve all the details
     if This_Comp.Status = ABORTED and then
       This_Comp.Max_LTS_Depth < Max_LTS_Depth then
       -- 
       --  WE RESTART THE COMPUTATION
       --
       -- THE computation was aborted, but now it is being restarted
       --    Old_Info.SubComputations and
       --    Old_Info.SubEvolutions have been already cleared 
       --     when the Aborted status has been set.
       --
       This_Comp.Status := NOT_YET_STARTED;
       This_Comp.Rec_Depth := Rec_Depth;
       This_Comp.Max_LTS_Depth := Max_LTS_Depth;
       Store(This_Comp,Integer(Cref));
       Status := This_Comp.Status;
       --
     elsif This_Comp.Status=TMP_TRUE or This_Comp.Status=TMP_FALSE then 
       This_Comp.Owner := Owner;
       This_Comp.Status := NOT_YET_STARTED;
       This_Comp.Max_LTS_Depth := Max_LTS_Depth;
--       if NonInteractive = False then
       if NoExplanations = False then
         Free(This_Comp.Subcomputations);
         This_Comp.Subcomputations :=  new Computations_Table(1..0);
         Free(This_Comp.SubEvolutions);
         This_Comp.SubEvolutions := new Evolutions_Table(1..0);
       end if;
       Free(This_Comp.Supercomputations);
       This_Comp.Supercomputations :=  new Computations_Table(1..0);
       This_Comp.Rec_Depth := Rec_Depth;
       Store(This_Comp,Integer(Cref));
       Status := This_Comp.Status;
       --
     elsif This_Comp.Status=IN_PROGRESS then
       Status := This_Comp.Status;
       -- the only case in which we return a different REC_DEPTH
       Rec_Depth := This_Comp.Rec_Depth; 
     elsif This_Comp.Status=ABORTED then
       Status := This_Comp.Status;
       Rec_Depth := This_Comp.Rec_Depth;
     else
        -- This_Comp.Status=FOUND_TRUE / FOUND_FALSE 
       Status := This_Comp.Status;
     end if;
     ---
   else
     --
     --  WE START THE COMPUTATION
      --
     Cref := abs(Cref);
     This_Comp.OwnerImage:=  new String'(NickName(Owner,"c"));
     This_Comp.Owner := Owner;
     This_Comp.Status := NOT_YET_STARTED;
     This_Comp.Subcomputations :=  new Computations_Table(1..0);
     This_Comp.SubEvolutions := new Evolutions_Table(1..0);
     This_Comp.Supercomputations :=  new Computations_Table(1..0);
     This_Comp.Last_Exploration := 0;
     This_Comp.Max_LTS_Depth := Max_LTS_Depth;
     This_Comp.Progressive := 
            new String'(Integer'Image(Integer(Cref)));
     This_Comp.Rec_Depth := Rec_Depth;
     --
     All_Computations_Count := All_Computations_Count+1;
     Store(This_Comp,INteger(Cref));
     Mark_Step;
     Status := Not_Yet_Started;
   end if;
   return;
 end Check_Computation;


 ----------
 -- NEWS Sett 2016:
 --  in the case of recursive Subcomputations the SubCref is added 
 --  in the FIRST position of the sub table.
 --  This allows, in the case of UNTIL to save both the subcomp of Form and recursive-Until
 --  while preserving in Subcomp(1) the recursive case, without the need of a Set_SubComp.
 -----------
  procedure Add_Subcomputation (Cref: Computation_Step;
                                SubCref: Computation_Step;
                                with_Evolution: Evolution_Data  := No_Evolution) is
  --
    Old_Info: Computation_Element; 
    Sub_Info: Computation_Element; 
    This_Sub: Computation_Step := SubCref;
    Tmp1: Computations_Table_Ref; 
    Tmp2: Evolutions_Table_Ref;
    Found: Boolean;
  begin
    -- 
    -- update the database w.r.t. the current computation Cref
    -- 
    Old_Info := Retrieve(Integer(Cref));  --   NOTICE   Cref can be == SubCref!!
    Sub_Info := Retrieve(Integer(SubCref));
    -- if NOT interactive (no explanations needed) no need to record subcomputations
    if NoExplanations = False then
--      if Old_Info.Form /= Sub_Info.Form then 
        -- subcompuations are added to the end of the table
        Tmp1 :=  new  Computations_Table'(Old_Info.SubComputations.all & SubCref);
        Free (Old_Info.SubComputations);
        Old_Info.SubComputations :=  Tmp1;
        Tmp2 := new  Evolutions_Table'(Old_Info.SubEvolutions.all & with_Evolution);
        Free (Old_Info.SubEvolutions);
        Old_Info.SubEvolutions := Tmp2;
        --
--      else  -- put recursion as first subcomputation
--        Tmp1 :=  new  Computations_Table'(SubCref & Old_Info.SubComputations.all);
--        Free (Old_Info.SubComputations);
--        Old_Info.SubComputations :=  Tmp1;
--        Tmp2 := new  Evolutions_Table'(with_Evolution & Old_Info.SubEvolutions.all);
--        Free (Old_Info.SubEvolutions);
--        Old_Info.SubEvolutions := Tmp2;
--      end if;
    end if;
    --
    Old_Info.Max_LTS_Depth := Max_LTS_Depth;  --?? necessary??
    Store(Old_Info,Integer(Cref));
    Sub_Info := Retrieve(Integer(SubCref));  -- Old_info saved can become new Sub_info!!
    -- supercomputations are instead needed for consolidations
    Found := False;
    for K in Sub_Info.SuperComputations.all'Range loop
       if Sub_Info.SuperComputations(K) = Cref then
         Found := True;
         exit;
       end if;
    end loop;
    if not Found then
      Tmp1 :=  new  Computations_Table'(Sub_Info.SuperComputations.all & Cref);
      Free (Sub_Info.SuperComputations);
      Sub_Info.SuperComputations :=  Tmp1;
    end if;
    --
    Store(Sub_Info,Integer(SubCref));
  end Add_Subcomputation;

  procedure Set_Subcomputation (Cref: Computation_Step;
                                SubCref: Computation_Step;
                                with_Evolution: Evolution_Data  := No_Evolution) is
    This_Sub: Computation_Step := SubCref;
    Old_Info: Computation_Element;
    Tmp1: Computations_Table_Ref;
    Found: Boolean;
  begin
    --
    -- update the database w.r.t. the current computation Cref
    --
    Old_Info := Retrieve(Integer(Cref));
    -- if NOT interactive (no explanations needed) no need to record subcomputations
--    if NonInteractive = False then
    if NoExplanations = False then
      Free(Old_Info.SubComputations);
      Old_Info.SubComputations :=
         new  Computations_Table'(1..1 => This_Sub);
      Free(Old_Info.SubEvolutions);
      Old_Info.SubEvolutions :=
         new Evolutions_Table'(1..1 =>with_Evolution);
    end if;
    Old_Info.Max_LTS_Depth := Max_LTS_Depth;
    Store(Old_Info,Integer(Cref));
    Old_Info := Retrieve(Integer(SubCref));
    --
    Found := False;
    for K in Old_Info.SuperComputations.all'Range loop
       if Old_Info.SuperComputations(K) = Cref then
         Found := True;
       end if;
    end loop;
    if not Found then
      Tmp1 :=  new  Computations_Table'(Old_Info.SuperComputations.all & Cref);
      Free (Old_Info.SuperComputations);
      Old_Info.SuperComputations :=  Tmp1;
    end if;
    --
    Store(Old_Info,Integer(SubCref));
    --
  end Set_Subcomputation;

  function Get_Formula (Cref:  Computation_Step) return Formula_Ref is
  begin
    return Retrieve(Integer(Cref)).Form;
  end Get_Formula;

  function  Get_State (Cref:  Computation_Step) return System_Configuration is
  begin
     return Retrieve(Integer(Cref)).State;
  end Get_State;

  function Get_Context (Cref:  Computation_Step) return Computations_Table is
  begin
    return Retrieve(Integer(Cref)).Context.all;
  end Get_Context;

  function  Get_Owner (Cref:  Computation_Step) return System_Configuration is
  begin
     return Retrieve(Integer(Cref)).Owner;
  end Get_Owner;

  function  Get_Status (Cref:  Computation_Step) return Computation_Status is
  begin
     return  Retrieve(Integer(Cref)).Status;
  end Get_Status;

  --------------------------------------------------
  -- Lo stato di una subcomputation di Cref, relativa ad una formula Form di tipo AG, 
  --  e' diventata FOUND_FALSE / ABORTED
  -- Da FOUND_TRUE  diventa FOUND_FALSE/ ABORTED
  -- N.B.  Non viene chiamata se Form e' la top level formula
  --------------------------------------------------
  procedure ConsolidateAG (Cref: Computation_Step;
                         Form: Formula_Ref;
                         Status: Computation_Status; 
                         Rec_Depth: Natural) is
    This_Comp: Computation_Element := Retrieve(Integer(Cref));
  begin
      --
    if This_Comp.Formimage.all = Form.Fimage.all then
      -- propaga il nuovo stato sullo sulle AG di livello superiore
      if Status = ABORTED and then (This_Comp.Status = FOUND_TRUE) then
        -- se la parent era TRUE (a causa di ricorsione) ma in realta la AG e' ABORTED
        -- allora bisogna correggere il valore del parent, e continuare ricorsivamente
        Done_Computations := Done_Computations-1;
        This_Comp.Status := Status;
        This_Comp.Max_LTS_Depth  := Max_LTS_Depth;
        Store(This_Comp,Integer(Cref));
        for I in This_Comp.SuperComputations'Range loop
          ConsolidateAG(This_Comp.SuperComputations(I),Form,Status,Rec_Depth);
        end loop;
       -- BEWARE: DOBBIAMO PROPAGARE ANCHE IL N_DEPTH (SALVATO IN REC_DEPTH?)
        --
      elsif Status = FOUND_FALSE and then 
            (This_Comp.Status = FOUND_TRUE or else This_Comp.Status = ABORTED) then
        -- se la parent era TRUE (a causa di ricorsione) o ABORTED ma in realta la AG e' FALSE
        -- allora bisogna correggere il valore del parent, e continuare ricorsivamente
        This_Comp.Owner := This_Comp.State;    --  TOBEDELETED  ?!?!
        This_Comp.Status := Status;
        Store(This_Comp,Integer(Cref));
        for I in This_Comp.SuperComputations'Range loop
          ConsolidateAG(This_Comp.SuperComputations(I),Form,Status,Rec_Depth);
        end loop;
        --
--    elsif Status = TMP_FALSE and then 
--          (This_Comp.Status = FOUND_TRUE or This_Comp.Status = ABORTED) then
--      -- *** succede solo se la formula contiene variabili di punto fisso ***
--      -- se la parent era TRUE (a causa di ricorsione) o ABORTED ma in realta la AG e' FALSE
--      -- allora bisogna correggere il valore del parent, e continuare ricorsivamente
--      This_Comp.Owner := This_Comp.State;    --  TOBEDELETED  ?!?!
--      This_Comp.Status := Status;
--      Store(This_Comp,Cref);
--      for I in This_Comp.SuperComputations'Range loop
--        ConsolidateAG(This_Comp.SuperComputations(I),Form,Status,Rec_Depth);
--      end loop;
--     -- BEWARE: DOBBIAMO PROPAGARE ANCHE IL REC_DEPTH 
       --
      end if;
    end if;  -- This_Comp.Form = Form
  end ConsolidateAG;
  

  procedure ConsolidateEF (Cref: Computation_Step;
                         Form: Formula_Ref;
                         Status: Computation_Status; Rec_Depth: Natural) is
    This_Comp: Computation_Element := Retrieve(Integer(Cref));
  begin
    if This_Comp.Formimage.all = Form.Fimage.all then
      -- propaga il nuovo stato sullo sulle EF di livello superiore
      if Status = ABORTED and then (This_Comp.Status = FOUND_FALSE) then
        -- se la parent era FALSE (a causa di ricorsione) ma in realta la EF e' ABORTED
        -- allora bisogna correggere il valore del parent, e continuare ricorsivamente
        Done_Computations := Done_Computations-1;
        This_Comp.Status := Status;
        This_Comp.Max_LTS_Depth  := Max_LTS_Depth;
        Store(This_Comp,Integer(Cref));
        for I in This_Comp.SuperComputations'Range loop
          ConsolidateEF(This_Comp.SuperComputations(I),Form,Status,Rec_Depth);
        end loop;
        --
      elsif Status = FOUND_TRUE and then 
         (This_Comp.Status = FOUND_FALSE or else This_Comp.Status = ABORTED) then
        -- se la parent era FALSE (a causa di ricorsione) o ABORTED ma in realta la EF e' TRUE
        -- allora bisogna correggere il valore del parent, e continuare ricorsivamente
        This_Comp.Owner := This_Comp.State;    --  TOBEDELETED  ?!?!
        This_Comp.Status := Status;
        Store(This_Comp,Integer(Cref));
        for I in This_Comp.SuperComputations'Range loop
          ConsolidateEF(This_Comp.SuperComputations(I),Form,Status,Rec_Depth);
        end loop;
        --
      elsif Status = TMP_TRUE and then
            (This_Comp.Status = FOUND_FALSE or This_Comp.Status = ABORTED) then
        -- *** succede solo se la formula contiene variabili di punto fisso ***
        -- se la parent era TRUE (a causa di ricorsione) o ABORTED ma in realta la AG e' FALSE
        -- allora bisogna correggere il valore del parent, e continuare ricorsivamente
        This_Comp.Owner := This_Comp.State;    --  TOBEDELETED  ?!?!
        This_Comp.Status := Status;
        Store(This_Comp,Integer(Cref));
        for I in This_Comp.SuperComputations'Range loop
          ConsolidateEF(This_Comp.SuperComputations(I),Form,Status,Rec_Depth);
        end loop;
       -- BEWARE: DOBBIAMO PROPAGARE ANCHE IL REC_DEPTH
       --
      end if;
    end if;  -- This_Comp.Form = Form
  end ConsolidateEF;

  ------------------------------------------------------------------------------
  ----------------------------------------------------------------------------
  procedure Consolidate (Cref: Computation_Step;
                         Form: Formula_Ref;
                         Status: Computation_Status;
                         Rec_Depth: Natural) is
  begin
    --
    --  caso AF/EG   A[U]   E[W]  
    --     niente da consolidare
    --
    --  caso AG   A[W] [[]]
    --   
    if (Form.Kind= Fall and then Form.PRef.Kind=Always )  or else
       (Form.Kind= FWsquare) or else
       (Form.Kind= Fall and then Form.PRef.Kind=Wuntil2)  or else
       (Form.Kind= Fall and then Form.PRef.Kind=Wuntil1)  then
     ConsolidateAG(Cref,Form,Status,Rec_Depth);
    end if;
    --
    --  caso EF   E[U] <<>>
    --   
    if (Form.Kind= Fexist and then Form.PRef.Kind=Eventually)  or else 
       (Form.Kind= FWangle) or else
       (Form.Kind= Fexist and then Form.PRef.Kind=Until2)  or else
       (Form.Kind= Fexist and then Form.PRef.Kind=Until1) then
     ConsolidateEF(Cref,Form,Status,Rec_Depth);
    end if;
  end Consolidate;

  ------------------------------------------------------------------------------
  -- called:  Set_Status (Cref, IN_PROGRESS,C_Depth); -- initial value
  --          Set_Status (Cref, ABORTED,N_Depth);
  --          Set_Status(Cref, FOUND_...,C_Depth);  -- final result
  --          Set_Status(Cref, Tmp_Result);         -- implicitamente rimane C_Depth
  --          Set_Status(Cref, TMP_...,Rec_Depth);  -- final result
  ------------------------------------------------------------------------------
  procedure Set_Status (Cref: Computation_Step;
                         Status: Computation_Status;
                           Rec_Depth: Natural :=0) is
    This_Computation: Computation_Element;
    PreviousStatus: Computation_Status;
  begin
    if Status = FOUND_TRUE or Status= FOUND_FALSE then
       Done_Computations := Done_Computations+1;
    end if;
    --
    This_Computation := Retrieve(Integer(Cref));
    if Rec_Depth /=0 then 
      This_Computation.Rec_Depth := Rec_Depth; 
    end if;
    --
    PreviousStatus := This_Computation.Status;
    This_Computation.Status := Status;
    if Status = ABORTED then 
      This_Computation.Max_LTS_Depth := Max_LTS_Depth;
--      if NonInteractive = False then
      if NoExplanations =False then
        Free(This_Computation.Subcomputations);
        This_Computation.Subcomputations :=  new Computations_Table(1..0);
        Free(This_Computation.SubEvolutions);
        -- This_Comutation rimane comunque nelle SUPERCOMPUTATIONS delle SUBCOMPUTATIONS!!
        This_Computation.SubEvolutions := new Evolutions_Table(1..0);
      end if;
    end if;
    --
    Store(This_Computation,Integer(Cref));
    --
    ---------------------------------------------------------
    --  Quando si completa CON SUCCESSO la valutazione di una FORMULA CHIUSA ricorsiva (AG, EF AU, EU, ..),
    --  i risultati di tutte le SUPERCOMPUTATIONS RICORSIVE che non sono gia' definitivi (TMP_TRUE/TMP_FALSE) 
    --  diventano tali (FOUND_TRUE/FOUND_FALSE)
    ---------------------------------------------------------
    --  Nota che esisteranno comunque altre COMPUTATIONS (che non sono necessarie per
    --  la explanation) che sono state perse e che rischiano di dover
    --  essere rivalutate/riciclate ...  
    --  Non bisognerebbe mai fare una Set_Subcomputation prima della Set_Status
    ---------------------------------------------------------
    -- QUESTA FASE DI OTTIMIZZAZIONE POTREBBE  ESSERE EVITATA se la formula
    -- e' quella TOP_LEVEL e lo stato e' quello INIZIALE.
    -------------------------------------------------------------------------------------
    -- "Consolidate" is also called when a recursive subcomputation (EG AF)
    --  with Owner/=State completes with a definitive result (Last_Deph >= RecDepth)
    ------------------------------------------------------------------------------------
    -- do not consolidate the final result (unless ABORTED)!!!
    -- IN EFFETTI L'ECCEZIONE  "UNLESS ABORTED" SEMBREREBBE NONN SERVIRE!
    -- SE LA FORMULA TOP LEVEL E' ABORTED le SUPERCOMPUAZIONI TRUE POSSONO RIMANERE TALI
    if -- Status = ABORTED or else 
        UCTL_Types.This_Formula /= This_Computation.Form  then
      if ( PreviousStatus= IN_PROGRESS or else 
           PreviousStatus= FOUND_TRUE or else   -- forse non servono?
           PreviousStatus= FOUND_FALSE) and then
           This_Computation.SuperComputations /= null  then 
        for I in This_Computation.SuperComputations.all'Range loop
          Consolidate(
            This_Computation.SuperComputations(I),This_Computation.Form,Status,Rec_Depth);
        end loop;
      end if;
    end if;
  end Set_Status;

  function Get_SubEvolutions (Cref:  Computation_Step) return Evolutions_Table  is
  begin
     return  Retrieve(Integer(Cref)).SubEvolutions.all;
  end Get_SubEvolutions;

  function Get_Subcomputations (Cref:  Computation_Step) 
      return Computations_Table is
  begin
     return  Retrieve(Integer(Cref)).Subcomputations.all;
  end Get_Subcomputations;

  procedure Start_Exploration is
  begin
     Explorations_Count := Explorations_Count +1;
  end Start_Exploration;

  function  Already_Explored (Cref: Computation_Step) return Boolean is
    Tmp: Computation_Element;
  begin
   Tmp := Retrieve(Integer(Cref));
     if Tmp.Last_Exploration >= Explorations_Count then
       return True;
     else
       Tmp.Last_Exploration := Explorations_Count;
       Store(Tmp,Integer(Cref));
       return False;
     end if;
 end;

  function NickName (Cref: Computation_Step; Prefix: String := "X") 
        return String is
    Static: String := Retrieve(Integer(Cref)).Progressive.all;
  begin
    return Prefix & Static(2..Static'Length);
  end NickName;

  function Get_State_Computations(State: System_Configuration) return Computations_Table is
    keys: Int_Table := Comp_DB.Get_Key_Conflicts(Positive(State));
    K2: Computations_Table(1..keys'Length);
  begin
    for I in keys'Range loop
     K2(I) := Computation_Step(keys(I));
    end loop;
    return K2;
    -- return Computations_Table(keys);
  end Get_State_Computations;
  
  procedure Dump_Computations is
--     Cref: Cursor := No_Element;
--     This: Computation_Element; 
--     Sub: Computation_Element; 
  begin
    null;
-- 
--    Cref:=  First(All_Computations);
--    while Cref /= No_Element loop
--      This := Element(Cref);
--      Put("Form: " & This.FormImage.all);
--      Put("  State: " & This.StateImage.all);
--      Put( "  Progressive: " & This.Progressive.all);
 --     Put( "  Owner: " & This.OwnerImage.all);
 --     Put( "  Status: " & Computation_Status'Image(This.Status));
 --     Put( "  Sub: ");
 --     for I in This.SubComputations.all'Range loop
--         Sub := Element(This.SubComputations(I));
--         Put (Sub.Progressive.all & " ");
--      end loop;
--      New_Line;
--      Cref := next(Cref);
 ----   end loop;
  end Dump_Computations;

  function ComputationsSpace_Size return Natural is
  begin
    return Comp_DB.Current_Count;
  end;


begin
  null;
  -- Comp_DB.ReInitialize_DB;
end Computations_DB;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
--   OPTIMIZATIONS
--
-- 1) When looking for  the value of <form,state,owner>, we first look for a value
--    for <form,state,state> if ti exists
-- 
-- 2) When saving the value FOUND_* for <form,state,owner> , we save it for <form,state,state>
--
-- 3) When we set the value of <form,state,owner> to FOUND_FALSE, in the case of EF/AF  we propagate
--    it to all TMP_FALSE subcomputations.
--   (This happens either when owner=state or when C_Depth <= Rec_Depth)
--
-- 3b)  The same could be done for FOUND_TRUE replacing TMP_FALSE in the case of EF/AF if we guarantee that
--     FOUND_FALSE is saved instread of TMP_FALSE for all closed subcomputations
--
--  EXAMPLE      form = EF(s4)
--     
--   s0 -> s1 
--   s1 -> s2
--   s1 -> s3
--   s1 -> s4
--   s2 -> s2
--   s3 -> s0
--
--  Evaluation steps
--  Eval(form,s0)  <form,s0> = IN_PROGRESS
--    Eval(form,s1)  <form,s1> = IN_PROGRESS
--      Eval(form,s2)  <form,s2> = IN_PROGRESS
--        Eval(form,s2)  <form,s2> found IN_PROGRESS, return TMP_FALSE
--                 Res = TMP_False, C_Depth=Rec_Depth --> <form,s2> = FOUND_FALSE {propagarted to TMP descrndendets)
--      Eval (form,s3) <form,s3) = IN_PROGRESS
--        Eval(form,s0)  <form,s0> found IN_PROGRESS, return TMP_FALSE
--                 Res = TMP_FALSE  <form,s2> = TMP_FALSE (Rec_Depth < C_Depth)
--      Eval(form,s4)  <form,s4> = FOUND_TRUE 
--      <form,s1> = FOUND_TRUE {propagated to all TMP decendents!!}
--         <form,s3> = FOUND_TRUE
--  <form,s0> = FOUND_TRUE    
--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
