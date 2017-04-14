with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Conversion;
procedure MyHash is
 type Int_Table is array (Positive range <> ) of Integer;

 function FingertPrints (This_Table: Int_Table) return Int_Table is
  -- Int64 = Integer= 32bits  -- max 2 miliardi
  Result: Int_Table(1..2);
  type BitVector is array(1..32) of Boolean;
  Pragma Pack(Bitvector);
  for BitVector'Size use 32;
  Tmp: Integer;
  WorkBits1: BitVector;
  WorkBits2: BitVector;
  WorkBits: BitVector;
  Function Tobits is new Unchecked_Conversion (Integer, BitVector); 
  Function ToInt is new Unchecked_Conversion (BitVector,Integer); 
 begin
   WorkBits := Tobits(0);
   for I In This_Table'Range loop
     Tmp := This_Table(I);
     WorkBits1 := Tobits(This_Table(I));
     for J in 2..I loop
        -- rotate This_table(I) J bits on the right
        Tmp :=  Tmp/2;
        WorkBits2 := Tobits(Tmp);
        WorkBits2(32) := WorkBits1(1);
        WorkBits1 := WorkBits2;
        Tmp := ToInt(WorkBits1);
     end loop;
     WorkBits := WorkBits xor Workbits1;
   end loop;
   Result(1) := ToInt(WorkBits);
   --
   WorkBits := Tobits(0);
   for I In This_Table'Range loop
     Tmp := This_Table(I);
     WorkBits1 := Tobits(This_Table(I));
     for J in 2..I loop
        -- rotate This_table(I) 2*J bits on the right
        Tmp :=  Tmp/2;
        WorkBits2 := Tobits(Tmp);
        WorkBits2(32) := WorkBits1(1);
        WorkBits1 := WorkBits2;
        Tmp := ToInt(WorkBits);
        Tmp :=  Tmp/2;
        WorkBits2 := Tobits(Tmp);
        WorkBits2(32) := WorkBits1(1);
        WorkBits1 := WorkBits2;
        Tmp := ToInt(WorkBits1);
     end loop;
     WorkBits := WorkBits xor Workbits1;
   end loop;
   Result(2) := ToInt(WorkBits);
   --
   return Result;
 end FingertPrints;

  Test: Int_Table := FingertPrints((2,2,2));
begin
 Put_Line(Integer'Image(Test(1)));
 Put_Line(Integer'Image(Test(2)));
end MyHash;
