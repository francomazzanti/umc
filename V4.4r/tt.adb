with Ada.Text_IO; use Ada.Text_IO;
procedure tt is
 v1,v2: Integer;
begin
 v1 := Integer 'Last;
 v2 := Integer'Last -2;
 v1 := 10;
 v2 := 20;
 Put_Line(Integer'Image(Integer'Last)); 
 Put_Line(Integer'Image((v1+v2)/2)); 
end;
