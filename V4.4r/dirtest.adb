with Ada.Text_IO;
with Ada.Direct_IO;
procedure dirtest is
begin
 declare
   use Ada.Text_IO;
   FF: File_Type;
  begin
    Create(FF,OUT_File,"tmp");
    Put_line(FF, "12345678901234567890");
    Put_line(FF, "Hello1");
    Put_line(FF, "Hello2");
    Put_line(FF, "Hello3");
    Close(FF);
  end;
 declare
   package MYIO is new Ada.Direct_IO(Character);
   use MYIO; 
   FF: File_Type;
   STR: String := "abcdefghi";
  begin
    Open(FF,INOUT_File,"tmp");
    for  i in STR'RANGE loop
      Write(FF, STR(i), Positive_Count(i));
    end loop;
    Close(FF);
  end;
end;
