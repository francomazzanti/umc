with Ada.text_IO; use Ada.text_IO;
--------------------------------------------------------
-- reads a sequence of characters from standard input,
-- and reprint them on the standard output, substituting
--  %0A  with a New_Line
--  called by load_model.cgi to predigest its input data.
--  Since data can be large, it cannot be handled by sed
--  hence we first split it into lines, and then complete the
--  transformation with the traslation of all the control codes.
--   called as    mklines < source > target
--------------------------------------------------------
procedure mklines is
  Inputchar: Character;
  OD : Boolean := False;
begin
 while not End_Of_file loop
    Get(Inputchar);
    if Inputchar /= '%' then
      if OD then New_Line; OD := False; end if;
      Put(Inputchar);
    else
      Get(Inputchar);
      if Inputchar /= '0' then
        if OD then New_Line; OD := False; end if;
        Put ('%');
        Put(Inputchar);
      else
         Get(Inputchar);
         if Inputchar = 'D' then
           if OD then New_Line; end if;
           OD := True;
         elsif Inputchar = 'A' then
           OD := False;
           New_Line;
         else
           if OD then New_Line; OD := False; end if;
           put('%'); put('0'); put (Inputchar);
         end if;
      end if;
    end if;
 end loop;
 New_Line;
end mklines;
