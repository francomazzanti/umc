with Ada.text_io;  use Ada.text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
procedure getfile is
  Inputchar: Character;
begin
--
--   chiamata come    cat file | getfile >  formatted 
--    oppure           getfile < source > formatted
--
--  questo programmetto dovrebbe convertire files di testo con
--   line breaks di tipo DOS o MAC in files con line breaks di tipi UNIX
--  CR+LF   =   DOS   
--  LF      =   UNIX
--  CR      =   MAC
--
 while not End_Of_file loop
    Get_Immediate(Inputchar);
    if Inputchar = cr then
       New_line;
       while not End_Of_file loop
         Get_Immediate(Inputchar);
         if Inputchar = cr then
           New_line;
           --   continue  in the case of cr.cr.cr.cr
         elsif Inputchar = lf then
           exit;   -- in the case of cr.x
         else
           Put(Inputchar);
           exit;   -- in the case of cr.lf
         end if;
       end loop;
    elsif Inputchar = lf then
     New_line;
    else
      Put (Inputchar);
    end if;
 end loop;
end getfile;
