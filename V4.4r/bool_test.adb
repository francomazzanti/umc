with Ada.Text_IO;
with bool_dyn_store;
procedure bool_test is
 package myDB is new bool_dyn_store;
begin
Ada.text_io.put_line(Integer'Image(Integer'Last));
end;

