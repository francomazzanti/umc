with Ada.Text_io; use ada.text_io;
procedure sizetest is
 type T is access string;
  SS: T;
  N: Integer;
begin
  Put_line(Integer'Image(T'SIZE));
  Put_line(Integer'Image(N'SIZE));
  Put_line(Integer'Image(SS'SIZE));
end;


