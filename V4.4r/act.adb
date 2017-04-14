procedure act is
type t is access String;
v: T := new String'("AANBBH");

o: String renames v.all;
begin
 null;
end;
