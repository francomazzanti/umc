with my_atomic_counters; use my_atomic_counters;
with Ada.text_IO; use Ada.text_IO;
procedure atomictest is
  N :  Counter_Object :=0;
  AA: INteger;
  BB: INteger;
begin
  declare
    task subworker is end subworker;
    task body subworker is
    begin
      for I in 1..10_000_000 loop
        AA := Increment(N'Address);
      end loop;
    Put_line("subworker DONE");
    exception
       when Others => Put_line("subworker FAILED");
    end subworker;
  begin
    for I in 1..10_000_000 loop
       BB := Increment(N'Address);
    end loop;
  exception
     when Others => Put_line("subworker FAILED");
  end;
  Put_line("DONE " & Integer'Image(Current_Value(N'Address)));
exception
  when Others => Put_line("Main FAILED");
end atomictest;
