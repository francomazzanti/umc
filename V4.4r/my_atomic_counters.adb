--with System.Atomic_Primitives; use System.Atomic_Primitives;
-- /project/Ada/GNAT/2015/lib/gcc/x86_64-apple-darwin13.4.0/4.9.3/adainclude/s-atopri.ads
-- /project/Ada/GNAT/2015/lib/gcc/x86_64-apple-darwin13.4.0/4.9.3/adainclude/s-atopri.adb
package body My_Atomic_Counters is

--  type Lock_Data is record
--     ticket: Counter8 :=0 with atomic;
--     serving: Counter8 :=1 with atomic;
--  end record with Volatile;
--
--  type Lock_Table is array (Natural range <>) of Lock_Data;
--  type Lock_Table_Ref is access Lock_Table;

  function Increment32 (obj_addr: Address) return Counter32 is
    NN,MM: uint32;
  begin
     loop
         -- leggi N
         NN := Atomic_Load_32(obj_addr);
         -- incrementa e rileggi il valore sovrascritto;
         MM := Sync_Compare_And_Swap_32(obj_addr,NN,NN+1);
         if MM = NN then 
            return Counter32(NN+1);
         end if;
     end loop;
  end Increment32;

  procedure Increment32 (obj_addr: Address) is
    NOTUSED: Counter32;
  begin
    NOTUSED := Increment32(obj_addr);
  end;

  procedure Decrement32 (obj_addr: Address)is
    NOTUSED: Counter32;
  begin
    NOTUSED := Decrement32(obj_addr);
  end; 

  function Decrement32 (obj_addr: Address) return Counter32 is
    NN,MM: uint32;
  begin
     loop
       -- leggi N
       NN := Atomic_Load_32(obj_addr);
       -- incrementa e rileggi il valore sovrascritto;
       MM := Sync_Compare_And_Swap_32(obj_addr,NN,NN-1);
       if MM = NN then
          return Counter32(NN-1);
       end if;
     end loop;
  end Decrement32;

  function Current_Value32 (obj_addr: Address) return Counter32 is
    NN: Counter32;
  begin
    NN := Counter32(Atomic_Load_32(obj_addr));
    return NN;
  end Current_Value32;

  function Increment8 (obj_addr: Address) return Counter8 is
    NN,MM: uint8;
  begin
     loop
       -- leggi N
       NN := Atomic_Load_8(obj_addr);
       -- incrementa e rileggi il valore sovrascritto;
       MM := Sync_Compare_And_Swap_8(obj_addr,NN,NN+1);
       if MM = NN then
          return Counter8(NN+1);
       end if;
     end loop;
  end Increment8;

  function Decrement8 (obj_addr: Address) return Counter8 is
    NN,MM: uint8;
  begin
     loop
       -- leggi N
       NN := Atomic_Load_8(obj_addr);
       -- incrementa e rileggi il valore sovrascritto;
       MM := Sync_Compare_And_Swap_8(obj_addr,NN,NN-1);
       if MM = NN then
          return Counter8(NN-1);
       end if;
     end loop;
  end Decrement8;

  function Current_Value8 (obj_addr: Address) return Counter8 is
    NN: Counter8;
  begin
    NN := Counter8(Atomic_Load_8(obj_addr));
    return NN;
  end Current_Value8;

  procedure Seize(TA: Address; SA: Address) is
    N,M: Counter8;
  begin
     N := Counter8(Increment8(TA));
    loop
      M := Counter8(Current_Value8(SA));
      if N = M then 
        exit; 
      else 
        -- delay(0.0);
        null;
      end if;
    end loop;
  end Seize;
  pragma Inline(Seize);

  procedure Release(TA: Address; SA: Address) is
    N: Counter8;
  begin
    N := Counter8(Increment8(SA));
  end Release;
  pragma Inline(Release);


  procedure SeizeLock (L: Lock_Ref) is
     use Spin_Locks;
  begin
      Lock(Spin_Lock(L.all));
--     Seize(L.ticket'Address, L.serving'Address);
  end;

  procedure ReleaseLock (L: Lock_Ref) is
     use Spin_Locks;
  begin
      UnLock(Spin_Lock(L.all));
--     Release(L.ticket'Address, L.serving'Address);
  end;

end My_Atomic_Counters;
