with System; use System;
with System.Atomic_Primitives; use System.Atomic_Primitives;
-- /project/Ada/GNAT/2015/lib/gcc/x86_64-apple-darwin13.4.0/4.9.3/adainclude/s-atopri.ads
--with System.Multiprocessors.Spin_Locks;
with Spin_Locks;
package My_Atomic_Counters is

  type Counter32 is mod 2**32 with Size => 32;
  type Counter8 is mod 2**8 with Size => 8;

  procedure Increment32 (obj_addr: Address);
  pragma Inline(Increment32);

  function Increment32 (obj_addr: Address) return Counter32;
  pragma Inline(Increment32);

  procedure Decrement32 (obj_addr: Address);
  pragma Inline(Decrement32);

  function Decrement32 (obj_addr: Address) return Counter32;
  pragma Inline(Decrement32);

  function Current_Value32 (obj_addr: Address) return Counter32;
  pragma Inline(Current_Value32);

  function Increment8 (obj_addr: Address) return Counter8;
  pragma Inline(Increment8);

  function Decrement8 (obj_addr: Address) return Counter8;
  pragma Inline(Decrement8);

  function Current_Value8 (obj_addr: Address) return Counter8;
  pragma Inline(Current_Value8);

  type Lock_Data is limited private;
  type Lock_Ref is access Lock_Data;

  type Locks_Table is array (Natural range <>) of Lock_Ref;
  type Locks_Table_Ref is access Locks_Table;

  procedure SeizeLock (L: Lock_Ref);
  pragma Inline (SeizeLock);

  procedure ReleaseLock (L: Lock_Ref);
  pragma Inline (ReleaseLock);

private

   type Lock_Data is new Spin_Locks.Spin_Lock;

--  type Lock_Data is record
--     ticket: Counter8 :=0 with atomic;
--     serving: Counter8 :=1 with atomic;
--  end record with Volatile;

--  protected type Lock_Data is
--    entry Seize;
 --   procedure Release;
--  private
--    Busy: Boolean := False;
--  end Lock_Data;

end My_Atomic_Counters;
