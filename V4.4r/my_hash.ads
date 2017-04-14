generic
  type Element_Ref is private;
  type Key_Data is private;
  with function Matching (K1: Key_Data; K2:Key_Data) return Boolean;
  with function Mk_key (EE: Key_Data) return Natural;
  with function Initial_Element(Key: Key_Data) return Element_Ref;
  with procedure Set_Progressive(N:positive; ER: Element_Ref);
  --
  Thread_Safe: Boolean := True;
  --
package my_hash is
  function CheckElement (KK: Key_Data; JUST_ADDED: out Boolean) return Element_Ref;
  function Current_Count return Natural;
end My_Hash;

