with System;
with Ada.Unchecked_Conversion;

package body ACO.Utils.Byte_Order is
   use type System.Bit_Order;

   function Unchecked_To_Unsigned_16 is
      new Ada.Unchecked_Conversion (Octets_2, Unsigned_16);

   function Unchecked_To_Unsigned_32 is
      new Ada.Unchecked_Conversion (Octets_4, Unsigned_32);

   function Unchecked_To_Octets_2 is
      new Ada.Unchecked_Conversion (Unsigned_16, Octets_2);

   function Unchecked_To_Octets_4 is
      new Ada.Unchecked_Conversion (Unsigned_32, Octets_4);

   function Swap_Bus (X : Unsigned_16) return Unsigned_16
   is
      O : Octets_2 with
         Address => X'Address, Alignment => Unsigned_16'Alignment;
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return X;
      else
         return Unchecked_To_Unsigned_16 ((O (1), O (0)));
      end if;
   end Swap_Bus;

   function Swap_Bus (X : Unsigned_32) return Unsigned_32
   is
      O : Octets_4 with
         Address => X'Address, Alignment => Unsigned_32'Alignment;
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return X;
      else
         return Unchecked_To_Unsigned_32 ((O (3), O (2), O (1), O (0)));
      end if;
   end Swap_Bus;

   function Swap_Bus (X : Octets_2) return Unsigned_16
   is
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return Unchecked_To_Unsigned_16 (X);
      else
         return Unchecked_To_Unsigned_16 ((X (1), X (0)));
      end if;
   end Swap_Bus;

   function Swap_Bus (X : Octets_4) return Unsigned_32
   is
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return Unchecked_To_Unsigned_32 (X);
      else
         return Unchecked_To_Unsigned_32 ((X (3), X (2), X (1), X (0)));
      end if;
   end Swap_Bus;

   function Swap_Bus (X : Unsigned_16) return Octets_2
   is
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return Unchecked_To_Octets_2 (X);
      else
         declare
            O : constant Octets_2 := Unchecked_To_Octets_2 (X);
         begin
            return (O (1), O (0));
         end;
      end if;
   end Swap_Bus;

   function Swap_Bus (X : Unsigned_32) return Octets_4
   is
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return Unchecked_To_Octets_4 (X);
      else
         declare
            O : constant Octets_4 := Unchecked_To_Octets_4 (X);
         begin
            return (O (3), O (2), O (1), O (0));
         end;
      end if;
   end Swap_Bus;

   procedure Swap (X : in out Octets)
   is
      Tmp : Unsigned_8;
      I : Natural := 0;
   begin
      while X'First + I < X'Last - I loop
         Tmp := X (X'First + I);
         X (X'First + I) := X (X'Last - I);
         X (X'Last - I) := Tmp;
         I := I + 1;
      end loop;
   end Swap;

   function Swap_Bus (X : in Octets) return Octets
   is
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return X;
      else
         declare
            Tmp : Octets := X;
         begin
            Swap (Tmp);
            return Tmp;
         end;
      end if;
   end Swap_Bus;

end ACO.Utils.Byte_Order;
