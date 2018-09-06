with Interfaces;

package ACO.Utils.Byte_Order is
   use Interfaces;

   type Octets_2 is array (0 .. 1) of Unsigned_8;

   type Octets_4 is array (0 .. 3) of Unsigned_8;

   type Octets_8 is array (0 .. 7) of Unsigned_8;

   function Swap_Bus (X : Unsigned_16) return Unsigned_16;
   pragma Inline (Swap_Bus);

   function Swap_Bus (X : Unsigned_32) return Unsigned_32;
   pragma Inline (Swap_Bus);

   function Swap_Bus (X : Octets_2) return Unsigned_16;
   pragma Inline (Swap_Bus);

   function Swap_Bus (X : Octets_4) return Unsigned_32;
   pragma Inline (Swap_Bus);

   function Swap_Bus (X : Unsigned_16) return Octets_2;
   pragma Inline (Swap_Bus);

   function Swap_Bus (X : Unsigned_32) return Octets_4;
   pragma Inline (Swap_Bus);

end ACO.Utils.Byte_Order;
