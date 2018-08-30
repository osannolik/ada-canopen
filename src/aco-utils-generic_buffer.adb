package body ACO.Utils.Generic_Buffer is

   procedure Put_Blocking
      (This : in out Protected_Buffer;
       Item : in     Item_Type)
   is
      Success : Boolean;
   begin
      This.Buffer.Put (Item, Success);

      if not Success then
         Ada.Synchronous_Task_Control.Suspend_Until_True (This.Non_Full);
         This.Buffer.Put (Item, Success);
      end if;

      Ada.Synchronous_Task_Control.Set_True (This.Non_Empty);
   end Put_Blocking;

   procedure Get_Blocking
      (This : in out Protected_Buffer;
       Item :    out Item_Type)
   is
      Success : Boolean;
   begin
      This.Buffer.Get (Item, Success);

      if not Success then
         Ada.Synchronous_Task_Control.Suspend_Until_True (This.Non_Empty);
         This.Buffer.Get (Item, Success);
      end if;

      Ada.Synchronous_Task_Control.Set_True (This.Non_Full);
   end Get_Blocking;

   procedure Get
      (This : in out Protected_Buffer;
       Item :    out Item_Type)
   is
      Success : Boolean;
   begin
      This.Buffer.Get (Item, Success);
      Ada.Synchronous_Task_Control.Set_True (This.Non_Full);
   end Get;

   function Count
      (This : Protected_Buffer)
       return Natural
   is
   begin
      return This.Buffer.Nof_Items;
   end Count;

   function Is_Empty
      (This : Protected_Buffer)
       return Boolean
   is
   begin
      return This.Buffer.Nof_Items = 0;
   end Is_Empty;

   function Is_Full
      (This : Protected_Buffer)
       return Boolean
   is
   begin
      return This.Buffer.Nof_Items >= Max_Nof_Items;
   end Is_Full;

   protected body Buffer_Type is

      procedure Inc (Index : in out Index_Type)
      is
      begin
         if Index >= Items'Last then
            Index := Items'First;
         else
            Index := Index_Type'Succ (Index);
         end if;
      end Inc;

      procedure Put
         (Item    : in     Item_Type;
          Success :    out Boolean)
      is
      begin
         Success := not Is_Full;

         if not Is_Full then
            Items (Next_Index) := Item;

            Inc (Next_Index);

            Count := Count + 1;

            Is_Empty := False;

            Is_Full := (Count >= Max_Nof_Items);
         end if;
      end Put;

      procedure Get
         (Item    : out Item_Type;
          Success : out Boolean)
      is
      begin
         Success := not Is_Empty;

         if not Is_Empty then
            Item := Items (Old_Index);

            Inc (Old_Index);

            Count := Count - 1;

            Is_Full := False;

            Is_Empty := (Count <= 0);
         end if;
      end Get;

      function Nof_Items return Natural
      is
      begin
         return Count;
      end Nof_Items;

   end Buffer_Type;

end ACO.Utils.Generic_Buffer;
