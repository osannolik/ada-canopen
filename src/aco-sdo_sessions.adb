package body ACO.SDO_Sessions is

   function Create (Endpoint : Endpoint_Type) return SDO_Session is
      (SDO_Session'(Endpoint  => Endpoint,
                    State     => Initiated,
                    Nof_Bytes => 0,
                    Count     => 0));

   function Get_Endpoint
      (CAN_Id         : Id_Type;
       Client_CAN_Ids : SDO_CAN_Id_Array;
       Server_CAN_Ids : SDO_CAN_Id_Array)
       return Endpoint_Type
   is
      I : Endpoint_Nr := Endpoint_Nr'Succ (No_Endpoint_Id);
   begin
      for Client_CAN_Id of Client_CAN_Ids loop
         if Client_CAN_Id.S2C = CAN_Id then
            return (Id => I, Role => Client, CAN_Id => Client_CAN_Id);
         end if;
         I := Endpoint_Nr'Succ (I);
      end loop;

      for Server_CAN_Id of Server_CAN_Ids loop
         if Server_CAN_Id.C2S = CAN_Id then
            return (Id => I, Role => Server, CAN_Id => Server_CAN_Id);
         end if;
         I := Endpoint_Nr'Succ (I);
      end loop;

      return No_Endpoint;
   end Get_Endpoint;

--     function Get
--        (This : Session_List;
--         Id   : Endpoint_Nr)
--         return SDO_Session
--     is
--        S : SDO_Session;
--     begin
--        if This.In_List (Id) then
--           S.Id := Id;
--           return This.List.Get (This.List.Index (S));
--        else
--           return No_Session;
--        end if;
--     end Get;

   procedure Put
      (This    : in out Session_List;
       Session : in     SDO_Session)
   is
   begin
      This.List.Add (Session);
   end Put;

   function Is_Full (This : Session_List) return Boolean is
      (This.List.Is_Full);

   function In_List
      (This     : Session_List;
       Endpoint : Endpoint_Type)
       return Boolean
   is
   begin
      return This.List.In_List (Create (Endpoint));
   end In_List;

end ACO.SDO_Sessions;
