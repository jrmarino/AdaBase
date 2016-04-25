with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Bad_Select is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

   row : ARS.DataRow_Access;
   sql : String := "SELECT fruit, calories FROM froits " &
                   "WHERE color = 'red'";

begin

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   CON.STMT := CON.DR.query (sql);

   TIO.Put_Line ("Query successful: " & CON.STMT.successful'Img);

   if not CON.STMT.successful then
      TIO.Put_Line ("  Driver message: " & CON.STMT.last_driver_message);
      TIO.Put_Line ("     Driver code: " & CON.STMT.last_driver_code'Img);
      TIO.Put_Line ("       SQL State: " & CON.STMT.last_sql_state);
      --  Fix SQL typo
      sql (31) := 'u';
      TIO.Put_Line ("");
      TIO.Put_Line ("SQL now: " & sql);
      CON.STMT := CON.DR.query (sql);
      TIO.Put_Line ("Query successful: " & CON.STMT.successful'Img);

      row := CON.STMT.fetch_next;
      TIO.Put_Line ("   Number fields:" & row.count'Img);

      CON.STMT.discard_rest;
      TIO.Put_Line ("  Data discarded: " & CON.STMT.discards_possible'Img);
   end if;
   CON.DR.disconnect;

end Bad_Select;
