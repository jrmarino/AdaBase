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

   row : ARS.DataRow;
   sql : String := "SELECT fruit, calories FROM froits " &
                   "WHERE color = 'red'";

begin

   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   declare
      stmt : CON.Stmt_Type := CON.DR.query (sql);
   begin
      TIO.Put_Line ("Query successful: " & stmt.successful'Img);

      if not stmt.successful then
         TIO.Put_Line ("  Driver message: " & stmt.last_driver_message);
         TIO.Put_Line ("     Driver code: " & stmt.last_driver_code'Img);
         TIO.Put_Line ("       SQL State: " & stmt.last_sql_state);
         --  Fix SQL typo
         sql (31) := 'u';
         TIO.Put_Line ("");
         TIO.Put_Line ("SQL now: " & sql);
         stmt := CON.DR.query (sql);
         TIO.Put_Line ("Query successful: " & stmt.successful'Img);

         row := stmt.fetch_next;
         if not row.data_exhausted then
            TIO.Put_Line ("   Number fields:" & row.count'Img);

            stmt.discard_rest;
            TIO.Put_Line ("  Data discarded: " & stmt.data_discarded'Img);
         end if;
      end if;
   end;
   CON.DR.disconnect;

end Bad_Select;
