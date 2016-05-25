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

   row : ARS.Datarow;
   sql : String := "SELECT fruit, calories FROM froits " &
                   "WHERE color = 'red'";

   success : Boolean := True;

begin

   --  PostgreSQL will abort a transaction even for a bad select
   --  so in this case, let's not start any transactions
   CON.DR.set_trait_autocommit (True);

   CON.connect_database;

   declare
      stmt : CON.Stmt_Type := CON.DR.query (sql);
   begin
      TIO.Put_Line ("Query successful: " & stmt.successful'Img);

      if not stmt.successful then
         TIO.Put_Line ("  Driver message: " & stmt.last_driver_message);
         TIO.Put_Line ("     Driver code: " & stmt.last_driver_code'Img);
         TIO.Put_Line ("       SQL State: " & stmt.last_sql_state);
         success := False;
      end if;
   end;

   if not success then
      --  Fix SQL typo
      sql (31) := 'u';
      TIO.Put_Line ("");
      TIO.Put_Line ("SQL now: " & sql);
      declare
         stmt : CON.Stmt_Type := CON.DR.query (sql);
      begin
         TIO.Put_Line ("Query successful: " & stmt.successful'Img);

         row := stmt.fetch_next;
         if not row.data_exhausted then
            TIO.Put_Line ("   Number fields:" & row.count'Img);

            stmt.discard_rest;
            TIO.Put_Line ("  Data discarded: " & stmt.data_discarded'Img);
         end if;
      end;
   end if;
   CON.DR.disconnect;

end Bad_Select;
