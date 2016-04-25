with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Logger.Facility;

procedure Wipe_Out is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;

   numrows : AdaBase.AffectedRows;

   bfast : constant String := "breakfast";
   cmd1  : constant String := "CREATE TABLE " & bfast &
                              " AS SELECT id, fruit FROM fruits";

begin

   CON.DR.command_standard_logger (device => ALF.screen, action => ALF.attach);

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   --  delete breakfast table if it already exists
   --  No need to set cascade; it illustrates logging on MySQL (only)
   CON.DR.query_drop_table (tables      => bfast,
                            cascade     => True,
                            when_exists => True);

   --  create breakfast table
   numrows := CON.DR.execute (sql => cmd1);

   --  clear contents of breakfast table
   CON.DR.query_clear_table (table => bfast);

   --  drop breakfast table again (minimal arguments)
   CON.DR.query_drop_table (tables => bfast);

   CON.DR.commit;
   CON.DR.disconnect;

end Wipe_Out;
