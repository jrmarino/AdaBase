with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Logger.Facility;

procedure Fruit3 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;

   numrows : AdaBase.Affected_Rows;

   log : constant String := "/tmp/fruit3.test.log";
   cmd : constant String := "DELETE FROM fruits WHERE color = 'red'";

begin

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.attach);
   CON.DR.set_logger_filename (filename => log);
   TIO.Put_Line ("The " & log & " has been attached.");

   CON.connect_database;

   numrows := CON.DR.execute (sql => cmd);
   TIO.Put_Line ("SQL: " & cmd);
   TIO.Put_Line ("Result: Deleted" & numrows'Img & " rows");

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.detach);
   TIO.Put_Line ("The " & log & " has been deattached.");

   numrows := CON.DR.execute (sql => cmd);
   TIO.Put_Line ("Second execution:");
   TIO.Put_Line ("Result: Deleted" & numrows'Img & " rows");

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.attach);
   TIO.Put_Line ("The " & log & " has been attached.");

   CON.DR.rollback;
   CON.DR.disconnect;

end Fruit3;
