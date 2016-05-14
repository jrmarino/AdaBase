with AdaBase;
with Connect;
with MyLogger;
with Ada.Text_IO;
with AdaBase.Logger.Facility;

procedure Fruit4 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;

   numrows : AdaBase.AffectedRows;

   cmd1 : constant String := "INSERT INTO fruits (fruit, color, calories) " &
                             "VALUES ('blueberry', 'purple', 1)";
   cmd2 : constant String := "INSERT INTO fruits (fruit, color, calories) " &
                             "VALUES ('date', 'brown', 66)";
   atch : constant String := "The custom logger has been attached.";
   dtch : constant String := "The custom logger has been detached.";

begin

   CON.DR.attach_custom_logger (logger_access => MyLogger.clogger'Access);
   TIO.Put_Line (atch);

   CON.connect_database;

   numrows := CON.DR.execute (sql => cmd1);
   TIO.Put_Line ("SQL: " & cmd1);
   TIO.Put_Line ("Result: Inserted" & numrows'Img & " rows");
   TIO.Put_Line ("ID of last inserted row:" & CON.DR.last_insert_id'Img);

   CON.DR.detach_custom_logger;
   TIO.Put_Line (dtch);

   numrows := CON.DR.execute (sql => cmd2);
   TIO.Put_Line ("SQL: " & cmd2);
   TIO.Put_Line ("Result: Inserted" & numrows'Img & " rows");
   TIO.Put_Line ("ID of last inserted row:" & CON.DR.last_insert_id'Img);

   CON.DR.attach_custom_logger (logger_access => MyLogger.clogger'Access);
   TIO.Put_Line (atch);

   CON.DR.commit;
   CON.DR.disconnect;

end Fruit4;
