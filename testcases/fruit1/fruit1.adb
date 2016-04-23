with AdaBase;
with Connect;
with Ada.Text_IO;

procedure Fruit1 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;

   numrows : AdaBase.AffectedRows;

   cmd : constant String := "DELETE FROM fruits WHERE color = 'red'";

begin

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   numrows := CON.DR.execute (sql => cmd);

   TIO.Put_Line ("SQL: " & cmd);
   TIO.Put_Line ("Result: Deleted" & numrows'Img & " rows");

   CON.DR.rollback;
   CON.DR.disconnect;

end Fruit1;
