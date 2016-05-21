with AdaBase;
with Connect;
with Ada.Text_IO;

procedure Fruit2 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;

   numrows : AdaBase.Affected_Rows;

   --  Intentionally broken UPDATE command (calories misspelled)
   cmd : constant String := "UPDATE fruits set caloriesx = 14 " &
                            "WHERE fruit = 'strawberry'";

begin

   CON.connect_database;
   CON.DR.set_trait_error_mode (trait => AdaBase.raise_exception);

   TIO.Put_Line ("SQL: " & cmd);
   declare
   begin
      numrows := CON.DR.execute (sql => cmd);
      TIO.Put_Line ("Result: Updated" & numrows'Img & " rows");
      CON.DR.rollback;
   exception
      when others =>
         TIO.Put_Line ("Error!");
         TIO.Put_Line ("Driver message: " & CON.DR.last_driver_message);
         TIO.Put_Line ("   Driver code: " & CON.DR.last_driver_code'Img);
         TIO.Put_Line ("     SQL State: " & CON.DR.last_sql_state);
   end;
   CON.DR.disconnect;

end Fruit2;
