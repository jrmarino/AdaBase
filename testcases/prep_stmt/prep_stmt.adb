with AdaBase;
with Connect;
with Ada.Text_IO;
with CommonText;
with AdaBase.Results.Sets;
with AdaBase.Logger.Facility;

procedure Prep_Stmt is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package CT  renames CommonText;
   package AR  renames AdaBase.Results;
   package ARS renames AdaBase.Results.Sets;
   package ALF renames AdaBase.Logger.Facility;

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

   declare
      max_calories : aliased AR.byte2 := 200;
      min_calories : constant AR.byte2 := 5;
      row : ARS.DataRow_Access;
   begin
      CON.STMT := CON.DR.prepare_select
        (tables     => "fruits",
         columns    => "*",
         conditions => "color = ? and calories > :mincal and calories < ?");

      CON.STMT.assign (1, "red");
      CON.STMT.assign ("mincal", min_calories);
      CON.STMT.assign (3, max_calories'Unchecked_Access);

      if CON.STMT.execute then
         TIO.Put_Line ("execute succeeded");
         for c in Natural range 1 .. CON.STMT.column_count loop
            TIO.Put_Line ("Column" & c'Img & " heading: " &
                          CON.STMT.column_name (c));
         end loop;
         TIO.Put_Line ("returned rows: " & CON.STMT.rows_returned'Img);
         loop
            exit when not CON.STMT.fetch_next (row);
            TIO.Put_Line (row.column (2).as_string & " (" &
                          row.column ("color").as_string & ") " &
                          row.column ("calories").as_string  & " calories");
         end loop;
      else
         TIO.Put_Line ("execute failed");
      end if;
   end;

   declare
      sql : String := "INSERT INTO fruits (fruit, color, calories) " &
                      "VALUES ('potato','tan', 77)";
   begin
      CON.STMT := CON.DR.prepare (sql);
      if CON.STMT.execute then
         TIO.Put_Line ("Inserted row " & CON.STMT.last_insert_id'Img);
      end if;
      CON.DR.rollback;
   end;

   CON.DR.disconnect;

end Prep_Stmt;
