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

   CON.connect_database;

   declare
      max_calories : aliased AR.byte2 := 200;
      min_calories : constant AR.byte2 := 5;
      row          : ARS.DataRow;
      stmt : CON.Stmt_Type := CON.DR.prepare_select
        (tables     => "fruits",
         columns    => "*",
         conditions => "color = ? and calories > :mincal and calories < ?");
   begin
      stmt.assign (1, "red");
      stmt.assign ("mincal", min_calories);
      stmt.assign (3, max_calories'Unchecked_Access);

      if stmt.execute then
         TIO.Put_Line ("execute succeeded");
         for c in Natural range 1 .. stmt.column_count loop
            TIO.Put_Line ("Column" & c'Img & " heading: " &
                          stmt.column_name (c));
         end loop;
         TIO.Put_Line ("returned rows: " & stmt.rows_returned'Img);
         loop
            row := stmt.fetch_next;
            exit when row.data_exhausted;
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
      stmt : CON.Stmt_Type := CON.DR.prepare (sql);
   begin
      if stmt.execute then
         TIO.Put_Line ("Inserted row " & stmt.last_insert_id'Img);
         TIO.Put_Line ("Affected rows: " & stmt.rows_affected'Img);
      end if;
      CON.DR.rollback;
   end;

   CON.DR.disconnect;

end Prep_Stmt;
