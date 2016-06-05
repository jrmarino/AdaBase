with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Spatial2 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

begin

   CON.connect_database;

   declare
      sql  : constant String := "SELECT sp_point, sp_linestring, " &
                                "ST_AsText (sp_point) as point2 " &
                                "FROM spatial_plus";
      stmt : CON.Stmt_Type := CON.DR.query (sql);
      row  : ARS.Datarow;
   begin
      if not stmt.successful then
         TIO.Put_Line (stmt.last_driver_message);
         return;
      end if;
      loop
         row := stmt.fetch_next;
         exit when row.data_exhausted;
         for x in Natural range 1 .. row.count loop
            TIO.Put ("column : ");
            TIO.Put (stmt.column_name (x));
            TIO.Put (" : " & row.column (x).native_type'Img);
            TIO.Put_Line (" : " & row.column (x).as_string);
         end loop;
      end loop;
   end;

   CON.DR.disconnect;

end Spatial2;
