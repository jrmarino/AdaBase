with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure DS_Fetch is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

begin

   CON.connect_database;

   declare
      sql  : constant String := "SELECT * FROM fruits WHERE color = 'orange'";
      stmt : CON.Stmt_Type := CON.DR.query (sql);
      row  : ARS.Datarow;
   begin
      TIO.Put_Line (" Query successful: " & stmt.successful'Img);
      TIO.Put_Line ("   Data Discarded: " & stmt.data_discarded'Img);
      TIO.Put_Line ("Number of columns:" & stmt.column_count'Img);
      TIO.Put_Line ("   Number of rows:" & stmt.rows_returned'Img);

      TIO.Put_Line ("");
      for c in Natural range 1 .. stmt.column_count loop
         TIO.Put_Line ("Column" & c'Img & ":");
         TIO.Put_Line ("   TABLE: " & stmt.column_table (c));
         TIO.Put_Line ("    NAME: " & stmt.column_name (c));
         TIO.Put_Line ("    TYPE: " & stmt.column_native_type (c)'Img);
      end loop;

      TIO.Put_Line ("");
      loop
         row := stmt.fetch_next;
         exit when row.data_exhausted;
         TIO.Put (CT.zeropad (Natural (row.column (1).as_byte2), 2) & " ");
         declare
            fruit : String := row.column ("fruit").as_string;
            frlen : Natural := fruit'Length;
            rest  : String (1 .. 12 - frlen) := (others => ' ');
         begin
            TIO.Put (rest & fruit);
         end;
         TIO.Put (" (" & row.column ("color").as_string & ") calories =");
         TIO.Put_Line (row.column (4).as_byte2'Img);
      end loop;
   end;

   CON.DR.disconnect;

end DS_Fetch;
