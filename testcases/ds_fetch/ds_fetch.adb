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

   row : ARS.DataRow_Access;
   sql : constant String := "SELECT * FROM fruits WHERE color = 'orange'";

begin

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   CON.STMT := CON.DR.query (sql);

   TIO.Put_Line (" Query successful: " & CON.STMT.successful'Img);
   TIO.Put_Line ("   Data Discarded: " & CON.STMT.data_discarded'Img);
   TIO.Put_Line ("Number of columns:" & CON.STMT.column_count'Img);
   TIO.Put_Line ("   Number of rows:" & CON.STMT.rows_returned'Img);

   TIO.Put_Line ("");
   for c in Natural range 1 .. CON.STMT.column_count loop
      TIO.Put_Line ("Column" & c'Img & ":");
      TIO.Put_Line ("   TABLE: " & CON.STMT.column_table (c));
      TIO.Put_Line ("    NAME: " & CON.STMT.column_name (c));
      TIO.Put_Line ("    TYPE: " & CON.STMT.column_native_type (c)'Img);
   end loop;

   TIO.Put_Line ("");
   loop
      exit when not CON.STMT.fetch_next (row);
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

   CON.DR.disconnect;

end DS_Fetch;
