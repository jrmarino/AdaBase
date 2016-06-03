with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;
with AdaBase.Results.Sets;

procedure UTF8 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package WIO renames Ada.Wide_Text_IO;
   package WWO renames Ada.Wide_Wide_Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

begin

   CON.connect_database;

   TIO.Put_Line ("Use terminal encoding UTF-8 or ISO8859-1");
   TIO.Put_Line ("Either UTF8 fields or string fields will look right, " &
                 "but not both");
   declare
      sql  : constant String := "SELECT * FROM funny_names";
      stmt : CON.Stmt_Type := CON.DR.query (sql);
      row  : ARS.Datarow;
   begin
      loop
         row := stmt.fetch_next;
         exit when row.data_exhausted;
         TIO.Put_Line ("");
         TIO.Put_Line ("    UTF8: " & row.column ("first_name").as_utf8 &
                       " " & row.column ("surname").as_utf8);
         TIO.Put_Line ("  STRING: " & row.column ("first_name").as_string &
                       " " & row.column ("surname").as_string);
         WIO.Put_Line (" WSTRING: " & row.column ("first_name").as_wstring &
                       " " & row.column ("surname").as_wstring);
         WWO.Put_Line ("WWSTRING: " & row.column ("first_name").as_wwstring &
                       " " & row.column ("surname").as_wwstring);
      end loop;
   end;

   CON.DR.disconnect;

end UTF8;
