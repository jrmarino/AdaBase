with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Stored_Procs is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;

   procedure dump_result;
   procedure dump_result
   is
      function pad (S : String) return String;
      function pad (S : String) return String
      is
         field : String (1 .. 15) := (others => ' ');
         len   : Natural := S'Length;
      begin
         field (1 .. len) := S;
         return field;
      end pad;

      row     : ARS.DataRow_Access;
      numcols : constant Natural := CON.STMT.column_count;
   begin
      for c in Natural range 1 .. numcols loop
         TIO.Put (pad (CON.STMT.column_name (c)));
      end loop;
      TIO.Put_Line ("");
      for c in Natural range 1 .. numcols loop
         TIO.Put ("============== ");
      end loop;
      TIO.Put_Line ("");
      loop
         exit when not CON.STMT.fetch_next (row);
         for c in Natural range 1 .. numcols loop
            TIO.Put (pad (row.column (c).as_string));
         end loop;
         TIO.Put_Line ("");
      end loop;
      TIO.Put_Line ("");
   end dump_result;

   sql         : constant String := "CALL multiple_rowsets";
   set_fetched : Boolean := True;
   set_present : Boolean;

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
   loop
      if set_fetched then
         dump_result;
      end if;
      CON.STMT.fetch_next_set (set_present, set_fetched);
      exit when not set_present;
   end loop;

   CON.DR.disconnect;

end Stored_Procs;
