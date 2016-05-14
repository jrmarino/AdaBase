with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Stored_Procs is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;

   stmt_acc : CON.Stmt_Type_access;

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

      row     : ARS.DataRow;
      numcols : constant Natural := stmt_acc.column_count;
   begin
      for c in Natural range 1 .. numcols loop
         TIO.Put (pad (stmt_acc.column_name (c)));
      end loop;
      TIO.Put_Line ("");
      for c in Natural range 1 .. numcols loop
         TIO.Put ("============== ");
      end loop;
      TIO.Put_Line ("");
      loop
         row := stmt_acc.fetch_next;
         exit when row.data_exhausted;
         for c in Natural range 1 .. numcols loop
            TIO.Put (pad (row.column (c).as_string));
         end loop;
         TIO.Put_Line ("");
      end loop;
      TIO.Put_Line ("");
   end dump_result;

   sql         : constant String := "CALL multiple_rowsets";
   set_fetched : Boolean;
   set_present : Boolean;

begin

   CON.connect_database;

   declare
      stmt : aliased CON.Stmt_Type := CON.DR.query (sql);
   begin
      if stmt.successful then
         set_fetched := True;
      else
         TIO.Put_Line ("Stored procedures not supported on " &
            CON.DR.trait_driver);
      end if;
      set_fetched := stmt.successful;
      stmt_acc := stmt'Unchecked_Access;
      loop
         if set_fetched then
            dump_result;
         end if;
         stmt.fetch_next_set (set_present, set_fetched);
         exit when not set_present;
      end loop;
   end;

   CON.DR.disconnect;

end Stored_Procs;
