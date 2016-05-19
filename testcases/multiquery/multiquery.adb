with AdaBase;
with Connect;
with Ada.Text_IO;
with Ada.Exceptions;

procedure MultiQuery is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package EX  renames Ada.Exceptions;

   numrows : AdaBase.AffectedRows;
   setting : Boolean;
   nextone : Boolean;

   SQL : constant String :=
         "DELETE FROM fruits WHERE color = 'red'; " &
         "DELETE FROM fruits WHERE color = 'orange'";
begin

   CON.connect_database;

   TIO.Put_Line ("This demonstration shows how multiple queries in the " &
                 "same SQL string are handled.");
   TIO.Put_Line ("SQL string used: " & SQL);
   TIO.Put_Line ("");

   setting := CON.DR.trait_multiquery_enabled;
   nextone := not setting;

   TIO.Put_Line ("Testing query with MultiQuery option set to " & setting'Img);
   TIO.Put_Line ("--  Execution attempt #1  --");
   begin
      numrows := CON.DR.execute (SQL);
      TIO.Put_Line ("Query succeeded");
      CON.DR.rollback;
   exception
      when ouch : others =>
         TIO.Put_Line ("Exception: " & EX.Exception_Message (ouch));
         TIO.Put_Line ("Failed to test this setting");
   end;

   TIO.Put_Line ("");
   TIO.Put_Line ("Attempt to toggle MultiQuery setting to " & nextone'Img);
   begin
      CON.DR.set_trait_multiquery_enabled (nextone);
      TIO.Put_Line ("--  Execution attempt #2  --");
      numrows := CON.DR.execute (SQL);
      TIO.Put_Line ("Query succeeded");
      CON.DR.rollback;
   exception
      when ouch : others =>
         TIO.Put_Line ("Exception: " & EX.Exception_Message (ouch));
         TIO.Put_Line ("Failed to test this setting");
   end;

   CON.DR.disconnect;

end MultiQuery;
