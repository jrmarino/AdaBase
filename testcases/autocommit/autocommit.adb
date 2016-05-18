with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Autocommit is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

   insX : constant String := "INSERT INTO fruits (id, fruit, color, calories)";
   ins1 : constant String := insX & " VALUES (100,'cucumber','green',16)";
   ins2 : constant String := insX & " VALUES (101,'kumquat','yellow',71)";
   ins3 : constant String := insX & " VALUES (102,'persimmon','orange',127)";
   sel  : constant String := "SELECT * FROM fruits WHERE id > 99";
   del  : constant String := "DELETE FROM fruits WHERE id > 99";

   procedure clear_table;
   procedure test_uno (expected : Natural; version2 : Boolean);
   procedure test_dos (expected : Natural; version2 : Boolean);
   procedure test_tres (expected : Natural);
   procedure expect (impacted : AdaBase.AffectedRows; expected : Natural);
   procedure show_results (expected : Natural);

   procedure expect (impacted : AdaBase.AffectedRows; expected : Natural) is
   begin
      TIO.Put_Line ("Rows expected:" & expected'Img);
      TIO.Put_Line ("Rows returned:" & impacted'Img);
      if Natural (impacted) = expected then
         TIO.Put_Line ("=== PASSED ===");
      else
         TIO.Put_Line ("=== FAILED ===   <<----------------------------");
      end if;
      TIO.Put_Line ("");
   end expect;

   procedure show_results (expected : Natural) is
   begin
      CON.connect_database;
      declare
         stmt : CON.Stmt_Type := CON.DR.query (sel);
      begin
         expect (stmt.rows_returned, expected);
      end;
      CON.DR.disconnect;
   end show_results;

   procedure test_uno (expected : Natural; version2 : Boolean) is
      AR : AdaBase.AffectedRows;
   begin
      CON.connect_database;
      AR := CON.DR.execute (ins1);
      AR := CON.DR.execute (ins2);
      if version2 then
         CON.DR.commit;
      end if;
      CON.DR.disconnect;
      show_results (expected);
   end test_uno;

   procedure test_dos (expected : Natural; version2 : Boolean)
   is
      AR : AdaBase.AffectedRows;
   begin
      CON.connect_database;
      AR := CON.DR.execute (ins1);
      CON.DR.set_trait_autocommit (True);
      AR := CON.DR.execute (ins2);
      CON.DR.set_trait_autocommit (False);
      AR := CON.DR.execute (ins3);
      if version2 then
         CON.DR.commit;
      end if;
      CON.DR.disconnect;
      show_results (expected);
   end test_dos;

   procedure test_tres (expected : Natural)
   is
      AR : AdaBase.AffectedRows;
   begin
      CON.connect_database;
      AR := CON.DR.execute (ins1);
      CON.DR.set_trait_autocommit (True);
      CON.DR.disconnect;
      show_results (expected);   
   end test_tres;

   procedure clear_table
   is
      AR : AdaBase.AffectedRows;
   begin
      CON.connect_database;
      AR := CON.DR.execute (del);
      CON.DR.commit;
      CON.DR.disconnect;
   end clear_table;

begin

   clear_table;

   TIO.Put_Line ("=== PRECONNECT AUTOCOMMIT => OFF (DEFAULT) ===");
   CON.DR.set_trait_autocommit (False);
   test_uno (0, False);

   clear_table;

   TIO.Put_Line ("=== PRECONNECT AUTOCOMMIT => ON ===");
   CON.DR.set_trait_autocommit (True);
   test_uno (2, False);

   clear_table;

   TIO.Put_Line ("=== CONNECT AC=0, INS, AC=1, INS, AC=0, INS ===");
   CON.DR.set_trait_autocommit (False);
   test_dos (2, False);

   clear_table;

   TIO.Put_Line ("=== IMPLICIT COMMIT (AC/OFF => ON) ===");
   CON.DR.set_trait_autocommit (False);
   test_tres (1);

   clear_table;

   TIO.Put_Line ("=== PRECONNECT AUTOCOMMIT => OFF WITH COMMIT ===");
   CON.DR.set_trait_autocommit (False);
   test_uno (2, True);

   clear_table;

   TIO.Put_Line ("=== PRECONNECT AUTOCOMMIT => ON WITH COMMIT (NO-OP) ===");
   CON.DR.set_trait_autocommit (True);
   test_uno (2, True);

   clear_table;

   TIO.Put_Line ("=== CONNECT AC=0, INS, AC=1, INS, AC=0, INS COMMIT ===");
   CON.DR.set_trait_autocommit (False);
   test_dos (3, True);

end Autocommit;
