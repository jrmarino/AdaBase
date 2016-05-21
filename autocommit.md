---
title: Auto-commit Settings
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Driver.Base.[DB].trait_autocommit ()</h3>
<p>This is a connection attribute.  It returns True when the driver is
configured to commit queries immediately (e.g. one query transactions).
By default, explicit transactions are assumed, thus by default this
attribute is False.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_autocommit (trait : Boolean)</h3>
<p>This procedure is used to change the autocommit behavior.  There is
only a noticable effect when the trait setting is opposite of the
driver's current setting.  Passing True will ensure each query is
followed by a commit while passing False will ensure no query is
realized until a commit command is executed.</p>
<pre class="code">
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
   procedure expect (impacted, expected : Natural);
   procedure show_results (expected : Natural);

   procedure expect (impacted, expected : Natural) is
   begin
      TIO.Put_Line ("Rows expected:" & expected'Img);
      TIO.Put_Line ("Rows returned:" & impacted'Img);
      if impacted = expected then
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
         row  : ARS.Datarow;
         NR   : Natural := 0;
      begin
         loop
            row := stmt.fetch_next;
            exit when row.data_exhausted;
            NR := NR + 1;
         end loop;
         expect (NR, expected);
      end;
      CON.DR.disconnect;
   end show_results;

   procedure test_uno (expected : Natural; version2 : Boolean) is
      AR : AdaBase.Affected_Rows;
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
      AR : AdaBase.Affected_Rows;
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
      AR : AdaBase.Affected_Rows;
   begin
      CON.connect_database;
      AR := CON.DR.execute (ins1);
      CON.DR.set_trait_autocommit (True);
      CON.DR.disconnect;
      show_results (expected);
   end test_tres;

   procedure clear_table
   is
      AR : AdaBase.Affected_Rows;
   begin
      CON.connect_database;
      AR := CON.DR.execute (del);
      if not CON.DR.trait_autocommit then
         CON.DR.commit;
      end if;
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
</pre>
<p class="caption">Example code: testcases/autocommit/autocommit.adb</p>
<br/>
<pre class="output">
=== PRECONNECT AUTOCOMMIT => OFF (DEFAULT) ===
Rows expected: 0
Rows returned: 0
=== PASSED ===

=== PRECONNECT AUTOCOMMIT => ON ===
Rows expected: 2
Rows returned: 2
=== PASSED ===

=== CONNECT AC=0, INS, AC=1, INS, AC=0, INS ===
Rows expected: 2
Rows returned: 2
=== PASSED ===

=== IMPLICIT COMMIT (AC/OFF => ON) ===
Rows expected: 1
Rows returned: 1
=== PASSED ===

=== PRECONNECT AUTOCOMMIT => OFF WITH COMMIT ===
Rows expected: 2
Rows returned: 2
=== PASSED ===

=== PRECONNECT AUTOCOMMIT => ON WITH COMMIT (NO-OP) ===
Rows expected: 2
Rows returned: 2
=== PASSED ===

=== CONNECT AC=0, INS, AC=1, INS, AC=0, INS COMMIT ===
Rows expected: 3
Rows returned: 3
=== PASSED ===

</pre>
<p class="caption">Output using any driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.trait_client }}</li>
    <li>{{ page.trait_server }}</li>
    <li>{{ page.trait_driver }}</li>
    <li>{{ page.trait_column_case}}</li>
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_blob_size }}</li>
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_multiquery }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
