---
title: Abstract Query: Drop Table
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].query_drop_table (tables : String;
when_exists : Boolean := False; cascade : Boolean := False)</h3>
<p>The procedure constructs and an appropriate and potentially driver-specific
query to drop a table in the connected database. Existence checks and
cascading deletions only occur if explicitly requested.  Multiple tables
can be dropped simulaneously by separating them with commas (however this
is not supported by all drivers, so it's more portable to issue multiple
single table query_drop_table commands).</p>

<pre class="code">
with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Logger.Facility;

procedure Wipe_Out is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;

   numrows : AdaBase.AffectedRows;

   bfast : constant String := "breakfast";
   cmd1  : constant String := "CREATE TABLE " & bfast &
                              " AS SELECT id, fruit FROM fruits";

begin

   CON.DR.command_standard_logger (device => ALF.screen, action => ALF.attach);

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   --  delete breakfast table if it already exists
   --  No need to set cascade; it illustrates logging on MySQL (only)
   CON.DR.query_drop_table (tables      => bfast,
                            cascade     => True,
                            when_exists => True);

   --  create breakfast table
   numrows := CON.DR.execute (sql => cmd1);

   --  clear contents of breakfast table
   CON.DR.query_clear_table (table => bfast);

   --  drop breakfast table again (minimal arguments)
   CON.DR.query_drop_table (tables => bfast);

   CON.DR.commit;
   CON.DR.disconnect;

end Wipe_Out;
</pre>
<p class="caption">Example code: testcases/wipe_out/wipe_out.adb</p>

<br/>
<pre class="output">
2016-04-25 07:11:51    mysql :       Connect : Connection to adabase_examples database succeeded.
2016-04-25 07:11:51    mysql :          Note : Requested CASCADE has no effect on MySQL
2016-04-25 07:11:51    mysql :       Execute : DROP TABLE IF EXISTS breakfast CASCADE
2016-04-25 07:11:51    mysql :       Execute : CREATE TABLE breakfast AS SELECT id, fruit FROM fruits
2016-04-25 07:11:51    mysql :       Execute : TRUNCATE breakfast
2016-04-25 07:11:51    mysql :       Execute : DROP TABLE breakfast
2016-04-25 07:11:51    mysql :    Disconnect : Disconnect From database
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.query_clear_table }}</li>
    <li>{{ page.standard_logger }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.commit }}</li>
  </ul>
</div>
