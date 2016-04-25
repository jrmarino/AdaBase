---
title: Discard rest of result data
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Statement.Base.[STMT].discard_rest</h3>
<p>
This procedure frees up the resources used by the statement during the
iteration of the current result set.  The resources are normally freed
after the last row is fetched from the result set, but the <b>discard_rest</b>
procedure discards the remaining rows directly and without having to complete
the iteration.  It is analogous to "close cursor" routines seen on other
drivers.
</p>
<p>
Flushing unretrieved rows is required before the statement can perform
another query.  If the driver cannot natively discard the data, it will
internally iterate though the result set to simulate closing the cursor.
</p>
<pre class="code">
with AdaBase;
with Connect;
with CommonText;
with Ada.Text_IO;
with AdaBase.Results.Sets;

procedure Bad_Select is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ARS renames AdaBase.Results.Sets;
   package CT  renames CommonText;

   row : ARS.DataRow_Access;
   sql : String := "SELECT fruit, calories FROM froits " &
                   "WHERE color = 'red'";

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

   TIO.Put_Line ("Query successful: " & CON.STMT.successful'Img);

   if not CON.STMT.successful then
      TIO.Put_Line ("  Driver message: " & CON.STMT.last_driver_message);
      TIO.Put_Line ("     Driver code: " & CON.STMT.last_driver_code'Img);
      TIO.Put_Line ("       SQL State: " & CON.STMT.last_sql_state);
      --  Fix SQL typo
      sql (31) := 'u';
      TIO.Put_Line ("");
      TIO.Put_Line ("SQL now: " & sql);
      CON.STMT := CON.DR.query (sql);
      TIO.Put_Line ("Query successful: " & CON.STMT.successful'Img);

      row := CON.STMT.fetch_next;
      TIO.Put_Line ("   Number fields:" & row.count'Img);

      CON.STMT.discard_rest;
      TIO.Put_Line ("  Data discarded: " & CON.STMT.data_discarded'Img);
   end if;
   CON.DR.disconnect;

end Bad_Select;
</pre>
<p class="caption">Example code: testcases/bad_select/bad_select.adb</p>
<br/>
<pre class="output">
Query successful: FALSE
  Driver message: Table 'adabase_examples.froits' doesn't exist
     Driver code:  1146
       SQL State: 42S02

SQL now: SELECT fruit, calories FROM fruits WHERE color = 'red'
Query successful: TRUE
   Number fields: 2
  Data discarded: TRUE
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_statements }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.stmt_successful }}</li>
    <li>{{ page.stmt_data_discarded }}</li>
    <li>{{ page.stmt_query }}</li>
    <li>{{ page.stmt_driver_msg }}</li>
    <li>{{ page.stmt_driver_code }}</li>
    <li>{{ page.stmt_driver_state }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.fetch_next }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
  </ul>
</div>
