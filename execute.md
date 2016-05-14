---
title: Execute (Driver)
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type TraxID          is mod 2 ** 64;
   subtype AffectedRows is TraxID;
end AdaBase;
</pre>
<h3>AffectedRows function<br/>
AdaBase.Driver.Base.[DB].execute (sql : String)</h3>
<p>This executes an SQL statement in a single function call,
returning the number of rows affected by the statement.</p>
<p>This function does not return results from a SELECT statement.
If results are desired, use the {{ page.query }} function to obtain a direct
or prepared statement that can be executed instead.</p>
<pre class="code">
with AdaBase;
with Connect;
with Ada.Text_IO;

procedure Fruit1 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;

   numrows : AdaBase.AffectedRows;

   cmd : constant String := "DELETE FROM fruits WHERE color = 'red'";

begin

   CON.connect_database;

   numrows := CON.DR.execute (sql => cmd);

   TIO.Put_Line ("SQL: " & cmd);
   TIO.Put_Line ("Result: Deleted" & numrows'Img & " rows");

   CON.DR.rollback;
   CON.DR.disconnect;

end Fruit1;
</pre>
<p class="caption">Example code: testcases/fruit1/fruit1.adb</p>
<br/>
<pre class="output">
SQL: DELETE FROM fruits WHERE color = 'red'
Result: Deleted 5 rows
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.query }}</li>
    <li>{{ page.commit }}</li>
    <li>{{ page.rollback }}</li>
  </ul>
</div>
