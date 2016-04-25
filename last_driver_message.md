---
title: Last Driver Message
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type ErrorMode       is (silent, warning, raise_exception);
   subtype TSqlState    is String (1 .. 5);
   subtype DriverCodes  is Integer range -999 .. 1999;
   type TraxID          is mod 2 ** 64;
   subtype AffectedRows is TraxID;
end AdaBase;
</pre>
<h3>String function<br/>
AdaBase.Driver.Base.[DB].last_driver_message ()</h3>
<p>This function returns a driver-specific error message regarding the last
operation performed by the database handle.</p>
<pre class="code">
with AdaBase;
with Connect;
with Ada.Text_IO;

procedure Fruit2 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;

   numrows : AdaBase.AffectedRows;

   --  Intentionally broken UPDATE command (calories misspelled)
   cmd : constant String := "UPDATE fruits set caloriesx = 14 " &
                            "WHERE fruit = 'strawberry'";

begin

   declare
   begin
      CON.connect_database;
      CON.DR.set_trait_error_mode (trait => AdaBase.raise_exception);
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   TIO.Put_Line ("SQL: " & cmd);
   declare
   begin
      numrows := CON.DR.execute (sql => cmd);
      TIO.Put_Line ("Result: Updated" & numrows'Img & " rows");
      CON.DR.rollback;
   exception
      when others =>
         TIO.Put_Line ("Error!");
         TIO.Put_Line ("Driver message: " & CON.DR.last_driver_message);
         TIO.Put_Line ("   Driver code: " & CON.DR.last_driver_code'Img);
         TIO.Put_Line ("     SQL State: " & CON.DR.last_sql_state);
   end;
   CON.DR.disconnect;

end Fruit2;
</pre>
<p class="caption">Example code: testcases/fruit2/fruit2.adb</p>
<br/>
<pre class="output">
SQL: UPDATE fruits set caloriesx = 14 WHERE fruit = 'strawberry'
Error!
Driver message: Unknown column 'caloriesx' in 'field list'
   Driver code:  1054
     SQL State: 42S22
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.driver_code }}</li>
    <li>{{ page.driver_state }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.rollback }}</li>
    <li>{{ page.trait_error_mode }}</li>
  </ul>
</div>