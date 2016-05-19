---
title: MultiQuery Trait
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Driver.Base.[DB].trait_multiquery_enabled ()</h3>
<p>This is a connection attribute.  It returns True if the driver is
configured to accept multiple queries separated by semicolons in the
query string.  This feature is fully supported by all drivers, but it
only applies to direct statements.  Prepare statements are limited to
single queries by definition.  This attribute is <b>False</b> by
default on all drivers.
</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_multiquery_enabled (trait : Boolean)</h3>
<p>This procedure is used to set the multiquery support.  It can be set
anytime (before or after establishing the connection).</p>
<p>
The SQLite driver supports multiple queries in direct statements by
default, and in fact this behavior cannot be disabled.  Attempting to do
so results in an exception (demonstrated below).
</p>
<pre class="code">
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

   procedure followup (numrows : AdaBase.AffectedRows)
   is
      use type AdaBase.AffectedRows;
   begin
      if numrows = 0 then
         TIO.Put_Line ("Query failed!");
         TIO.Put_Line ("Driver Message: " & CON.DR.last_driver_message);
      else
         TIO.Put_Line ("Query succeeded");
      end if;
   end followup;

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
      followup (numrows);
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
      followup (numrows);
      CON.DR.rollback;
   exception
      when ouch : others =>
         TIO.Put_Line ("Exception: " & EX.Exception_Message (ouch));
         TIO.Put_Line ("Failed to test this setting");
   end;

   CON.DR.disconnect;

end MultiQuery;
</pre>
<p class="caption">testcases/multiquery/multiquery.adb</p>
<br/>
<pre class="output">
This demonstration shows how multiple queries in the same SQL string are handled.
SQL string used: DELETE FROM fruits WHERE color = 'red'; DELETE FROM fruits WHERE color = 'orange'

Testing query with MultiQuery option set to FALSE
--  Execution attempt #1  --
Exception: Driver is configured to allow only one query at time, but this SQL contains multiple queries: DELETE FROM fruits WHERE color = 'red'; DELETE FROM fruits WHERE color = 'orange'
Failed to test this setting

Attempt to toggle MultiQuery setting to TRUE
--  Execution attempt #2  --
Query succeeded
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
    <li>{{ page.trait_autocommit }}</li>
    <li>{{ page.trait_column_case}}</li>
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_blob_size }}</li>
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
