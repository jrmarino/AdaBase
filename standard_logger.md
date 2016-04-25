---
title: Error Mode Settings
---

<div class="leftside">
<pre class="code">
package AdaBase.Logger.Facility is
   type TAction is (attach, detach);
   type TLogger is (file, screen);
end AdaBase.Logger.Facility;
</pre>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].command_standard_logger (device : TLogger; action : TAction)</h3>
<p>There are three types of loggers (mechanisms uses to log AdaBase activity)
and it is possible to use one instance of all three types simultaneously.  The
first two types are considered "standard" and they write to screen (stdout) and
a file.  This procedure handles the activation and deactivation of these
standard loggers.  The third type is "custom" which uses a dedicated set of
attach and detach functions to operate.</p>
<p>To activate a standard logger, set the "action" argument to "attach".  To
deactivate an active logger, execute the procedure again with "action" set to
"detach".  Attempting to activate an active logger or deactivate an inactive
logger will result in thrown exception.  The device argument must be set
to either "screen" or "file".  If the log filename is not defined, the log
will be written to the /tmp/adabase.log file.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_logger_filename (filename : String)</h3>
<p>This procedure is used to define the location of the standard log
file.  The file will be created if necessary, and it will be appended
if it already exists.</p>
<pre class="code">
with AdaBase;
with Connect;
with Ada.Text_IO;
with AdaBase.Logger.Facility;

procedure Fruit3 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;

   numrows : AdaBase.AffectedRows;

   log : constant String := "/tmp/fruit3.test.log";
   cmd : constant String := "DELETE FROM fruits WHERE color = 'red'";

begin

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.attach);
   CON.DR.set_logger_filename (filename => log);
   TIO.Put_Line ("The " & log & " has been attached.");

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connect failed.");
         return;
   end;

   numrows := CON.DR.execute (sql => cmd);
   TIO.Put_Line ("SQL: " & cmd);
   TIO.Put_Line ("Result: Deleted" & numrows'Img & " rows");

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.detach);
   TIO.Put_Line ("The " & log & " has been deattached.");

   numrows := CON.DR.execute (sql => cmd);
   TIO.Put_Line ("Second execution:");
   TIO.Put_Line ("Result: Deleted" & numrows'Img & " rows");

   CON.DR.command_standard_logger (device => ALF.file, action => ALF.attach);
   TIO.Put_Line ("The " & log & " has been attached.");

   CON.DR.rollback;
   CON.DR.disconnect;

end Fruit3;
</pre>
<p class="caption">Example code: testcases/fruit3/fruit3.adb</p>
<br/>
<pre class="output">
The /tmp/fruit3.test.log has been attached.
SQL: DELETE FROM fruits WHERE color = 'red'
Result: Deleted 5 rows
The /tmp/fruit3.test.log has been deattached.
Second execution:
Result: Deleted 0 rows
The /tmp/fruit3.test.log has been attached.
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<pre class="output">
2016-04-24 11:05:04    mysql :       Connect : Connection to adabase_examples database succeeded.
2016-04-24 11:05:04    mysql :       Execute : DELETE FROM fruits WHERE color = 'red'
2016-04-24 11:05:04    mysql :    Disconnect : Disconnect From database
</pre>
<p class="caption">Contents of /tmp/fruit3.test.log</p>
<br/>
<p class="caption">See {{ page.query_drop_table }} and {{ page.trait_compressed }}
for other examples using the standard screen logger.</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.custom_logger }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.rollback }}</li>
  </ul>
</div>
