---
title: Custom Loggers
---

<div class="leftside">
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].attach_custom_logger (logger_access :
AdaBase.Logger.Base.BaseClass_Logger_access)</h3>
<p>The procedure allows a customized logger that meets the criteria
of the logger interface to be used to record database activity.  Once
attached, logging begins immediately.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].detach_custom_logger ()</h3>
<p>This procedure permanently removes an attached custom logger.  To
restore logging, the logger will have to be attached again.</p>

<pre class="code">
with AdaBase.Logger.Base;

package MyLogger is

   package ALB renames AdaBase.Logger.Base;

   type CustomLogger is new ALB.Base_Logger and ALB.AIL.iLogger
      with null record;

   overriding
   procedure reaction (listener : CustomLogger);

   clogger : aliased CustomLogger;

end MyLogger;
</pre>
<p class="caption">Example custom logger code: testcases/fruit4/mylogger.ads</p>

<pre class="code">
with Ada.Text_IO.Unbounded_IO;

package body MyLogger is

   package UIO renames Ada.Text_IO.Unbounded_IO;
   package TIO renames Ada.Text_IO;

   overriding
   procedure reaction (listener : CustomLogger) is
   begin
      if listener.is_error then
         TIO.Put_Line ("## SQLSTATE: " & listener.sqlstate);
         TIO.Put_Line ("##   Driver:"  & listener.error_code'Img &
                                    "(" & listener.driver'Img & ")");
         TIO.Put      ("##    Error: "); UIO.Put_Line (listener.error_msg);
         TIO.Put_Line ("##    Phase: " & listener.category'Img);
      else
         TIO.Put_Line ("##    Phase: " & listener.category'Img);
         TIO.Put      ("##  message: "); UIO.Put_Line (listener.message);
      end if;
   end reaction;

end MyLogger;
</pre>
<p class="caption">Example custom logger code: testcases/fruit4/mylogger.adb</p>

<pre class="code">
with AdaBase;
with Connect;
with MyLogger;
with Ada.Text_IO;
with AdaBase.Logger.Facility;

procedure Fruit4 is

   package CON renames Connect;
   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;

   numrows : AdaBase.AffectedRows;

   cmd1 : constant String := "INSERT INTO fruits (fruit, color, calories) " &
                             "VALUES ('blueberry', 'purple', 1)";
   cmd2 : constant String := "INSERT INTO fruits (fruit, color, calories) " &
                             "VALUES ('date', 'brown', 66)";
   atch : constant String := "The custom logger has been attached.";
   dtch : constant String := "The custom logger has been detached.";

begin

   CON.DR.attach_custom_logger (logger_access => MyLogger.clogger'Access);
   TIO.Put_Line (atch);

   CON.connect_database;

   numrows := CON.DR.execute (sql => cmd1);
   TIO.Put_Line ("SQL: " & cmd1);
   TIO.Put_Line ("Result: Inserted" & numrows'Img & " rows");
   TIO.Put_Line ("ID of last inserted row:" & CON.DR.last_insert_id'Img);

   CON.DR.detach_custom_logger;
   TIO.Put_Line (dtch);

   numrows := CON.DR.execute (sql => cmd2);
   TIO.Put_Line ("SQL: " & cmd2);
   TIO.Put_Line ("Result: Inserted" & numrows'Img & " rows");
   TIO.Put_Line ("ID of last inserted row:" & CON.DR.last_insert_id'Img);

   CON.DR.attach_custom_logger (logger_access => MyLogger.clogger'Access);
   TIO.Put_Line (atch);

   CON.DR.commit;
   CON.DR.disconnect;

end Fruit4;
</pre>
<p class="caption">Example code: testcases/fruit4/fruit4.adb</p>

<br/>
<pre class="output">
The custom logger has been attached.
##    Phase: CONNECTING
##  message: Connection to adabase_examples database succeeded.
##    Phase: EXECUTION
##  message: INSERT INTO fruits (fruit, color, calories) VALUES ('blueberry', 'purple', 1)
SQL: INSERT INTO fruits (fruit, color, calories) VALUES ('blueberry', 'purple', 1)
Result: Inserted 1 rows
ID of last inserted row: 38
The custom logger has been detached.
SQL: INSERT INTO fruits (fruit, color, calories) VALUES ('date', 'brown', 66)
Result: Inserted 1 rows
ID of last inserted row: 39
The custom logger has been attached.
##    Phase: DISCONNECTING
##  message: Disconnect From database
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.standard_logger }}</li>
    <li>{{ page.connect }}</li>
    <li>{{ page.disconnect }}</li>
    <li>{{ page.execute }}</li>
    <li>{{ page.commit }}</li>
    <li>{{ page.last_insert_id }}</li>
  </ul>
</div>
