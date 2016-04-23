---
title: Compressed Protocol Trait
---

<div class="leftside">
<h3>Boolean function<br/>
AdaBase.Driver.Base.[DB].trait_protocol_compressed ()</h3>
<p>This is a connection attribute.  It returns True if the driver is
configured to have the client and server compress the communication
protocol between them.</p>
<br/>
<h3>Procedure<br/>
AdaBase.Driver.Base.[DB].set_trait_protocol_compressed (trait : Boolean)</h3>
<p>This procedure activates the compression on the protocol between the
client and the server.  For MySQL, it must be set before the connection is
made, otherwise an exception is thrown.  The MySQL protocol is compressed
by default, so normally the procedure is used to disable compression.</p>
<br/>
<p>Most drivers do not support these compression traits, and trying to use
them on an unsupported driver will result in a compilation failure.</p>
<pre class="code">
with Ada.Text_IO;
with AdaBase;
with AdaBase.Logger.Facility;
with Connect;

procedure Traits_MySQL is

   package TIO renames Ada.Text_IO;
   package ALF renames AdaBase.Logger.Facility;
   package CON renames Connect;

   --  Database_Driver renames MySQL driver using subtype

   procedure display_versions (driver : CON.Database_Driver);
   procedure display_traits   (driver : CON.Database_Driver);

   procedure display_versions (driver : CON.Database_Driver) is
   begin
      TIO.Put_Line ("   client info: " & driver.trait_client_info);
      TIO.Put_Line ("client version: " & driver.trait_client_version);
      TIO.Put_Line ("   server info: " & driver.trait_server_info);
      TIO.Put_Line ("server version: " & driver.trait_server_version);
      TIO.Put_Line ("        driver: " & driver.trait_driver);
   end display_versions;

   procedure display_traits (driver : CON.Database_Driver) is
   begin
      TIO.Put_Line ("");
      TIO.Put_Line ("    autocommit: " & driver.trait_autocommit'Img);
      TIO.Put_Line ("   column case: " & driver.trait_column_case'Img);
      TIO.Put_Line ("    error_mode: " & driver.trait_error_mode'Img);
      TIO.Put_Line ("     blob_size: " & driver.trait_max_blob_size'Img);
      TIO.Put_Line ("    compressed: " & driver.trait_protocol_compressed'Img);
      TIO.Put_Line ("    multiquery: " & driver.trait_multiquery_enabled'Img);
      TIO.Put_Line (" using buffers: " & driver.trait_query_buffers_used'Img);
   end display_traits;

begin

   CON.DR.command_standard_logger (device => ALF.screen,
                                  action => ALF.attach);

   declare
   begin
      CON.connect_database;
   exception
      when others =>
         TIO.Put_Line ("database connection failed, ending.");
         return;
   end;

   display_versions (driver => CON.DR);
   display_traits   (driver => CON.DR);

   CON.DR.set_trait_error_mode         (trait => AdaBase.raise_exception);
   CON.DR.set_trait_multiquery_enabled (trait => True);
   CON.DR.set_trait_query_buffers_used (trait => True);

   display_traits   (driver => CON.DR);
   CON.DR.disconnect;

   CON.DR.set_trait_query_buffers_used  (trait => False);
   CON.DR.set_trait_protocol_compressed (trait => False);

   CON.connect_database;
   display_traits   (driver => CON.DR);
   CON.DR.disconnect;

end Traits_MySQL;
</pre>
<p class="caption">Example code: testcases/traits/traits.adb</p>
<br/>
<pre class="output">
2016-04-22 20:47:44    mysql :       Connect : Connection to test database succeeded.
   client info: 5.6.27
client version: 5.06.27
   server info: 5.6.27
server version: 5.06.27
        driver: MySQL 5.5+ native driver

    autocommit: FALSE
   column case: NATURAL_CASE
    error_mode: WARNING
     blob_size:  4096
    compressed: TRUE
    multiquery: FALSE
 using buffers: TRUE

    autocommit: FALSE
   column case: NATURAL_CASE
    error_mode: RAISE_EXCEPTION
     blob_size:  4096
    compressed: TRUE
    multiquery: TRUE
 using buffers: TRUE
2016-04-22 20:47:44    mysql :    Disconnect : Disconnect From database
2016-04-22 20:47:44    mysql :       Connect : Connection to test database succeeded.

    autocommit: FALSE
   column case: NATURAL_CASE
    error_mode: RAISE_EXCEPTION
     blob_size:  4096
    compressed: FALSE
    multiquery: TRUE
 using buffers: FALSE
2016-04-22 20:47:44    mysql :    Disconnect : Disconnect From database
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>This trait is limited to specific drivers.  Specially [DB] is limited to "MySQL.MySQL_Driver"</p>
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
    <li>{{ page.trait_multiquery }}</li>
    <li>{{ page.trait_buffers }}</li>
    <li>{{ page.screen_logger }}</li>
  </ul>
</div>
