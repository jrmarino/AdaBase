---
title: Client Traits
---

<div class="leftside">
<h3>String function<br/>
AdaBase.Driver.Base.[DB].trait_client_version ()</h3>
<p>This is a connection attribute.  It returns the version number as reported by
the database client as a string.</p>
<br/>
<h3>String function<br/>
AdaBase.Driver.Base.[DB].trait_client_info ()</h3>
<p>This is a connection attribute.  It returns a string containing additional
information about the the database client, such as the client library version
number.</p>
<pre class="code">
with Ada.Text_IO;
with AdaBase;
with Connect;

procedure Traits is

   package TIO renames Ada.Text_IO;
   package CON renames Connect;

   --  Database_Driver renames specific driver using subtype

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
   end display_traits;

begin

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

   CON.DR.set_trait_autocommit    (trait => True);
   CON.DR.set_trait_column_case   (trait => AdaBase.upper_case);
   CON.DR.set_trait_error_mode    (trait => AdaBase.silent);
   CON.DR.set_trait_max_blob_size (trait => 2 ** 16);

   display_traits   (driver => CON.DR);
   CON.DR.disconnect;

end Traits;
</pre>
<p class="caption">Example code: testcases/traits/traits.adb</p>
<br/>
<pre class="output">
   client info: 5.6.27
client version: 5.06.27
   server info: 5.6.27
server version: 5.06.27
        driver: MySQL 5.5+ native driver

    autocommit: FALSE
   column case: NATURAL_CASE
    error_mode: WARNING
     blob_size:  4096

    autocommit: TRUE
   column case: UPPER_CASE
    error_mode: SILENT
     blob_size:  65536
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
<p>{{ page.supported_drivers }}</p>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.trait_server }}</li>
    <li>{{ page.trait_driver }}</li>
    <li>{{ page.trait_autocommit }}</li>
    <li>{{ page.trait_column_case}}</li>
    <li>{{ page.trait_error_mode }}</li>
    <li>{{ page.trait_blob_size }}</li>
    <li>{{ page.trait_compressed }}</li>
    <li>{{ page.trait_multiquery }}</li>
    <li>{{ page.trait_buffers }}</li>
  </ul>
</div>
