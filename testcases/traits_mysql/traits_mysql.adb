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
