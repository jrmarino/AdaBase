with Ada.Text_IO;
with AdaBase;
with Connect;

procedure Traits is

   package TIO renames Ada.Text_IO;
   package CON renames Connect;

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

end Traits;
