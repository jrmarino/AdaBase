with Ada.Text_IO;
with Ada.Exceptions;
with AdaBase;
with Connect;
with GNAT.Traceback.Symbolic;

procedure Traits is

   package SYM renames GNAT.Traceback.Symbolic;
   package TIO renames Ada.Text_IO;
   package CON renames Connect;
   package EX  renames Ada.Exceptions;

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
      TIO.Put_Line (" multiquery on: " & driver.trait_multiquery_enabled'Img);
      TIO.Put_Line ("      encoding: " & driver.trait_character_set);
   end display_traits;

begin

   CON.connect_database;

   display_versions (driver => CON.DR);
   display_traits   (driver => CON.DR);

   CON.DR.disconnect;

   CON.DR.set_trait_autocommit    (trait => True);
   CON.DR.set_trait_column_case   (trait => AdaBase.upper_case);
   CON.DR.set_trait_error_mode    (trait => AdaBase.silent);
   CON.DR.set_trait_max_blob_size (trait => 2 ** 16);

   CON.DR.set_trait_multiquery_enabled (True);
   CON.DR.set_trait_character_set ("");

   CON.connect_database;
   display_traits   (driver => CON.DR);
   CON.DR.disconnect;

exception
   when E : others =>
      TIO.Put_Line ("");
      TIO.Put_Line ("exception name: " & EX.Exception_Name (E));
      TIO.Put_Line ("exception msg : " & EX.Exception_Message (E));
      TIO.Put_Line ("Traceback:");
      TIO.Put_Line (SYM.Symbolic_Traceback (E));

end Traits;
