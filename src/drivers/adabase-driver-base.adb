--
--  Copyright (c) 2015 John Marino <draco@marino.st>
--
--  Permission to use, copy, modify, and distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--


package body AdaBase.Driver.Base is


   ------------------------
   --  trait_autocommit  --
   ------------------------
   overriding
   function trait_autocommit (driver : Base_Driver) return Boolean
   is
   begin
      return driver.connection.all.autoCommit;
   end trait_autocommit;


   -------------------------
   --  trait_column_case  --
   -------------------------
   overriding
   function trait_column_case (driver : Base_Driver) return ACB.AD.CaseMode
   is
   begin
      return driver.connection.all.caseMode;
   end trait_column_case;


   ------------------------
   --  trait_error_mode  --
   ------------------------
   overriding
   function trait_error_mode (driver : Base_Driver) return ACB.AD.ErrorMode
   is
   begin
      return driver.connection.all.ErrorMode;
   end trait_error_mode;


   -------------------------
   --  trait_string_mode  --
   -------------------------
   overriding
   function trait_string_mode (driver : Base_Driver) return ACB.AD.StringMode
   is
   begin
      return driver.connection.all.stringMode;
   end trait_string_mode;


   -----------------------
   --  trait_connected  --
   -----------------------
   overriding
   function trait_connected (driver : Base_Driver) return Boolean
   is
   begin
      return driver.connection.all.connected;
   end trait_connected;


   --------------------
   --  trait_driver  --
   --------------------
   overriding
   function trait_driver (driver : Base_Driver) return ACB.AD.textual
   is
   begin
      return driver.connection.all.description;
   end trait_driver;


   -------------------------
   --  trait_client_info  --
   -------------------------
   overriding
   function trait_client_info (driver : Base_Driver)
                               return ACB.AD.textual
   is
   begin
      return driver.connection.all.clientInfo;
   end trait_client_info;


   ----------------------------
   --  trait_client_version  --
   ----------------------------
   overriding
   function trait_client_version (driver : Base_Driver)
                                  return ACB.AD.textual
   is
   begin
      return driver.connection.all.clientVersion;
   end trait_client_version;


   -------------------------
   --  trait_server_info  --
   -------------------------
   overriding
   function trait_server_info (driver : Base_Driver)
                               return ACB.AD.textual
   is
   begin
      return driver.connection.all.serverInfo;
   end trait_server_info;


   ----------------------------
   --  trait_server_version  --
   ----------------------------
   overriding
   function trait_server_version (driver : Base_Driver)
                                  return ACB.AD.textual
   is
   begin
      return driver.connection.all.serverVersion;
   end trait_server_version;


   ---------------------------
   --  trait_max_blob_size  --
   ---------------------------
   overriding
   function trait_max_blob_size (driver : Base_Driver)
                                 return ACB.AD.BLOB_maximum
   is
   begin
      return driver.connection.all.maxBlobSize;
   end trait_max_blob_size;


   ----------------------------
   --  set_trait_autocommit  --
   ----------------------------
   overriding
   procedure set_trait_autocommit  (driver : Base_Driver;
                                    trait  : Boolean)
   is
   begin
      driver.connection.all.setAutoCommit (auto => trait);
   end set_trait_autocommit;


   -----------------------------
   --  set_trait_column_case  --
   -----------------------------
   overriding
   procedure set_trait_column_case (driver : Base_Driver;
                                    trait  : ACB.AD.CaseMode)
   is
   begin
      driver.connection.all.setCaseMode (mode => trait);
   end set_trait_column_case;


   ----------------------------
   --  set_trait_error_mode  --
   ----------------------------
   overriding
   procedure set_trait_error_mode  (driver : Base_Driver;
                                    trait  : ACB.AD.ErrorMode)
   is
   begin
      driver.connection.all.setErrorMode (mode => trait);
   end set_trait_error_mode;


   -----------------------------
   --  set_trait_string_mode  --
   -----------------------------
   overriding
   procedure set_trait_string_mode (driver : Base_Driver;
                                    trait  : ACB.AD.StringMode)
   is
   begin
      driver.connection.all.setStringMode (mode => trait);
   end set_trait_string_mode;


   -------------------------------
   --  set_trait_max_blob_size  --
   -------------------------------
   overriding
   procedure set_trait_max_blob_size (driver : Base_Driver;
                                      trait  : ACB.AD.BLOB_maximum)
   is
   begin
      driver.connection.all.setMaxBlobSize (maxsize => trait);
   end set_trait_max_blob_size;


   -----------------------
   --  standard_logger  --
   -----------------------
   overriding
   procedure command_standard_logger (driver : Base_Driver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction)
   is
   begin
      logger.standard_logger (logger => device, action => action);
   end command_standard_logger;


   ----------------------------
   --  detach_custom_logger  --
   ----------------------------
   overriding
   procedure detach_custom_logger (driver : Base_Driver)
   is
   begin
      logger.detach_custom_logger;
   end detach_custom_logger;


   ----------------------------
   --  attach_custom_logger  --
   ----------------------------
   overriding
   procedure attach_custom_logger
     (driver        : Base_Driver;
      logger_access : ALF.AL.BaseClass_Logger_access)
   is
   begin
      logger.attach_custom_logger (logger_access => logger_access);
   end attach_custom_logger;

   ------------------------------------------------------------------------
   --  PRIVATE ROUTINES NOT COVERED BY INTERFACES                        --
   ------------------------------------------------------------------------

   -----------
   --  SUS  --
   -----------
   function SUS (fixed : String) return ACB.AD.textual
   is
   begin
      return ACB.AD.SU.To_Unbounded_String (Source => fixed);
   end SUS;


   -----------
   --  USS  --
   -----------
   function USS (loose : ACB.AD.textual) return String
   is
   begin
      return ACB.AD.SU.To_String (Source => loose);
   end USS;


end AdaBase.Driver.Base;
