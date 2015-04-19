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

with AdaBase.Statement;
with AdaBase.Logger.Facility;
with AdaBase.DataTypes;

package AdaBase.Interfaces.Driver is

   type iDriver is interface;

   package AD renames AdaBase.DataTypes;
   package AS renames AdaBase.Statement;
   package ALF renames AdaBase.Logger.Facility;

   procedure disconnect (driver : out iDriver) is null;
   procedure commit     (driver : iDriver) is null;
   procedure rollback   (driver : iDriver) is null;

   --  Only common traits are in interface.  There might also be
   --  driver-specific traits, but those are found in driver specifications

   function trait_autocommit     (driver : iDriver)
                                  return Boolean is abstract;
   function trait_column_case    (driver : iDriver)
                                  return AD.CaseMode is abstract;
   function trait_error_mode     (driver : iDriver)
                                  return AD.ErrorMode is abstract;
   function trait_string_mode    (driver : iDriver)
                                  return AD.StringMode is abstract;
   function trait_connected      (driver : iDriver)
                                  return Boolean is abstract;
   function trait_max_blob_size  (driver : iDriver)
                                  return AD.BLOB_maximum is abstract;
   function trait_driver         (driver : iDriver)
                                  return AD.textual is abstract;
   function trait_client_info    (driver : iDriver)
                                  return AD.textual is abstract;
   function trait_client_version (driver : iDriver)
                                  return AD.textual is abstract;
   function trait_server_info    (driver : iDriver)
                                  return AD.textual is abstract;
   function trait_server_version (driver : iDriver)
                                  return AD.textual is abstract;

   procedure set_trait_autocommit    (driver : iDriver;
                                      trait  : Boolean) is null;
   procedure set_trait_column_case   (driver : iDriver;
                                      trait  : AD.CaseMode) is null;
   procedure set_trait_error_mode    (driver : iDriver;
                                      trait  : AD.ErrorMode) is null;
   procedure set_trait_string_mode   (driver : iDriver;
                                      trait  : AD.StringMode) is null;
   procedure set_trait_max_blob_size (driver : iDriver;
                                      trait  : AD.BLOB_maximum) is null;

   function last_insert_id       (driver : iDriver)
                                  return AD.TraxID is abstract;

   function last_sql_state       (driver : iDriver)
                                  return AD.TSqlState is abstract;

   function last_error_info (driver : iDriver)
                             return AD.Error_Info is abstract;

   function query_literal (driver : iDriver;
                           sql    : AD.textual)
                           return  AS.Base'Class is abstract;

   procedure command_standard_logger (driver : iDriver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction) is null;

   procedure detach_custom_logger (driver : iDriver) is null;
   procedure attach_custom_logger
     (driver        : iDriver;
      logger_access : ALF.AL.BaseClass_Logger_access) is null;

end AdaBase.Interfaces.Driver;
