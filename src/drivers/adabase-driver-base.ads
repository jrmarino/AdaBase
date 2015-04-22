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

with AdaBase.Connection.Base;
with AdaBase.Logger.Facility;
with AdaBase.Interfaces.Driver;

package AdaBase.Driver.Base is

   package ACB renames AdaBase.Connection.Base;
   package ALF renames AdaBase.Logger.Facility;
   package AID renames AdaBase.Interfaces.Driver;

   type Base_Driver is abstract new Base_Pure and AID.iDriver with private;

   overriding
   procedure command_standard_logger (driver : Base_Driver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction);

   overriding
   procedure detach_custom_logger (driver : Base_Driver);

   overriding
   procedure attach_custom_logger
     (driver        : Base_Driver;
      logger_access : ALF.AL.BaseClass_Logger_access);

   overriding
   function trait_autocommit     (driver : Base_Driver)
                                  return Boolean;

   overriding
   function trait_column_case    (driver : Base_Driver)
                                  return ACB.AD.CaseMode;

   overriding
   function trait_error_mode     (driver : Base_Driver)
                                  return ACB.AD.ErrorMode;

   overriding
   function trait_string_mode    (driver : Base_Driver)
                                  return ACB.AD.StringMode;

   overriding
   function trait_connected      (driver : Base_Driver)
                                  return Boolean;

   overriding
   function trait_driver         (driver : Base_Driver)
                                  return ACB.AD.textual;

   overriding
   function trait_client_info    (driver : Base_Driver)
                                  return ACB.AD.textual;

   overriding
   function trait_client_version (driver : Base_Driver)
                                  return ACB.AD.textual;

   overriding
   function trait_server_info    (driver : Base_Driver)
                                  return ACB.AD.textual;

   overriding
   function trait_server_version (driver : Base_Driver)
                                  return ACB.AD.textual;

   overriding
   function trait_max_blob_size    (driver : Base_Driver)
                                    return ACB.AD.BLOB_maximum;

   overriding
   procedure set_trait_autocommit  (driver : Base_Driver;
                                    trait  : Boolean);

   overriding
   procedure set_trait_column_case (driver : Base_Driver;
                                    trait  : ACB.AD.CaseMode);

   overriding
   procedure set_trait_error_mode  (driver : Base_Driver;
                                    trait  : ACB.AD.ErrorMode);

   overriding
   procedure set_trait_string_mode (driver : Base_Driver;
                                    trait  : ACB.AD.StringMode);

   overriding
   procedure set_trait_max_blob_size (driver : Base_Driver;
                                      trait  : ACB.AD.BLOB_maximum);

   overriding
   procedure query_clear_table       (driver : Base_Driver;
                                      table  : ACB.AD.textual);

   overriding
   procedure query_drop_table        (driver      : Base_Driver;
                                      tables      : ACB.AD.textual;
                                      when_exists : Boolean := False;
                                      cascade     : Boolean := False);

   overriding
   function execute (driver : Base_Driver; sql : ACB.AD.textual)
                     return ACB.AD.AffectedRows;

private

   logger : ALF.LogFacility;

   type Base_Driver is abstract new Base_Pure and AID.iDriver with
      record
         connection_active : Boolean := False;
         connection        : ACB.Base_Connection_Access;
         dialect           : ACB.AD.TDriver := ACB.AD.foundation;
      end record;

   function SUS (fixed : String) return ACB.AD.textual;
   function USS (loose : ACB.AD.textual) return String;

   procedure log_nominal (driver   : Base_Driver;
                          category : ACB.AD.LogCategory;
                          message  : ACB.AD.textual);

   procedure log_problem
     (driver     : Base_Driver;
      category   : ACB.AD.LogCategory;
      message    : ACB.AD.textual;
      pull_codes : Boolean := False;
      break      : Boolean := False);

end AdaBase.Driver.Base;
