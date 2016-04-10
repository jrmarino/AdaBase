--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Connection.Base;
with AdaBase.Logger.Facility;
with AdaBase.Interfaces.Driver;
with Ada.Strings.Unbounded;

package AdaBase.Driver.Base is

   package SU  renames Ada.Strings.Unbounded;
   package ACB renames AdaBase.Connection.Base;
   package ALF renames AdaBase.Logger.Facility;
   package AID renames AdaBase.Interfaces.Driver;

   subtype drvtext is SU.Unbounded_String;

   type Base_Driver is abstract new Base_Pure and AID.iDriver with private;

   blank : constant drvtext := SU.Null_Unbounded_String;

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
                                  return CaseMode;

   overriding
   function trait_error_mode     (driver : Base_Driver)
                                  return ErrorMode;

   overriding
   function trait_connected      (driver : Base_Driver)
                                  return Boolean;

   overriding
   function trait_driver         (driver : Base_Driver)
                                  return String;

   overriding
   function trait_client_info    (driver : Base_Driver)
                                  return String;

   overriding
   function trait_client_version (driver : Base_Driver)
                                  return String;

   overriding
   function trait_server_info    (driver : Base_Driver)
                                  return String;

   overriding
   function trait_server_version (driver : Base_Driver)
                                  return String;

   overriding
   function trait_max_blob_size    (driver : Base_Driver)
                                    return BLOB_maximum;

   overriding
   procedure set_trait_autocommit  (driver : Base_Driver;
                                    trait  : Boolean);

   overriding
   procedure set_trait_column_case (driver : Base_Driver;
                                    trait  : CaseMode);

   overriding
   procedure set_trait_error_mode  (driver : Base_Driver;
                                    trait  : ErrorMode);

   overriding
   procedure set_trait_max_blob_size (driver : Base_Driver;
                                      trait  : BLOB_maximum);

   overriding
   procedure query_clear_table       (driver : Base_Driver;
                                      table  : String);

   overriding
   procedure query_drop_table        (driver      : Base_Driver;
                                      tables      : String;
                                      when_exists : Boolean := False;
                                      cascade     : Boolean := False);

private

   logger : aliased ALF.LogFacility;

   type Base_Driver is abstract new Base_Pure and AID.iDriver with
      record
         connection_active : Boolean := False;
         connection        : ACB.Base_Connection_Access;
         dialect           : TDriver := foundation;
      end record;

   function SUS (fixed : String)  return drvtext;
   function USS (loose : drvtext) return String;

   procedure log_nominal (driver   : Base_Driver;
                          category : LogCategory;
                          message  : drvtext);

   procedure log_problem
     (driver     : Base_Driver;
      category   : LogCategory;
      message    : drvtext;
      pull_codes : Boolean := False;
      break      : Boolean := False);

   overriding
   procedure initialize (Object : in out Base_Driver) is null;


end AdaBase.Driver.Base;
