--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with CommonText;
with AdaBase.Connection.Base;
with AdaBase.Logger.Facility;
with AdaBase.Interfaces.Driver;

package AdaBase.Driver.Base is

   package CT  renames CommonText;
   package ACB renames AdaBase.Connection.Base;
   package ALF renames AdaBase.Logger.Facility;
   package AID renames AdaBase.Interfaces.Driver;

   type Base_Driver is abstract new Base_Pure and AID.iDriver with private;

   overriding
   procedure command_standard_logger (driver : Base_Driver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction);

   overriding
   procedure set_logger_filename (driver  : Base_Driver; filename : String);

   overriding
   procedure detach_custom_logger (driver : Base_Driver);

   overriding
   procedure attach_custom_logger
     (driver        : Base_Driver;
      logger_access : ALF.AL.BaseClass_Logger_access);

   overriding
   procedure disconnect          (driver : out Base_Driver);

   overriding
   procedure rollback            (driver : Base_Driver);

   overriding
   procedure commit              (driver : Base_Driver);

   overriding
   function last_insert_id       (driver : Base_Driver) return Trax_ID;

   overriding
   function last_sql_state       (driver : Base_Driver) return SQL_State;

   overriding
   function last_driver_code     (driver : Base_Driver) return Driver_Codes;

   overriding
   function last_driver_message  (driver : Base_Driver) return String;

   overriding
   procedure basic_connect       (driver   : out Base_Driver;
                                  database : String;
                                  username : String := blankstring;
                                  password : String := blankstring;
                                  socket   : String := blankstring);

   overriding
   procedure basic_connect       (driver   : out Base_Driver;
                                  database : String;
                                  username : String := blankstring;
                                  password : String := blankstring;
                                  hostname : String := blankstring;
                                  port     : Posix_Port);

   overriding
   function trait_autocommit     (driver : Base_Driver)
                                  return Boolean;

   overriding
   function trait_column_case    (driver : Base_Driver)
                                  return Case_Modes;

   overriding
   function trait_error_mode     (driver : Base_Driver)
                                  return Error_Modes;

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
                                    return BLOB_Maximum;

   overriding
   function trait_multiquery_enabled (driver : Base_Driver)
                                      return Boolean;

   overriding
   procedure set_trait_multiquery_enabled (driver : Base_Driver;
                                           trait  : Boolean);

   overriding
   procedure set_trait_autocommit  (driver : Base_Driver;
                                    trait  : Boolean);

   overriding
   procedure set_trait_column_case (driver : Base_Driver;
                                    trait  : Case_Modes);

   overriding
   procedure set_trait_error_mode  (driver : Base_Driver;
                                    trait  : Error_Modes);

   overriding
   procedure set_trait_max_blob_size (driver : Base_Driver;
                                      trait  : BLOB_Maximum);

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
         connection        : ACB.Base_Connection_Access;
         connection_active : Boolean     := False;
         dialect           : Driver_Type := foundation;
         database          : CT.Text     := CT.blank;
      end record;

   procedure log_nominal (driver   : Base_Driver;
                          category : Log_Category;
                          message  : CT.Text);

   procedure log_problem
     (driver     : Base_Driver;
      category   : Log_Category;
      message    : CT.Text;
      pull_codes : Boolean := False;
      break      : Boolean := False);

   overriding
   procedure initialize (Object : in out Base_Driver) is null;

   function assembly_common_select (distinct   : Boolean;
                                    tables     : String;
                                    columns    : String;
                                    conditions : String;
                                    groupby    : String;
                                    having     : String;
                                    order      : String) return String;

   procedure private_connect (driver   : out Base_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String     := blankstring;
                              socket   : String     := blankstring;
                              port     : Posix_Port := portless) is null;

end AdaBase.Driver.Base;
