--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Statement.Base;
with AdaBase.Logger.Facility;

package AdaBase.Interfaces.Driver is

   type iDriver is interface;

   package ASB renames AdaBase.Statement.Base;
   package ALF renames AdaBase.Logger.Facility;

   procedure disconnect (driver : out iDriver) is null;
   procedure commit     (driver : iDriver) is null;
   procedure rollback   (driver : iDriver) is null;

   --  Only common traits are in interface.  There might also be
   --  driver-specific traits, but those are found in driver specifications

   function trait_autocommit     (driver : iDriver)
                                  return Boolean is abstract;
   function trait_column_case    (driver : iDriver)
                                  return CaseMode is abstract;
   function trait_error_mode     (driver : iDriver)
                                  return ErrorMode is abstract;
   function trait_connected      (driver : iDriver)
                                  return Boolean is abstract;
   function trait_max_blob_size  (driver : iDriver)
                                  return BLOB_maximum is abstract;
   function trait_driver         (driver : iDriver)
                                  return String is abstract;
   function trait_client_info    (driver : iDriver)
                                  return String is abstract;
   function trait_client_version (driver : iDriver)
                                  return String is abstract;
   function trait_server_info    (driver : iDriver)
                                  return String is abstract;
   function trait_server_version (driver : iDriver)
                                  return String is abstract;

   procedure set_trait_autocommit    (driver : iDriver;
                                      trait  : Boolean) is null;
   procedure set_trait_column_case   (driver : iDriver;
                                      trait  : CaseMode) is null;
   procedure set_trait_error_mode    (driver : iDriver;
                                      trait  : ErrorMode) is null;
   procedure set_trait_max_blob_size (driver : iDriver;
                                      trait  : BLOB_maximum) is null;

   function last_insert_id       (driver : iDriver)
                                  return TraxID is abstract;

   function last_sql_state       (driver : iDriver)
                                  return TSqlState is abstract;

   function last_driver_code     (driver : iDriver)
                                  return DriverCodes is abstract;

   function last_driver_message  (driver : iDriver)
                                  return String is abstract;

   function execute              (driver : iDriver;
                                  sql    : String)
                                  return AffectedRows is abstract;

   procedure command_standard_logger (driver : iDriver;
                                      device : ALF.TLogger;
                                      action : ALF.TAction) is null;

   procedure set_logger_filename (driver  : iDriver;
                                  filename : String) is null;

   procedure detach_custom_logger (driver : iDriver) is null;
   procedure attach_custom_logger
     (driver        : iDriver;
      logger_access : ALF.AL.BaseClass_Logger_access) is null;


   ------------------------------------------------------------------------
   -- QUERIES                                                            --
   ------------------------------------------------------------------------
   procedure query_clear_table   (driver : iDriver;
                                  table  : String) is abstract;

   procedure query_drop_table    (driver      : iDriver;
                                  tables      : String;
                                  when_exists : Boolean := False;
                                  cascade     : Boolean := False) is abstract;

   --  This query functions returning statements were intended to part of the
   --  interface, but the return type also must be an interface (iStatement)
   --  and I can't see how to accomplish that.  For now, remove them from the
   --  interface requirements and just implement them on each specific driver.
   --
   --  function query                (driver : iDriver;
   --                                 sql    : String)
   --                                 return ASB.basic_statement is abstract;
   --
   --  function query_select         (driver      : iDriver;
   --                                 distinct    : Boolean := False;
   --                                 tables      : String;
   --                                 columns     : String;
   --                                 conditions  : String := blankstring;
   --                                 groupby     : String := blankstring;
   --                                 having      : String := blankstring;
   --                                 order       : String := blankstring;
   --                                 limit       : TraxID := 0;
   --                                 offset      : TraxID := 0)
   --                                 return ASB.basic_statement is abstract;

   ------------------------------------------------------------------------
   -- CONNECTIONS                                                        --
   ------------------------------------------------------------------------

   --  These are guaranteed to work (at least one of the them, but the
   --  individual driver could define a driver-specific version as well.
   --  Checked "trait_connected" to determine if connection was successful

   procedure basic_connect (driver   : out iDriver;
                            database : String;
                            username : String;
                            password : String;
                            socket   : String) is null;

   procedure basic_connect (driver   : out iDriver;
                            database : String;
                            username : String;
                            password : String;
                            hostname : String;
                            port     : PosixPort) is null;

end AdaBase.Interfaces.Driver;
