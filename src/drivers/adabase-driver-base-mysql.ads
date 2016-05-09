--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Driver;
with AdaBase.Connection.Base.MySQL;
with AdaBase.Statement.Base.MySQL;

package AdaBase.Driver.Base.MySQL is

   package AID renames AdaBase.Interfaces.Driver;
   package ACM renames AdaBase.Connection.Base.MySQL;
   package ASM renames AdaBase.Statement.Base.MySQL;

   type MySQL_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   procedure disconnect (driver : out MySQL_Driver);

   overriding
   procedure commit (driver : MySQL_Driver);

   overriding
   procedure rollback (driver : MySQL_Driver);

   overriding
   function last_insert_id (driver : MySQL_Driver) return TraxID;

   overriding
   function last_sql_state (driver : MySQL_Driver) return TSqlState;

   overriding
   function last_driver_code (driver : MySQL_Driver) return DriverCodes;

   overriding
   function last_driver_message (driver : MySQL_Driver) return String;

   overriding
   function execute (driver : MySQL_Driver; sql : String)
                     return AffectedRows;

   function trait_protocol_compressed (driver : MySQL_Driver) return Boolean;
   function trait_multiquery_enabled  (driver : MySQL_Driver) return Boolean;
   function trait_query_buffers_used  (driver : MySQL_Driver) return Boolean;

   procedure set_trait_protocol_compressed (driver : MySQL_Driver;
                                            trait  : Boolean);
   procedure set_trait_multiquery_enabled  (driver : MySQL_Driver;
                                            trait  : Boolean);
   procedure set_trait_query_buffers_used  (driver : MySQL_Driver;
                                            trait  : Boolean);

   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring);

   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
                            port     : PosixPort);

   function query          (driver     : MySQL_Driver;
                            sql        : String)
                            return ASM.MySQL_statement;

   function prepare        (driver     : MySQL_Driver;
                            sql        : String)
                            return ASM.MySQL_statement;

   function query_select   (driver     : MySQL_Driver;
                            distinct   : Boolean := False;
                            tables     : String;
                            columns    : String;
                            conditions : String := blankstring;
                            groupby    : String := blankstring;
                            having     : String := blankstring;
                            order      : String := blankstring;
                            null_sort  : NullPriority := native;
                            limit      : TraxID := 0;
                            offset     : TraxID := 0)
                            return ASM.MySQL_statement;

   function prepare_select (driver     : MySQL_Driver;
                            distinct   : Boolean := False;
                            tables     : String;
                            columns    : String;
                            conditions : String := blankstring;
                            groupby    : String := blankstring;
                            having     : String := blankstring;
                            order      : String := blankstring;
                            null_sort  : NullPriority := native;
                            limit      : TraxID := 0;
                            offset     : TraxID := 0)
                            return ASM.MySQL_statement;

private

   backend : aliased ACM.MySQL_Connection;

   type MySQL_Driver is new Base_Driver and AID.iDriver with
      record
         local_connection : ACM.MySQL_Connection_Access := null;
         database : CT.Text := CT.blank;
      end record;

   procedure private_connect (driver   : out MySQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String    := blankstring;
                              socket   : String    := blankstring;
                              port     : PosixPort := portless);

   function private_query   (driver : MySQL_Driver; sql : String)
                             return ASM.MySQL_statement;

   function private_prepare (driver : MySQL_Driver; sql : String)
                             return ASM.MySQL_statement;

   overriding
   procedure initialize (Object : in out MySQL_Driver);

end AdaBase.Driver.Base.MySQL;
