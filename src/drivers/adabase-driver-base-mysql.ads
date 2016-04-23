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
   function query (driver : MySQL_Driver; sql : String)
                   return AID.ASB.basic_statement;

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
                            username : String;
                            password : String;
                            socket   : String);

   overriding
   procedure basic_connect (driver   : out MySQL_Driver;
                            database : String;
                            username : String;
                            password : String;
                            hostname : String;
                            port     : PosixPort);

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

   function private_query (driver : MySQL_Driver; sql : String)
                           return ASM.MySQL_statement_access;

   overriding
   procedure initialize (Object : in out MySQL_Driver);

end AdaBase.Driver.Base.MySQL;
