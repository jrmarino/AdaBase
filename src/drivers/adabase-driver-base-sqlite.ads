--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Driver;
with AdaBase.Connection.Base.SQLite;
with AdaBase.Statement.Base.SQLite;

package AdaBase.Driver.Base.SQLite is

   package AID renames AdaBase.Interfaces.Driver;
   package ACS renames AdaBase.Connection.Base.SQLite;
   package ASS renames AdaBase.Statement.Base.SQLite;

   type SQLite_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   procedure disconnect (driver : out SQLite_Driver);

   overriding
   procedure commit (driver : SQLite_Driver);

   overriding
   procedure rollback (driver : SQLite_Driver);

   overriding
   function last_insert_id (driver : SQLite_Driver) return TraxID;

   overriding
   function last_sql_state (driver : SQLite_Driver) return TSqlState;

   overriding
   function last_driver_code (driver : SQLite_Driver) return DriverCodes;

   overriding
   function last_driver_message (driver : SQLite_Driver) return String;

   overriding
   function execute (driver : SQLite_Driver; sql : String)
                     return AffectedRows;

   overriding
   procedure basic_connect (driver   : out SQLite_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring);

   overriding
   procedure basic_connect (driver   : out SQLite_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
                            port     : PosixPort);

   overriding
   procedure query_drop_table  (driver      : SQLite_Driver;
                                tables      : String;
                                when_exists : Boolean := False;
                                cascade     : Boolean := False);

   overriding
   procedure query_clear_table (driver : SQLite_Driver;
                                table  : String);

   overriding
   procedure set_trait_multiquery_enabled  (driver : SQLite_Driver;
                                            trait  : Boolean);

   function query          (driver     : SQLite_Driver;
                            sql        : String)
                            return ASS.SQLite_statement;

   function prepare        (driver     : SQLite_Driver;
                            sql        : String)
                            return ASS.SQLite_statement;

   function query_select   (driver     : SQLite_Driver;
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
                            return ASS.SQLite_statement;

   function prepare_select (driver     : SQLite_Driver;
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
                            return ASS.SQLite_statement;

private

   backend : aliased ACS.SQLite_Connection;

   type SQLite_Driver is new Base_Driver and AID.iDriver with
      record
         local_connection : ACS.SQLite_Connection_Access := null;
         database : CT.Text := CT.blank;
      end record;

   procedure private_connect (driver   : out SQLite_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String    := blankstring;
                              socket   : String    := blankstring;
                              port     : PosixPort := portless);

   function private_statement (driver   : SQLite_Driver;
                               sql      : String;
                               prepared : Boolean)
                               return ASS.SQLite_statement;

   overriding
   procedure initialize (Object : in out SQLite_Driver);

end AdaBase.Driver.Base.SQLite;
