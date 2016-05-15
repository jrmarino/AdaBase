--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Driver;
with AdaBase.Connection.Base.SQLite;
with AdaBase.Statement.Base.PostgreSQL;

package AdaBase.Driver.Base.PostgreSQL is

   package AID renames AdaBase.Interfaces.Driver;
   package CON renames AdaBase.Connection.Base.PostgreSQL;
   package SMT renames AdaBase.Statement.Base.PostgreSQL;

   type PostgreSQL_Driver is new Base_Driver and AID.iDriver with private;

   overriding
   procedure disconnect (driver : out PostgreSQL_Driver);

   overriding
   procedure commit (driver : PostgreSQL_Driver);

   overriding
   procedure rollback (driver : PostgreSQL_Driver);

   overriding
   function last_insert_id (driver : PostgreSQL_Driver) return TraxID;

   overriding
   function last_sql_state (driver : PostgreSQL_Driver) return TSqlState;

   overriding
   function last_driver_code (driver : PostgreSQL_Driver) return DriverCodes;

   overriding
   function last_driver_message (driver : PostgreSQL_Driver) return String;

   overriding
   function execute (driver : PostgreSQL_Driver; sql : String)
                     return AffectedRows;

   overriding
   procedure basic_connect (driver   : out PostgreSQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            socket   : String := blankstring);

   overriding
   procedure basic_connect (driver   : out PostgreSQL_Driver;
                            database : String;
                            username : String := blankstring;
                            password : String := blankstring;
                            hostname : String := blankstring;
                            port     : PosixPort);

   overriding
   procedure query_drop_table  (driver      : PostgreSQL_Driver;
                                tables      : String;
                                when_exists : Boolean := False;
                                cascade     : Boolean := False);

   overriding
   procedure query_clear_table (driver : PostgreSQL_Driver;
                                table  : String);

   overriding
   procedure set_trait_multiquery_enabled  (driver : PostgreSQL_Driver;
                                            trait  : Boolean);

   function query          (driver     : PostgreSQL_Driver;
                            sql        : String)
                            return SMT.PostgreSQL_statement;

   function prepare        (driver     : PostgreSQL_Driver;
                            sql        : String)
                            return SMT.PostgreSQL_statement;

   function query_select   (driver     : PostgreSQL_Driver;
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
                            return SMT.PostgreSQL_statement;

   function prepare_select (driver     : PostgreSQL_Driver;
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
                            return SMT.PostgreSQL_statement;

private

   backend : aliased CON.PostgreSQL_Connection;

   type PostgreSQL_Driver is new Base_Driver and AID.iDriver with
      record
         local_connection : CON.PostgreSQL_Connection_Access := null;
         database : CT.Text := CT.blank;
      end record;

   procedure private_connect (driver   : out PostgreSQL_Driver;
                              database : String;
                              username : String;
                              password : String;
                              hostname : String    := blankstring;
                              socket   : String    := blankstring;
                              port     : PosixPort := portless);

   function private_statement (driver   : PostgreSQL_Driver;
                               sql      : String;
                               prepared : Boolean)
                               return SMT.PostgreSQL_statement;

   overriding
   procedure initialize (Object : in out PostgreSQL_Driver);

end AdaBase.Driver.Base.PostgreSQL;
