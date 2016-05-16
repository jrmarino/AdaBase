--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Connection;
with AdaBase.Bindings.PostgreSQL;
with AdaBase.Results;
with Ada.Exceptions;

package AdaBase.Connection.Base.PostgreSQL is

   package AIC renames AdaBase.Interfaces.Connection;
   package BND renames AdaBase.Bindings.PostgreSQL;
   package AR  renames AdaBase.Results;
   package EX  renames Ada.Exceptions;

   type PostgreSQL_Connection is new Base_Connection and AIC.iConnection
   with private;
   type PostgreSQL_Connection_Access is access all PostgreSQL_Connection;

   -----------------------------------------
   --  SUBROUTINES REQUIRED BY INTERFACE  --
   -----------------------------------------

   overriding
   procedure connect      (conn     : out PostgreSQL_Connection;
                           database : String;
                           username : String := blankstring;
                           password : String := blankstring;
                           hostname : String := blankstring;
                           socket   : String := blankstring;
                           port     : PosixPort := portless);

   overriding
   procedure setCompressed (conn : out PostgreSQL_Connection;
                            compressed : Boolean);

   overriding
   function compressed     (conn : PostgreSQL_Connection) return Boolean;

   overriding
   procedure setUseBuffer  (conn : out PostgreSQL_Connection;
                            buffered : Boolean);

   overriding
   function useBuffer      (conn : PostgreSQL_Connection) return Boolean;

   overriding
   procedure setMultiQuery (conn : out PostgreSQL_Connection;
                            multiple : Boolean);

   overriding
   function multiquery     (conn : PostgreSQL_Connection) return Boolean;

   overriding
   procedure setAutoCommit (conn : out PostgreSQL_Connection; auto : Boolean);

   overriding
   function description    (conn : PostgreSQL_Connection) return String;

   overriding
   function SqlState       (conn : PostgreSQL_Connection) return TSqlState;

   overriding
   function driverMessage  (conn : PostgreSQL_Connection) return String;

   overriding
   function driverCode     (conn : PostgreSQL_Connection) return DriverCodes;

   overriding
   function lastInsertID   (conn : PostgreSQL_Connection) return TraxID;

   overriding
   procedure commit        (conn : out PostgreSQL_Connection);

   overriding
   procedure rollback      (conn : out PostgreSQL_Connection);

   overriding
   procedure disconnect    (conn : out PostgreSQL_Connection);

   overriding
   procedure execute       (conn : out PostgreSQL_Connection; sql : String);

   overriding
   function rows_affected_by_execution (conn : PostgreSQL_Connection)
                                        return AffectedRows;

   overriding
   procedure setTransactionIsolation (conn : out PostgreSQL_Connection;
                                      isolation : TransIsolation);


   ---------------------------------------------------
   --  SUBROUTINES PARTICULAR TO POSTGRESQL DRIVER  --
   ---------------------------------------------------
   function SqlState (conn : PostgreSQL_Connection; res : BND.PGresult_Access)
                      return TSqlState;

   procedure discard_pgresult    (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access);

   function rows_in_result       (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access)
                                  return AffectedRows;

   function rows_impacted        (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access)
                                  return AffectedRows;

   function field_data_is_binary (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access;
                                  column_number : Natural) return Boolean;

   function fields_count  (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return Natural;

   function field_is_null (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           row_number    : Natural;
                           column_number : Natural) return Boolean;

   function field_length  (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           row_number    : Natural;
                           column_number : Natural) return Natural;

   function field_name    (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           column_number : Natural) return String;

   function driverMessage (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return String;

   function driverCode    (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return DriverCodes;

   ------------------
   --  EXCEPTIONS  --
   ------------------

   UNSUPPORTED_BY_PGSQL  : exception;
   NOT_WHILE_CONNECTED   : exception;
   DISCONNECT_FAILED     : exception;
   CONNECT_FAILED        : exception;
   AUTOCOMMIT_FAIL       : exception;
   TRAXISOL_FAIL         : exception;
   TRAX_BEGIN_FAIL       : exception;
   ROLLBACK_FAIL         : exception;
   COMMIT_FAIL           : exception;
   QUERY_FAIL            : exception;
   METADATA_FAIL         : exception;
   STMT_NOT_VALID        : exception;
   STMT_RESET_FAIL       : exception;
   STMT_FETCH_FAIL       : exception;

private

   type PostgreSQL_Connection is new Base_Connection and AIC.iConnection
     with record
      info_description : String (1 .. 29) := "PostgreSQL 9.1+ native driver";
      prop_multiquery  : Boolean := False;
      dummy            : Boolean := False;
      handle           : aliased BND.PGconn_Access := null;
      cmd_sql_state    : TSqlState := stateless;
      cmd_rows_impact  : AffectedRows := 0;
   end record;

   function is_ipv4_or_ipv6 (teststr : String) return Boolean;
   function convert_version (pgsql_version : Natural) return CT.Text;
   function get_library_version return Natural;

   procedure Initialize (conn : in out PostgreSQL_Connection);
   procedure private_execute (conn : out PostgreSQL_Connection; sql : String);
   procedure begin_transaction (conn : out PostgreSQL_Connection);
   function get_server_version (conn : PostgreSQL_Connection) return Natural;
   function get_server_info    (conn : PostgreSQL_Connection) return CT.Text;
   function connection_attempt_succeeded (conn : PostgreSQL_Connection)
                                          return Boolean;

   overriding
   procedure finalize (conn : in out PostgreSQL_Connection);

end AdaBase.Connection.Base.PostgreSQL;
