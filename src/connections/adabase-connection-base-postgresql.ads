--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Connection;
with AdaBase.Bindings.PostgreSQL;
with AdaBase.Results;
with Ada.Containers.Ordered_Maps;
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
                           port     : Posix_Port := portless);

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
   function SqlState       (conn : PostgreSQL_Connection) return SQL_State;

   overriding
   function driverMessage  (conn : PostgreSQL_Connection) return String;

   overriding
   function driverCode     (conn : PostgreSQL_Connection) return Driver_Codes;

   overriding
   function lastInsertID   (conn : PostgreSQL_Connection) return Trax_ID;

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
                                        return Affected_Rows;

   overriding
   procedure setTransactionIsolation (conn : out PostgreSQL_Connection;
                                      isolation : Trax_Isolation);


   ---------------------------------------------------
   --  SUBROUTINES PARTICULAR TO POSTGRESQL DRIVER  --
   ---------------------------------------------------
   function SqlState             (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access)
                                 return SQL_State;

   procedure discard_pgresult    (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access);

   function rows_in_result       (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access)
                                  return Affected_Rows;

   function rows_impacted        (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access)
                                  return Affected_Rows;

   function field_data_is_binary (conn : PostgreSQL_Connection;
                                  res  : BND.PGresult_Access;
                                  column_number : Natural) return Boolean;

   function prepare_statement (conn : PostgreSQL_Connection;
                               stmt : aliased out BND.PGresult_Access;
                               name : String;
                               sql  : String) return Boolean;

   function destroy_statement (conn : out PostgreSQL_Connection;
                               name : String) return Boolean;

   function direct_stmt_exec  (conn : out PostgreSQL_Connection;
                               stmt : aliased out BND.PGresult_Access;
                               sql  : String) return Boolean;

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

   function field_type    (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           column_number : Natural) return field_types;

   function field_table   (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           column_number : Natural) return String;

   function field_string  (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           row_number    : Natural;
                           column_number : Natural) return String;

   function field_binary  (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access;
                           row_number    : Natural;
                           column_number : Natural;
                           max_length    : Natural) return String;

   function driverMessage (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return String;

   function driverCode    (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return Driver_Codes;

   function markers_found (conn : PostgreSQL_Connection;
                           res  : BND.PGresult_Access) return Natural;

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

   type table_cell is record
      column_1 : CT.Text;
   end record;

   type data_type_rec is record
      data_type : field_types;
   end record;

   package table_map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Positive,
      Element_Type => table_cell);

   package type_map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Positive,
      Element_Type => data_type_rec);

   type PostgreSQL_Connection is new Base_Connection and AIC.iConnection
     with record
      info_description : String (1 .. 29) := "PostgreSQL 9.1+ native driver";
      prop_multiquery  : Boolean := False;
      dummy            : Boolean := False;
      handle           : aliased BND.PGconn_Access := null;
      cmd_sql_state    : SQL_State := stateless;
      cmd_rows_impact  : Affected_Rows := 0;

      --  For last insert id support using INSERT INTO ... RETURNING
      cmd_insert_return : Boolean := False;
      insert_return_val : Trax_ID := 0;

      --  Upon connection, dump tables and data types and store them
      tables            : table_map.Map;
      data_types        : type_map.Map;
   end record;

   function is_ipv4_or_ipv6 (teststr : String) return Boolean;
   function convert_version (pgsql_version : Natural) return CT.Text;
   function get_library_version return Natural;
   function convert_data_type (pg_type : String; category : Character;
                               typelen : Integer) return field_types;
   function refined_byte_type (byteX : field_types; constraint : String)
                               return field_types;

   procedure Initialize (conn : in out PostgreSQL_Connection);
   procedure private_execute (conn : out PostgreSQL_Connection; sql : String);
   procedure begin_transaction (conn : out PostgreSQL_Connection);
   procedure cache_table_names (conn : out PostgreSQL_Connection);
   procedure cache_data_types  (conn : out PostgreSQL_Connection);
   function piped_tables       (conn : PostgreSQL_Connection) return String;
   function select_last_val    (conn : PostgreSQL_Connection) return Trax_ID;
   function get_server_version (conn : PostgreSQL_Connection) return Natural;
   function get_server_info    (conn : PostgreSQL_Connection) return CT.Text;
   function within_transaction (conn : PostgreSQL_Connection) return Boolean;
   function connection_attempt_succeeded (conn : PostgreSQL_Connection)
                                          return Boolean;
   function private_select     (conn : PostgreSQL_Connection; sql : String)
                                return BND.PGresult_Access;

   overriding
   procedure finalize (conn : in out PostgreSQL_Connection);

end AdaBase.Connection.Base.PostgreSQL;
