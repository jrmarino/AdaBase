--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with AdaBase.Interfaces.Connection;
with AdaBase.Bindings.SQLite;
with Ada.Exceptions;

package AdaBase.Connection.Base.SQLite is

   package AIC renames AdaBase.Interfaces.Connection;
   package BND renames AdaBase.Bindings.SQLite;
   package EX  renames Ada.Exceptions;

   type SQLite_Connection is new Base_Connection and AIC.iConnection
   with private;
   type SQLite_Connection_Access is access all SQLite_Connection;

   -----------------------------------------
   --  SUBROUTINES REQUIRED BY INTERFACE  --
   -----------------------------------------

   overriding
   procedure connect      (conn     : out SQLite_Connection;
                           database : String;
                           username : String := blankstring;
                           password : String := blankstring;
                           hostname : String := blankstring;
                           socket   : String := blankstring;
                           port     : PosixPort := portless);

   overriding
   procedure setCompressed (conn : out SQLite_Connection; compressed : Boolean);

   overriding
   function compressed     (conn : SQLite_Connection) return Boolean;

   overriding
   procedure setUseBuffer  (conn : out SQLite_Connection; buffered : Boolean);

   overriding
   function useBuffer      (conn : SQLite_Connection) return Boolean;

   overriding
   procedure setMultiQuery (conn : out SQLite_Connection; multiple : Boolean);

   overriding
   function multiquery     (conn : SQLite_Connection) return Boolean;

   overriding
   procedure setAutoCommit (conn : out SQLite_Connection; auto : Boolean);

   overriding
   function description   (conn : SQLite_Connection) return String;

   overriding
   function SqlState      (conn : SQLite_Connection) return TSqlState;

   overriding
   function driverMessage (conn : SQLite_Connection) return String;

   overriding
   function driverCode    (conn : SQLite_Connection) return DriverCodes;

   overriding
   function lastInsertID  (conn : SQLite_Connection) return TraxID;

   overriding
   procedure commit       (conn : out SQLite_Connection);

   overriding
   procedure rollback     (conn : out SQLite_Connection);

   overriding
   procedure disconnect   (conn : out SQLite_Connection);

   overriding
   procedure execute      (conn : out SQLite_Connection; sql : String);

   overriding
   function rows_affected_by_execution (conn : SQLite_Connection)
                                        return AffectedRows;

   overriding
   procedure setTransactionIsolation (conn : out SQLite_Connection;
                                      isolation : TransIsolation);

   -----------------------------------------------
   --  SUBROUTINES PARTICULAR TO SQLITE DRIVER  --
   -----------------------------------------------

   function prepare_statement (conn : SQLite_Connection;
                               stmt : aliased out BND.sqlite3_stmt_Access;
                               sql  : String) return Boolean;

   function prep_markers_found (conn : SQLite_Connection;
                                stmt : BND.sqlite3_stmt_Access) return Natural;

   function fields_in_result (conn : SQLite_Connection;
                              stmt : BND.sqlite3_stmt_Access) return Natural;

   function field_database (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String;

   function field_table    (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String;

   function field_name     (conn  : SQLite_Connection;
                            stmt  : BND.sqlite3_stmt_Access;
                            index : Natural) return String;

   procedure get_field_meta_data (conn  : SQLite_Connection;
                                  stmt  : BND.sqlite3_stmt_Access;
                                  database  : String;
                                  table     : String;
                                  column    : String;
                                  data_type : out BND.enum_field_types;
                                  nullable  : out Boolean);

   procedure reset_prep_stmt (conn  : SQLite_Connection;
                              stmt  : BND.sqlite3_stmt_Access);


   ------------------
   --  EXCEPTIONS  --
   ------------------

   UNSUPPORTED_BY_SQLITE : exception;
   NOT_WHILE_CONNECTED   : exception;
   DISCONNECT_FAILED     : exception;
   CONNECT_FAILED        : exception;
   AUTOCOMMIT_FAIL       : exception;
   TRAXISOL_FAIL         : exception;
   ROLLBACK_FAIL         : exception;
   COMMIT_FAIL           : exception;
   QUERY_FAIL            : exception;
   METADATA_FAIL         : exception;
   STMT_NOT_VALID        : exception;
   STMT_RESET_FAIL       : exception;

private

   type SQLite_Connection is new Base_Connection and AIC.iConnection
     with record
      info_description : String (1 .. 21) := "SQLite3 native driver";
      in_transaction   : Boolean := False;
      handle           : aliased BND.sqlite3_Access := null;
   end record;

   function PUTF82S (cstr : BND.ICS.chars_ptr) return String;

   procedure begin_transaction (conn : out SQLite_Connection);

   procedure Initialize (conn : in out SQLite_Connection);

   overriding
   procedure finalize (conn : in out SQLite_Connection);

   procedure private_execute (conn : SQLite_Connection; sql : String);


--     type fldlen is array (Positive range <>) of Natural;
--     type fetch_status is (success, truncated, spent, error);
--
--     overriding
--     procedure setTransactionIsolation (conn      : out SQLite_Connection;
--                                        isolation :     TransIsolation);
--
--     procedure use_result   (conn : SQLite_Connection;
--                             result_handle : out ABM.SQLite_RES_Access);
--
--     procedure free_result  (conn : SQLite_Connection;
--                             result_handle : out ABM.SQLite_RES_Access);
--
--     procedure store_result (conn : SQLite_Connection;
--                             result_handle : out ABM.SQLite_RES_Access);
--
--     function rows_in_result (conn : SQLite_Connection;
--                              result_handle : ABM.SQLite_RES_Access)
--                              return AffectedRows;
--
--     function fetch_field    (conn : SQLite_Connection;
--                              result_handle : ABM.SQLite_RES_Access)
--                              return ABM.SQLite_FIELD_Access;
--
--     function fetch_row      (conn : SQLite_Connection;
--                              result_handle : ABM.SQLite_RES_Access)
--                              return ABM.SQLite_ROW_access;
--
--     function fetch_next_set (conn : SQLite_Connection) return Boolean;
--
--     function fetch_lengths  (conn : SQLite_Connection;
--                              result_handle : ABM.SQLite_RES_Access;
--                              num_columns   : Positive) return fldlen;
--
--
--
--     -----------------------------------------------------------------------
--     --  PREPARED STATEMENT FUNCTIONS                                     --
--     -----------------------------------------------------------------------
--
--     function prep_bind_parameters (conn : SQLite_Connection;
--                                    stmt : ABM.SQLite_STMT_Access;
--                                    bind : out ABM.SQLite_BIND_Array)
--                                    return Boolean;
--
--     function prep_bind_result     (conn : SQLite_Connection;
--                                    stmt : ABM.SQLite_STMT_Access;
--                                    bind : out ABM.SQLite_BIND_Array)
--                                    return Boolean;
--
--     function prep_execute         (conn : SQLite_Connection;
--                                    stmt : ABM.SQLite_STMT_Access)
--                                    return Boolean;
--
--     function prep_rows_in_result  (conn : SQLite_Connection;
--                                    stmt : ABM.SQLite_STMT_Access)
--                                    return AffectedRows;
--
--     function prep_fetch_bound     (conn : SQLite_Connection;
--                                    stmt : ABM.SQLite_STMT_Access)
--                                    return fetch_status;
--
--     function prep_close_statement (conn : SQLite_Connection;
--                                    stmt : ABM.SQLite_STMT_Access)
--                                    return Boolean;
--
--     function prep_rows_affected_by_execution (conn : SQLite_Connection;
--                                               stmt : ABM.SQLite_STMT_Access)
--                                               return AffectedRows;
--
--  private
--
--     function S2P (S : CT.Text) return ABM.ICS.chars_ptr;
--     function S2P (S : String)  return ABM.ICS.chars_ptr;
--
--     procedure set_character_set (conn : MySQL_Connection);
--


end AdaBase.Connection.Base.SQLite;
